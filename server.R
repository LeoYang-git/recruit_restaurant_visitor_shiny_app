#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Input dynamic UI --------------------------------------------------------------
  # 1. Cuisine Selection --------------------------
  ls_cuisine <- list(unique(raw_air_store_info$air_genre_name),
                     unique(raw_hpg_store_info$hpg_genre_name))
  names(ls_cuisine) <- c("AIR","HPG")
  output$cuisineselect <- renderUI({
    cuisine_options <- switch(input$datasource,
                              HPG = ls_cuisine$HPG,
                              AIR = ls_cuisine$AIR)
    
    selectizeInput("cuisinechosen", "Type of Cuisine",
                   choices = cuisine_options,
                   multiple = TRUE)
  })
  
  # 2. Leaflet output -----------------------------------
  rv_rest_in_cuisine <- reactive({
    df_temp <- switch(input$datasource,
                      AIR = raw_air_store_info %>% rename(cuisine = air_genre_name),
                      HPG= raw_hpg_store_info %>% rename(cuisine = hpg_genre_name))
    
    if(is.null(input$cuisinechosen)){
      df_temp
    }else{
      df_temp %>% filter(cuisine %in% input$cuisinechosen)
    }
  })
  
  output$map <- renderLeaflet({
    view_lat <- rv_rest_in_cuisine()$latitude %>% mean
    view_lon <- rv_rest_in_cuisine()$longitude %>% mean
    
    leaflet( rv_rest_in_cuisine()) %>%
      setView(lng = view_lon, lat = view_lat, zoom = 6) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude)
  })
  
  # Reactive Value DF ---------------------------------------------
  rv_reservation <- reactive({
    req(input$datasource,input$inscope_daterange)
    
    df_temp <- switch(input$datasource,
                      AIR = df_res_air,
                      HPG = df_res_hpg)
    
    if(length(input$cuisinechosen)> 0){
      df_resv <- df_temp %>% 
        filter(visit_d >= as.Date(input$inscope_daterange[1]),
               visit_d <= as.Date(input$inscope_daterange[2])) %>% 
        filter(cuisine %in% input$cuisinechosen)
    }else{
      df_resv <- df_temp %>% 
        filter(visit_d >= as.Date(input$inscope_daterange[1]),
               visit_d <= as.Date(input$inscope_daterange[2]))
    }
    
    df_resv 
  })
  
  rv_visit <- reactive({
    req(input$datasource == "AIR")
    
    if(length(input$cuisinechosen)> 0){
      df_visit <- df_vis_air %>% 
        filter(visit_d >= as.Date(input$inscope_daterange[1]),
               visit_d <= as.Date(input$inscope_daterange[2])) %>% 
        filter(cuisine %in% input$cuisinechosen)
    }else{
      df_visit <- df_vis_air %>% 
        filter(visit_d >= as.Date(input$inscope_daterange[1]),
               visit_d <= as.Date(input$inscope_daterange[2]))
    }
    df_visit 
  })
  
  # Output ------------------------------------------------------------------
  # 1. All time reservation and visit plot -------------------
  output$all_time_visits <- renderPlotly({
    store_id_col <- switch(input$datasource,
                           AIR = "air_store_id",
                           HPG = "hpg_store_id")
    
    df_rev_temp <- rv_reservation() %>% 
      group_by(across(all_of(c("visit_d",store_id_col)))) %>% 
      summarise(daily_visit = sum(reserve_visitors)) %>% 
      group_by(visit_d) %>% 
      summarise(median_visit = median(daily_visit),
                mean_visit = mean(daily_visit),
                total_visit = sum(daily_visit))
    fig <- plot_ly() %>% 
      add_trace(data = df_rev_temp, x = ~visit_d, y = ~ total_visit, name = "Reservation", mode = "lines",type = "scatter") %>% 
      layout(title = "Total Reservation By Day",
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Number of Reservation'))
    
    if(input$datasource == "AIR"){
    df_vis_temp <- rv_visit()  %>% 
      group_by(visit_d,air_store_id) %>% 
      summarise(daily_visit = sum(visitors)) %>% 
      group_by(visit_d) %>% 
      summarise(median_visit = median(daily_visit),
                mean_visit = mean(daily_visit),
                total_visit = sum(daily_visit))
    
    fig <- fig %>% add_trace(data = df_vis_temp, x = ~visit_d, y = ~ total_visit, name = "Visits", mode = "lines",type = "scatter") %>% 
      layout(title = "Total Reservation/Visit By Day",
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Number of Reservation/Visit'))
    }
    
    fig
  })
  
  # 2. Popular Day of Week ------------------------------------------------------
  days_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  output$popular_day_of_week <- renderPlotly({
    df_resv_dow <- rv_reservation() %>% 
      group_by(day_of_week) %>% 
      summarise(total_visit = sum(reserve_visitors)) %>% 
      ungroup() %>% 
      mutate(day_of_week = factor(day_of_week, levels = days_of_week))
    
    fig <- df_resv_dow %>% 
      plot_ly( x = ~ day_of_week, y = ~total_visit, name = "Total Reservation", type = "bar" ) %>% 
      layout(title = 'Total Reservation Per Day of Week', 
             xaxis = list(title = 'Day of Week'), 
             yaxis = list(title = 'Number of Reservation'))
    
    if(input$datasource == "AIR"){
      df_visit_dow <- rv_visit() %>% 
        group_by(day_of_week) %>% 
        summarise(total_visit = sum(visitors)) %>% 
        ungroup() %>% 
        mutate(day_of_week = factor(day_of_week, levels = days_of_week)) %>% 
        arrange(day_of_week)
      
      fig2 <- df_visit_dow %>% 
        plot_ly(x = ~ day_of_week, y = ~total_visit, name = "Total Visit", type = "bar" ) %>% 
        layout(title = 'Total Visit Per Day of Week', 
               xaxis = list(title = 'Day of Week'), 
               yaxis = list(title = 'Number of Visit'))
      
      fig <- subplot(fig,fig2,nrows = 1)
    }
    fig
  }) 
  
  # 3. Popular Hr of each day of week ------------------------------------------
  rv_hr_by_dow <- reactive({
    rv_reservation() %>% 
      group_by(day_of_week,visit_h) %>% 
      summarise(total_visit = sum(reserve_visitors),
                median_visit = median(reserve_visitors),
                avg_visit = mean(reserve_visitors)) %>% 
      ungroup()
  })
  
  popular_hr_dow <- function(df,dow){
    # Create plotly for popular hr of a given day of week
    df %>% filter(day_of_week == dow) %>%
      plot_ly( x = ~ visit_h, y = ~total_visit, type = "bar" ) %>%
      layout(title = 'Visitor with Reservation Per Hour',
             xaxis = list(title = 'Hour',tickvals = seq(from = 0, to = 24, by =3)),
             yaxis = list(title = 'Number of Visitors with Reservation'),
             legend = list(title=list(text='<b> Reservation Made From </b>')))
  }
  
  output$popular_hr_MON <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Monday")) 
  output$popular_hr_TUE <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Tuesday")) 
  output$popular_hr_WED <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Wednesday")) 
  output$popular_hr_THU <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Thursday")) 
  output$popular_hr_FRI <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Friday")) 
  output$popular_hr_SAT <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Saturday")) 
  output$popular_hr_SUN <- renderPlotly(popular_hr_dow(df = rv_hr_by_dow(), dow = "Sunday")) 
  
})

