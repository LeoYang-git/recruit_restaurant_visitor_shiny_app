library(shinydashboard)
library(leaflet)
library(shinycssloaders)

header <- dashboardHeader(
  title = "Recruit Restaurant Visits"
)

body <- dashboardBody(
  column(width = 3,
         box(width = NULL, status = "warning",
             radioButtons("datasource", "Show Restaurants on",
                          choices = c(
                            HotPepperGourmet = "HPG",
                            AirREGI = "AIR"
                          ),
                          selected = "AIR"
             ),
             p(
               class = "text-muted",
               paste("Note: Hot Pepper Gourmet provides make online reservation. While ",
                     "AirREGI is reservation control and cash register system. Hence,",
                     "actual visitors data is only available for restaurant with AirREGI"
               )
             )
         ),
         
         box(width = NULL, status = "warning",
             uiOutput("cuisineselect"),
             p(class = "text-muted",
               br(),
               "Cuisine type differs in HotPepperGourmet and AirREGI"
             )),
         box(width = NULL, status = "warning",
             dateRangeInput("inscope_daterange", "Select Date Period",
                            start = "2016-01-01",
                            end   = "2017-04-22"),
             p(class = "text-muted",
               "Source data available for the period from 2016-01-01 to 2017-04-22."
             )),
         box(width = NULL, status = "info",
             withSpinner(leafletOutput("map", height = 500)))
  ),
  column(width = 9,
         box(width = NULL, status = "primary",
             withSpinner(plotlyOutput("all_time_visits", height = 500))),
         box(width = NULL, status = "primary",
             withSpinner(plotlyOutput("popular_day_of_week", height = 500))),
         box(width = NULL, status = "info",
             tabsetPanel(
               tabPanel("MON",withSpinner(plotlyOutput("popular_hr_MON"))),
               tabPanel("TUE",withSpinner(plotlyOutput("popular_hr_TUE"))),
               tabPanel("WED",withSpinner(plotlyOutput("popular_hr_WED"))),
               tabPanel("THU",withSpinner(plotlyOutput("popular_hr_THU"))),
               tabPanel("FRI",withSpinner(plotlyOutput("popular_hr_FRI"))),
               tabPanel("SAT",withSpinner(plotlyOutput("popular_hr_SAT"))),
               tabPanel("SUN",withSpinner(plotlyOutput("popular_hr_SUN")))
             )
         )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)