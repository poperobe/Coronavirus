
library(tidyverse)
library(jsonlite)
library(httr)
library(shiny)
library(config)
configs<-config::get()
path <- "https://covid19-tracker.p.rapidapi.com/statistics/US"
request <- GET(url = path,
               add_headers(	"x-rapidapi-host"= configs$host,
                            "x-rapidapi-key"= configs$key)
)
response <- content(request, as = "text", encoding = "UTF-8")
df <- fromJSON(response, flatten = TRUE) %>%
    data.frame()

# Define UI for application that reports data from a coronavirus API
ui <- fluidPage(

    # Application title
    titlePanel("Corona Virus Tracker"),

    # Sidebar for filtering
    sidebarLayout(
        sidebarPanel(
            selectInput("state_box",
                        "Select State:",
                        choices=sort(unique(df$stats.state)),
                        multiple=T),
            selectInput("county_box",
                        "Select County:",
                        choices=sort(unique(df$stats.province)),
                        multiple=T)
        ),

        # Show a table that reports total stats for the selected areas
        mainPanel(
           tableOutput("corona_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    observeEvent(input$state_box,{
        cities<-df %>%
            filter(stats.state %in% input$state_box |
                       is.null(input$state_box)) %>%
            distinct(stats.province) %>%
            arrange(stats.province)
        updateSelectInput(session,
                          "county_box",
                          "Select County",
                          choices=cities[1])
    })
    
    output$corona_table <- renderTable({
        df %>%
            filter(
                stats.state %in% input$state_box | is.null(input$state_box),
                stats.province %in% input$county_box | is.null(input$county_box)
            ) %>%
            ungroup() %>%
            summarise_at(
                vars(stats.latest.confirmed,
                     stats.latest.deaths,
                     stats.latest.recovered),
                sum,
                na.rm = TRUE
            ) %>% 
            rename("Total Confirmed Cases"="stats.latest.confirmed",
                   "Total Deaths"="stats.latest.deaths",
                   "Total Recovered"="stats.latest.recovered")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
