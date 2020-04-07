# This is a dashboard designed to pull current coronavirus stats and report on them
library(tidyverse)
library(jsonlite)
library(httr)
library(shiny)
library(shinydashboard)


# UI ####
ui <- dashboardPage(

    # Application title
    dashboardHeader(
        title="Corona Virus Tracker"
    ),

    # Sidebar for filtering
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Charts", tabName="charts",icon = icon("line-chart"))
        ),
        uiOutput("state_filter")
    ),
    # Show a table that reports total stats for the selected areas
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    tableOutput("corona_table")
            ),
            tabItem("charts",
                    plotOutput("corona_trend")
            )
        )
    )
    
)

# SERVER ####
server <- function(input, output,session) {
    # 1. Query API ####
    path <- "https://covidtracking.com/api/v1/states/daily.json"
    request <- GET(url = path
    )
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE) %>%
        data.frame()
    df$state_name<-state.name[match(df$state,state.abb)]
    df$date<-as.Date(as.character(df$date),format="%Y%m%d")
    # 2. Filters for user ####
    output$state_filter<-renderUI({
        selectInput("state_box",
                    "Select State:",
                    choices=sort(unique(df$state_name)),
                    selected = "Ohio",
                    multiple=T)
        
    })
    
    
    # 3. Create totals table ####
    output$corona_table <- renderTable({
        curr_date <- max(df$date, na.rm = T)
        df %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   date == curr_date) %>%
            ungroup() %>%
            summarise_at(
                vars(positive,
                     death,
                     recovered),
                sum,
                na.rm = TRUE
            ) %>% 
            rename("Total Confirmed Cases"="positive",
                   "Total Deaths"="death",
                   "Total Recovered"="recovered")
    })
    
    output$corona_trend<-renderPlot({
        filter_data<-df %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box))
        ggplot(filter_data) +geom_line(aes(date,positive,group=state_name))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
