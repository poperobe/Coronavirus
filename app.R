# This is a dashboard designed to pull current coronavirus stats and report on them
library(tidyverse)
library(jsonlite)
library(httr)
library(shiny)
library(shinydashboard)

# Function to convert variable names to display names
Proper<-function(x){
    translate<-NULL
    translate<-state.name
    names(translate)<-state.abb
    translate<-c(translate,
                 "positive"="Total Confirmed Cases",
                 "negative"="Total Negative Tests",
                 "death"="Total Deaths",
                 "recovered"="Total Recovered",
                 "positiveincrease"="New Confirmed Cases",
                 "negativeincrease"="New Negative Tests",
                 "deathincrease"="New Deaths",
                 "date"="Date",
                 "days_since"="Days Since First Case"
                 )
    sapply(x,function(y){
        if(is.na(translate[tolower(y)]))y
        else translate[tolower(y)]
    })
}

# UI ####
ui <- dashboardPage(

    # Application title
    dashboardHeader(
        title="Corona Virus Tracker",
        dropdownMenuOutput("date_notif")
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
                    plotOutput("corona_trend"),
                    fluidRow(
                        column(2,radioButtons("scale_box",
                                     "Choose Scale:",
                                     choices = c("Standard","Log"),
                                     selected="Standard")),
                        column(2,radioButtons("time_box",
                                              "Select Time measure:",
                                              choices=c("Absolute"="date","Relative"="days_since"))),
                        column(8,radioButtons("metric_box",
                                     "Choose metric:",
                                     choices=c("Total Confirmed Cases"="positive",
                                               "Total Negative Tests"="negative",
                                               "Total Deaths"="death",
                                               "Total Recovered"="recovered",
                                               "New Confirmed Cases"="positiveIncrease",
                                               "New Negative Tests"="negativeIncrease",
                                               "New Deaths"="deathIncrease"),
                                     selected="positive"))
                    )
            )
        )
    )
    
)

# SERVER ####
server <- function(input, output,session) {
    # 1. Query API and format data ####
    path <- "https://covidtracking.com/api/v1/states/daily.json"
    request <- GET(url = path
    )
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE) %>% data.frame()
    
    abbs<-c(state.abb,'AS','DC','GU','MP','PR','VI')
    s_names<-c(state.name,
               'American Samoa',
               'District of Columbia',
               'Guam',
               'Northern Mariana Islands',
               'Puerto Rico',
               'US Virgin Islands')
    df$state_name<-s_names[match(df$state,abbs)]
    
    df$date<-as.Date(as.character(df$date),format="%Y%m%d")
    
    first_date<-df %>% 
        filter(positive>0) %>% 
        group_by(state) %>% 
        summarize(first_date=min(date))
    df<-df %>% left_join(first_date) %>% 
        mutate(days_since=as.integer(date-first_date))
    
    # 2. Filters for user ####
    output$state_filter<-renderUI({
        selectInput("state_box",
                    "Select State:",
                    choices=sort(unique(df$state_name)),
                    selected = "Ohio",
                    multiple=T)
        
    })
    
    # 2. Create Date notification ####
    output$date_notif <- renderMenu({
        dropdownMenu(
            type = "notifications",
            icon = icon("calendar"),
            badgeStatus = NULL,
            headerText = "Data date range:",
            notificationItem(HTML(paste0(
                min(df$date, na.rm = T),
                "<br>through<br>",
                max(df$date, na.rm = T)
            )),
            icon = icon(NULL))
        )
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
                     negative,
                     death,
                     recovered),
                sum,
                na.rm = TRUE
            ) %>% 
            rename("Total Confirmed Cases"="positive",
                   "Total Negative Tests"="negative",
                   "Total Deaths"="death",
                   "Total Recovered"="recovered")
    })
    
    # 4. Create trend chart
    output$corona_trend<-renderPlot({
        filter_data<-df %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box)) %>% 
            mutate(value=!!sym(input$metric_box),
                   time=!!sym(input$time_box))
        
        p<-ggplot(filter_data) +geom_line(aes(time,value,color=state_name)) +
            labs(x=Proper(input$time_box),
                 y=Proper(input$metric_box),
                 title=paste(Proper(input$metric_box),"over Time"),
                 color="State")
        
        if(input$scale_box=="Log"){
            p + 
                scale_y_log10()
        }
        else p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
