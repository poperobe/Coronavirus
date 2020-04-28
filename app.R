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
    names(translate)<-tolower(state.abb)
    translate<-c(translate,
                 "as"="American Samoa",
                 "dc"="District of Columbia",
                 "gu"="Guam",
                 "mp"="Northern Mariana Islands",
                 "pr"="Puerto Rico",
                 "vi"="US Virgin Islands",
                 "positive"="Total Confirmed Cases",
                 "negative"="Total Negative Tests",
                 "death"="Total Deaths",
                 "recovered"="Total Recovered",
                 "positiveincrease"="New Confirmed Cases",
                 "negativeincrease"="New Negative Tests",
                 "deathincrease"="New Deaths",
                 "date"="Date",
                 "days_since"="Days Since First Occurrence"
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
            menuItem("General Info", tabName="state_chart",icon = icon("line-chart")),
            menuItem("Stay At Home Impact", tabName="expo_chart",icon = icon("line-chart")),
            menuItem("Forecast", tabName="forecast_chart",icon = icon("line-chart"))
        ),
        selectInput(
            "state_box",
            "Select State:",
            choices = "Ohio",
            selected = "Ohio",
            multiple = T
        ), 
        radioButtons(
            "scale_box",
            "Choose Scale:",
            choices = c("Standard", "Log"),
            selected = "Standard"
        ),
        radioButtons(
            "time_box",
            "Select Time measure:",
            choices = c("Absolute" = "date", "Relative" =
                            "days_since")
        ),
        radioButtons(
            "metric_box",
            "Choose metric:",
            choices = c(
                "Total Confirmed Cases" = "positive",
                "Total Negative Tests" = "negative",
                "Total Deaths" = "death",
                "Total Recovered" = "recovered",
                "New Confirmed Cases" = "positiveIncrease",
                "New Negative Tests" = "negativeIncrease",
                "New Deaths" = "deathIncrease"
            ),
            selected = "positive"
        )
        
    ),
    # Show a table that reports total stats for the selected areas
    dashboardBody(
        tabItems(
            tabItem("state_chart",
                    tableOutput("corona_table"),
                    plotOutput("corona_trend")
                    
            ),
            tabItem("expo_chart",
                    tableOutput("expo_table"),
                    plotOutput("expo_trend"),
                    HTML("By the end of March, most Americans were under stay at home orders, causing COVID-19 to spread much slower. 
                    This graph shows what could have happened if the virus continued to spread unopposed just one more week.
                    <br>Just one more week of exponential growth could have results in up to ten times more infected people. 
                    <br>Model fit can best be seen by looking at the logarithmic scale. Fit measures to be added in future release.<br>")
                    
            ),
            tabItem("forecast_chart",
                    tableOutput("forecast_table"),
                    plotOutput("forecast_trend"),
                    HTML("Since the start of April, the trends for most every category appears linear, implying the virus has begun to spread at a linear rate.
                         Indeed, when looking at the new confirmed cases over this time period, the rate has barely changed at all.<br>")
                    
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
    df[is.na(df)]<-0

    df$state_name<-Proper(df$state)
    
    df$date<-as.Date(as.character(df$date),format="%Y%m%d")
    
    first_case<-reactive({
        (df %>% 
             filter(eval(sym(input$metric_box))>0,
                    state_name %in% input$state_box |
                        is.null(input$state_box)) %>% 
             ungroup() %>% 
             summarize(first_date=min(date)))[[1]]
    })
    
    # 2. Filters for user ####
    # output$state_filter<-renderUI({
    updateSelectInput(
        session,
        "state_box",
        "Select State:",
        choices = sort(unique(df$state_name)),
        selected = "Ohio"
    )
        
    # })
    
    # 2. Create Date notification ####
    output$date_notif <- renderMenu({
        dropdownMenu(
            type = "notifications",
            icon = icon("calendar"),
            badgeStatus = NULL,
            headerText = HTML("<b>Data date range:</b>"),
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
                function(x) as.integer(sum(x,na.rm = TRUE))
            ) %>% 
            rename("Total Confirmed Cases"="positive",
                   "Total Negative Tests"="negative",
                   "Total Deaths"="death",
                   "Total Recovered"="recovered")
    })
    
    # 4. Create trend chart ####
    output$corona_trend<-renderPlot({
        
        first_date<-df %>% 
            filter(eval(sym(input$metric_box))>0) %>% 
            group_by(state) %>% 
            summarize(first_date=min(date))

        filter_data<-df %>% left_join(first_date) %>% 
            mutate(days_since=as.integer(date-first_date),
                   value=!!sym(input$metric_box),
                   time=!!sym(input$time_box)) %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   days_since>=0)
        
        p<-ggplot(filter_data) +geom_line(aes(time,value,color=state_name)) +
            labs(x=Proper(input$time_box),
                 y=Proper(input$metric_box),
                 title=paste(Proper(input$metric_box),"over Time"),
                 color="State")
        
        if(input$scale_box=="Log"){
            p + scale_y_log10()
        }
        else p
    })
    
    # 5. Create exp model ####
    model_data<-reactive({

        
        filter_data<-df %>%
            mutate(value=!!sym(input$metric_box),
                   days_since=as.integer(date-first_case()))%>% 
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   value>0,
                   date<as.Date("2020-04-01")) %>% 
            group_by(days_since) %>% summarize(value=sum(value)) %>% 
            select(days_since,value)
        #generate model for exponential growth prior to stay at home order impact
        coefs<-lm(log(filter_data$value)~filter_data$days_since)[[1]]
    })
    
    # 6. Create log comparison table ####
    output$expo_table <- renderTable({
        curr_date <- as.Date("2020-04-07")
        days_since<-as.integer(curr_date-first_case())
        table0<-df %>% 
            mutate(value=!!sym(input$metric_box)) %>% 
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   date==curr_date) %>%
            ungroup() %>%
            summarize(value=sum(value))
        actual_val<-as.integer(table0[[1]])
        expect_val<-as.integer(exp(model_data()[[2]]*days_since+model_data()[[1]]))
        difference<-actual_val-expect_val
        prct_diff<-scales::percent( actual_val/expect_val-1)
        table1<-data.frame(actual_val,expect_val,difference,prct_diff)
        names(table1)<-c(Proper(input$metric_box),"Expected value","Observed Difference","Percent Difference")
        table1
    })

    # 7. Create log comparison chart ####
    output$expo_trend<-renderPlot({
        #filter data first
        filter_data<-df %>% 
            mutate(value=!!sym(input$metric_box),
                   days_since=as.integer(date-first_case()),
                   time=!!sym(input$time_box))  %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   date<=as.Date("2020-04-07"),
                   positive>0)%>% 
            group_by(days_since,time) %>% summarize(value=sum(value))
        
        p<-ggplot(filter_data) +geom_line(aes(time,value),color="Red") +
            geom_line(aes(time,exp(model_data()[[2]]*days_since+model_data()[[1]]))) +
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
    
    # 8. Create new model ####
    new_model_data<-reactive({
        
        
        filter_data<-df %>%
            mutate(value=!!sym(input$metric_box),
                   days_since=as.integer(date-first_case()))%>% 
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   value>0,
                   date>=as.Date("2020-04-01")) %>% 
            group_by(days_since) %>% summarize(value=sum(value)) %>% 
            select(days_since,value)
        #generate model for linear growth after stay at home order impact
        coefs<-lm(filter_data$value~filter_data$days_since)[[1]]
    })
    
    # 9. Create comparison table ####
    output$forecast_table <- renderTable({
        curr_date <- max(df$date,na.rm=T)
        days_since<-as.integer(curr_date-first_case())
        table0<-df %>% 
            mutate(value=!!sym(input$metric_box)) %>% 
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   date==curr_date) %>%
            ungroup() %>%
            summarize(value=sum(value))
        expect_val<-as.integer(new_model_data()[[2]]*(days_since+7)+new_model_data()[[1]])
        table1<-data.frame(expect_val)
        names(table1)<-paste("Expected",Proper(input$metric_box),"next week")
        table1
    })
    
    # 10. Create log comparison chart ####
    output$forecast_trend<-renderPlot({
        #filter data first
        filter_data<-df %>% 
            mutate(value=!!sym(input$metric_box),
                   days_since=as.integer(date-first_case()),
                   time=!!sym(input$time_box))  %>%
            filter(state_name %in% input$state_box |
                       is.null(input$state_box),
                   date>=as.Date("2020-04-01"),
                   positive>0)%>% 
            group_by(days_since,time) %>% summarize(value=sum(value))
        
        p<-ggplot(filter_data) +geom_line(aes(time,value),color="Red") +
            geom_line(aes(time,new_model_data()[[2]]*days_since+new_model_data()[[1]])) +
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
