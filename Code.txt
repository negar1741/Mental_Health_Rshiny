
options(scipen = 999)
library("tidyverse")
library("stringr")
library("shinydashboard")
library(shinyWidgets)
library("plotly")
library("shiny")
library("shinyjs")
library("DT")
library("dplyr")
library("markdown")
library("tidyr")
library("ggplot2")
library("hrbrthemes")
library("leaflet")
library(dashboardthemes)
library(lubridate)
library(kableExtra)
library(readr)
df <- read_csv("dfghadim.csv")
#df$Timestamp <- as.Date(with(df, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
#as.Date(with(df$Timestamp, paste(Year, Month, Day, sep = "-")), "%m-%d-%Y")
#as.Date(df$Timestamp,"%m-%d-%Y")
#Year(df$Timestamp)
#as.POSIXct(df$Timestamp, format = "%m-%d-%y %H:%M", tz = "UTC")
#as.POSIXct(x$datetime3, format = "%Y-%m-%d %H:%M%:%S", tz = "UTC")
vasat<-read_csv("df.csv")
df2<-vasat %>% 
  pivot_longer( c(19,20,23,24, 16),values_to = "value")
df3<-data.frame(table(df2$name,df2$value))
df3
df4<-vasat %>% 
  filter(Country=="United States") %>% 
  pivot_longer( c(8),values_to = "value")

mapd<-data.frame(table(df4$value, df4$state))
mapd<-mapd %>% 
  pivot_wider(names_from = Var1, values_from = Freq)
tech<-df[c(12,9)]
tech<-data.frame(table(df$work_interfere,df$tech_company))
colnames(tech)<- c("work_interfere","tech","Count")

ui <- dashboardPage(
  dashboardHeader(title = "Mental Health", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "first", icon = icon("dashboard")),
      menuItem("Main", tabName = "second", icon = icon("table")),
      menuItem("Map for US/ Tech", tabName = "third", icon = icon("map")))
    ,
    
    uiOutput("typeSelectOutput"),
    radioButtons("parameter",
                 label = "Select Gender:",
                 choices = c("Male","Female","Other"
                 ),
                 selected = "Female"),
    radioGroupButtons("tt",
                 label = "Tech company?",
                 choices = c("No","Yes")
                 ,
                 selected = "Yes")),
  dashboardBody(shinyDashboardThemes(
    theme = "blue_gradient"),
    tabItems(
      tabItem(
        tabName = "first",h4("It is important for both employer and employee to acknowledge the importance of mental health and how it impacts work performance at work especially in the tech industry. Create a dashboard using Shiny which is intended for both employer and employee (current or new) who are working in the tech industry to be able to visually explore the interaction of variables from the 2014 Mental Health in Tech Survey. "),
        box(
        sliderInput("AgeInput",
                    label = "Age Range",
                    min = 11,
                    max = 72,
                    step = 5,
                    value = c(11, 72),
                    sep = ""), background = "aqua"),br(),
        plotOutput("first_plot"),
        br(),
        plotOutput("second_plot")),
      tabItem(tabName = "second",fixedRow(column(1,plotOutput("third_plot", width = 1400)), column(2,box(
        h4("You can customize the title!"),
        textInput("caption",
                  label = "Title of graph:",
                  value = "Did majority ask for help?", width = 1400),
        br(),
        
        radioButtons("changecol",
                     label = "Select criteria for plot below:",
                     choices = c("mental_health_consequence","mental_health_interview","phys_health_consequence","phys_health_interview","seek_help"
                     ),
                     selected = "seek_help"), width = 800, height = 400, background = "aqua")))
              ,br(),
             
        
              plotOutput("fourth_plot", width = 1400)),
        
        
      
      tabItem(tabName = "third",
              plotlyOutput("Map", inline = T),
              verbatimTextOutput("click"),
              plotOutput("techplot")
      ))))
  

server <- function(input, output) {
  
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Select Country from list:",
                sort(unique(df$Country)),
                multiple = TRUE, selected = c("Poland","Germany","Canada"), selectize = FALSE)})
  
  output$first_plot <- renderPlot({
    df %>% 
      filter(Age >= input$AgeInput[1], Age <= input$AgeInput[2]) %>% 
      ggplot() +
      geom_bar(mapping = aes( Age, color = Gender, fill = Gender)) +
      theme_modern_rc() + labs(title = "Age of participants", subtitle = "Please filter Age")
    
  })
  
  output$second_plot <- renderPlot({
    df %>% 
      filter(Gender == input$parameter) %>%
      filter(Country %in% input$typeInput) %>%
      group_by(Country) %>%
      ggplot() +
      geom_bar(mapping = aes(x = Country, color = Gender, fill = Country)) +
      theme_modern_rc()+ labs(title = "Counts of participants by Gender and Country", subtitle = "Please filter Gender and Country, ctrl+a on country for all")
    
  })
  
  output$third_plot <- renderPlot({
    data.frame(table(df$seek_help)) %>%  
      arrange(desc(Var1)) %>%
      mutate(prop = Freq / sum(Freq) *100) %>%
      mutate(ypos = cumsum(prop)- 0.6*prop ) %>%
      ggplot( aes(x="", y=prop, fill=Var1)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0)  + 
      theme(legend.position="none") + geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
      scale_fill_brewer(palette="Set1") + labs(title=input$caption, subtitle = "Seek help info") + theme_modern_rc()
  })
  output$fourth_plot<-renderPlot({
    df3 %>%
      filter(Var1 == input$changecol) %>% 
      group_by(Var1) %>%
      ggplot() +
      geom_col(mapping=aes(x=Var2, y=Freq))+ theme_modern_rc() +labs(title = "Responses to different criterias", subtitle = "Please choose one option")
    
  })
  output$Map <- renderPlotly({
      plot_data <- mapd %>%
        summarise(
          State= Var2,
          Sought_treatment = Yes,
          No_Treatment = No)
         
  
      
      plot_data$click <- with(
        plot_data,
        paste(
          State, "<br>",
          "Sought treatment:", Sought_treatment, "<br>",
          "Did not seek treatment", No_Treatment)
          
        
      )
      g <- list(
        scope = "usa",
        projection = list(type = "albers usa"),
        lakecolor = toRGB("white")
      )
      
      plot_ly(
        z = plot_data$Sought_treatment,
        locations = plot_data$State,
        text = plot_data$click,
        type = "choropleth",
        color = plot_data$Sought_treatment,
        locationmode = "USA-states",
        zmin = 0,
        zmax = 90,
        marker = list(
          line = list(
            width = 0
          ),
          opacity = 0.5
        )
      ) %>%
        layout(
          geo = g,
          title = paste0(
            "Treatment records: "
          ), font = t
        )
  })
  output$techplot <- renderPlot(
    {tech %>% 
        filter(tech== input$tt) %>% 
        ggplot( aes(x = work_interfere, y = Count, fill = work_interfere, color=work_interfere)) + 
        geom_bar(stat = "identity")+ theme_modern_rc()+labs(title = "Technology and work inference", subtitle = "Please pick Yes or No  for tech company from sidebar")
      
    }
  )
    
}

shinyApp(ui, server)

