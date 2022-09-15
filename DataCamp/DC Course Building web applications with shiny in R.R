
# https://campus.datacamp.com/courses/building-web-applications-with-shiny-in-r
# 2021-03-14
#install.packages("shiny")
#install.packages("DT")
#install.packages("babynames")
library(shiny)
library(tidyverse)
library(babynames)


#----- Ch 1 - Getting Started with Shiny -----
ui <- fluidPage(
  textInput("name", "What is your name?"),  # textInput(id, label, default)
  textOutput("greeting")
)
server <- function(input, output){
  output$greeting <- renderText({
    paste("Hello,", input$name)
  })  
}
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  titlePanel("Baby Name Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter Name', 'David')
    ),
    mainPanel(plotOutput('trend'))
  )
)
server <- function(input, output, session){
  output$trend <- renderPlot({
    data_name <- subset(
      babynames, name == input$name
    )
    ggplot(data_name) +
      ggplot(subset(babynames, name == data_name)) +
      geom_line(aes(x = year, y = prop, color = sex))
    })
}
shinyApp(ui = ui, server = server)



#----- Ch 2 - Inputs, Outputs and Layouts -----
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  selectInput("sex", "Select Sex", selected="F", choices=c("M", "F")),
  sliderInput("year", "year", value = "1900", min = 1900, max = 2010),
  plotOutput('plot_top_10_names')  # Add plot output to display top 10 most popular names
)
server <- function(input, output, session) {

  output$plot_top_10_names <- renderPlot({  # Render plot of top 10 most popular names
    
    top_10_names <- babynames %>%   # Get top 10 names by sex and year
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
    
    ggplot(top_10_names, aes(x = name, y = prop)) +  # Plot top 10 names by sex and year
      geom_col(fill = "#263e63")
  })
}
shinyApp(ui = ui, server = server)

# static table with tableOutput
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  selectInput('sex', 'Select Sex', choices = c("F", "M")),
  sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900),
  tableOutput("table_top_10_names")
  
)
server <- function(input, output, session){
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  output$table_top_10_names <- renderTable({
    top_10_names()
  })
}
shinyApp(ui = ui, server = server)


# Using DTOutput for dynamic table
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  selectInput('sex', 'Select Sex', choices = c("F", "M")),
  sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900),
  DT::DTOutput("table_top_10_names")
  
)
server <- function(input, output, session){
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
}
shinyApp(ui = ui, server = server)

# plots
top_trendy_names <- tibble::tribble(
  ~name,~sex,~total,~max,~nb_years,~trendiness,
  "Kizzy","F",2325,1116,30,0.48,
  "Deneen","F",3603,1604,52,0.445,
  "Royalty","F",1806,747,14,0.414,
  "Mareli","F",1024,411,21,0.401,
  "Moesha","F",1067,426,14,0.399,
  "Marely","F",2577,1004,28,0.390,
  "Kanye","M",1319,508,16,0.385,
  "Tennille","F",2172,769,32,0.354,
  "Aitana","F",1625,564,23,0.347,
  "Kadijah","F",1418,486,36,0.343,
  "Shaquille","M",5439,1784,29,0.328,
  "Catina","F",4178,1370,47,0.328,
  "Allisson","F",2377,767,21,0.323,
  "Emberly","F",1471,467,34,0.317,
  "Nakia","M",1991,612,40,0.307,
  "Jaslene","F",2870,872,17,0.304,
  "Kyrie","M",5858,1774,31,0.303,
  "Akeelah","F",1331,403,17,0.303,
  "Zayn","M",3347,988,25,0.295,
  "Talan","M",3640,1059,28,0.291
  )

ui <- fluidPage(
  selectInput('name', 'Select Name', top_trendy_names$name),
  plotly::plotlyOutput("plot_trendy_names")
)
server <- function(input, output, session){
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  output$plot_trendy_names <- plotly::renderPlotly({
    plot_trends()
  })
}
shinyApp(ui = ui, server = server)

#--- 
ui <- fluidPage(
  titlePanel("Trendy Names"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("journal"),
  sidebarLayout (
    sidebarPanel(
      selectInput('name', 'Select Name', top_trendy_names$name)
    ),
    mainPanel(
      tabsetPanel(
          tabPanel('Plot', plotly::plotlyOutput('plot_trendy_names') ),
          tabPanel('Table', DT::DTOutput('table_trendy_names') )
      )
    )
  )
)
server <- function(input, output, session){  # DO NOT MODIFY
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  output$plot_trendy_names <- plotly::renderPlotly({
    plot_trends()
  })
  output$table_trendy_names <- DT::renderDT({
    babynames %>% 
      filter(name == input$name)
  })
}
shinyApp(ui = ui, server = server)

#--- App1: Multilingual Greeting
library(shiny)
library(tidyverse)

#library(gapminder)
ui <- fluidPage(
  selectInput('greeting_type', 'Greeting', c("Hello", "Bonjour")),
  textInput('name', "Your Name"),
  textOutput('greeting')
)
server <- function(input, output, session) {
  output$greeting <- renderText({
      paste(input$greeting_type, input$name, sep = ", ")
  })
}
shinyApp(ui = ui, server = server)

#--- App2: Popular Baby Names
library(shiny)
library(tidyverse)
library(babynames)
get_top_names <- function(.year, .sex) {
  babynames %>% 
    filter(year == .year) %>% 
    filter(sex == .sex) %>% 
    slice_max(prop, n = 15) %>% 
    mutate(name = forcats::fct_inorder(name))
}
get_top_names(1980, "F")

ui <- fluidPage(
  titlePanel("Popular Baby Names"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Sex", c("M", "F")),
      sliderInput("year", "Year", 1880, 2017, 1990)
    ),
    mainPanel("main",
      #plotly::plotlyOutput('plot_trendy_names')
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    d <- get_top_names(input$year, input$sex)
    ggplot(d, aes(x = name, y = prop)) +
      geom_col()
      })
  output$table <- renderTable({
    get_top_names(input$year, input$sex)
  })
}
shinyApp(ui = ui, server = server)

#----- Ch 3 - Reactive Programming -----
ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120)
    ),
    mainPanel(
      textOutput("bmi"),
      textOutput("bmi_range")
    )
  )
)

server <- function(input, output, session) {
  rval_bmi <- reactive({
    input$weight/(input$height^2)
  })
  
  
  output$bmi <- renderText({
    bmi <- rval_bmi()
    paste("Your BMI is", round(bmi, 1))
  })
  output$bmi_range <- renderText({
    bmi <- rval_bmi()
    bmi_status <- cut(bmi, 
                      breaks = c(0, 18.5, 24.9, 29.9, 40),
                      labels = c('underweight', 'healthy', 'overweight', 'obese')
    )
    paste("You are", bmi_status)
  })
}

shinyApp(ui = ui, server = server)

#--- ex6
ui <- fluidPage(
  titlePanel('BMI Calculator'),
  sidebarLayout(
    sidebarPanel(
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120)
    ),
    mainPanel(
      textOutput("bmi"),
      textOutput("bmi_status")
    )
  )
)

server <- function(input, output, session) {
  rval_bmi <- reactive({
    input$weight/(input$height^2)
  })
  rval_bmi_status <- reactive({
    cut(rval_bmi(), 
        breaks = c(0, 18.5, 24.9, 29.9, 40),
        labels = c('underweight', 'healthy', 'overweight', 'obese')
    )
  })
  output$bmi <- renderText({
    bmi <- rval_bmi()
    paste("Your BMI is", round(bmi, 1))
  })
  output$bmi_status <- renderText({
    bmi_status <- rval_bmi_status()
    paste("You are", bmi_status)
  })
}

shinyApp(ui = ui, server = server)

#-- ex8
ui <- fluidPage(
  textInput('name', 'Enter your name')
)

server <- function(input, output, session) {
  observe({
    showNotification(
      paste("You have entered the name", input$name)
    )
  })
}

shinyApp(ui = ui, server = server)

# stop with isolate()
# delay with eventReactive()
# trigger w/ observeEvent()

#-- ex 16
library(shiny)
library(tidyverse)
ui <- fluidPage(
  titlePanel("Inches to Centimeters Conversion"),
  sidebarLayout(
    sidebarPanel(
      numericInput("height", "Height (in)", 60),
      actionButton("show_height_cm", "Show height in cm")
    ),
    mainPanel(
      textOutput("height_cm")
    )
  )
)
server <- function(input, output, session) {
  rval_height_cm <- eventReactive(input$show_height_cm, {
    input$height * 2.54
  })
  
  output$height_cm <- renderText({
    height_cm <- rval_height_cm()
    paste("Your height in centimeters is", height_cm, "cm")
  })
}
shinyApp(ui = ui, server = server)

#----- Ch 4 - Build Shiny Apps -----
library(shiny)
library(tidyverse)

usa_ufo_sightings <- read_csv("D:/opendata/datacamp/datacamp_usa_ufo_sightings.csv")
#unique(usa_ufo_sightings$shape)
#unique(usa_ufo_sightings$state)

ui <- fluidPage(
  titlePanel("UFO Sightings"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a U.S. state", selected="AL", choices = unique(usa_ufo_sightings$state)),
      dateRangeInput("dates", "Choose a date range:", start = "1920-01-01", end = "1980-01-01"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Shapes",
          plotOutput("shapes")
        ),
        tabPanel("Table",
          tableOutput("duration_table")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$shapes <- renderPlot({
    usa_ufo_sightings %>% 
      filter(state == input$state,
             date_sighted >= input$dates[1],
             date_sighted <= input$dates[2]) %>% 
      ggplot(aes(x = shape)) + 
      geom_bar() +
      labs(x = "Shape", y = "# Sighted")
  })
  
  output$duration_table <- renderTable({
    usa_ufo_sightings %>% 
    filter(
      state == input$state,
      date_sighted >= input$dates[1],
      date_sighted <= input$dates[2]
    ) %>%
      group_by(shape) %>%
      summarize(
        nb_sighted = n(),
        avg_duration = mean(duration_sec),
        median_duration = median(duration_sec),
        min_duration = min(duration_sec),
        max_duration = max(duration_sec)
      )
  })
  
}

shinyApp(ui, server)

#--- Mental health

#install.packages("shinyWidgets")
#install.packages("shinydashboard")
library(shiny)
library(tidyverse)
library(shinyWidgets)
shinyWidgetsGallery()

mental_health_survey <- read_csv("D:/opendata/datacamp/datacamp_mental_health_survey_edited.csv")
ui <- fluidPage(
  titlePanel("2014 Mental Health in Tech Survey"),
  sidebarPanel(
    checkboxGroupInput(
        inputId = "mental_health_consequence",
        label = "Do you think that discussing a mental health issue with your employer would have negative consequences?",
        choices = c("Maybe", "Yes", "No"),
        selected = "Maybe"
      ),
      pickerInput(
        inputId = "mental_vs_physical",
        label = "Do you feel that your employer takes mental health as seriously as physical health?",
        choices = c("Don't Know", "No", "Yes"),
        multiple = TRUE
      )
  ),
  mainPanel(
    plotOutput("age")
  )
)

server <- function(input, output, session) {
  # CODE BELOW: Build a histogram of the age of respondents
  # Filtered by the two inputs
  output$age <- renderPlot({
    validate(
      need(input$mental_vs_physical != "", "Select a mental vs physical option")
    )
    mental_health_survey %>%
      filter(
        mental_health_consequence %in% input$mental_health_consequence,
        mental_vs_physical %in% input$mental_vs_physical
      ) %>%
      ggplot(aes(Age)) +
      geom_histogram()
  })
}

shinyApp(ui, server)

#--- Mass Shootings ----

library(shiny)
library(leaflet)
mass_shootings <- read_csv("D:/opendata/datacamp/mass-shootings.csv")

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%} #controls{background-color:white;padding:20px;}")
)

server <- function(input, output, session) {
  rval_mass_shootings <- reactive({
    mass_shootings %>% 
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities
      )
  })
  output$map <- leaflet::renderLeaflet({
    rval_mass_shootings() %>%
      leaflet() %>% 
      addTiles() %>%
      setView( -98.58, 39.82, zoom = 5) %>% 
      addTiles() %>% 
      addCircleMarkers(
        popup = ~ summary, radius = ~ fatalities,
        fillColor = 'red', color = 'red', weight = 1
      )
  })
}

shinyApp(ui, server)




