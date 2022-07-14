
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
#----- Ch 3 - Reactive Programming -----


#----- Ch 4 - Build Shiny Apps -----

