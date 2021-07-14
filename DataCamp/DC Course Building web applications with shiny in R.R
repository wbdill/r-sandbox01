
# https://campus.datacamp.com/courses/building-web-applications-with-shiny-in-r
# 2021-03-14
install.packages("shiny")
library(shiny)
library(tidyverse)
install.packages("babynames")
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
      geom_line(
        aes(x = year, y = prop, color = sex)
      )
    })
}
shinyApp(ui = ui, server = server)
#----- Ch 2 - Inputs, Outputs and Layouts -----

#----- Ch 3 - Reactive Programming -----


#----- Ch 4 - Build Shiny Apps -----

