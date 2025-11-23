library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Palmer Penguins Explorer",titleWidth = 450,
                  tags$li(a(href = 'https://allisonhorst.github.io/palmerpenguins/',
                            img(src = 'penguins.png',
                                title = "Penguins Homepage", height = "30px", width = "50px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  
  dashboardSidebar(
    selectInput(inputId = "species", label = "Select Penguin Species:", choices = c("Adelie", "Gentoo", "Chinstrap"), multiple = FALSE),
    radioButtons(inputId = "sex", label = "Select Penguin Sex:", choices = c("female", "male")),
    sliderInput("bmass", "Select Body Mass Range:", min = 2500, max = 7000, value = c(4000, 5000)),
    sliderInput("year", "Select year of the study:", min = 2007, max = 2009, value = 2007, step = 1, animate = TRUE)
  ),
  dashboardBody(
    tabBox(width = 12, id = "tabs",
           tabPanel("Scatter Plot", 
                    fluidRow(box(width=12, title = "Scatter Plot", collapsible = TRUE, status = "warning", solidHeader = TRUE,
                                 plotOutput("plot1")))
           ),
           tabPanel("Data", 
                    fluidRow(
                      box(width=12, title = "Data", collapsible = TRUE, status = "warning", solidHeader = TRUE,
                          dataTableOutput("table1")) 
                    )
                    
           ),
           tabPanel("line Plot", 
                    fluidRow(box(width=12, title = "Scatter Plot", collapsible = TRUE, status = "warning", solidHeader = TRUE,
                                 plotOutput("plot2")))
           ),
    )
  )
)


server <- function(input, output) {
  dig.df=read.csv("DIG.csv")
  
  dig_sub <- reactive({
    dig.df %>% select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = dig_sub(), aes(x = AGE, y = BMI)) +
      geom_point()
  })
  output$plot2 <- renderPlot({ 
    ggplot(data = dig_sub(), aes(x = AGE, y = BMI)) +
      geom_line()
  })
  
  output$table1 <- renderDataTable({ 
    dig_sub()
  })
  
}

shinyApp(ui = ui, server = server)