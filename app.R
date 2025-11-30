library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dplyr)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Digitalis Investigation Group (DIG) Explorer",titleWidth = 450,
                  tags$li(a(href = 'https://github.com/hotsoupisgood/ASSIGNMENT_5_5105',"Source (GitHub)"),
                          class = "dropdown")),
  
  dashboardSidebar(
    selectInput(inputId = "cvd",  label = "Filter on CVD Status:", choices = c("CVD", "No CVD", "All"), selected="All"),
    selectInput(inputId = "sex", label = "Select Subject Sex:",    choices = c("female", "male"), multiple=T, selected=c("female","male")),
    sliderInput("bmass", "Select Body Mass Range:",  min = 0, max = 25,   value = c(0, 25)),
    sliderInput("age", "Select Age of the Subject:", min = 15, max = 100, value = c(15, 100))
  ),
  dashboardBody(
    tabBox(width = 12, id = "tabs",
           # Tab 1
           tabPanel("Descriptive Table", 
                    fluidRow(box(width=12, title = "Descriptive Table", collapsible = F, status = "warning", solidHeader = TRUE,
                                 plotOutput("table1")))
           ),
           # Tab 2
           tabPanel("Hospitalisation", 
                    fluidRow(
                      box(width=12, title = "Hospitalisation", collapsible = F, status = "warning", solidHeader = TRUE,
                          dataTableOutput("plot1")) 
                    )
                    
           ),
           # Tab 3
           tabPanel("Death", 
                    fluidRow(box(width=12, title = "Scatter Plot", collapsible = F, status = "warning", solidHeader = TRUE,
                                 plotOutput("plot2")))
           ), 
           # Tab 4
           tabPanel("Survival", 
                       fluidRow(box(width=12, title = "Survival", collapsible = F, status = "warning", solidHeader = TRUE,
                                    plotOutput("plot3")))
           ),
           # Tab 5
           tabPanel("Scatter", 
                    fluidRow(box(width=12, title = "Scatter", collapsible = F, status = "warning", solidHeader = TRUE,
                                 plotOutput("plot4")))
           )
    )
  )
)


server <- function(input, output) {
  dig.df=read.csv("DIG.csv")
  
  dig_sub <- reactive({
    dig.df %>% select(ID, TRTMT, AGE, SEX, BMI, DIABP, SYSBP, HYPERTEN, CVD, WHF, HOSP,DEATH, DEATHDAY)
  })
  
  #output$plot1 <- renderPlot({ 
    #ggplot(data = dig_sub(), aes(x = AGE, y = BMI)) +
    #  geom_point()
  #})
 
  
  #output$table1 <-#make function to output table
  #output$table1 <-#make function to output plot1
  #output$table1 <-#make function to output plot 2
  #output$table1 <-#make function to output plot 3
}

shinyApp(ui = ui, server = server)