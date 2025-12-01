library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dplyr)

ui <- dashboardPage(
  # change the background
  skin = "green",
  dashboardHeader(title = "Digitalis Investigation Group (DIG) Explorer",titleWidth = 450,
                  tags$li(a(href = 'https://github.com/hotsoupisgood/ASSIGNMENT_5_5105',"Source (GitHub)"),
                          class = "dropdown")),
  # filters hamburg menu
  dashboardSidebar(
    selectInput(inputId = "cvd",  label = "Filter on CVD Status:", choices = c("CVD", "No CVD", "All"), selected="All"),
    selectInput(inputId = "sex", label = "Select Subject Sex:",    choices = c("FEMALE", "MALE"), multiple=T, selected=c("FEMALE","MALE")),
    sliderInput("bmass", "Select Body Mass Range:",  min = 0, max = 25,   value = c(0, 25)),
    sliderInput("age", "Select Age of the Subject:", min = 15, max = 100, value = c(15, 100))
  ),
  # tabs with plots
  dashboardBody(
    tabBox(width = 12, id = "tabs",
           # Tab 1
           tabPanel("Descriptive Table", 
                    fluidRow(
                      box(width=12, title = "Descriptive Table", collapsible = F, status = "warning", solidHeader = TRUE,
                          dataTableOutput("table1"))
                      )
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

# app logic
server <- function(input, output) {
  # load dataset
  dig.df=read.csv("DIG.csv")
  # global filtering
  dig.df=dig.df %>% select(ID, TRTMT, AGE, SEX, BMI, DIABP, SYSBP, HYPERTEN, CVD, WHF, HOSP,DEATH, DEATHDAY)
  # filter change event handler
  dig_filtered_df <- reactive({
    df_copy=dig.df
    mapping_sex=c("FEMALE"=2,"MALE"=1)
    df_copy=df_copy%>%filter(SEX %in% mapping_sex[input$sex])
    return(df_copy)
  })
  # display mapping
  
  #output$plot1 <- renderPlot({ 
    #ggplot(data = dig_sub(), aes(x = AGE, y = BMI)) +
    #  geom_point()
  #})
 
  
  output$table1=renderDataTable({
    dig_filtered_df()},
    options=list(
      scrollX = T
    )
  )
  #output$table1 <-#make function to output plot1
  #output$table1 <-#make function to output plot 2
  #output$table1 <-#make function to output plot 3
}
# launch app
shinyApp(ui = ui, server = server)