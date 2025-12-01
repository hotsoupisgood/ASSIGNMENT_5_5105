library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dplyr)

# load dataset globally
dig.df=read.csv("DIG.csv") # this is not good practice, will reconfigure if I have time

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
    sliderInput("bmi", "Select Body Mass Range:",    
                min =     min(dig.df$BMI,na.rm=T),  max=max(dig.df$BMI,na.rm=T),# range
                value = c(min(dig.df$BMI,na.rm=T),  max(dig.df$BMI,na.rm=T))), # defaults 
    sliderInput("age", "Select Age of the Subject:", 
                min=min(dig.df$AGE,na.rm=T), max=max(dig.df$AGE,na.rm=T),    
                value = c(min(dig.df$AGE,na.rm=T), max(dig.df$AGE,na.rm=T)))
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
                          plotOutput("plot1")) 
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
  # global filtering
  dig.df=dig.df %>% select(ID, TRTMT, AGE, SEX, BMI, DIABP, SYSBP, HYPERTEN, CVD, WHF, HOSP,DEATH, DEATHDAY)
  # filter change event handler
  dig_filtered_df <- reactive({
    # make copy to not change global in case we need the global df
    df_copy=dig.df
    # gender handler
    mapping_sex=c("FEMALE"=2,"MALE"=1)
    df_copy=df_copy%>%filter(SEX %in% mapping_sex[input$sex])
    # age handler (inclusive filter)
    df_copy=df_copy%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    # bmi handler (inclusive filter)
    df_copy=df_copy%>%filter(BMI>=input$bmi[1] & BMI<=input$bmi[2])
    return(df_copy)
  })
  # display mapping
  # overview table
  output$table1=renderDataTable({
    dig_filtered_df()},
    options=list(
      scrollX = T
    )
  )
  # hospitalization between treatment groups
  output$plot1 <- renderPlot({ 
    #Hospitalization
    ggplot(data=dig_filtered_df(), aes(x=CVD, fill = factor(TRTMT))) + 
      labs(x="Treatment",
           title="Hospitalisation between treatment groups") +
      scale_fill_manual(values = c("#669933", "#FFCC66"),
                        name = "Treatment group") +
      geom_bar(alpha=0.7, position="fill") +
      labs(x="Hospitalization for CVD",title = "Hospitalization in due to CVD in each")
  })
  
  #Hospitalization due to CVD
  output$plot2 <- renderPlot({ 
    ggplot(data=dig_filtered_df(), aes(x=CVD, fill = factor(TRTMT))) + 
      labs(x="Treatment",
           title="Hospitalisation between treatment groups") +
      scale_fill_manual(values = c("#669933", "#FFCC66"),
                        name = "Treatment group") +
      geom_bar(alpha=0.7, position="fill") +
      labs(x="Hospitalization for CVD",title = "Hospitalization in due to CVD in each")
  })
  
  # Hospitalisation due to WHF
  output$plot3 <- renderPlot({ 
    ggplot(data=dig_filtered_df(), aes(x=WHF, fill = factor(TRTMT))) + 
      labs(x="Treatment",
           title="Hospitalisation between treatment groups") +
      scale_fill_manual(values = c("#f09c1b", "#4f78b4"),
                        name = "Treatment group") +
      geom_bar(alpha=0.7, position="fill") +
      labs(x="Hospitalization for CVD",title = "Hospitalization due to WHF in treatment groups")
    
  })
 
  
  #output$table1 <-#make function to output plot1
  #output$table1 <-#make function to output plot 2
  #output$table1 <-#make function to output plot 3
}
# launch app
shinyApp(ui = ui, server = server)