library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(plotly)
library(rsconnect)
# load dataset globally
dig.df=read.csv("DIG.csv") # this is not good practice, will reconfigure if I have time

ui <- dashboardPage(
  # change the background
  skin = "green",
  # build the header
  dashboardHeader(title = "Digitalis Investigation Group (DIG) Explorer",titleWidth = 450,
                  tags$li(a(href = 'https://github.com/hotsoupisgood/ASSIGNMENT_5_5105',"Source (GitHub)"),
                          class = "dropdown")),
  # filters hamburg menu
  dashboardSidebar(
    #selectInput(inputId = "cvd",  label = "Filter on CVD Status:", choices = c("CVD", "No CVD", "All"), selected="All"),
    selectInput(inputId = "sex", label = "Select Subject Sex:",    choices = c("Female","Male","All"),  selected=c("All")),
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
           # Tab 1-------------------------------------
           tabPanel("Descriptive Table", 
                    fluidRow(
                      box(width=12, title = "Descriptive Table", collapsible = F, status = "warning", solidHeader = TRUE,
                          dataTableOutput("table1"))
                      )
           ),
           # Tab 2-------------------------------------
           tabPanel("Hospitalisation", 
                    fluidRow(box(plotlyOutput("tb1_plot1"), width=12, title = "Hospitalization due to Worsening Heart Failure (WHF)", collapsible = T, status = "warning", solidHeader = TRUE)), 
                    fluidRow(box(plotlyOutput("tb1_plot2"), width=12, title = "Hospitalization due to Cardio Vasuclar Disease (CVD)", collapsible = T, status = "warning", solidHeader = TRUE))
                    ),
           # Tab 3-------------------------------------
           tabPanel("Mortality", 
                    fluidRow(box(plotlyOutput("tb2_plot1"),width=12, title = "By Cardio Vascular Disease", collapsible = T, status = "warning", solidHeader = TRUE)), 
                    fluidRow(box(plotlyOutput("tb2_plot2"), width=12, title = "By Worsening Heart Failure", collapsible = T, status = "warning", solidHeader = TRUE))
           ), 
           # Tab 4-------------------------------------
           tabPanel("Survival", 
                       fluidRow(box(plotlyOutput("tb3_plot1"), width=12, title = "Survival", collapsible = F, status = "warning", solidHeader = TRUE))
           ),
           # Tab 5-------------------------------------
           tabPanel("Scatter", 
                    fluidRow(box(plotlyOutput("tb4_plot1"), width=12, title = "Scatter", collapsible = F, status = "warning", solidHeader = TRUE))
           )
    )
  )
)

# app logic
server <- function(input, output) {
  #Filtering ------
  dig.df=clean_df(dig.df)
  # filter change event handler
  dig_filtered_df <- reactive({
    # make copy to not change global in case we need the global df
    df_copy=dig.df
    # gender handler
    if(!input$sex=="All"){
      df_copy=df_copy%>%filter(SEX==input$sex)
    }
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
  #Hospitalization-------------------------------------------------------------
  # Hospitalization in WHF
  output$tb1_plot1=renderPlotly({hosp_treat(dig_filtered_df())})
  # Hospitalisation due to CVD
  output$tb1_plot2=renderPlotly({hosp_cvd(dig_filtered_df())})
  #Mortality Rate-------------------------------------------------------
  # Death due to CVD
  output$tb2_plot1=renderPlotly({cvd_plot_strat(dig_filtered_df())})
  # Death due to WHF
  output$tb2_plot2=renderPlotly({whf_plot_strat(dig_filtered_df())})
  # Survival plot
  output$tb3_plot1=renderPlotly({surv_plot(dig_filtered_df())})
  #Scatter Plot-------------------------------------------------------
  output$tb4_plot1=renderPlotly({animation_plot(dig_filtered_df())})
}
#Plotting-------------------------------------------------------
#Hospitalization-----------
hosp_treat=function(input_df){ 
  return(ggplot(data=input_df, aes(x=WHF, fill = factor(TRTMT))) + 
    labs(x="Treatment",
         title="Hospitalisation between treatment groups") +
    scale_fill_manual(values = c("#f09c1b", "#4f78b4"),
                      name = "Treatment group") +
    geom_bar(alpha=0.7, position="fill") +
    labs(x="Hospitalization for CVD",title = "Hospitalization due to WHF in treatment groups"))
}
hosp_cvd=function(input_df){
  return(ggplot(data=input_df, aes(x=CVD, fill = factor(TRTMT))) + 
    labs(x="Treatment",
         title="Hospitalisation between treatment groups") +
    scale_fill_manual(values = c("#669933", "#FFCC66"),
                      name = "Treatment group") +
    geom_bar(alpha=0.7, position="fill") +
    labs(x="Hospitalization for CVD",title = "Hospitalization in due to CVD in each"))
}
#Death--------------------
#Death due to CVD
cvd_plot_strat=function(input_df){
  cvd_mortality_by_treatment<- input_df%>%
    group_by(TRTMT, CVD)%>%
    summarise(
      Total = n(),
      Deaths = sum(DEATH== "Death", na.rm = T),
      Mortality_Rate = Deaths/Total
    )
  cvd_plot_strat_<- ggplot(cvd_mortality_by_treatment,
                          aes(x = CVD, y=Mortality_Rate , fill = TRTMT))+
    #using position dodge to place each treatment groups side by side
    geom_col(position = position_dodge(width = 0.8), color = "lightblue")+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("#8970b3", "#2d7e74"))+
    labs(
      title = "Mortality rate by CVD status in each treatment group ",
      x = "Cardiovascular Disease(CVD) status",
      y = "Mortality Rate(%)",
      fill = "Treatment Group",
      caption = "\n Mortality rate by among patients with and without CVD in each treatment group")
  return(ggplotly(cvd_plot_strat_))
}
#Death due to WHF
whf_plot_strat=function(input_df){
  whf_mortality_by_treatment<- input_df%>%
    group_by(TRTMT, WHF)%>%
    summarise(
      Total = n(),
      Deaths = sum(DEATH== "Death", na.rm = T),
      Mortality_Rate = Deaths/Total
    )
  whf_plot_strat_<- ggplot(whf_mortality_by_treatment,
                          aes(x = WHF, y=Mortality_Rate , fill = TRTMT))+
    #using position dodge to place each treatment groups side by side
    geom_col(position = position_dodge(width = 0.8), color = "lightblue")+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("#8970b3", "#e74"))+
    labs(
      title = "Mortality rate by Worsening Heart Failure in each treatment group ",
      x = "Worsening Heart Failure(WHF) status",
      y = "Mortality Rate(%)",
      fill = "Treatment Group",
      caption = "\n Mortality rate by among patients with and without Worsening Heart Failure in each treatment group")
  return(ggplotly(whf_plot_strat_))
}
#Survival plot
surv_plot=function(input_df){
  # Graphical summary
  return(ggplot(input_df, 
         aes(x = Month, fill = TRTMT, y = after_stat(density)) ) + 
    geom_histogram() +
    geom_density(alpha = 0.8) +
    labs(title = "Distribution of summary statstics DIG participants in each treatment group", 
         subtitle = "Histogram for Month of Death and Treatment Group",
         caption = "\n: Distribution of month of death of patients by
              in each treatment arms",
         x = "Month", 
         y= "Frequency"))
}
#Scatter/animation---------
animation_plot=function(input_df){
  # Animated graphical presentation 
  return(input_df%>% # omitting any missed BP measurement from the dataset
           filter(
             !is.na(SYSBP),
             !is.na(DIABP),
             !is.na(TRTMT),
             !is.na(HYPERTEN),
           ) %>%
    plot_ly(
      x = ~SYSBP , 
      y = ~DIABP , 
      size = ~BMI, 
      color = ~TRTMT, 
      colors = c('gold', 'black'),
      frame = ~ Month, 
      text = ~ID, 
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers'
    ) %>% 
    layout(
      xaxis = list(
        type = "log"
      )
    ) %>% animation_opts(frame = 1000, transition = 250, redraw = FALSE))
}

#Cleaning-------------------------------------------------------
clean_df=function(input_df){
  # format df for plotting
  temp_df <- input_df
  
  temp_df<- select(temp_df, "ID", "TRTMT", "AGE", "SEX", "BMI", "CREAT",
           "DIABP", "SYSBP", "HYPERTEN", "CVD", "WHF", "HOSP", 
           "HOSPDAYS", "DEATH", "DEATHDAY")
  temp_df <- na.omit(temp_df)
  
  colnames(temp_df)[1] <- "ID" 
  #TRTMT
  temp_df$TRTMT <- as.numeric(as.character(temp_df$TRTMT))
  temp_df$TRTMT<- factor(temp_df$TRTMT,
                       levels = c(0,1),
                       labels = c("placebo", "Digoxin"))
  
  #SEX
  temp_df$SEX <- as.numeric(as.character(temp_df$SEX))
  temp_df$SEX<- factor(temp_df$SEX,
                       levels = c(1,2),
                       labels = c("Male", "Female")) 
  
  temp_df$CVD <- factor(temp_df$CVD ,
                        levels = c(0 , 1), 
                        labels = c("No", "Yes"))
  # WHF
  temp_df$WHF <- factor(temp_df$WHF,
                       levels  = c(0,1), 
                        labels= c("Not Worsening HF", "Worsening HF"))
  
  temp_df$HOSP <- factor(temp_df$HOSP,
                        levels  = c(0,1), 
                        labels= c("Not Hospitalized", "Hospitalized"))
  #HYPERTEN
  temp_df$HYPERTEN <- as.numeric(as.character(temp_df$HYPERTEN))
  temp_df$HYPERTEN<- factor(temp_df$HYPERTEN,
                       levels = c(0,1),
                       labels = c("Not Hypertensive", "Hypertensive")) 
  
  #DEATH
  temp_df$DEATH <- as.numeric(as.character(temp_df$DEATH))
  temp_df$DEATH<- factor(temp_df$DEATH,
                       levels = c(0,1),
                       labels = c("Alive", "Death")) 
  temp_df <- temp_df%>%
    mutate(Month = round(DEATHDAY/30, ))
return(temp_df)
}

# launch app
shinyApp(ui = ui, server = server)