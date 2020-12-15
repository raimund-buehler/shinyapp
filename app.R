# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(foreign)
library(data.table)
library(here)
library(metafor)
library(metaviz)
library(ggplot2)
library(ggplotify)
library(gghighlight)
library(tidyverse)
library(shinydashboard)
library(DT)
library(puniform)
library(stringr)
library(poibin)
library(weightr)
library(pwr)
library(kableExtra)
library(haven)
library(htmltools)
library(rmarkdown)
options(scipen=999)

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# UI ----
source(here("ui_x.R"), local = TRUE)

# SERVER ----
server <- function(input, output, session) {
  
  ####ABOUT TEXT
  output$AboutText <- renderUI({
    HTML(paste(strong("MetaShine"), "is an app to simplify meta-analytic calculations, publication bias 
              methods and plotting of meta-analytic data. You can upload your dataset and 
              perform calculations on it, provided the file is formatted correctly. 
              All requirements and functions are explained on this page. A default file (mozart.sav) 
              is provided to showcase the workings of the app. Any suggestions for improvement or questions can 
              be directed to the authors listed below:", br(), br(),
              tags$a(href="https://ufind.univie.ac.at/de/person.html?id=53023", "Magdalena Siegel (Author 1)"), br(),
              tags$a(href="https://ufind.univie.ac.at/de/person.html?id=104295", "Raimund Buehler (Author 2)"), br(),
              tags$a(href="https://ufind.univie.ac.at/de/person.html?id=29985", "Jakob Pietschnig (PI)"), br()
              )
         )
  })
  output$AboutData <- renderUI({
    HTML(paste("Here you can upload a datafile using the “browse” button. The file has to be in .sav 
              format for now (other formats might be implemented later). A default file called “mozart.sav” 
              is pre-selected and displayed to the right in a box labeled “Datafile”.", br(), br(),
               "Below the upload button you’ll find a box labeled “Input”. 
              MetaShine automatically scans the columns of your datafile to find what it assumes to be effect sizes, 
              standard errors etc. If appropriate column names are found, the associated selection boxes are marked green. 
              If not, associated selection boxes are marked yellow and you will be asked to specify the appropriate column. 
              Ideally, you should format your data in a way similar to the default data set for the meta-analysis to work. 
              . In some cases you also need to specify the effect metric (Cohen’s d, Hedge’s g, Odds ratios, etc.). 
              Effect sizes are converted internally and a data file including converted effect sizes can be downloaded 
              in the “Reports and Downloads” section.
              When you have selected all the columns, click on the button labeled “Submit” to submit the data."))
  })
  output$AboutMA <- renderUI({
    HTML(paste(strong("Meta-Analysis:"), br(), "On this page, you can run the Meta-Analysis.",
    strong("You will have to execute this first"), ", because later calculations rely on results 
    calculated here. You can choose between fixed or random-effects models and 
    select the estimator of between study-variance. The summary effect and 
    heterogeneity statistics will be displayed to the right. On the bottom, 
    a forest plot depicting summary effects for different estimators of between 
    study-variance is displayed (sensitivity analysis) and can be downloaded 
    in the “Reports and Downloads” section. 
    Even further down, you can open a box to access the full R output.", br(), br(),
    strong("Subgroup Analysis:"), br(), "Here you can do subgroup analyses 
    using categorical variables. The selection boxes default to the column 
    in your dataset with the least levels, but you can also choose any other 
    moderator variable and reference category. In the default example, 
    the summary estimates for published vs unpublished articles are displayed in the boxes below.", br(), br(),
    strong("Meta-Regression"), br(), "This page lets you examine the influence of moderator 
    variables on effect sizes. You can select any number of moderator variables 
    and check for residual heterogeneity similarly to the subgroup analysis."))
  })
  output$AboutPlots <- renderUI({
    HTML(paste("Forest and funnel plots are automatically generated once you have executed 
               the meta-analysis. For", strong("forest plots"), "you can choose between standard and cumulative 
               as well as a number of different variants and colors on the right. If you have a lot 
               of studies in your dataset, you can adjust the height of the plot accordingly.", br(), br(),
               strong("Funnel plots"), "are displayed in two variants (contour-enhanced and sunset). 
               You can also show studies imputed by trim-and-fill and eggers regression line. 
               More information about these methods is provided in the “Publication Bias” section. 
               All plots can be downloaded via “Reports & Downloads”."))
  })
  output$AboutPubbias <- renderUI({
    HTML(paste("Here you can access a number of publication bias methods. 
               A short description of each method is given. In the section 
               “Summary Bias Analyses”, you get an overview of the results of 
               the methods you have chosen to execute. Boxes in red mark methods 
               that indicate publication bias, whereas boxes in green mark those 
               that do not. You can also specify thresholds for this in the box 
               labeled “thresholds”. Note that you need to execute each method 
               separately in order for it to show up in the summary section."))
  })
  output$AboutRepDown <- renderUI({
    HTML(paste("On this page, you can download all your calculations and plots. 
               You can choose between formats and download some results as R objects, 
               in case you wish to perform further analysis in R. The “Download Report” 
               Button generates a PDF file that summarizes all calculations you have done. "))
  })
  
  
  
  ####DATA INPUT
  # output$helpFile <- renderUI({
  #   tipify(tags$a(icon("question-circle"), id = "q1"),title = "test", placement = "right")
  #      #tags$style(type = "text/css", "#q1 {text-align: right;}")
  # })

  
  source(here("data_input.R"), local = TRUE)
  
  ######SPECIFY DT COLUMS   
  
  source(here("choose_cols.R"), local = TRUE)
  
  
  #Calculate all effectsizes and store them in separate Data table data_reac$DTall
  observeEvent(input$SubmitButton, {
  req(para$es)
  req(para$se)
  req(para$n)
  type_ES <- para$es
  data <- as.data.table(data_reac$DT)
  source(here("calc_effectsize.R"), local = TRUE)
  data_reac$DTall <- data
  })

  
  ######PRIMARY SELECT
  
  source(here("primary_select_shiny.R"), local = TRUE)
  
  #####META ANALYSIS
  
  source(here("Meta-Analysis.R"), local = TRUE)
  
  
  #####Subgroup Analysis
  
  source(here("subgroup.R"), local = TRUE)
  
  # ** Meta-Regression ----
  source(here("metaregression.R"), local = TRUE)
  
  
  # Create plots ----
  source(here("plots.R"), local = TRUE)
  
  #####Publication Bias Methods
  source(here("pubbias.R"), local = TRUE)
  
  # Download Section ----
  source(here("downloads.R"), local = TRUE)
  
}

shinyApp(ui = ui, server = server)
