# LIBRARIES ----
library(shiny)
library(shinyWidgets)
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
library(weightr)
library(pwr)

# UI ----
source(here("ui_x.R"), local = TRUE)


# SERVER ----
server <- function(input, output, session) {
  ####DATA INPUT
  
  source(here("data_input.R"), local = TRUE)
  
  ######SPECIFY DT COLUMS   
  
  source(here("choose_cols.R"), local = TRUE)
  
  
  #Calculate all effectsizes and store them in separate Data table data_reac$DTall
  observe({
  req(para$es)
  type_ES <- para$es
  data <- as.data.table(data_reac$DT)
  source(here("calc_effectsize.R"), local = TRUE)
  data_reac$DTall <- data
  })
  
  output$dwn_dat <- downloadHandler(
    filename = "dat.RDS",
    content = function(file){
      saveRDS(data_reac$DTall, file = file)
    }
  )
  
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
  
}

shinyApp(ui = ui, server = server)
