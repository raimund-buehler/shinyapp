##UI is semi-automated: for each needed parameter, potential column names are checked for matches.
##If a column name matches, the name of the column and it's use is displayed to the user as text (i. e. Effect size used: 'd').
##If there are several matches (i.e. in a data file with several effect sizes), the user is asked to specify which one should be used for the calculation (candidate column names are preselected).
##If no match is found, the user is asked to provide the appropriate column from all column names.
##parameters are stored as reactive values inside "para" and can be used when later sourcing calculation files (e.g. via para$es, para$se etc)

## CHOOSE EFFECT SIZE TYPE

para <- reactiveValues()

output$EStype <- renderUI({
  req(input$file)
  vec <- repcols$DT$es
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "ES", label = "Effect Size:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "ES", label = "Please choose the effect size in your data set", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "ES", label = "Please choose the effect size in your data set", c("Choose one" = "", "r", "z", "g", "d", "OR", "logOR")))
  }
})

#  If no column name matches, both ES type and column have to be specified
output$EScolumn <- renderUI({
  req(input$file)
  if (length(unique(repcols$DT$es)) == 1 & is.na(repcols$DT$es[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "EScol", label = "Please choose the column that contains the effect size values", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

##CHOOSE STANDARD ERROR COLUMN

output$SEcolumn <- renderUI({
  req(input$file)
  vec <- repcols$DT$se
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "SE", label = "Standard Error:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "SE", label = "Standard Error:", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "SE", label = "Standard Error:", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

# ##CHOOSE YEAR

output$Year<- renderUI({
  req(input$file)
  vec <- repcols$DT$year
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "year", label = "Publication Year:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "year", label = "Publication Year:", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "year", label = "Publication Year:", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

##CHOOSE SAMPLE SIZE

output$SampleSize<- renderUI({
  req(input$file)
  vec <- repcols$DT$n
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "n", label = "Sample size:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "n", label = "Sample size:", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "n", label = "Sample size:", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

##CHOOSE STUDY ID
output$StudyID<- renderUI({
  req(input$file)
  vec <- repcols$DT$id
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "id", label = "Study Name:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "id", label = "Study Name:", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "id", label = "Study Name:", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

##CHOOSE PUBLICATION STATUS

output$PubStatus<- renderUI({
  req(input$file)
  vec <- repcols$DT$pub
  if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
    box(background = "green", width = NULL, height = "75px",
    selectInput(inputId = "pub", label = "Publication Status:", choices = vec[1]))
  } else if (length(unique(vec)) > 1) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "pub", label = "Publication Status:", choices = c("Choose one" = "", unique(vec))))
  } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
    box(background = "yellow", width = NULL, height = "75px",
    selectInput(inputId = "pub", label = "Publication Status:", choices = c("Choose one" = "", colnames(data_reac$DT))))
  }
})

# pub values

# output$PubValuePub <- renderUI({
#   req(input$file)
#   req(para$pub)
#   pubcol <- para$pub
#   para$pubvalpub <- input$pubvalpub
#   selectInput(inputId = "pubvalpub", label = "Please specify, which value in your dataset refers to PUBLISHED studies:", choices = c("Choose one" = "", unique(data_reac$DT[, ..pubcol])), selected = tail(input$pubvalpub))
# })
# 
# output$PubValueUnpub <- renderUI({
#   req(input$file)
#   req(para$pub)
#   pubcol <- para$pub
#   para$pubvalunpub <- input$pubvalunpub
#   selectInput(inputId = "pubvalunpub", label = "Please specify, which value in your dataset refers to UNPUBLISHED studies:", choices = c("Choose one" = "", unique(data_reac$DT[, ..pubcol])), selected = tail(input$pubvalunpub))
# })

##store values in appropriate reactive values and create new columns in data_reac$DT if necessary

observeEvent(input$SubmitButton, {
  ##ES
  
  para$es <- input$ES
  
  if(length(unique(repcols$DT$es)) == 1 & is.na(repcols$DT$es[1]) == TRUE){
  data_reac$DT[, para$es := .SD[[input$EScol]]]}
  
  ##SE
  para$se <- input$SE
  if(length(unique(repcols$DT$se)) == 1 & is.na(repcols$DT$se[1]) == TRUE)
  data_reac$DT[, paste0(para$es, ".SE") := .SD[[input$SE]]]
  
  ##YEAR
  para$year <- input$year
  
  ##N
  para$n <- input$n
  if(length(unique(repcols$DT$n)) == 1 & is.na(repcols$DT$n[1]) == TRUE)
  data_reac$DT[, n := .SD[[input$n]]]
  
  ##ID
  para$id <- input$id
  
  ##PUB
  para$pub <- input$pub
})