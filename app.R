library(shiny)
library(shinyWidgets)
library(foreign)
library(data.table)
library(here)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Please select a .sav file", accept = ".sav", placeholder = "No file selected"),

      uiOutput("EStype"),
      uiOutput("EScolumn"),
      uiOutput("SEcolumn"),
      uiOutput("Year"),
      uiOutput("SampleSize"),
      uiOutput("StudyID"),
      uiOutput("PubStatus"),
      uiOutput("PubValuePub"),
      uiOutput("PubValueUnpub"),
      uiOutput("primarySelect"),
      uiOutput("candidateSelect"),
      uiOutput("primaryPublSelect"),
      uiOutput("candidatePublSelect"),
      #textInput(inputId = "primary_name", label = "Please specify the primary study")
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About", verbatimTextOutput("about")),
                  tabPanel("Datafile", tableOutput("table")),
                  tabPanel("Plots", textOutput("plots")),
                  tabPanel("Check Colnames", verbatimTextOutput("checkCols")),
                  tabPanel("Effect size calc", tableOutput("EScalc")),
                  tabPanel("Results", textOutput("results"))
        )
      )
    )
  )


# Define server logic ----
server <- function(input, output, session) {
## READ DATA FILE and coerce to data.table
  
  data_reac <- reactiveValues()
  observe({
    req(input$file)
    data_reac$DT <- as.data.table(read.spss(input$file$datapath))
  })
  
#CHECK COLNAMES
   repcols <- reactiveValues()
   observe({
   repcols$DT <- {
     req(input$file)
     data <- data_reac$DT
     source(here("check_colnames_shiny.R"), local = TRUE)
     report.colnames
   }
   })
   
   output$checkCols <- renderPrint({
     req(input$file)
     c(repcols$DT,
     "para$es" = para$es,
     "para$se" = para$se,
     "para$year" = para$year,
     "para$n" = para$n,
     "para$id" = para$id,
     "para$pub" = para$pub,
     "para$pubvalpub" = para$pubvalpub,
     "para$pubvalunpub" = para$pubvalunpub)
   })
   
######SPECIFY DT COLUMS   
   
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
       para$es <- vec[1]
       verbatimTextOutput("SingleES")
     } else if (length(unique(vec)) > 1) {
       para$es <- input$ES
       selectInput(inputId = "ES", label = "Please choose the effect size in your data set", choices = c("Choose one" = "", unique(vec)), selected = tail(input$ES))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$es <- input$ES
       selectInput(inputId = "ES", label = "Please choose the effect size in your data set", c("Choose one" = "", "r", "z", "g", "d", "OR", "logOR"), selected = tail(input$ES))
     }
   })

   
   output$SingleES <- renderText({
     paste("Effect size:   ", "'", unique(repcols$DT$es), "'", sep = "")
   })
   
   #If no column name matches, both ES type and column have to be specified
   output$EScolumn <- renderUI({     
     req(input$file)
     if (length(unique(repcols$DT$es)) == 1 & is.na(repcols$DT$es[1]) == TRUE) {
       selectInput(inputId = "EScol", label = "Please choose the colum in your dataset that contains the effect size values", choices = c("Choose one" = "", colnames(data_reac$DT)))
     }  
     })


##CHOOSE STANDARD ERROR COLUMN

   output$SEcolumn <- renderUI({
     req(input$file)
     vec <- repcols$DT$se
     if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
       para$se <- vec[1]
       verbatimTextOutput("SingleSE")
     } else if (length(unique(vec)) > 1) {
       para$se <- input$SE
       selectInput(inputId = "SE", label = "Sampling variance", choices = unique(vec), selected = tail(input$SE))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$se <- input$SE
       selectInput(inputId = "SE", label = "Sampling variance:", choices = c("Choose one" = "", colnames(data_reac$DT)), selected = tail(input$SE))
     }
   })

   output$SingleSE <- renderText({
     paste("Sampling variance:   ", "'", unique(repcols$DT$se), "'", sep = "")
   })

# 
# ##CHOOSE YEAR
   
   output$Year<- renderUI({
     req(input$file)
     vec <- repcols$DT$year
     if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
       para$year <- vec[1]
       verbatimTextOutput("SingleYear")
     } else if (length(unique(vec)) > 1) {
       para$year <- input$year
       selectInput(inputId = "year", label = "Publication Year:", choices = unique(vec), selected = tail(input$year))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$year <- input$year
       selectInput(inputId = "year", label = "Publication Year:", choices = c("Choose one" = "", colnames(data_reac$DT)), selected = tail(input$year))
     }
   })
   
   output$SingleYear <- renderText({
     paste("Publication Year:   ", "'", unique(repcols$DT$year), "'", sep = "")
   })

##CHOOSE SAMPLE SIZE

   output$SampleSize<- renderUI({
     req(input$file)
     vec <- repcols$DT$n
     if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
       para$n <- vec[1]
       verbatimTextOutput("SingleN")
     } else if (length(unique(vec)) > 1) {
       para$n <- input$n
       selectInput(inputId = "n", label = "Sample size:", choices = unique(vec), selected = tail(input$n))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$n <- input$n
       selectInput(inputId = "n", label = "Sample size:", choices = c("Choose one" = "", colnames(data_reac$DT)), selected = tail(input$n))
     }
   })
   
   output$SingleN <- renderText({
     paste("Sample size:   ", "'", unique(repcols$DT$n), "'", sep = "")
   })
   
##CHOOSE STUDY ID
   output$StudyID<- renderUI({
     req(input$file)
     vec <- repcols$DT$id
     if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
       para$id <- vec[1]
       verbatimTextOutput("SingleID")
     } else if (length(unique(vec)) > 1) {
       para$id <- input$id
       selectInput(inputId = "id", label = "Study Name:", choices = unique(vec), selected = tail(input$id))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$id <- input$id
       selectInput(inputId = "id", label = "Study Name:", choices = c("Choose one" = "", colnames(data_reac$DT)), selected = tail(input$id))
     }
   })
   
   output$SingleID <- renderText({
     paste("Study Name:   ", "'", unique(repcols$DT$id), "'", sep = "")
   })
##CHOOSE PUBLICATION STATUS (VALUES)?
   
   output$PubStatus<- renderUI({
     req(input$file)
     vec <- repcols$DT$pub
     if (length(unique(vec)) == 1 & is.na(vec[1]) == FALSE) {
       para$pub <- vec[1]
       verbatimTextOutput("SinglePub")
     } else if (length(unique(vec)) > 1) {
       para$pub <- input$pub
       selectInput(inputId = "pub", label = "Publication Status:", choices = unique(vec), selected = tail(input$pub))
     } else if (length(unique(vec)) == 1 & is.na(vec[1]) == TRUE) {
       para$pub <- input$pub
       selectInput(inputId = "pub", label = "Publication Status:", choices = c("Choose one" = "", colnames(data_reac$DT)), selected = tail(input$pub))
     }
   })
   
   output$SinglePub <- renderText({
     paste("Publication Status:   ", "'", unique(repcols$DT$pub), "'", sep = "")
   })
   
   #values
   
   output$PubValuePub <- renderUI({
     req(input$file)
     req(para$pub)
     pubcol <- para$pub
     para$pubvalpub <- input$pubvalpub
     selectInput(inputId = "pubvalpub", label = "Please specify, which value in your dataset refers to PUBLISHED studies:", choices = c("Choose one" = "", unique(data_reac$DT[, ..pubcol])), selected = tail(input$pubvalpub))
   })
   
   output$PubValueUnpub <- renderUI({
     req(input$file)
     req(para$pub)
     pubcol <- para$pub
     para$pubvalunpub <- input$pubvalunpub
     selectInput(inputId = "pubvalunpub", label = "Please specify, which value in your dataset refers to UNPUBLISHED studies:", choices = c("Choose one" = "", unique(data_reac$DT[, ..pubcol])), selected = tail(input$pubvalunpub))
   })
   
   
   
####ASSIGNING PRIMARY STUDY CANDIDATES; CONFIRM BY USER

  
  ###TAB OUTPUT
  #Display Data table
  
  output$table <- renderTable({
    data_reac$DT
  })
  
  
  #Calc Effectsizes
  output$EScalc <- renderTable({
    req(input$file)
    data <- data_reac$DT
    setnames(data, input$EScol, as.character(input$ES))
    type_ES <- input$ES
    source(here("calc_effectsize_shiny.R"), local = TRUE)
    data
  })
  
  output$about <- renderPrint({
    c(para$pubvalpub, para$pubvalunpub)
  })
  
  output$plots <- renderText({
    "This is the plot section"
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
