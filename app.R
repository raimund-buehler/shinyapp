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
      #uiOutput("structure"),
      uiOutput("EScolumn"),
      uiOutput("EStype"),
      uiOutput("primarySelect"),
      uiOutput("candidateSelect"),
      uiOutput("primaryPublSelect"),
      uiOutput("candidatePublSelect"),
      #textInput(inputId = "primary_name", label = "Please specify the primary study"),
      #radioButtons(inputId = "ES", label = "Unsure, which is the primary study? Tell us both candidates!", choices = c("yes", "no"))
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About", textOutput("about")),
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
   data <- reactive({
    req(input$file)
               
    data <- read.spss(input$file$datapath)
    data$studyname <- trimws(data$studyname)
    data <- as.data.table(data)
    data
  })
   
   # output$structure <- renderUI({
   #   req(input$file)
   #   radioButtons(inputId = "struc", label = "Is the dataset structured according to the requirements?", choices = c("yes", "no"), selected = character(0))
   # })

## CHOOSE EFFECT SIZE COLUMN
   
   output$EScolumn <- renderUI({
     req(input$file)
     selectInput(inputId = "chooseEScol", label = "Please choose the variable that contains your effect size values", choices = colnames(data()), selected = character(0))
   })
   
##CHOOSE EFFECT SIZE TYPE
   output$EStype <- renderUI({
     req(input$chooseEScol)
     radioButtons(inputId = "ES", label = "Please choose the effect size in your data set", choices = c("z", "r", "d", "g", "OR", "logOR"), selected = character(0))
   })

####ASSIGNING PRIMARY STUDY CANDIDATES; CONFIRM BY USER
  
  #preselect earliest overall studies in dataset (column "studyname" required)
  primary_temp <- reactive({
    data <- data()
    data[year == min(year)]$studyname
  })
  
  #preselect earliest published studies
  primary_publ_temp <- reactive({
    data <- data()
    data[pub == "pub", .SD[year == min(year)]]$studyname
  })
  
  #Let user choose the earliest study, if more than one study of same (earliest) year is found
  #if less than one: just confirm the study that was found 
  output$primarySelect <- renderUI({
    req(input$ES)
    if(length(primary_temp())>1){
       radioButtons("primaryChoice", label = "Please select the earliest study in your dataset", choices = c(primary_temp(), "unsure"), selected = character(0))
     } else {
       radioButtons(inputId = "primaryChoice", label = paste("Is this the earliest study in your dataset:", primary_temp(), "?"), choices = c("yes", "no", "unsure"), selected = character(0))
     }
   })
  
  #If user selects unsure, checkboxes are displayed of the same preselected studies and user can choose two candidates
  output$candidateSelect <- renderUI({ 
  req(input$primaryChoice)
  if(input$primaryChoice == "unsure"){
       checkboxGroupInput("candidateChoice", label = "Please select the TWO candidate studies that are most likely the earliest studies!", choices = primary_temp())
     }
  })
  
  #Set maximum of checkbox choices for primary candidates to two
  observe({
    if(length(input$candidateChoice) > 2){
      updateCheckboxGroupInput(session, inputId = "candidateChoice", selected= tail(input$candidateChoice, 2))
    }
  })
  
  #Same thing, but for published studies only
  output$primaryPublSelect <- renderUI({
    req(input$primaryChoice)
    if(length(primary_publ_temp())>1){
      radioButtons("primaryPublChoice", label = "Please select the earliest PUBLISHED study in your dataset", choices = c(primary_publ_temp(), "unsure"), selected = character(0))
    } else {
      radioButtons(inputId = "primaryPublChoice", label = paste("Is this the earliest PUBLISHED study in your dataset:", primary_publ_temp(), "?"), choices = c("yes", "no", "unsure"), selected = character(0))
    }
  })
  
  #select candidates
  output$candidatePublSelect <- renderUI({ 
    req(input$primaryPublChoice)
    if(input$primaryPublChoice == "unsure"){
      checkboxGroupInput("candidatePublChoice", label = "Please select the TWO candidate PUBLISHED studies that are most likely the earliest studies!", choices = primary_publ_temp())
    }
  })
  
  #limit amount of checked boxes to two
  observe({
    if(length(input$candidatePublChoice) > 2){
      updateCheckboxGroupInput(session, inputId = "candidatePublChoice", selected= tail(input$candidatePublChoice, 2))
    }
  })
  
  ###DISPLAY DATA TABLE
  output$table <- renderTable({
    data()
  })
  #Check Colnames
  
  output$checkCols <- renderText({
    req(input$file)
    data <- as.data.table(data())
    type_ES <- input$ES

    source(here("check_colnames.R"), local = TRUE)
    report.colnames
  }, sep = "\n")
  
  
  #Calc Effectsizes
  output$EScalc <- renderTable({
    req(input$file)
    data <- as.data.table(data())
    setnames(data, input$chooseEScol, as.character(input$ES))
    type_ES <- input$ES
    source(here("calc_effectsize.R"), local = TRUE)
    data
  })
  
  output$about <- renderText({
    "This is the about section"
  })
  
  output$plots <- renderText({
    "This is the plot section"
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
