library(shiny)
library(foreign)
library(data.table)
library(here)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Please select a .sav file", accept = ".sav", placeholder = "No file selected"),
      radioButtons(inputId = "ES", label = "Please choose the effect size in your data set", choices = c("z", "r", "d", "g", "OR", "logOR"))
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
server <- function(input, output) {
  data <- reactive({req(input$file)
               
           data <- read.spss(input$file$datapath)
           data$studyname <- trimws(data$studyname)
           data <- as.data.table(data)
           data
          })
  
  output$table <- renderTable({
    #render the file that was selected, req(file) is need to avoid error message when no file is uploaded
    data()
  })
  
  output$about <- renderText({
    "This is the about section"
  })
  
  output$plots <- renderText({
    "This is the plot section"
  })
  #Check Colnames
  
  output$checkCols <- renderText({
    req(input$file)
    data <- as.data.table(data())
    type_ES <- input$ES

    source(here("check_colnames.R"), local = TRUE)
    report.colnames
  }, sep = "\n")
  
  
  #Cald Effectsizes
  output$EScalc <- renderTable({
    req(input$file)
    data <- as.data.table(data())
    type_ES <- input$ES
    
    source(here("calc_effectsize.R"), local = TRUE)
    data
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)