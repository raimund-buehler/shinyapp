library(shiny)
library(foreign)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "Please select a .sav file", accept = ".sav", placeholder = "No file selected")
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About", textOutput("about")),
                  tabPanel("Datafile", tableOutput("table")),
                  tabPanel("Plots", textOutput("plots")),
                  tabPanel("Results", textOutput("results"))
        )
      )
    )
  )


# Define server logic ----
server <- function(input, output) {
  
  output$table <- renderTable({
    #render the file that was selected, req(file) is need to avoid error message when no file is uploaded
    file <- input$file1
    
    req(file)
    
    read.spss(file$datapath)
  })
  
  output$about <- renderText({
    "This is the about section"
  })
  
  output$plots <- renderText({
    "This is the plot section"
  })
  
  output$results <- renderText({
    "This is the results section"
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)