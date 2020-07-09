library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "Please select a .csv file", accept = ".csv", placeholder = "No file selected")
      ),
    mainPanel(
      tableOutput("table"))
    )
  )


# Define server logic ----
server <- function(input, output) {
  
  output$table <- renderTable({
    file <- input$file1
    
    req(file)
    
    read.csv(file$datapath)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)