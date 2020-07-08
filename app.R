library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(fileInput("file", label = ("Please select a .csv file"))),
    fluidRow(column(4, verbatimTextOutput("value")))
  )
)

# Define server logic ----
server <- function(input, output) {
  output$value <- renderPrint({
    str(input$file)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)