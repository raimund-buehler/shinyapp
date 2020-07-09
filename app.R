library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel(fileInput("file", label = ("Please select a .csv file"))),
    fluidRow(tableOutput("table")),
  )
)


# Define server logic ----
server <- function(input, output) {
  
  output$table <- renderTable({
    read.csv(input$file$datapath)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)