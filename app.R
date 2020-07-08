library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Decline Effects App"),
  
  sidebarLayout(
    sidebarPanel("Please select a .csv file"),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)