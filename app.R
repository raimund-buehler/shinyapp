library(shiny)
library(shinyWidgets)
library(foreign)
library(data.table)
library(here)
library(metafor)
library(metaviz)
library(ggplot2)
library(ggplotify)

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
      
      navbarPage("",
                 tabPanel("About", textOutput("about")),
                 tabPanel("Datafile", tableOutput("table")),
                 navbarMenu("Plots", 
                            
                            # Panel for Forest Plots
                            tabPanel("Forest Plots",
                                     fluidRow(
                                       column(7, align = "center",
                                              # Action Buttons for normal and cumulative forest plot
                                              actionButton(inputId = "normalforest",
                                                           label = "Standard"),
                                              actionButton(inputId = "cumulforest",
                                                           label = "Cumulative")
                                              
                                       )),
                                     
                                     # plot forest plot
                                     fluidRow(
                                       column(7,
                                              plotOutput("forest")),
                                       column(5,
                                              # select forest plot variant
                                              selectInput(inputId = "forestvariant",
                                                          label = "Select Forest Plot Variant",
                                                          choices = c("Classic Forest Plot", "Thick Forest Plot", "Rainforest Plot"),
                                                          selected = "classic"),
                                              
                                              selectInput(inputId = "forestcol",
                                                          label = "Select Colors",
                                                          choices = c("Blues", "Greys", "Oranges", "Greens",
                                                                      "Reds", "Purples")),
                                              
                                              # select variable to sort forest plot
                                              uiOutput("sortingvar"),
                                              
                                              checkboxInput(inputId = "descendingforest",
                                                            label = "Descending Order"))
                                     ),
                                     fluidRow(downloadButton("dwn_forest"))),
                            
                            # Panel for Funnel Plots
                            tabPanel("Funnel Plots",
                                     # split output 50:50
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), 
                                                   plotOutput("normal_funnel"), 
                                                   plotOutput("sunset_funnel"))
                                     ),
                                     
                                     checkboxInput(inputId = "choice_trimfill",
                                                   label = "Show Studies Imputed by Trim-and-Fill"),
                                     checkboxInput(inputId = "choice_egger",
                                                   label = "Show Egger's Regression Line"),
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"),
                                                   downloadButton("dwn_funnel",
                                                                  label = "Download Funnel Plot"),
                                                   downloadButton("dwn_funnelsunset",
                                                                  label = "Download Sunset Funnel Plot"
                                                   ))
                                     )
                            )),
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
      "para$pubvalunpub" = para$pubvalunpub
      )
  })
  
  ######SPECIFY DT COLUMS   
  
  source(here("choose_cols.R"), local = TRUE)
  
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
    setnames(data, input$EScol, as.character(para$ES))
    type_ES <- input$ES
    source(here("calc_effectsize_shiny.R"), local = TRUE)
    data
  })
  
  output$about <- renderPrint({
    c(para$pubvalpub, para$pubvalunpub)
  })
  
  # Create plots ----
  # ** Forest plots ----
  # creative reactive values to monitor change in forest plot type
  rv <- reactiveValues(type_forest = "standard")
  
  observeEvent(input$normalforest, {rv$type_forest <- "standard"})
  observeEvent(input$cumulforest, {rv$type_forest <- "cumulative"})
  
  # Draw forest plot 
  # forestplotInput() is created as reactive object
  # which serves as input to the plot via renderPlot() as well as the download function
  forestplotInput <- reactive({
    req(para$es)
    req(para$se)
    
    # transform selected variant into input for viz_forest
    choice_forest <- switch(input$forestvariant, 
                            "Classic Forest Plot" = "classic",
                            "Thick Forest Plot" = "thick",
                            "Rainforest Plot" = "rain")
    
    # transform selected effect size into axis label
    choice_es <- switch(para$es, 
                        "z" = "Fisher z",
                        "r" = "Pearson r",
                        "d" = "Cohen d",
                        "g" = "Hedges g",
                        "OR" = "Odds Ratio",
                        "logOR" = "Odds Ratio (Log-Scale)")
    
    
    # sort data by selected variable
    data <- data_reac$DT[order(get(input$sortingvar), decreasing = input$descendingforest)]
    
    # plot forest plot variant
    p <- viz_forest(x = data[, .SD, .SDcols = c(para$es, para$se)],
                    study_labels = trimws(data[[para$id]]),
                    xlab = choice_es,
                    variant = choice_forest,
                    annotate_CI = TRUE,
                    type = rv$type_forest,
                    col = input$forestcol)
    
    if(rv$type_forest == "standard"){
      # add centered title
      as.ggplot(p) + ggtitle("Forest Plot") +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (rv$type_forest == "cumulative"){
      as.ggplot(p) + ggtitle("Cumulative Forest Plot") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$forest <- renderPlot({
    print(forestplotInput())
  })
  
  # download forest plot
  output$dwn_forest <- downloadHandler(
    filename = "forestplot.png",
    content = function(file) {
      ggsave(file, plot = forestplotInput(), device = "png", dpi = 300)
    }
  )
  # ** Funnel plots ----
  # same approach as for forest plots: *_funnel_input() is created as reactive object
  # serving as input for renderPlot() and download function
  normal_funnel_input <- reactive({
    req(para$es)
    req(para$se)
    p <- viz_funnel(data_reac$DT[, .SD, .SDcols = c(para$es, para$se)],
                    egger = input$choice_egger,
                    trim_and_fill = input$choice_trimfill)
    as.ggplot(p) + ggtitle("Contour-Enhanced Funnel Plot") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$normal_funnel <- renderPlot({
    print(normal_funnel_input())
    
  })
  
  sunset_funnel_input <- reactive({
    req(para$es)
    req(para$se)
    p <- viz_sunset(data_reac$DT[, .SD, .SDcols = c(para$es, para$se)])
    as.ggplot(p) + ggtitle("Sunset Funnel Plot") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$sunset_funnel <- renderPlot({
    print(sunset_funnel_input())
  })
  # download funnel plots
  # currently only .png is supported
  output$dwn_funnel <- downloadHandler(
    filename = "funnelplot.png",
    content = function(file) {
      ggsave(file, plot = normal_funnel_input(), device = "png", dpi = 300)
    }
  )
  output$dwn_funnelsunset <- downloadHandler(
    filename = "sunset-funnelplot.png",
    content = function(file) {
      ggsave(file, plot = sunset_funnel_input(), device = "png", dpi = 300)
    }
  )
  
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
