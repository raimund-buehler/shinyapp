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
  
  
  #<<<<<<< user-input-interface
  output$SingleES <- renderText({
    paste("Effect size:   ", "'", unique(repcols$DT$es), "'", sep = "")
  })
  
#  If no column name matches, both ES type and column have to be specified
  output$EScolumn <- renderUI({
    req(input$file)
    if (length(unique(repcols$DT$es)) == 1 & is.na(repcols$DT$es[1]) == TRUE) {
      selectInput(inputId = "EScol", label = "Please choose the colum in your dataset that contains the effect size values", choices = c("Choose one" = "", colnames(data_reac$DT)))
    }
  })
  
  #Set name of EScol in data_reac$DT to para$ES
  
  # observeEvent(input$EScol, {
  #   req(input$EScol)
  #   data <- data_reac$DT
  #   old <- input$EScol
  #   new <- para$es
  #   setnames(data, old, new)})
  
  # sorting variable (forest plot)
  output$sortingvar <- renderUI({
    req(input$file)
    selectInput(inputId = "sortingvar",
                label = "Select Sorting Variable",
                choices = colnames(data_reac$DT))
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
