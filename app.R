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
      #uiOutput("structure"),
      uiOutput("EScolumn"),
      uiOutput("SEcolselect"),
      uiOutput("EStype"),
      uiOutput("nameselect"),
      uiOutput("yearselect"),
      uiOutput("primarySelect"),
      uiOutput("candidateSelect"),
      uiOutput("primaryPublSelect"),
      uiOutput("candidatePublSelect"),
      #textInput(inputId = "primary_name", label = "Please specify the primary study"),
      #radioButtons(inputId = "ES", label = "Unsure, which is the primary study? Tell us both candidates!", choices = c("yes", "no"))
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
                                     
                                     # plotOutput("normal_funnel"),
                                     #  plotOutput("sunset_funnel"),
                                     # uiOutput("select_trimfill"),
                                     #  uiOutput("choice_egger"),
                                     
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
   
   # standard error
   output$SEcolselect <- renderUI({
     req(input$file)
     selectInput(inputId = "SEcol", label = "Please choose the variable that contains the standard errors",
                 choices = colnames(data()))
   })
   
##CHOOSE EFFECT SIZE TYPE
   output$EStype <- renderUI({
     req(input$chooseEScol)
     radioButtons(inputId = "ES", label = "Please choose the effect size in your data set", choices = c("z", "r", "d", "g", "OR", "logOR"), selected = character(0))
   })
   
   # studyname
   output$nameselect <- renderUI({
     req(input$file)
     selectInput(inputId = "namecol", label = "Please select the variable that includes unique study IDs",
                 choices = colnames(data()))
   })
   
   # year
   output$yearselect <- renderUI({
     req(input$file)
     selectInput(inputId = "yearcol", label = "Please select the variable that indicates the publication year",
                 choices = colnames(data()))
   })
   
   # sorting variable (forest plot)
   output$sortingvar <- renderUI({
     req(input$file)
     selectInput(inputId = "sortingvar",
                 label = "Select Sorting Variable",
                 choices = colnames(data()))
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
    req(input$chooseEScol)
    req(input$SEcol)
    
    # transform selected variant into input for viz_forest
    choice_forest <- switch(input$forestvariant, 
                            "Classic Forest Plot" = "classic",
                            "Thick Forest Plot" = "thick",
                            "Rainforest Plot" = "rain")
    
    # transform selected effect size into axis label
    choice_es <- switch(input$ES, 
                        "z" = "Fisher z",
                        "r" = "Pearson r",
                        "d" = "Cohen d",
                        "g" = "Hedges g",
                        "OR" = "Odds Ratio",
                        "logOR" = "Odds Ratio (Log-Scale)")
    
    
    # sort data by selected variable
    data <- data()[order(get(input$sortingvar), decreasing = input$descendingforest)]
    
    # plot forest plot variant
    p <- viz_forest(x = data[, .SD, .SDcols = c(input$chooseEScol, input$SEcol)],
                    study_labels = data[[input$namecol]],
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
    req(input$chooseEScol)
    req(input$SEcol)
    p <- viz_funnel(data()[, .SD, .SDcols = c(input$chooseEScol, input$SEcol)],
                    egger = input$choice_egger,
                    trim_and_fill = input$choice_trimfill)
    as.ggplot(p) + ggtitle("Contour-Enhanced Funnel Plot") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$normal_funnel <- renderPlot({
    print(normal_funnel_input())
    
  })
  
  sunset_funnel_input <- reactive({
    req(input$chooseEScol)
    req(input$SEcol)
    p <- viz_sunset(data()[, .SD, .SDcols = c(input$chooseEScol, input$SEcol)])
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
