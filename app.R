library(shiny)
library(shinyWidgets)
library(foreign)
library(data.table)
library(here)
library(metafor)
library(metaviz)
library(ggplot2)
library(ggplotify)
library(gghighlight)
library(tidyverse)


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
                 
                 # create menu for results of meta and publication bias analyses
                 navbarMenu("Meta-Analysis", 
                            
                            # Meta-Analysis
                            tabPanel("Meta-Analysis",
                                     radioButtons(inputId = "metamodel",
                                                  label = "Select Meta-Analytic Model",
                                                  choiceNames = c("Fixed-effect Model", "Random-effects Model"),
                                                  choiceValues = c("fe", "re"),
                                                  selected = "re"),
                                     uiOutput("select_re_type"),
                                     h3("Results"),
                                     textOutput("meta_out_1"),
                                     p(),
                                     tableOutput("meta_out_2"),
                                     p(),
                                     h4("Test for Heterogeneity"),
                                     textOutput("meta_out_3"),
                                     p(),
                                     verbatimTextOutput("meta_res"),
                                     downloadButton(outputId = "dwn_meta_res",
                                                    label = "Download Results"),
                                     plotOutput("meta_sens")
                                     ),
                            
                            # Moderator Analyses (Meta-Regression, Subgroup-Analyses)
                            tabPanel("Moderator Analyses",
                                     # h3("Subgroup Analysis With One Categorical Moderator"),
                                    
                                     uiOutput("select_catmod"),
                                     verbatimTextOutput("mod_res"),
                                     plotOutput("plot_subgroup")
                            )
                            
                 ),
                 
                 # Publication Bias Analyses
                 navbarMenu("Publication Bias",
                            # Begg & Mazumdar 
                            tabPanel("Begg & Mazumdar's Rank Test"),
                            
                            # Sterne & Egger
                            tabPanel("Sterne & Egger's Regression"),
                            
                            # Trim-and-fill
                            tabPanel("Trim-and-Fill"),
                            
                            # Pcurve
                            tabPanel("p-curve"),
                            
                            # Puniform and puniform*
                            tabPanel("p-uniform and p-uniform*"),
                            
                            # Selection Models
                            tabPanel("Selection Models"),
                            
                            # Test of Excess Significance
                            tabPanel("Test of Excess Significance")
                            )
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
  
  
  # Analyses ----
  # ** Meta-Analysis ----
  # Selection of between-study variance estimator
  # Only shown if re-model was selected (default)
  
  
  output$select_re_type <- renderUI({
    req(input$metamodel)
    if(input$metamodel == "re"){
      selectInput(inputId = "select_re_type",
                  label = "Select Estimator of Between-Study Variance",
                  choices = c("DerSimonian-Laird (DL)", "Hedges (HE)",
                              "Hunter-Schmidt (HS)", "Sidik-Jonkman (SJ)",
                              "Maximum-Likelihood (ML)", "Restricted Maximum-Likelihood (REML)",
                              "Empirical Bayes Estimator (EB)", "Paule-Mandel Estimator (PM)"),
                  selected = "Restricted Maximum-Likelihood (REML)")
    }
  })
  
  # store estimator
  estim <- reactive({
    if (input$metamodel == "re"){
      estim <- switch(input$select_re_type,
                        "DerSimonian-Laird (DL)" = "DL", 
                        "Hedges (HE)" = "HE",
                        "Hunter-Schmidt (HS)" = "HS",
                        "Sidik-Jonkman (SJ)" = "SJ",
                        "Maximum-Likelihood (ML)" = "ML",
                        "Restricted Maximum-Likelihood (REML)" = "REML",
                        "Empirical Bayes Estimator (EB)" = "EB", 
                        "Paule-Mandel Estimator (PM)" = "PM")
    } else if (input$metamodel == "fe"){
      estim<- "FE"
    }
    estim
  })


  # Do the meta-analysis
meta_res_output <- reactive({
    req(input$metamodel)
    
    res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                 method = estim())
 
  })
  
  output$meta_res <- renderPrint({
    summary(meta_res_output())
  })
  
  output$meta_out_1 <- renderText(
    sprintf("Random-Effects Model (k = %s; tau^2 estimator: %s)", meta_res_output()$k.all, meta_res_output()$method)
  )
  
  output$meta_out_2 <- renderTable(
    data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                       "tau (square root of estimated tau^2 value):",
                       "I^2 (total heterogeneity / total variability):",
                       "H^2 (total variability / sampling variability):"),
               val = c(paste(round(meta_res_output()$tau2, 4), " (SE = ", round(meta_res_output()$se.tau2, 4), ")", sep = ""),
                       round(sqrt(meta_res_output()$tau2), 4),
                       paste(round(meta_res_output()$I2, 2), "%", sep = ""),
                       round(meta_res_output()$H2, 2))), colnames = FALSE
  )
  
  output$meta_out_3 <- renderText(
    sprintf("Q(df = %.0f) = %.4f, p %s", meta_res_output()$k.all - 1, meta_res_output()$QE, 
            if(meta_res_output()$QEp < .0001){paste("< .0001")} else {paste("= ", round(meta_res_output()$Qp, 4))})
  )
    
  
  output$dwn_meta_res <- downloadHandler(
    filename = "meta_results.txt",
    content = function(file) {
      sink(file)
      print(meta_res_output())
      sink()
    }
  )
  
  res.estim <- reactive({
  req(data_reac$DT)
    
  # calculate all models
  estim <- c("FE", "DL", "HE", "HS", "SJ", "ML", "REML", "EB", "PM")
  
  res.sens.est <- lapply(estim, function(x){
    res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
               method = x)
    data.frame(es = res$b, se = res$se)
  })
  
  # name list according to method
  names(res.sens.est) <- estim
  
  # create dataframe for plotting
  df.sens <- as.data.table(rbindlist(res.sens.est, idcol = "method"))
  df.sens[, `:=` (method_ordered = fct_reorder(method, es),
                  label = paste(round(es, 2), " (", round(es - 1.96 * se, 2),
                                "; ", round(es + 1.96 * se, 2), ")", sep = ""))]
  df.sens
  })
    
  # create forest plot
output$meta_sens <- renderPlot({
  ggplot(data = res.estim()) + 
    
    # add vertical dashed line to indicate null effect 
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # add point estimate
    geom_point(aes(x = es, y = method_ordered)) +
    
    # add 95% CI
    geom_segment(aes(x = es - 1.96 * se, xend = es + 1.96 * se,
                     y = method_ordered, yend = method_ordered)) +
    
    # add labels
    geom_text(aes(x = es, y = method_ordered, label = label), vjust = -1.5) +
    
    # highlight selected estimation method
    gghighlight(method == estim(), 
                use_direct_label = FALSE,
                unhighlighted_params = list(color = "#525252")) +
    
    # set x-axis limits
    xlim(c(-1, 1)) +
    
    # set labels and title
    xlab("Effect size") + 
    ylab("Estimator") +
    ggtitle("Sensitivity Analysis Based on Estimation Method") +
    
    # set theme and center title
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})
  
  
  
# ** Moderator analyses ----

# **** Subgroup analysis with one categorical moderator ----
# select moderator
output$select_catmod <- renderUI({
  req(data_reac$DT)
    selectInput(inputId = "select_catmod",
                label = "Select Moderator Variable",
                choices = colnames(data_reac$DT))
  
})

# do the moderator analysis
# use estimator chosen in the default meta 
mod_res_output <- reactive({
  req(input$metamodel)
  
  res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
             mods = ~ factor(data_reac$DT[[input$select_catmod]]),
             method = estim())
  
})

output$mod_res <- renderPrint({
  summary(mod_res_output())
})

# subgroup plot
mod_plot_output <- reactive({
  req(mod_res_output())
  viz_forest(x = data_reac$DT[, .SD, .SDcols = c(para$es, para$se)],
             group = data_reac$DT[[input$select_catmod]],
             study_labels = trimws(data_reac$DT[[para$id]]),
             summary_label = levels(as_factor(data_reac$DT[[input$select_catmod]])),
             method = estim(),
             annotate_CI = TRUE)
})

output$plot_subgroup <- renderPlot(
  print(mod_plot_output())
)

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
    # check if metamodel has valid input
    # if not, use REML as default method
    if(isTruthy(input$metamodel)){
      p <- viz_forest(x = data[, .SD, .SDcols = c(para$es, para$se)],
                      study_labels = trimws(data[[para$id]]),
                      xlab = choice_es,
                      variant = choice_forest,
                      annotate_CI = TRUE,
                      type = rv$type_forest,
                      col = input$forestcol,
                      method = "REML")
    } else {
    # use chosen estimator in tab meta-analysis as method to estimate summary es
    p <- viz_forest(x = data[, .SD, .SDcols = c(para$es, para$se)],
                    study_labels = trimws(data[[para$id]]),
                    xlab = choice_es,
                    variant = choice_forest,
                    annotate_CI = TRUE,
                    type = rv$type_forest,
                    col = input$forestcol,
                    method = estim())
    }
    
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
