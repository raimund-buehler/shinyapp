# LIBRARIES ----
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
library(shinydashboard)
library(DT)

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Decline Effects App"),
  
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "file", icon = icon("table")),
      menuItem("Plots", tabName = "plots",
               menuSubItem("Forest Plot", tabName = "forest"),
               menuSubItem("Funnel Plot", tabName = "funnel"), icon = icon("chart-area")),
      
      menuItem("Meta-Analysis", tabName = "MA",
               menuSubItem("Meta-Analysis", tabName = "MAsub"),
               menuSubItem("Subgroup Analysis", tabName = "MoA"),
               menuSubItem("Meta-Regression", tabName = "metareg"), icon = icon("calculator")),
      
      menuItem("Publication Bias", tabName = "PB",
#               >>>>>>> 4f903728099c33f6bc6fb060b89cbd71d75422af
               menuSubItem("Begg & Mazumdar's Rank Test", tabName = "B_M"),
               menuSubItem("Sterne & Egger's Regression", tabName = "S_E"),
               menuSubItem("Trim-and-Fill", tabName = "trif"),
               menuSubItem("p-curve", tabName = "pcurve"),
               menuSubItem("p-uniform and p-uniform*", tabName = "puni"),
               menuSubItem("Selection Models", tabName = "SelMod"),
               menuSubItem("Test of Excess Significance", tabName = "TES"), icon = icon("bolt"))
      
    )
  ),
  
  # Body ----
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "font_correction.css")),
    tabItems(
      
      # ** About ----
      tabItem(tabName = "about",
              h2("ABOUT PAGE")),
      
      # ** Data input ----
      tabItem(tabName = "file",
              fluidRow(
                column(width = 4,
                       box(
                         width = NULL,
                         fileInput(inputId = "file", label = "Please select a .sav file", accept = ".sav", placeholder = "No file selected")
                       ),
                       uiOutput("choices")
                ),
                column(width = 8, uiOutput("table")
                )
              )
      ),
      
      # ** Forest Plot ----
      tabItem(tabName = "forest", 
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
      
      # ** Funnel Plot ----
      tabItem(tabName = "funnel", 
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
                            )
                )
              )
      ),
      
      # ** Meta-Analysis ----
      tabItem(tabName = "MAsub", 
              radioButtons(inputId = "metamodel",
                           label = "Select Meta-Analytic Model",
                           choiceNames = c("Fixed-effect Model", "Random-effects Model"),
                           choiceValues = c("fe", "re"),
                           selected = "re"),
              uiOutput("select_re_type"),
              h3("Results"),
              p(),
              h4("Model Type"),
              textOutput("meta_out_1"),
              p(),
              h4("Summary Effect"),
              textOutput("meta_out_2"),
              p(),
              h4("Heterogeneity Statistics"),
              tableOutput("meta_out_3"),
              p(),
              h4("Test for Heterogeneity"),
              textOutput("meta_out_4"),
              p(),
              verbatimTextOutput("meta_res"),
              downloadButton(outputId = "dwn_meta_res",
                             label = "Download Results"),
              downloadButton(outputId = "dwn_meta_res_obj",
                             label = "Download R-Object with Results"),
              plotOutput("meta_sens")
      ),
      
      # ** Subgroup Analysis ----
      tabItem(tabName = "MoA",
              h3("Subgroup Analysis With One Categorical Moderator"),
              
              uiOutput("select_catmod"),
              uiOutput("select_ref_mod"),
              checkboxInput(inputId = "mod_intrcpt", 
                            label = "Model with Intercept",
                            value = FALSE),
              checkboxInput("knha_mod", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE),
              verbatimTextOutput("mod_res"),
              plotOutput("plot_subgroup")
      ),
      
      
      # ** Meta-Regression ----
      tabItem(tabName = "metareg",
              h3("Meta-Regression"),
              uiOutput("select_reg_mod"),
              checkboxInput("knha_reg", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE),
              verbatimTextOutput("meta_reg")
      ),
      
      tabItem(tabName = "B_M", verbatimTextOutput("BM")),
      tabItem(tabName = "S_E", verbatimTextOutput("SterneEgger")),
      tabItem(tabName = "trif", verbatimTextOutput("TRFI")),
      tabItem(tabName = "pcurve", verbatimTextOutput("pcur")),
      tabItem(tabName = "puni", verbatimTextOutput("p_uni")),
      tabItem(tabName = "SelMod", verbatimTextOutput("Sel_Mod")),
      tabItem(tabName = "TES", verbatimTextOutput("TestOfExc"))
      
    )
  )
)


# SERVER ----
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
    ranktest(rma$res)
  })
  
  
  ##RENDER DATA TABLE
  output$table <- renderUI({
    req(input$file)
    box(title = "Datafile", width = NULL, dataTableOutput("DTable"))
  }) 
  
  output$DTable <- DT::renderDataTable(data_reac$DT,
                                       options = list(pageLength = 15, info = FALSE, lengthMenu = list(c(15,-1), c("15","All")))
  )
  output$choices <- renderUI({
    box(title = "Input",
        width = NULL,
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
    )
    
  })
  
  ######SPECIFY DT COLUMS   
  
  source(here("choose_cols.R"), local = TRUE)
  
  ######PRIMARY SELECT
  
  source(here("primary_select_shiny.R"), local = TRUE)
  
  # Analyses ----
  # ** Meta-Analysis ----
  # **** Selection of between-study variance estimator
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
  
  # **** Run the meta-analysis ----
  meta_res_output <- reactive({
    req(input$metamodel)
    
    res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
               method = estim())
    
  })  
  
  # **** Prep output ----
  
  output$meta_res <- renderPrint({
    summary(meta_res_output())
  })
  
  output$meta_out_1 <- renderText(
    sprintf("%s Model; k = %s%s", 
            if(input$metamodel == "fe") {paste("Fixed-Effect")} else {paste("Random-Effects")},
            meta_res_output()$k.all,
            if(input$metamodel == "fe"){paste("")} else {sprintf("; tau^2 estimator: %s", meta_res_output()$method)})
  )
  
  
  output$meta_out_2 <- renderText(
    sprintf("%s = %.2f, se = %.2f, 95%% CI [%.2f; %.2f], p %s, z = %.2f", 
            para$es,
            meta_res_output()$b,
            meta_res_output()$se,
            meta_res_output()$ci.lb,
            meta_res_output()$ci.ub,
            if(meta_res_output()$pval < .0001){paste("< .0001")} else {paste("= ", round(meta_res_output()$pval, 4))},
            meta_res_output()$zval)
  )
  
  output$meta_out_3 <- renderTable(
    data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                       "tau (square root of estimated tau^2 value):",
                       "I^2 (total heterogeneity / total variability):",
                       "H^2 (total variability / sampling variability):"),
               val = c(paste(round(meta_res_output()$tau2, 4), " (SE = ", round(meta_res_output()$se.tau2, 4), ")", sep = ""),
                       round(sqrt(meta_res_output()$tau2), 4),
                       paste(round(meta_res_output()$I2, 2), "%", sep = ""),
                       round(meta_res_output()$H2, 2))), colnames = FALSE
  )
  
  output$meta_out_4 <- renderText(
    sprintf("Q(df = %.0f) = %.4f, p %s", meta_res_output()$k.all - 1, meta_res_output()$QE, 
            if(meta_res_output()$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(meta_res_output()$QEp, 4), 
                                                                            scientific = FALSE))})
    

  )
  
  
  output$dwn_meta_res <- downloadHandler(
    filename = "meta_results.txt",
    content = function(file) {
      sink(file)
      print(meta_res_output())
      sink()
    }
  )
  
  output$dwn_meta_res_obj <- downloadHandler(
    filename = "meta_results.RDS",
    content = function(file){
      saveRDS(meta_res_output(), file = file)
    }
  )
  
  # **** Sensitivity Analysis Plot ----
  
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
  
  
  # ** Subgroup analysis with one categorical moderator  ----
  
  # **** Select moderator ----
  output$select_catmod <- renderUI({
    req(data_reac$DT)
    lvl <- apply(data_reac$DT, 2, function(x) {
      length(levels(as_factor(x)))
    })
    preselect_mod <- names(which(lvl == min(lvl)))[1]
    selectInput(inputId = "select_catmod",
                label = "Select Moderator Variable",
                choices = colnames(data_reac$DT),
                selected = preselect_mod)
    
  })
  
  # **** Select reference category ----
  output$select_ref_mod <- renderUI({
    selectInput(inputId = "select_ref_mod",
                label = "Select Reference Category of Moderator",
                choices = levels(factor(data_reac$DT[[input$select_catmod]])))
    
  })
  
  # **** Do the moderator analysis ----
  
  # use estimator chosen in the default meta 
  mod_res_output <- reactive({
    req(input$metamodel)
    
    # do the moderator analysis
    
    if(input$mod_intrcpt == FALSE){
      res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                 mods = ~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                  ref = input$select_ref_mod) - 1,
                 method = estim(),
                 knha = input$knha_mod)
    } else if (input$mod_intrcpt == TRUE){
      res <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                 mods =~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                 ref = input$select_ref_mod),
                 method = estim(),
                 knha = input$knha_mod)
      
    }
    res_out <- res
    
    # **** Shorten names of coefficients in object ----
    attr(res_out$beta, "dimnames")[[1]] <- gsub(".*)","", attr(res$beta, "dimnames")[[1]])
    res_out
  })
  
  output$mod_res <- renderPrint({
    print(mod_res_output())
  })
  
  # ** Subgroup plot ----
  mod_plot_output <- reactive({
    req(mod_res_output())
    df <- data.table(mod_res_output()$beta, keep.rownames = TRUE)
    df[, `:=` (ci_ub =  mod_res_output()$ci.ub,
               ci_lb = mod_res_output()$ci.lb,
               rn = str_to_title(rn))]
    colnames(df) <- c("mod", "es", "ci.ub", "ci.lb")
    df[, label := paste0(round(es, 2), " (", round(ci.lb, 2), "; ", round(ci.ub, 2), ")")]
    
    ggplot(data = df) +
      xlim(c(-2, 2)) +
      xlab("Effect size") +
      ylab("Subgroup") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(aes(x = es, y = mod)) +
      geom_segment(aes(x = ci.lb, xend = ci.ub, 
                       y = mod, yend = mod)) +
      geom_text(aes(x = es, y = mod, label = label), vjust = -2) + 
      theme_minimal()
    
  })
  
  output$plot_subgroup <- renderPlot(
    print(mod_plot_output())
  )
  
  # ** Meta-Regression ----
  
  # **** Select moderators ----
  output$select_reg_mod <- renderUI({
    req(data_reac$DT)
    selectInput(inputId = "select_reg_mod",
                label = "Select all moderator variables",
                choices = colnames(data_reac$DT),
                multiple = TRUE)
  })
  
  # **** Do the meta-regression ----
  meta_reg_output <- reactive({
    req(input$select_reg_mod)
    
    # paste all moderators as formula 
    mods <- paste0("data_reac$DT[[", "'", input$select_reg_mod, "'", "]]", collapse = " + ")
    
    # do the analysis
    res.reg <- rma(data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                   mods = as.formula(paste("~", mods)),
                   method = estim(),
                   knha = input$knha_reg)
    
    
  })
  
  output$meta_reg <- renderPrint({
    print(meta_reg_output())
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
                    trim_and_fill = input$choice_trimfill,
                    method = estim())
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
  
  #####Publication Bias Methods
  ##Begg and Mazumdar
  
  output$BM <- renderPrint({
    ranktest(meta_res_output())
  })
  
  ##Sterne and Egger
  output$SterneEgger <- renderPrint({
    regtest(meta_res_output())
  }) 
  
  ##Trim and Fill
  output$TRFI <- renderPrint({
    if (sign(meta_res_output()$b) == 1) {
      trimfill(meta_res_output(), side = "left")
    } else if (sign(meta_res_output()$b) == -1) {
      trimfill(meta_res_output(), side = "right")
    }
  })
  
  ##pcurve
  output$pcur <- renderPrint({
  })
  
}

shinyApp(ui = ui, server = server)
