# ** Forest plots ----

# sorting variable (forest plot)
output$sortingvar <- renderUI({
  req(input$file)
  selectInput(inputId = "sortingvar",
              label = "Select Sorting Variable",
              choices = colnames(data_reac$DT),
              selected = para$year)
})

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
  req(para$id)
  req(input$sortingvar)
  
  
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

plotheight <- reactive({
  paste0(input$forestheight, "px")
})

output$forestplotOut <- renderPlot(
 print(forestplotInput())
)

output$forest <- renderUI({
  req(forestplotInput())
  plotOutput("forestplotOut", height = plotheight(), width = "100%")
})

#output$forest <- renderPlot(height = input$forest_height, 
                         #   {
 # req(forestplotInput())
  #print(forestplotInput())
#})


# download forest plot
plotheight_inch <- reactive({
  plotheight()/300
})

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
  req(normal_funnel_input())
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
  req(sunset_funnel_input())
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