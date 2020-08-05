# data ----
dt_dwn <- reactive({
dt_dwn <- data_reac$DTall
# rename variable OR to oddsratio because OR is reserved variable name in SPSS
# also renamed for rds output for consistency
setnames(dt_dwn, old = "OR", new = "oddsratio")
return(dt_dwn)
})

output$dwn_data <- downloadHandler(
  filename = function(){paste0("data", input$dwn_select_filetype)},
  content = function(file){
    if (input$dwn_select_filetype == ".sav"){
      haven::write_sav(dt_dwn(), file)
    } else if (input$dwn_select_filetype == ".rds"){
      saveRDS(dt_dwn(), file = file)
    }
  }
)

# meta-analysis ----
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

# subgroup analysis ----
output$dwn_sbgrp_res <- downloadHandler(
  filename = "subgroup_results.txt",
  content = function(file) {
    sink(file)
    print(mod_res_output())
    sink()
  }
)

output$dwn_sbgrp_res_obj <- downloadHandler(
  filename = "subgroup_results.RDS",
  content = function(file){
    saveRDS(mod_res_output(), file = file)
  }
)

# subgroup analysis ----
output$dwn_sbgrp <- downloadHandler(
  filename = "subgroup_analysis.png",
  content = function(file) {
    ggsave(file, plot = mod_plot_output(), device = "png", dpi = 300)
  }
)

# meta-regression ----
output$dwn_metareg_res <- downloadHandler(
  filename = "metaregression_results.txt",
  content = function(file) {
    sink(file)
    print(meta_reg_output())
    sink()
  }
)

output$dwn_metareg_res_obj <- downloadHandler(
  filename = "metaregression_results.RDS",
  content = function(file){
    saveRDS(meta_reg_output(), file = file)
  }
)

# forest plot ----
plotheight_inch <- reactive({
  as.numeric(input$forestheight)/300
})



output$dwn_forest <- downloadHandler(
  filename = "forestplot.png",
  content = function(file) {
    ggsave(file, plot = forestplotInput()$p.print, 
           height = plotheight_inch() * 4.16,
           width = (956/300) * 4.16,
           device = "png", dpi = 300)
  }
)

output$forest_example <- renderPlot(
  print(forestplotInput()$p.screen)
)

# funnel plot ----
output$funnel_example <- renderPlot(
  print(normal_funnel_input())
)

output$funnelsunset_example <- renderPlot(
  print(sunset_funnel_input())
)

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

# sensitivity analysis ----
output$dwn_sens <- downloadHandler(
  filename = "sensitivity_analysis.png",
  content = function(file) {
    ggsave(file, plot = meta_sens(), device = "png", dpi = 300)
  }
)

# Publication Bias ----
# ** Small Study Effects ----
out_dwn_sse <- reactive({
  fileext <- switch(input$dwn_select_filetype_sse,
         "Results (.txt)" = ".txt",
         "R-Object (.rds)" = ".rds")
  fileext
})

output$dwn_bm <- downloadHandler(
  filename = function(){
   fileext <-  switch(input$dwn_select_filetype_sse,
           "Results (.txt)" = ".txt",
           "R-Object (.rds)" = ".rds")
    paste0("results_beggmazumdar", fileext)},
  content = function(file){
    if(input$dwn_select_filetype_sse == "Results (.txt)"){
      sink(file)
      print(BMres$res)
      sink()
    } else if(input$dwn_select_filetype_sse == "R-Object (.rds)"){
      saveRDS(BMres$res, file)
    }
  }

)

output$dwn_se <- downloadHandler(
  filename = function(){
    fileext <-  switch(input$dwn_select_filetype_sse,
                       "Results (.txt)" = ".txt",
                       "R-Object (.rds)" = ".rds")
    paste0("results_sterneegger", fileext)},
  content = function(file){
    if(input$dwn_select_filetype_sse == "Results (.txt)"){
      sink(file)
      print(SEres$res)
      sink()
    } else if(input$dwn_select_filetype_sse == "R-Object (.rds)"){
      saveRDS(SEres$res, file)
    }
  }
  
)

output$dwn_tf <- downloadHandler(
  filename = function(){
    fileext <-  switch(input$dwn_select_filetype_sse,
                       "Results (.txt)" = ".txt",
                       "R-Object (.rds)" = ".rds")
    paste0("results_trimfill", fileext)},
  content = function(file){
    if(input$dwn_select_filetype_sse == "Results (.txt)"){
      sink(file)
      print(TFres$res)
      sink()
    } else if(input$dwn_select_filetype_sse == "R-Object (.rds)"){
      saveRDS(TFres$res, file)
    }
  }
  
)

# *pvalue-based methods ----


output$dwn_pcurve_plot <- downloadHandler(
  filename = "pcurve_plot.png",
  content = function(file) {
    png(file, width=2600, height=2400, res=400)
    source(here("pcurve_plot.R"), local = TRUE)
    dev.off()
  })