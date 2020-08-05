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