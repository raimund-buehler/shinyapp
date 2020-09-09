# Report ----
# set parameters ----
params_report <- reactive({
  source(here("params_report.R"), local = TRUE)
  return(params)
})


params_meta <- reactive({
  source(here("params_meta.R"), local = TRUE)
  return(params_meta)
})

params_sbgrp <- reactive({
  source(here("params_sbgrp.R"), local = TRUE)
  return(params_sbgrp)
})

params <- reactive({
  params <- c(params_report(), params_meta(), params_sbgrp())
})

#params <- reactive({

 # source(here("params_report.R"), local = TRUE)
  #if(){source(here("params_meta.R"), local = TRUE)}
  #if(){source(here("params_sbgrp.R"), local = TRUE)}
  #return(params)
#})


# render UI for optional checkboxes ----
output$dwn_report_sbgrp <- renderUI({
  req(input$go_moa > 0)
  prettyCheckbox(
  inputId = "dwn_report_sbgrp",
  label = "Results of Subgroup Analysis", 
  value = TRUE,
  status = "warning"
  )
})

output$dwn_report_metareg <- renderUI({
  req(input$go_metareg > 0)
prettyCheckbox(
  inputId = "dwn_report_metareg",
  label = "Results of Metaregression", 
  value = TRUE,
  status = "warning"
)
})


output$dwn_report <- downloadHandler(
 filename = "report.pdf",
    content = function(file) {

     tempReport <- file.path(tempdir(), "report.Rmd")
     file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
     params <- params()
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
     )
    }
  )



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


output$pcurve_example <- renderPlot({
  print(pcurve_plot())
})

output$dwn_pcurve_inputstring <- downloadHandler(
  filename = "pcurve_input_webapp.txt",
  content = function(file) {
    cat(pcurve()$raw, sep = "\n", file = file)
  }
)
  
  output$dwn_pcurve_res <- downloadHandler(
    filename = "pcurve_results.txt",
    content = function(file) {
      sink(file)
      print(pcurve_table())
      cat("\n")
      cat("Power of tests included in p-curve (correcting for selective reporting): ",
             percent(pcurve()$hat), sep = "")
      cat("\n")
      cat(
        "90% Confidence interval: ",
        percent(pcurve()$power.ci.lb), 
        " ; ",
        percent(pcurve()$power.ci.ub), sep = "")
        sink()
    }
  )

  output$dwn_puni_res <- downloadHandler(
    filename = function(){
      fileext <-  switch(input$dwn_select_filetype_puni, 
                         "Results (.txt)" = ".txt",
                         "R-Object (.rds)" = ".rds")
      paste0("results_puniform", fileext)},
    content = function(file){
      if(input$dwn_select_filetype_puni == "Results (.txt)"){
        sink(file)
        print(PUNIres$res)
        sink()
      } else if(input$dwn_select_filetype_puni == "R-Object (.rds)"){
        saveRDS(PUNIres$res, file)
      }
    }
  )
  
  output$dwn_punistar_res <- downloadHandler(
    filename = function(){
      fileext <-  switch(input$dwn_select_filetype_puni, 
                         "Results (.txt)" = ".txt",
                         "R-Object (.rds)" = ".rds")
      paste0("results_puniform_star", fileext)},
    content = function(file){
      if(input$dwn_select_filetype_puni == "Results (.txt)"){
        sink(file)
        print(PUNISTres$res)
        sink()
      } else if(input$dwn_select_filetype_puni == "R-Object (.rds)"){
        saveRDS(PUNISTres$res, file)
      }
    }
  )
  
  
  # Other methods ----
  output$dwn_tes_res <- downloadHandler(
    filename = "test_excess_significance.txt",
    content = function(file){
      cat("Chisquare-test for difference between observed and expected significant studies:", "\n\n", "p = ", 
          TESres$res, file = file)
    }
  )
  
  output$dwn_selmod <- downloadHandler(
    filename = "selection_models.txt",
    content = function(file){
      cat("Results of Selection Models According to Vevea & Woods (2005): ",
          "\n",
          "Moderate one-tailed selection: ", round(SelMods$mod1$output_adj$par[[2]], 4), "\n",
          "Severe one-tailed selection: ", round(SelMods$sev1$output_adj$par[[2]], 4), "\n",
          "Moderate two-tailed selection: ", round(SelMods$mod2$output_adj$par[[2]], 4), "\n",
          "Severe two-tailed selection: ", round(SelMods$sev2$output_adj$par[[2]], 4), "\n",
          file = file)
    }
  )
  
  output$dwn_selmod_mod1 <- downloadHandler(
    filename = "selection_model_mod1.RDS",
    content = function(file){
      saveRDS(SelMods$mod1, file = file)
    }
  )
  
  output$dwn_selmod_sev1 <- downloadHandler(
    filename = "selection_model_sev1.RDS",
    content = function(file){
      saveRDS(SelMods$sev1, file = file)
    }
  )
  
  output$dwn_selmod_mod2 <- downloadHandler(
    filename = "selection_model_mod2.RDS",
    content = function(file){
      saveRDS(SelMods$mod2, file = file)
    }
  )
  
  output$dwn_selmod_sev2 <- downloadHandler(
    filename = "selection_model_sev2.RDS",
    content = function(file){
      saveRDS(SelMods$sev2, file = file)
    }
  )