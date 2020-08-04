
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