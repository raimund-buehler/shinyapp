# ** Subgroup analysis with one categorical moderator  ----

# **** Select moderator ----
output$select_catmod <- renderUI({
  validate(
    need(isTruthy(meta_res_output()), 
         "Please run meta-analysis first"))
  
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
  req(meta_res_output())
  req(input$select_catmod)
  
  
  selectInput(inputId = "select_ref_mod",
              label = "Select Reference Category of Moderator",
              choices = levels(factor(data_reac$DT[[input$select_catmod]])))
  
})

# **** Do the moderator analysis ----

# use estimator chosen in the default meta 
mod_res_output <- reactive({
  req(input$select_catmod)
  req(input$select_ref_mod)
  validate(
    need(isTruthy(meta_res_output()), "Please run the meta-analysis first")
  )
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
  req(mod_res_output())
  print(mod_res_output())
})

# ** Subgroup plot ----
mod_plot_output <- reactive({
  
  validate(
    need(isTruthy(meta_res_output()), "Please run the meta-analysis first")
  )
  
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