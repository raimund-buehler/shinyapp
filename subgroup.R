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
mod_res_output <- eventReactive(input$go_moa, {
  req(input$select_catmod)
  req(input$select_ref_mod)
  validate(
    need(isTruthy(meta_res_output()), "Please run the meta-analysis first")
  )

  
  # do the moderator analysis
  # moderator analysis is run four times (with/without intercept and with/without knha in model spec)
  l <- list()
  l$no_intrcpt <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                      mods = ~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                       ref = input$select_ref_mod) - 1,
                      method = estim(),
                      knha = FALSE)
  
  l$no_intrcpt_knha <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                           mods = ~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                            ref = input$select_ref_mod) - 1,
                           method = estim(),
                           knha = TRUE)
  
  l$intrcpt <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                   mods =~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                   ref = input$select_ref_mod),
                   method = estim(),
                   knha = FALSE)
  
  l$intrcpt_knha <- rma(yi = data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                        mods =~ relevel(factor(data_reac$DT[[input$select_catmod]]), 
                                        ref = input$select_ref_mod),
                        method = estim(),
                        knha = TRUE)
  
  # **** Shorten names of coefficients in object ----
  l.out <- lapply(l, function(x){
    attr(x$beta, "dimnames")[[1]] <- gsub(".*)","", attr(x$beta, "dimnames")[[1]])
    x
  })
  
  # **** table for model output ----
  dt.model <- data.table("Model" = if(input$metamodel == "fe") {"Fixed-Effect"} else {"Random-Effects"}, 
                   "k" = l.out$intrcpt$k.all, 
                   "tau^2 estimator" = if(input$metamodel == "fe"){paste("NA")} else {meta_res_output()$method})
  
  # **** table with coefficients ----
  l.df.coeff <- lapply(l.out, function(x){
    df <- data.frame(
      coefficient = attr(x$beta, "dimnames")[[1]],
      estimate = round(x$beta, 4),
      se = round(x$se, 4),
      testval = round(x$zval, 4),
      pval = round(x$pval, 4),
      ci.lb = round(x$ci.lb, 4),
      ci.ub = round(x$ci.ub, 4))
    
    df$sign <- ifelse(df$pval < 0.001, "***",
                      ifelse(df$pval < 0.01, "**", 
                             ifelse(df$pval < 0.05, "*", "")))
    
    if(x$test == 'knha'){
      names(df)[4] <- "tval"
    } else if (x$test == 'z'){
      names(df)[4] <- "zval"
    }
    return(df)
    
  })
  
  names(l.df.coeff) <- c("no_intrcpt", "no_intrcpt_knha", "intrcpt", "intrcpt_knha")

                          
  # **** table with heterogeneity stats ----
  if (input$mod_intrcpt == TRUE){ 
    df.het <- data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                       "tau (square root of estimated tau^2 value):",
                       "I^2 (total heterogeneity / total variability):",
                       "H^2 (total variability / sampling variability):",
                       "R^2 (amount of heterogeneity accounted for):"),
               val = c(paste(round(l.out$intrcpt$tau2, 3), " (SE = ", 
                             round(l.out$intrcpt$se.tau2, 3), ")", sep = ""),
                       round(sqrt(l.out$intrcpt$tau2), 3),
                       paste(round(l.out$intrcpt$I2, 2), "%", sep = ""),
                       round(l.out$intrcpt$H2, 2),
                       paste(round(l.out$intrcpt$R2, 2), "%", sep = "")))
  } else if (input$mod_intrcpt == FALSE){
    df.het <- data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                       "tau (square root of estimated tau^2 value):",
                       "I^2 (total heterogeneity / total variability):",
                       "H^2 (total variability / sampling variability):"),
               val = c(paste(round(l.out$no_intrcpt$tau2, 3), " (SE = ", 
                             round(l.out$no_intrcpt$se.tau2, 3), ")", sep = ""),
                       round(sqrt(l.out$no_intrcpt$tau2), 3),
                       paste(round(l.out$no_intrcpt$I2, 2), "%", sep = ""),
                       round(l.out$no_intrcpt$H2, 2)
               ))
    
  }
  
  # **** test for residual heterogeneity ----
  str.residhet <- sprintf("Q(df = %.0f) = %.4f, p %s", l.out$no_intrcpt$k.all - length(l.out$no_intrcpt$b), l.out$no_intrcpt$QE, 
          if(l.out$no_intrcpt$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$no_intrcpt$QEp, 4), 
                                                                                                  scientific = FALSE))})
  
  
  # **** test of moderators ----
  str.mod <- paste(
    if((input$mod_intrcpt == TRUE) & (length(l.out$intrcpt$b) > 2)){
      sprintf("Coefficients 2:%.0f:", length(l.out$intrcpt$b))  
    } else if ((input$mod_intrcpt == TRUE) & (length(l.out$intrcpt$b) == 2)){
      paste("Coefficient 2:") 
    } else if (input$mod_intrcpt == FALSE){
      sprintf("Coefficients 1:%.0f:", length(l.out$no_intrcpt$b))  
    }
    ,
    if((input$mod_intrcpt == TRUE) & (input$knha_mod == FALSE)){
      sprintf("QM (df = %.0f) = %.4f, p-val %s", 
              length(l.out$intrcpt$b) - 1,
              l.out$intrcpt$QM,
              if(l.out$intrcpt$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$intrcpt$QMp, 4), 
                                                                                                   scientific = FALSE))}
      )
    } else if((input$mod_intrcpt == FALSE) & (input$knha_mod == FALSE)){
      sprintf("QM (df = %.0f) = %.4f, p-val %s", 
              length(l.out$no_intrcpt$b),
              l.out$no_intrcpt$QM,
              if(l.out$no_intrcpt$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$no_intrcpt$QMp, 4), 
                                                                                                      scientific = FALSE))}
      )
    } else if((input$mod_intrcpt == TRUE) & (input$knha_mod == TRUE)){
      sprintf("F (df1 = %.0f, df2 = %.0f) = %.4f, p-val %s",
              length(l.out$intrcpt_knha$b) - 1,
              l.out$intrcpt_knha$k.all - length(l.out$intrcpt_knha$b),
              l.out$intrcpt_knha$QM,
              if(l.out$intrcpt_knha$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$intrcpt_knha$QMp, 4), 
                                                                                                   scientific = FALSE))}
              
      )
    } else if((input$mod_intrcpt == FALSE) & (input$knha_mod == TRUE)){
      sprintf("F (df1 = %.0f, df2 = %.0f) = %.4f, p-val %s",
              length(l.out$no_intrcpt_knha$b),
              l.out$no_intrcpt_knha$k.all - length(l.out$no_intrcpt_knha$b),
              l.out$no_intrcpt_knha$QM,
              if(l.out$no_intrcpt_knha$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$no_intrcpt_knha$QMp, 4), 
                                                                                                      scientific = FALSE))}
              
      )
    }
  )
  
  
  out <- list(results = l.out, 
              model = dt.model,
              coeff = l.df.coeff,
              het = df.het,
              resid.het = str.residhet,
              mod.test = str.mod)
  return(out)

})

mod_res_console <- eventReactive(input$go_moa, {
  if((input$mod_intrcpt == FALSE) & (input$knha_mod == FALSE)){
    print(mod_res_output()$results$no_intrcpt)
    
  } else if ((input$mod_intrcpt == FALSE) & (input$knha_mod == TRUE)){
    print(mod_res_output()$results$no_intrcpt_knha)
    
  } else if ((input$mod_intrcpt == TRUE) & (input$knha_mod == FALSE)){
    print(mod_res_output()$results$intrcpt)
    
  } else if ((input$mod_intrcpt == TRUE) & (input$knha_mod == TRUE)){
    print(mod_res_output()$results$intrcpt_knha)
  }
  
})

mod_res_coeff <- eventReactive(input$go_moa, {
  
  if((input$mod_intrcpt == FALSE) & (input$knha_mod == FALSE)){
   mod_res_output()$coeff$no_intrcpt
  } else if ((input$mod_intrcpt == FALSE) & (input$knha_mod == TRUE)){
  mod_res_output()$coeff$no_intrcpt_knha
  } else if ((input$mod_intrcpt == TRUE) & (input$knha_mod == FALSE)){
  mod_res_output()$coeff$intrcpt
  } else if ((input$mod_intrcpt == TRUE) & (input$knha_mod == TRUE)){
  mod_res_output()$coeff$intrcpt_knha
  }
  
}) 

output$mod_res <- renderPrint({
  mod_res_console()
})

# ** Output ----
output$mod_out_1 <- renderTable(mod_res_output()$model, striped = TRUE, bordered = TRUE)

output$mod_out_2 <- renderTable(mod_res_coeff(), digits = 4)
  
output$mod_out_3 <- renderTable(mod_res_output()$het, colnames = FALSE, striped = TRUE, bordered = TRUE)

output$mod_out_4 <- renderText(mod_res_output()$resid.het)

output$mod_out_5 <- renderText(mod_res_output()$mod.test)


# ** Subgroup plot ----
mod_plot_output <- reactive({
  
  validate(
    need(isTruthy(meta_res_output()), "Please run the meta-analysis first")
  )
  validate(
    need(isTruthy(mod_res_output()), "Please run the subgroup analysis first")
  )
  
  df <- data.table(mod_res_output()$results$no_intrcpt$beta, keep.rownames = TRUE)
  df[, `:=` (ci_ub =  mod_res_output()$results$no_intrcpt$ci.ub,
             ci_lb = mod_res_output()$results$no_intrcpt$ci.lb,
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

output$plot_subgroup <- renderPlot({
  req(mod_plot_output())
  print(mod_plot_output())
}
)
  

output$sbgrp_example <- renderPlot({
  req(mod_plot_output())
  print(mod_plot_output())
}
)