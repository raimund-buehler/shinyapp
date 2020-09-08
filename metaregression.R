
# **** Select moderators ----
output$select_reg_mod <- renderUI({
  req(data_reac$DT)
  selectInput(inputId = "select_reg_mod",
              label = "Select all moderator variables",
              choices = colnames(data_reac$DT),
              multiple = TRUE)
})

output$select_reg_catmod <- renderUI({
  req(input$select_reg_mod)
  selectInput(inputId = "select_reg_catmod",
              label = "Select all categorical moderator variables",
              choices = input$select_reg_mod,
              multiple = TRUE)
})

# **** Do the meta-regression ----
meta_reg_output <- eventReactive(input$go_metareg, {
  req(input$select_reg_mod)

  # paste all moderators as formula 
  mods_formula <- paste0(
    ifelse(input$select_reg_mod %in% input$select_reg_catmod,
           paste0("as_factor(data_reac$DT[[", "'", input$select_reg_mod, "'", "]])"),
           paste0("data_reac$DT[[", "'", input$select_reg_mod, "'", "]]")), collapse = " + ")
  
  # do the analysis
  # metaregression is run two times, with and without knapp-hartung-adjustment
  l <- list()
  l$res <- rma(data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                 mods = as.formula(paste("~", mods_formula)),
                 method = estim(),
                 knha = FALSE)
  
  l$res_knha <- rma(data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                    mods = as.formula(paste("~", mods_formula)),
                    method = estim(),
                    knha = TRUE)
  
  # shorten names in object
  l.out <- lapply(l, function(x){
  attr(x$beta, "dimnames")[[1]][-1] <- paste0(
    unlist(stringi::stri_extract_all_regex(attr(x$beta, "dimnames")[[1]][-1], '(?<=").*?(?=")')),
  str_replace(gsub(".*]]","", attr(x$beta, "dimnames")[[1]][-1]), "\\)", ":")
  )
  x
  })
  
  names(l.out) <- c("res", "res_knha")
  
  # **** table for model output ----
  dt.model <- data.table("Model" = if(input$metamodel == "fe") {"Fixed-Effect"} else {"Random-Effects"}, 
                         "k" = l.out$res$k.all, 
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

  
  
  # table with heterogeneity stats ----
  df.het <- data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                               "tau (square root of estimated tau^2 value):",
                               "I^2 (total heterogeneity / total variability):",
                               "H^2 (total variability / sampling variability):",
                               "R^2 (amount of heterogeneity accounted for):"),
                       val = c(paste(round(l.out$res$tau2, 3), " (SE = ", 
                                     round(l.out$res$se.tau2, 3), ")", sep = ""),
                               round(sqrt(l.out$res$tau2), 3),
                               paste(round(l.out$res$I2, 2), "%", sep = ""),
                               round(l.out$res$H2, 2),
                               paste(round(l.out$res$R2, 2), "%", sep = "")))
  
  # test for residual heterogeneity ----
  str.residhet <- sprintf("Q(df = %.0f) = %.4f, p %s", l.out$res$k.all - length(l.out$res$b), l.out$res$QE, 
                          if(l.out$res$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$res$QEp, 4), 
                                                                                                       scientific = FALSE))})
  
  str.mod <- paste(
    if(length(l.out$res$b) > 2){
      sprintf("Coefficients 2:%.0f:", length(l.out$res$b))  
    } else if (length(l.out$res$b) == 2){
      paste("Coefficient 2:") 
    } else if (input$mod_intrcpt == FALSE){
      sprintf("Coefficients 1:%.0f:", length(l.out$res$b))  
    }
    ,
    if(input$knha_reg == FALSE){
      sprintf("QM (df = %.0f) = %.4f, p-val %s", 
              length(l.out$res$b) - 1,
              l.out$res$QM,
              if(l.out$res$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$res$QMp, 4), 
                                                                                        scientific = FALSE))}
      )
    }  else if(input$knha_reg == TRUE){
      sprintf("F (df1 = %.0f, df2 = %.0f) = %.4f, p-val %s",
              length(l.out$res$b) - 1,
              l.out$res$k.all - length(l.out$res$b),
              l.out$res$QM,
              if(l.out$res$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(l.out$res$QMp, 4), 
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


metareg_res_coeff <- eventReactive(input$go_metareg, {
  
  if(input$knha_reg == FALSE){
    meta_reg_output()$coeff$res
  } else if (input$knha_reg == TRUE){
    meta_reg_output()$coeff$res_knha
  }
  
}) 

# Output 
output$metareg_out_1 <- renderTable(meta_reg_output()$model, striped = TRUE, bordered = TRUE)

output$metareg_out_2 <- renderTable(metareg_res_coeff(), digits = 4)

output$metareg_out_3 <- renderTable(meta_reg_output()$het, colnames = FALSE, striped = TRUE, bordered = TRUE)

output$metareg_out_4 <- renderText(meta_reg_output()$resid.het)

output$metareg_out_5 <- renderText(meta_reg_output()$mod.test)

# output$meta_reg <- renderPrint({
   #req(meta_reg_output())
  # print(meta_reg_output()$results)
# })

