
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
  res.reg <- rma(data_reac$DT[[para$es]], sei = data_reac$DT[[para$se]],
                 mods = as.formula(paste("~", mods_formula)),
                 method = estim(),
                 knha = input$knha_reg)
  
  # shorten names in object
  attr(res.reg$beta, "dimnames")[[1]][-1] <- paste0(
    unlist(stringi::stri_extract_all_regex(attr(res.reg$beta, "dimnames")[[1]][-1], '(?<=").*?(?=")')),
  str_replace(gsub(".*]]","", attr(res.reg$beta, "dimnames")[[1]][-1]), "\\)", ":")
  )
  
  
  # **** table for model output ----
  dt.model <- data.table("Model" = if(input$metamodel == "fe") {"Fixed-Effect"} else {"Random-Effects"}, 
                         "k" = res.reg$k.all, 
                         "tau^2 estimator" = if(input$metamodel == "fe"){paste("NA")} else {meta_res_output()$method})
  
  
  # **** table with coefficients ----
  if(input$knha_reg == FALSE){
    df.coeff <- data.frame(
      coefficient = attr(res.reg$beta, "dimnames")[[1]],
      estimate = round(res.reg$beta, 4),
      se = round(res.reg$se, 4),
      zval = round(res.reg$zval, 4),
      pval = round(res.reg$pval, 4),
      ci.lb = round(res.reg$ci.lb, 4),
      ci.ub = round(res.reg$ci.ub, 4)
    )
  } else if(input$knha_reg == TRUE){
    df.coeff <- data.frame(
      coefficient = attr(res.reg$beta, "dimnames")[[1]],
      estimate = round(res.reg$beta, 4),
      se = round(res.reg$se, 4),
      tval = round(res.reg$zval, 4),
      pval = round(res.reg$pval, 4),
      ci.lb = round(res.reg$ci.lb, 4),
      ci.ub = round(res.reg$ci.ub, 4)
    )
  } 
  df.coeff$sign <- ifelse(df.coeff$pval < 0.001, "***",
                          ifelse(df.coeff$pval < 0.01, "**", 
                                 ifelse(df.coeff$pval < 0.05, "*", "")))
  
  
  # table with heterogeneity stats ----
  df.het <- data.frame(txt = c("tau^2 (estimated amount of total heterogeneity):",
                               "tau (square root of estimated tau^2 value):",
                               "I^2 (total heterogeneity / total variability):",
                               "H^2 (total variability / sampling variability):",
                               "R^2 (amount of heterogeneity accounted for):"),
                       val = c(paste(round(res.reg$tau2, 3), " (SE = ", 
                                     round(res.reg$se.tau2, 3), ")", sep = ""),
                               round(sqrt(res.reg$tau2), 3),
                               paste(round(res.reg$I2, 2), "%", sep = ""),
                               round(res.reg$H2, 2),
                               paste(round(res.reg$R2, 2), "%", sep = "")))
  
  # test for residual heterogeneity ----
  str.residhet <- sprintf("Q(df = %.0f) = %.4f, p %s", res.reg$k.all - length(res.reg$b), res.reg$QE, 
                          if(res.reg$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(res.reg$QEp, 4), 
                                                                                                       scientific = FALSE))})
  
  str.mod <- paste(
    if(length(res.reg$b) > 2){
      sprintf("Coefficients 2:%.0f:", length(res.reg$b))  
    } else if (length(res.reg$b) == 2){
      paste("Coefficient 2:") 
    } else if (input$mod_intrcpt == FALSE){
      sprintf("Coefficients 1:%.0f:", length(res.reg$b))  
    }
    ,
    if(input$knha_reg == FALSE){
      sprintf("QM (df = %.0f) = %.4f, p-val %s", 
              length(res.reg$b) - 1,
              res.reg$QM,
              if(res.reg$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(res.reg$QMp, 4), 
                                                                                        scientific = FALSE))}
      )
    }  else if(input$knha_reg == TRUE){
      sprintf("F (df1 = %.0f, df2 = %.0f) = %.4f, p-val %s",
              length(res.reg$b) - 1,
              res.reg$k.all - length(res.reg$b),
              res.reg$QM,
              if(res.reg$QMp < 0.0001){paste("< .0001")} else {paste("= ", format(round(res.reg$QMp, 4), 
                                                                                        scientific = FALSE))}
              
      )
    }
  )
  
  
  
  out <- list(results = res.reg,
              model = dt.model,
              coeff = df.coeff,
              het = df.het,
              resid.het = str.residhet,
              mod.test = str.mod)
  return(out)
  
  
})

# Output 
output$metareg_out_1 <- renderTable(meta_reg_output()$model, striped = TRUE, bordered = TRUE)

output$metareg_out_2 <- renderTable(meta_reg_output()$coeff, digits = 4)

output$metareg_out_3 <- renderTable(meta_reg_output()$het, colnames = FALSE, striped = TRUE, bordered = TRUE)

output$metareg_out_4 <- renderText(meta_reg_output()$resid.het)

output$metareg_out_5 <- renderText(meta_reg_output()$mod.test)

# output$meta_reg <- renderPrint({
   #req(meta_reg_output())
  # print(meta_reg_output()$results)
# })

# renderUI for d