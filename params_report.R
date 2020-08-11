params <- list(
  
  # Data ----
  dat = input$file$name,
  metric = switch(para$es, 
                  "z" = "Fisher z",
                  "r" = "Pearson r",
                  "d" = "Cohen d",
                  "g" = "Hedges g",
                  "OR" = "Odds Ratio",
                  "logOR" = "Odds Ratio (Log-Scale)"),
  k = length(data_reac$DT[[para$es]]),
  k_pub = NA,
  n = data_reac$DT[[para$es]],
  n_pub = NA,
  in_study = para$prim,
  in_study_pub = "Author et al (2001)",
  
  # Meta-Analysis ----
  model_type = if(input$metamodel == "fe") {paste("Fixed-Effect")} else {paste("Random-Effects")},
  k_meta = meta_res_output()$k.all,
  estim = if(input$metamodel == "fe"){paste("")} else {sprintf("; tau^2 estimator: %s", meta_res_output()$method)},
  meta_es = meta_res_output()$b,
  meta_se = meta_res_output()$se,
  meta_ci.lb = meta_res_output()$ci.lb,
  meta_ci.ub = meta_res_output()$ci.ub,
  meta_pval = if(meta_res_output()$pval < .0001){paste("< .0001")} else {paste("= ", round(meta_res_output()$pval, 4))},
  meta_zval = meta_res_output()$zval,
  meta_tau2 = meta_res_output()$tau2
  
)
