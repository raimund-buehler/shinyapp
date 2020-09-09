params_meta <- list( 
              # Meta-Analysis ----
              model_type = NA,
              k_meta = NA,
              estim = NA,
              meta_es = NA,
              meta_se = NA,
              meta_ci.lb = NA,
              meta_ci.ub = NA,
              meta_pval = NA,
              meta_zval = NA,
              meta_tau2 = NA,
              meta_I2 = NA,
              meta_H2 = NA,
              qtest_df = NA,
              qtest_stat = NA,
              qtest_pval = NA
              )

if(isTruthy(meta_res_output())){
params_meta <- list( 
  # Meta-Analysis ----
  model_type = if(input$metamodel == "fe") {paste("Fixed-Effect")} else {paste("Random-Effects")},
  k_meta = meta_res_output()$k.all,
  estim = if(input$metamodel == "fe"){paste("not needed (fixed-effect model)")} else {paste(switch(meta_res_output()$method,
                                                                                                   "DL" = "DerSimonian-Laird (DL)", 
                                                                                                   "HE" = "Hedges (HE)",
                                                                                                   "HS" = "Hunter-Schmidt (HS)",
                                                                                                   "SJ" =  "Sidik-Jonkman (SJ)",
                                                                                                   "ML" = "Maximum-Likelihood (ML)",
                                                                                                   "REML" = "Restricted Maximum-Likelihood (REML)",
                                                                                                   "EB" = "Empirical Bayes Estimator (EB)", 
                                                                                                   "PM" = "Paule-Mandel Estimator (PM)"))},
  meta_es = meta_res_output()$b,
  meta_se = meta_res_output()$se,
  meta_ci.lb = meta_res_output()$ci.lb,
  meta_ci.ub = meta_res_output()$ci.ub,
  meta_pval = if(meta_res_output()$pval < .0001){paste("< .0001")} else {paste("= ", round(meta_res_output()$pval, 4))},
  meta_zval = meta_res_output()$zval,
  meta_tau2 = meta_res_output()$tau2,
  meta_I2 = meta_res_output()$I2,
  meta_H2 = meta_res_output()$H2,
  qtest_df = meta_res_output()$k.all - 1,
  qtest_stat = meta_res_output()$QE,
  qtest_pval = if(meta_res_output()$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(meta_res_output()$QEp, 4), 
                                                                                             scientific = FALSE))}
  
)
}