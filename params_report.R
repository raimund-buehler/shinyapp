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
  n = data_reac$DT[[para$n]],
  n_pub = NA,
  in_study = para$prim,
  in_study_pub = "Author et al (2001)",
  
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
                                                                                             scientific = FALSE))},
  
  # Subgroup Analysis ----
  sbgrp_k = mod_res_output()$intrcpt$k.all,
  sbgrp_tau2 = mod_res_output()$intrcpt$tau2,
  sbgrp_se.tau2 = mod_res_output()$intrcpt$se.tau2,
  sbgrp_I2 = mod_res_output()$intrcpt$I2,
  sbgrp_H2 = mod_res_output()$intrcpt$H2,
  sbgrp_R2 = mod_res_output()$intrcpt$R2,
  # sbgrp_QEpval = if(mod_res_output()$intrcpt$QEp < 0.0001){paste("< .0001")} else {paste("= ", format(round(mod_res_output()$intrcpt$QEp, 4), 
                                                                                           # scientific = FALSE))},
  # sbgrp_QE = mod_res_output()$intrcpt$QE,
  # sbgrp_df.resid = mod_res_output()$intrcpt$k.all - mod_res_output()$intrcpt$p,
  # sbgrp_QM = mod_res_output()$QE, 
  # sbgrp_df.mod = mod_res_output()$intrcpt$p - 1,
  
  # Publication Bias Analyses ----
  # ** Begg & Mazumdar ----
  pb_bm_tau = BMres$res$tau,
  pb_bm_pval = BMres$res$pval,
  thres_bm_pval = input$BM_p,
  
  # ** Sterne & Egger ----
  pb_se_zval = SEres$res$zval,
  pb_se_pval = SEres$res$pval,
  thres_se_pval = input$SE_p,
  
  # ** Trim-and-Fill ----
  pb_tf_k = TFres$res$k0,
  pb_tf_side = TFres$res$side,
  pb_tf_es = TFres$res$beta,
  thres_tf_adj = input$tf_adj,
  
  # ** Pcurve ----
  pb_pcurve_restab = pcurve_table(),
  pb_pcurve_powertab = pcurvePower(),
  pb_pcurve_pval.full = pcurve()$p.Zppr,
  pb_pcurve_pval.half =  pcurve()$p.Zppr.half,
  
  # ** Puniform and Puniform* ----
  pb_puni_zval = PUNIres$res$L.pb,
  pb_puni_pval = PUNIres$res$pval.pb,
  thres_puni_pval = input$puni_p,
  pb_punistar_zval = PUNISTres$res$L.pb,
  pb_punistar_pval = PUNISTres$res$pval.pb,
  thres_punistar_pval = input$punistar_p,
  
  # TES ----
  pb_tes_o = TESres$O,
  pb_tes_e = TESres$E,
  pb_tes_res = TESres$A,
  pb_tes_pval = TESres$res,
  thres_tes_pval = input$TES_p,
  
  # Selection Models ----
  pb_sel_mod1 = SelMods$mod1$output_adj$par[2],
  pb_sel_sev1 = SelMods$sev1$output_adj$par[2],
  pb_sel_mod2 = SelMods$mod2$output_adj$par[2],
  pb_sel_sev2 = SelMods$sev2$output_adj$par[2],
  pb_sel_unadj = SelMods$mod1$output_unadj$par[2],
  pb_sel_perc_mod1 = 1 / SelMods$mod1$output_unadj$par[2] * (SelMods$mod1$output_unadj$par[2] - SelMods$mod1$output_adj$par[2]),
  pb_sel_perc_sev1 = 1 / SelMods$sev1$output_unadj$par[2] * (SelMods$sev1$output_unadj$par[2] - SelMods$sev1$output_adj$par[2]),
  pb_sel_perc_mod2 = 1 / SelMods$mod2$output_unadj$par[2] * (SelMods$mod2$output_unadj$par[2] - SelMods$mod2$output_adj$par[2]),
  pb_sel_perc_sev2 = 1 / SelMods$sev2$output_unadj$par[2] * (SelMods$sev2$output_unadj$par[2] - SelMods$sev2$output_adj$par[2]),
  thres_sel_adj = input$sel_adj
  
)
