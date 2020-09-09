

params <- list(
  
  # Elements ----
  dwn_dat = input$dwn_report_dat,
  dwn_meta = input$dwn_report_meta,
  dwn_sbgrp = if(input$go_moa > 0){input$dwn_report_sbgrp} else {FALSE},
  dwn_metareg = input$dwn_report_metareg,
  dwn_pb = input$dwn_report_pb
  
)


params <- c(params, list( 
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
  n = data_reac$DT[[para$n]],
  in_study = para$prim
)
)


params <- c(params, 
list( 
 # Meta-Regression ----
  metareg_k = meta_reg_output()$results$res$k.all,
  metareg_df_coeff = meta_reg_output()$coeff,
  metareg_mods = length(meta_reg_output()$results$res$b),
  metareg_QE = meta_reg_output()$results$res$QE,
  metareg_QEp = meta_reg_output()$results$res$QEp,
  metareg_QM = meta_reg_output()$results$res$QM,
  metareg_QMp = meta_reg_output()$results$res$QMp,
  metareg_QM_knha = meta_reg_output()$results$res_knha$QM,
  metareg_QMp_knha = meta_reg_output()$results$res_knha$QMp,
  metareg_tau2 = meta_reg_output()$results$res$tau2,
  metareg_se.tau2 = meta_reg_output()$results$res$se.tau2,
  metareg_I2 = meta_reg_output()$results$res$I2,
  metareg_H2 = meta_reg_output()$results$res$H2,
  metareg_R2 = meta_reg_output()$results$res$R2
)
)



 params <- c(params, list( 
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
  pb_tes_o = TESres()$O,
  pb_tes_e = TESres()$E,
  pb_tes_res = TESres()$A,
  pb_tes_pval = TESres()$res,
  thres_tes_pval = input$TES_p,
  
  # Selection Models ----
  pb_sel_mod1 = SelMods()$mod1$output_adj$par[2],
  pb_sel_sev1 = SelMods()$sev1$output_adj$par[2],
  pb_sel_mod2 = SelMods()$mod2$output_adj$par[2],
  pb_sel_sev2 = SelMods()$sev2$output_adj$par[2],
  pb_sel_unadj = SelMods()$mod1$output_unadj$par[2],
  pb_sel_perc_mod1 = 1 / SelMods()$mod1$output_unadj$par[2] * (SelMods()$mod1$output_unadj$par[2] - SelMods()$mod1$output_adj$par[2]),
  pb_sel_perc_sev1 = 1 / SelMods()$sev1$output_unadj$par[2] * (SelMods()$sev1$output_unadj$par[2] - SelMods()$sev1$output_adj$par[2]),
  pb_sel_perc_mod2 = 1 / SelMods()$mod2$output_unadj$par[2] * (SelMods()$mod2$output_unadj$par[2] - SelMods()$mod2$output_adj$par[2]),
  pb_sel_perc_sev2 = 1 / SelMods()$sev2$output_unadj$par[2] * (SelMods()$sev2$output_unadj$par[2] - SelMods()$sev2$output_adj$par[2]),
  thres_sel_adj = input$sel_adj
 )
)




