# set parameter for metaregression
if(input$go_metareg > 0){ 
  
  params_metareg <- list(
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

} else {
  
  params_metareg <- list(
    metareg_k = NA,
    metareg_df_coeff = NA,
    metareg_mods = NA,
    metareg_QE = NA,
    metareg_QEp = NA,
    metareg_QM = NA,
    metareg_QMp = NA,
    metareg_QM_knha = NA,
    metareg_QMp_knha = NA,
    metareg_tau2 = NA,
    metareg_se.tau2 = NA,
    metareg_I2 = NA,
    metareg_H2 = NA,
    metareg_R2 = NA
  )
  
  
}



params_metareg