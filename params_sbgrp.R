if(input$go_moa > 0){ 
  
  params_sbgrp <- list(
    # Subgroup Analysis ----
    sbgrp_k = mod_res_output()$results$intrcpt$k.all,
    sbgrp_tau2 = mod_res_output()$results$intrcpt$tau2,
    sbgrp_se.tau2 = mod_res_output()$results$intrcpt$se.tau2,
    sbgrp_I2 = mod_res_output()$results$intrcpt$I2,
    sbgrp_H2 = mod_res_output()$results$intrcpt$H2,
    sbgrp_R2 = mod_res_output()$results$intrcpt$R2,
    sbgrp_mods = length(mod_res_output()$results$no_intrcpt$b),
    sbgrp_QE = mod_res_output()$results$intrcpt$QE,
    sbgrp_QEp = mod_res_output()$results$intrcpt$QEp,
    sbgrp_QM = mod_res_output()$results$intrcpt$QM,
    sbgrp_QMp = mod_res_output()$results$intrcpt_knha$QMp,
    sbgrp_QM_knha = mod_res_output()$results$intrcpt_knha$QM,
    sbgrp_QMp_knha = mod_res_output()$results$intrcpt_knha$QMp,
    sbgrp_df_coeff = mod_res_output()$coeff
  )

} else {
  
  params_sbgrp <- list(
    # Subgroup Analysis ----
    sbgrp_k = NA,
    sbgrp_tau2 = NA,
    sbgrp_se.tau2 = NA,
    sbgrp_I2 = NA,
    sbgrp_H2 = NA,
    sbgrp_R2 = NA,
    sbgrp_mods = NA,
    sbgrp_QE = NA,
    sbgrp_QEp = NA,
    sbgrp_QM = NA,
    sbgrp_QMp = NA,
    sbgrp_QM_knha = NA,
    sbgrp_QMp_knha = NA,
    sbgrp_df_coeff = NA
  )
  
  
}



params_sbgrp