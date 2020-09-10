# set parameter for selection models

if(input$go_selmod > 0){ 
  
  params_pb_selmods <- list(
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

} else {
  
  params_pb_selmods <- list(
    pb_sel_mod1 = NA,
    pb_sel_sev1 = NA,
    pb_sel_mod2 = NA,
    pb_sel_sev2 = NA,
    pb_sel_unadj = NA,
    pb_sel_perc_mod1 = NA,
    pb_sel_perc_sev1 = NA,
    pb_sel_perc_mod2 = NA,
    pb_sel_perc_sev2 = NA,
    thres_sel_adj = NA

  )
  
  
}



params_pb_selmods