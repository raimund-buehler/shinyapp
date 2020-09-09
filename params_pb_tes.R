# set parameter for test of excess significance
if(input$go_tes > 0){ 
  
  params_pb_tes <- list(
    pb_tes_o = TESres()$O,
    pb_tes_e = TESres()$E,
    pb_tes_res = TESres()$A,
    pb_tes_pval = TESres()$res,
    thres_tes_pval = input$TES_p
  )

} else {
  
  params_pb_tes <- list(
    pb_tes_o = NA,
    pb_tes_e = NA,
    pb_tes_res = NA,
    pb_tes_pval = NA,
    thres_tes_pval = NA
  )
  
  
}



params_pb_tes