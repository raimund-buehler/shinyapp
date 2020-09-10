# set parameter for begg & mazumdars rank test

if(input$go_BM > 0){ 
  
  params_pb_bm <- list(
    pb_bm_tau = BMres$res$tau,
    pb_bm_pval = BMres$res$pval,
    thres_bm_pval = input$BM_p
  )

} else {
  
  params_pb_bm <- list(
    # Subgroup Analysis ----
    pb_bm_tau = BMres$res$tau,
    pb_bm_pval = BMres$res$pval,
    thres_bm_pval = input$BM_p
  )
  
  
}



params_pb_bm