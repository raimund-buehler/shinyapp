# set parameter for sterne & egger regression

if(input$go_SE > 0){ 
  
  params_pb_se <- list(
    pb_se_zval = SEres$res$zval,
    pb_se_pval = SEres$res$pval,
    thres_se_pval = input$SE_p
  )

} else {
  
  params_pb_se <- list(
    pb_se_zval = NA,
    pb_se_pval = NA,
    thres_se_pval = NA
  )
  
  
}



params_pb_se