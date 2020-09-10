# set parameter for p-uniform and p-uniform*

if(input$go_puni > 0){ 
  
  params_pb_puni <- list(
    pb_puni_zval = PUNIres$res$L.pb,
    pb_puni_pval = PUNIres$res$pval.pb,
    thres_puni_pval = input$puni_p,
    pb_punistar_zval = PUNISTres$res$L.pb,
    pb_punistar_pval = PUNISTres$res$pval.pb,
    thres_punistar_pval = input$punistar_p

  )

} else {
  
  params_pb_puni <- list(
    pb_puni_zval = NA,
    pb_puni_pval = NA,
    thres_puni_pval = NA,
    pb_punistar_zval = NA,
    pb_punistar_pval = NA,
    thres_punistar_pval = NA
  )
  
  
}



params_pb_puni