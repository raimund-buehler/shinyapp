# set parameter for trim-and-fill
if(input$go_TRFI > 0){ 
  
  params_pb_tf <- list(
    pb_tf_k = TFres$res$k0,
    pb_tf_side = TFres$res$side,
    pb_tf_es = TFres$res$beta,
    thres_tf_adj = input$tf_adj
  )

} else {
  
  params_pb_tf <- list(
    pb_tf_k = NA,
    pb_tf_side = NA,
    pb_tf_es = NA,
    thres_tf_adj = NA
  )
  
  
}



params_pb_tf