# set parameter for pcurve
if(input$go_pcurve > 0){ 
  
  params_pb_pcurve <- list(
    pb_pcurve_restab = pcurve_table(),
    pb_pcurve_powertab = pcurvePower(),
    pb_pcurve_pval.full = pcurve()$p.Zppr,
    pb_pcurve_pval.half =  pcurve()$p.Zppr.half
  )

} else {
  
  params_pb_pcurve <- list(
    pb_pcurve_restab = NA,
    pb_pcurve_powertab = NA,
    pb_pcurve_pval.full = NA,
    pb_pcurve_pval.half =  NA
  )
  
  
}



params_pb_pcurve