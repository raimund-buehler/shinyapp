

params <- list(
  
  # Download Elements ----
  dwn_dat = input$dwn_report_dat,
  dwn_meta = input$dwn_report_meta,
  dwn_sbgrp = if(input$go_moa > 0){input$dwn_report_sbgrp} else {FALSE},
  dwn_metareg = input$dwn_report_metareg,
  dwn_pb_bm = if(input$go_BM > 0){input$dwn_report_pb_bm} else {FALSE},
  dwn_pb_se = if(input$go_SE > 0){input$dwn_report_pb_se} else {FALSE},
  dwn_pb_tf = if(input$go_TRFI > 0){input$dwn_report_pb_tf} else {FALSE},
  dwn_pb_pcurve = if(input$go_pcurve > 0){input$dwn_report_pb_pcurve} else {FALSE},
  dwn_pb_puni = if(input$go_puni > 0){input$dwn_report_pb_puni} else {FALSE},
  dwn_pb_selmods = if(input$go_selmod > 0){input$dwn_report_pb_selmods} else {FALSE},
  dwn_pb_tes = if(input$go_tes > 0){input$dwn_report_pb_tes} else {FALSE},
  dwn_pb = input$dwn_report_pb,

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






