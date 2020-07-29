##Begg and Mazumdar

output$BM <- renderPrint({
  ranktest(meta_res_output())
})

##Sterne and Egger
output$SterneEgger <- renderPrint({
  regtest(meta_res_output())
}) 

##Trim and Fill
output$TRFI <- renderPrint({
  if (sign(meta_res_output()$b) == 1) {
    trimfill(meta_res_output(), side = "left")
  } else if (sign(meta_res_output()$b) == -1) {
    trimfill(meta_res_output(), side = "right")
  }
})

##pcurve
##Where is the file altered? 
source(here("pcurve_demo.R"), local = TRUE)

##puniform
##needs r, --> calc-effectsize
output$p_uni <- renderPrint({
  n <- para$n
  if (sign(meta_res_output()$b) == 1){
    puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right",method="P")
  } else if (sign(meta_res_output()$b) == -1) {
    puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
  }
})

output$p_uni_star <- renderPrint({
  n <- para$n
  if (sign(meta_res_output()$b) == 1){
    puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right")
  } else if (sign(meta_res_output()$b) == -1) {
    puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left")
  }
})