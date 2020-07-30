##Begg and Mazumdar----

output$BM <- renderPrint({
  tryCatch(ranktest(meta_res_output()),
      error = function(e){
      "Please execute the Meta-Analysis first!"
              })
})

##Sterne and Egger----
output$SterneEgger <- renderPrint({
  tryCatch(regtest(meta_res_output()),
           error = function(e){
             "Please execute the Meta-Analysis first!"
           })
}) 

##Trim and Fill----
output$TRFI <- renderPrint({
  tryCatch(
  if (sign(meta_res_output()$b) == 1) {
    trimfill(meta_res_output(), side = "left")
  } else if (sign(meta_res_output()$b) == -1) {
    trimfill(meta_res_output(), side = "right")
  },            error = function(e){
    "Please execute the Meta-Analysis first!"
  })
})

##pcurve----
##Where is the file altered? 
source(here("pcurve_demo.R"), local = TRUE)

##puniform----
##needs r, --> calc-effectsize
output$p_uni <- renderPrint({
  n <- para$n
  tryCatch(
  if (sign(meta_res_output()$b) == 1){
    puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right",method="P")
  } else if (sign(meta_res_output()$b) == -1) {
    puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
  },           error = function(e){
    "Please execute the Meta-Analysis first!"
  })
})

output$p_uni_star <- renderPrint({
  n <- para$n
  tryCatch(
  if (sign(meta_res_output()$b) == 1){
    puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right")
  } else if (sign(meta_res_output()$b) == -1) {
    puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left")
  },            error = function(e){
    "Please execute the Meta-Analysis first!"
  })
})

##Vevea and woods selection models----
SelMods <- reactiveValues()
observe({
  req(input$file)
  req(para$prim)
  req(para$es)
  req(para$id)
  # specify p-value cutpoints
  vevwoo_steps <- c(.005, .010, .050, .100, .250, .350, .500, .650, .750, .900,
                    .950, .990, .995, 1.00)

  # specify weights for the selection models according to Vevea & Woods (2005)
  vevwoo_weights_list <- list(moderate_one = c(1.0, .99, .95, .90, .80, .75, .65, .60,
                                               .55, .50, .50, .50, .50, .50),
                              severe_one = c(1.0, .99, .90, .75, .60, .50, .40,
                                             .35, .30, .25, .10, .10, .10, .10),
                              moderate_two = c(1.0, .99, .95, .90, .80, .75, .60,
                                               .60, .75, .80, .90, .95, .99, 1.0),
                              severe_two = c(1.0, .99, .90, .75, .60, .50, .25, .25,
                                             .50, .60, .75, .90, .99, 1.0))

  data <- as.data.table(data_reac$DTall)
  es <- para$es
  prim <- which(data[[para$id]] == para$prim)

  #sign_primary
  sign_primary <- sign(data[[es]][prim])

  # Run all four models
  # Use coined effect sizes in case of negative initial effect
  if (sign_primary == -1){
    vevwoo.res <- lapply(vevwoo_weights_list, function(x){
      data[, weightfunct(effect = z,
                         v = z.SE^2,
                         steps = vevwoo_steps,
                         weights = rev(x))]
    })
  } else {
    vevwoo.res <- lapply(vevwoo_weights_list, function(x){
      data[, weightfunct(effect = z,
                         v = z.SE^2,
                         steps = vevwoo_steps,
                         weights = x)]
    })
  }
  SelMods$mod1 <- vevwoo.res$moderate_one
  SelMods$sev1 <- vevwoo.res$severe_one
  SelMods$mod2 <- vevwoo.res$moderate_two
  SelMods$sev2 <- vevwoo.res$severe_two
})

output$modone <- renderPrint({validate(need(isTruthy(SelMods$mod1), "Please select the earliest study in your dataset!"))
  SelMods$mod1})
output$sevone <- renderPrint({validate(need(isTruthy(SelMods$sev1), "Please select the earliest study in your dataset!"))
  SelMods$sev1})
output$modtwo <- renderPrint({validate(need(isTruthy(SelMods$mod2), "Please select the earliest study in your dataset!"))
  SelMods$mod2})
output$sevtwo <- renderPrint({validate(need(isTruthy(SelMods$sev2), "Please select the earliest study in your dataset!"))
  SelMods$sev2})

# EXCESS OF SIGNIFICANCE TEST - Ioannidis & Trikalinos (2007) ----

# **** number of significant studies (stored in ksign of for.power object), contingent on sign of summary effect (O)
#TES <- reactiveValues()

output$TestOfExc <- renderPrint({
  tryCatch({
  data <- as.data.table(data_reac$DTall)
  
  res.rma <- meta_res_output()
  
  if (sign(res.rma$b) == 1) {
  O <- data[, puniform(ri = r,ni = n, side="right", method="P")]$ksig
  } else if (sign(res.rma$b)==-1) {
  O <- data[, puniform(ri = r,ni = n, side="left", method="P")]$ksig
  }
  
  ## number of studies (overall)
  kall<-length(data$r)
  
  ## estimation of summary effect (z metric, fixed effect) and conversion to r
  MA.ES.z <- data[, rma(z, z.SE^2, method = "FE")]
  MA.ES.r <- as.numeric((exp(2 * MA.ES.z$b) - 1) / (exp(2 * MA.ES.z$b) + 1))
  
  # Calculating average power based on effect size estimate of overall effect
  P.ind.all.e <- mapply(pwr.r.test, n = data$n, r = MA.ES.r)
  PowP.ind.all.e <- sapply(P.ind.all.e[4, 1:kall], as.numeric)
  MeanPowerInd.all.e <- mean(PowP.ind.all.e) #average power for detecting summary effect
  
  # estimated number of significant studies = power * k = E
  E <- MeanPowerInd.all.e * kall
  
  
  ### chisquare for difference between observed and expected significant studies (A)
  
  A<-((O - E)^2 / E) + ((O - E)^2 / (kall - E)) ## for.power$ksig are observed sign. studies
  A
  res.it<-pchisq(A, df = 1, lower.tail = F)
  
  # caution: there may be fewer observed than expected significant studies, 
  # then the sign of for.res.it would be negative
  for.res.it <- O - E
  
  paste("chisquare-test for difference between observed and expected significant studies:\n p = ", res.it)}
, error = function(e){
  "Please execute the Meta-Analysis first!"
})
})