##Begg and Mazumdar----

BMres <- reactiveValues()

output$BMtau <- renderValueBox({
  valueBox(
    tryCatch({
    BMres$res <- ranktest(meta_res_output())
    round(BMres$res$tau, 3)},
      error = function(e){
      tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
    }), subtitle = "Kendall's tau", color = "light-blue")
})

output$BMp <- renderValueBox({
  valueBox(
    tryCatch({
      BMres$res <- ranktest(meta_res_output())
      format.pval(BMres$res$pval, eps = 0.0001, digits = 3)},
      error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
      }), subtitle = "p = ", color = "light-blue")
})

output$BMhelp <- renderInfoBox({
  box("This test can be used to examine whether the observed outcomes and 
  the corresponding sampling variances are correlated. A high correlation 
  would indicate that the funnel plot is asymmetric, which may be a result 
  of publication bias.", title = "Begg and Mazumdar's Rank Correlation Test")
  }) 

output$BMref <- renderText("Begg, C. B., & Mazumdar, M. (1994). Operating characteristics of a rank correlation test for publication bias. Biometrics, 50, 1088–1101.")

##Sterne and Egger----
SEres <- reactiveValues()

output$SEz <- renderValueBox({
  valueBox(
  tryCatch({SEres$res <- regtest(meta_res_output())
            round(SEres$res$zval, 3)},
           error = function(e){
             tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
           }), subtitle = "Test Statistic: z", color = "light-blue"
  )
}) 

output$SEp <- renderValueBox({
  valueBox(
    tryCatch({SEres$res <- regtest(meta_res_output())
    format.pval(SEres$res$pval, eps = 0.0001, digits = 3)},
    error = function(e){
      tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
    }), subtitle = "p = ", color = "light-blue"  
  )
}) 

output$SEmodel <- renderTable({
  req(SEres$res)
  coef(summary(SEres$res$fit))
}, rownames = TRUE)

output$SEhelp <- renderInfoBox({
  fluidRow(
    box("This test can be used to examine whether a relationship exists between the observed outcomes 
    and the chosen predictor (default = standard error). If such a relationship is present, then this 
    usually implies asymmetry in the funnel plot, which in turn may be an indication of publication bias.", 
    title = "Sterne & Egger's Regression", width = NULL)
  )
}) 

output$SEref <- renderText("Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in meta-analysis detected by a simple, graphical test. British Medical Journal, 315, 629–634.")

##Trim and Fill----
TFres <- reactiveValues()

output$TRFIk0 <- renderValueBox({
  valueBox(
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        TFres$res$k0
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        TFres$res$k0
      },            error = function(e){
      tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
    }), subtitle = "Number of missing studies", color = "light-blue"
  )
}) 

output$TRFIest <- renderValueBox({
  valueBox(
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        round(TFres$res$beta, 3)
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        round(TFres$res$beta, 3)
      },            error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
      }), subtitle = "Adjusted Estimate", color = "light-blue"
  )
}) 

output$TRFIside <- renderValueBox({
  valueBox(
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        TFres$res$side
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        TFres$res$side
      },            error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 50%")
      }), subtitle = "Side of funnel plot on which studies are imputed", color = "light-blue"
  )
}) 


output$TRFImodel <- renderTable({
  req(TFres$res)
  coef(summary(TFres$res, digits = 3))
}, rownames = TRUE)


output$TRFIhelp <- renderInfoBox({
  fluidRow(
    box("This method is a nonparametric (rank-based) data augmentation technique. 
        It can be used to estimate the number of studies missing from a meta-analysis due to the suppression 
        of the most extreme results on one side of the funnel plot. The method then augments the observed data so that the 
        funnel plot is more symmetric and recomputes the summary estimate based on the complete data. This should not 
        be regarded as a way of yielding a more 'valid' estimate of the overall effect or outcome, but as a way of examining the sensitivity of the 
        results to one particular selection mechanism (i.e., one particular form of publication bias).", 
        title = "Trim and Fill", width = NULL)
  )
}) 

output$TRFIref <- renderText(HTML(paste("Duval, S. J., & Tweedie, R. L. (2000). Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. Biometrics, 56, 455–463.")))

##pcurve----
source(here("pcurve_demo.R"), local = TRUE)

##puniform----
##needs r, --> calc-effectsize
PUNIres <- reactiveValues()
output$p_uni <- renderTable({
  n <- para$n
  tryCatch(
  if (sign(meta_res_output()$b) == 1){
    PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right", method="P")
    PUNIres$res
  } else if (sign(meta_res_output()$b) == -1) {
    PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
    PUNIres$res 
  },           error = function(e){
    "Please execute the Meta-Analysis first!"
  })
})

PUNISTres <- reactiveValues()
output$p_uni_star <- renderPrint({
  n <- para$n
  tryCatch(
  if (sign(meta_res_output()$b) == 1){
    PUNISTres$res <- puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right")
    PUNISTres$res
  } else if (sign(meta_res_output()$b) == -1) {
    PUNISTres$res <- puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left")
    PUNISTres$res
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

TESres <- reactiveValues()
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
  
  TESres$res <- res.it
  # caution: there may be fewer observed than expected significant studies, 
  # then the sign of for.res.it would be negative
  for.res.it <- O - E
  
  cat("Chisquare-test for difference between observed and expected significant studies:", "\n\n", "p = ", res.it)}
, error = function(e){
  "Please execute the Meta-Analysis first!"
})
})

output$pubboxes1 <- renderUI({
  req(BMres$res$pval)
  fluidRow(
    valueBox("Rank Correlation", paste("p = ", format.pval(BMres$res$pval, eps = 0.001, digits = 3)), width = 3, color = if(BMres$res$pval < 0.05){"red"}else{"green"}),
    valueBox("Sterne & Egger", paste("p = ", format.pval(SEres$res$pval, eps = 0.001, digits = 3)), width = 3, color = if(SEres$res$pval < 0.05){"red"}else{"green"}),
    valueBox("Trim and Fill", paste("p = ", format.pval(TFres$res$pval, eps = 0.001, digits = 3)), width = 3, color = if(TFres$res$pval < 0.05){"red"}else{"green"}),
    valueBox("p-curve", "p = ?", width = 3),
  )
})
output$pubboxes2 <-  renderUI({
  fluidRow(
    valueBox("p-uniform", paste("p = ", format.pval(PUNIres$res$pval.pb, eps = 0.001, digits = 3)), width = 3, color = if(PUNIres$res$pval.pb < 0.05){"red"}else{"green"}),
    valueBox("p-uniform*", paste("p = ", format.pval(PUNISTres$res$pval.pb, eps = 0.001, digits = 3)), width = 3, color = if(PUNISTres$res$pval.pb < 0.05){"red"}else{"green"}),
    valueBox("Selection Models", paste("p = ?"), width = 3),
    valueBox("TES", format.pval(TESres$res, eps = 0.001, digits = 3), width = 3, color = if(TESres$res < 0.05){"red"}else{"green"})
  )
})
output$pubboxes3 <- renderUI({
  fluidRow(
    
  )
})
