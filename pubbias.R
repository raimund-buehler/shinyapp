##Begg and Mazumdar----

BMres <- reactiveValues()

observeEvent(input$go_BM, {
  BMres$BMtau <- tryCatch({
    BMres$res <- ranktest(meta_res_output())
    round(BMres$res$tau, 3)},
    error = function(e){
      tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
    })
  BMres$BMp <- tryCatch({
    BMres$res <- ranktest(meta_res_output())
    format.pval(BMres$res$pval, eps = 0.0001, digits = 3)},
    error = function(e){
      tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
    })
})
 
output$BMtau <- renderValueBox({
  req(BMres$BMtau)
  valueBox(
    BMres$BMtau, subtitle = "Kendall's tau", color = "light-blue")
})

output$BMp <- renderValueBox({
  req(BMres$BMp)
  valueBox(BMres$BMp,
    subtitle = "P-value", color = "light-blue")
})

observeEvent(input$go_BM, {
  req(meta_res_output())
  output$FunnelBM <- renderPlot({
  #   p <- viz_funnel(data_reac$DT[, .SD, .SDcols = c(para$es, para$se)],
  #                   egger = FALSE,
  #                   trim_and_fill = FALSE,
  #                   method = estim())
  #   as.ggplot(p)
  req(normal_funnel_input())
  print(normal_funnel_input())
  })
})



output$BMhelp <- renderText({
  "This test can be used to examine whether the observed outcomes and 
  the corresponding sampling variances are correlated. A high correlation 
  would indicate that the funnel plot is asymmetric, which may be a result 
  of publication bias."
  }) 


output$BMref <- 
  renderUI(HTML(paste(strong("References:"), br(), "Begg, C. B., & Mazumdar, M. (1994). 
                      Operating characteristics of a rank correlation test for publication bias. 
                      Biometrics, 50, 1088–1101.")))

##Sterne and Egger----
SEres <- reactiveValues()

observeEvent(input$go_SE, {
  SEres$SEz <- tryCatch({SEres$res <- regtest(meta_res_output())
  round(SEres$res$zval, 3)},
  error = function(e){
    tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
  })
  SEres$SEp <- tryCatch({SEres$res <- regtest(meta_res_output())
  format.pval(SEres$res$pval, eps = 0.0001, digits = 3)},
  error = function(e){
    tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
  })
})

output$SEz <- renderValueBox({
  req(SEres$SEz)
  valueBox(
  SEres$SEz, subtitle = "Test Statistic: z", color = "light-blue"
  )
}) 

output$SEp <- renderValueBox({
  req(SEres$SEp)
  valueBox(
    SEres$SEp, subtitle = "P-Value", color = "light-blue"  
  )
}) 

output$SEmodel <- renderTable({
  req(SEres$res)
  coef(summary(SEres$res$fit))
}, rownames = TRUE)

observeEvent(input$go_SE, {
  req(meta_res_output())
output$FunnelSE <- renderPlot({
  # p <- viz_funnel(data_reac$DT[, .SD, .SDcols = c(para$es, para$se)],
  #                 egger = TRUE,
  #                 trim_and_fill = FALSE,
  #                 method = estim())
  # as.ggplot(p)
  req(normal_funnel_input())
  print(normal_funnel_input())
})
})

output$SEhelp <- renderText({
    "This test can be used to examine whether a relationship exists between the observed outcomes 
    and the chosen predictor (default = standard error). If such a relationship is present, then this 
    usually implies asymmetry in the funnel plot, which in turn may be an indication of publication bias."
}) 

output$SEref <- renderUI(HTML(paste(strong("References:"), br(), "Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). 
                                    Bias in meta-analysis detected by a simple, graphical test. British Medical Journal, 315, 629–634.")))

##Trim and Fill----
TFres <- reactiveValues()

observeEvent(input$go_TRFI, {
  TFres$TRFIk0 <- 
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        TFres$res$k0
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        TFres$res$k0
      },            error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
      })
  
  TFres$TRFIest <-
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        round(TFres$res$beta, 3)
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        round(TFres$res$beta, 3)
      },            error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
      })
  TFres$TRFIside <-
    tryCatch(
      if (sign(meta_res_output()$b) == 1) {
        TFres$res <- trimfill(meta_res_output(), side = "left")
        TFres$res$side
      } else if (sign(meta_res_output()$b) == -1) {
        TFres$res <- trimfill(meta_res_output(), side = "right")
        TFres$res$side
      },            error = function(e){
        tags$p("Please execute the Meta-Analysis first!", style = "font-size: 40%")
      })
  
  TFres$TRFIunadj <- round(meta_res_output()$b, 3)
  TFres$TRFIperc <- {
    unadj <- meta_res_output()$b
    adj <- TFres$res$beta
    1/unadj*(unadj-adj)
  }
  TFres$TRFImodel <- coef(summary(TFres$res, digits = 3))
})

output$TRFIk0 <- renderValueBox({
  req(TFres$TRFIk0)
  valueBox(TFres$TRFIk0, subtitle = "Number of missing studies", color = "light-blue")
}) 

output$TRFIest <- renderValueBox({
  req(TFres$TRFIest)
  valueBox(TFres$TRFIest, subtitle = "Adjusted Estimate", color = "light-blue")
}) 

output$TRFIside <- renderValueBox({
  req(TFres$TRFIside)
  valueBox(TFres$TRFIside, subtitle = "Side on which studies are imputed", color = "light-blue")
}) 

output$TRFIunadj <- renderValueBox({
  req(TFres$TRFIunadj)
  valueBox(TFres$TRFIunadj, color = "light-blue", subtitle = "Unadjusted Estimate")
})

output$TRFIperc <- renderValueBox({
  req(TFres$TRFIperc)
  valueBox(percent(TFres$TRFIperc), subtitle = "Percent Change", color = if(TFres$TRFIperc < 0.2){"green"}else{"red"})
})


output$TRFImodel <- renderTable({
  req(TFres$res)
  TFres$TRFImodel
}, rownames = TRUE)

output$FunnelTRFI <- renderPlot({
  req(normal_funnel_input())
  print(normal_funnel_input())
})


output$TRFIhelp <- renderText({
   "This method is a nonparametric (rank-based) data augmentation technique. 
        It can be used to estimate the number of studies missing from a meta-analysis due to the suppression 
        of the most extreme results on one side of the funnel plot. The method then augments the observed data so that the 
        funnel plot is more symmetric and recomputes the summary estimate based on the complete data. This should not 
        be regarded as a way of yielding a more 'valid' estimate of the overall effect or outcome, but as a way of examining the sensitivity of the 
        results to one particular selection mechanism (i.e., one particular form of publication bias)."}) 

output$TRFIref <- renderUI(HTML(paste(strong("References:"), br(), "Duval, S. J., & Tweedie, R. L. (2000). 
                                      Trim and fill: A simple funnel-plot-based method of testing and adjusting 
                                      for publication bias in meta-analysis. Biometrics, 56, 455–463.")))

##pcurve----
output$pcurveHelp <- renderText({"P-curve is based on the notion that, if a given set of studies has evidential value, 
                                the distribution of those one-tailed p-values will be right skewed. This means that 
                                very small p-values (such as p < .025) will be more numerous than larger p-values. 
                                If the distribution of significant p-values is left skewed and large p-values are more 
                                numerous than expected, it could be interpreted as evidence of p-hacking—researchers 
                                may be striving to obtain p-values that fall just below .05. 
                                P-curve uses two tests to assess whether the distribution is right skewed. 
                                The first is a binomial test comparing the proportion of observed p-values above and below .025; 
                                the second is a continuous test that calculates the probability of observing each individual p-value 
                                under the null hypothesis. The probabilities produced by this second test are then dubbed the studies’ “pp” values. 
                                These tests for right skew assess what is called the full p-curve. To test for “ambitious p-hacking,” 
                                or p-hacking to reach below .025 rather than .05, p-curve conducts the same tests for right skew on 
                                only the observed p-values that are below .025, or the “half p-curve.” 
                                If these tests for right skew are not significant, indicating that the studies lack evidential value 
                                and no true effect may be present, p-curve conducts another pair of binomial and continuous tests to 
                                assess whether the studies were underpowered (defined as having power below 33 percent)."})

observeEvent(input$go_pcurve, {
source(here("pcurve_demo.R"), local = TRUE)}
)

##puniform----
##needs r, --> calc-effectsize

output$puniHelp <- renderText("P-uniform is based on the idea that p-values, conditional on a true effect size, 
                             are uniformly distributed. The method performs two tests. The first assesses the 
                             null hypothesis that the population effect size is zero by examining whether the 
                             conditional distribution of observed p-values is uniform. The second test is a 
                             one-tailed test of whether the population effect size equals the effect- size 
                             estimate produced by a traditional fixed-effect meta-analysis. If they differ 
                             significantly publication bias may be a threat.  Finally, p-uniform provides an 
                             adjusted effect-size estimate and confidence interval by searching for the 
                             population effect size that does meet its qualification—the value where the distribution 
                             of conditional p-values is uniform.")

PUNIres <- reactiveValues()

p_uni_est <- eventReactive(input$go_puni, {
  n <- para$n
  tryCatch(
    if (sign(meta_res_output()$b) == 1){
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right", method="P")
      DT <- data.table("est" = PUNIres$res$est, "ci.lb" = PUNIres$res$ci.lb, "ci.ub" = PUNIres$res$ci.ub, "zval" = PUNIres$res$L.0, "pval" = PUNIres$res$pval.0)
      DT
    } else if (sign(meta_res_output()$b) == -1) {
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
      DT <- data.table("est" = PUNIres$res$est, "ci.lb" = PUNIres$res$ci.lb, "ci.ub" = PUNIres$res$ci.ub, "zval" = PUNIres$res$L.0, "pval" = PUNIres$res$pval.0)
      DT
    },           error = function(e){
      "Please execute the Meta-Analysis first!"
    })
  
})

output$p_uni_est <- renderTable({p_uni_est()})

puni_est_fe <- eventReactive(input$go_puni, {
  n <- para$n
  tryCatch(
    if (sign(meta_res_output()$b) == 1){
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right", method="P")
      DT <- data.table("est" = PUNIres$res$est.fe, "se" = PUNIres$res$se.fe, "ci.lb" = PUNIres$res$ci.lb.fe, "ci.ub" = PUNIres$res$ci.ub.fe, "zval" = PUNIres$res$zval.fe, "pval" = PUNIres$res$pval.fe)
      DT
    } else if (sign(meta_res_output()$b) == -1) {
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
      DT <- data.table("est" = PUNIres$res$est.fe, "se" = PUNIres$res$se.fe, "ci.lb" = PUNIres$res$ci.lb.fe, "ci.ub" = PUNIres$res$ci.ub.fe, "zval" = PUNIres$res$zval.fe, "pval" = PUNIres$res$pval.fe)
      DT
    },           error = function(e){
      "Please execute the Meta-Analysis first!"
    })
})

output$puni_est_fe <- renderTable({puni_est_fe()})

output$puni_L.pb <- renderValueBox({
  valueBox(tryCatch(
    tags$p(round(PUNIres$res$L.pb, 3), style = "font-size: 50%"),  
  error = function(e){
    ""
  }), subtitle = "Test Statistic: Z", color = "light-blue")
})

output$puni_pval.pb <- renderValueBox({
  valueBox(tags$p(format.pval(PUNIres$res$pval.pb, eps = 0.0001, digits = 3), style = "font-size: 50%"), subtitle = "P-value", color = "light-blue") 
})



PUNISTres <- reactiveValues()
puni_star_est <- eventReactive(input$go_puni, {
  n <- para$n
  tryCatch(
    if (sign(meta_res_output()$b) == 1){
      PUNISTres$res <- puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right")
      DT <- data.table("est" = PUNISTres$res$est, "ci.lb" = PUNISTres$res$ci.lb, "ci.ub" = PUNISTres$res$ci.ub, "zval" = PUNISTres$res$L.0, "pval" = PUNISTres$res$pval.0)
      DT
    } else if (sign(meta_res_output()$b) == -1) {
      PUNISTres$res <- puni_star(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left")
      DT <- data.table("est" = PUNISTres$res$est, "ci.lb" = PUNISTres$res$ci.lb, "ci.ub" = PUNISTres$res$ci.ub, "zval" = PUNISTres$res$L.0, "pval" = PUNISTres$res$pval.0)
      DT
    },            error = function(e){
      "Please execute the Meta-Analysis first!"
    })
})

output$puni_star_est <- renderTable({puni_star_est()})

puni_star_est_fe <- eventReactive(input$go_puni, {
  n <- para$n
  tryCatch(
    if (sign(meta_res_output()$b) == 1){
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="right", method="P")
      DT <- data.table("est" = PUNIres$res$est.fe, "se" = PUNIres$res$se.fe, "ci.lb" = PUNIres$res$ci.lb.fe, "ci.ub" = PUNIres$res$ci.ub.fe, "zval" = PUNIres$res$zval.fe, "pval" = PUNIres$res$pval.fe)
      DT
    } else if (sign(meta_res_output()$b) == -1) {
      PUNIres$res <- puniform(ri = data_reac$DTall$r, ni = data_reac$DTall$n, side="left",method="P")
      DT <- data.table("est" = PUNIres$res$est.fe, "se" = PUNIres$res$se.fe, "ci.lb" = PUNIres$res$ci.lb.fe, "ci.ub" = PUNIres$res$ci.ub.fe, "zval" = PUNIres$res$zval.fe, "pval" = PUNIres$res$pval.fe)
      DT
    },           error = function(e){
      "Please execute the Meta-Analysis first!"
    })
})

output$puni_star_est_fe <- renderTable({puni_star_est_fe()})

output$puni_star_L.pb <- renderValueBox({
  valueBox(tryCatch(
    tags$p(round(PUNISTres$res$L.pb, 3), style = "font-size: 50%"),  
    error = function(e){
      ""
    }), subtitle = "Test Statistic: Z", color = "light-blue")
})

output$puni_star_pval.pb <- renderValueBox({
  valueBox(tags$p(format.pval(PUNISTres$res$pval.pb, eps = 0.0001, digits = 3), style = "font-size: 50%"), subtitle = "P-value", color = "light-blue") 
})

output$puniref <- renderUI({HTML(paste(strong("References:"), br(), "Van Aert, R. C. M., Wicherts, J. M., & van Assen, M. A. L. M. (2016). 
                            Conducting meta-analyses on p-values: Reservations and recommendations for applying p-uniform and p-curve. 
                            Perspectives on Psychological Science, 11(5), 713-729. doi:10.1177/1745691616650874" , br(),
                            
                            "Van Assen, M. A. L. M., van Aert, R. C. M., & Wicherts, J. M. (2015). 
                            Meta-analysis using effect size distributions of only statistically significant studies. 
                            Psychological Methods, 20(3), 293-309. doi: http://dx.doi.org/10.1037/met0000025"))
  })

##Vevea and woods selection models----
SelMods <- eventReactive(input$go_selmod, {
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
  out <- list(mod1 = vevwoo.res$moderate_one,
              sev1 = vevwoo.res$severe_one,
              mod2 = vevwoo.res$moderate_two,
              sev2 = vevwoo.res$severe_two)
  
  return(out)

})

output$SelHelp <- renderText("Selection models adjust meta-analytic data sets by specifying a model that describes the 
                             mechanism by which studies may be suppressed. This model is combined with an effect-size 
                             model that describes the distribution of effect sizes in the absence of publication bias. 
                             The Vevea and Woods (2005) approach, which is implemented here, attempts to model the suppression 
                             of studies as a function of p-values by specifying selection models of varying severity and 
                             estimating the meta-analytic parameters contingent on each hypothetical selection pattern. 
                             This can serve as a sensitivity analysis to investigate how a specified pattern of publication 
                             bias could affect the summary estimate")


output$modone <- renderValueBox({validate(need(isTruthy(SelMods()$mod1), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$mod1$output_adj$par[2], 3), subtitle = "Adjusted Estimate", color = "light-blue")
  })
output$modone_unadj <- renderValueBox({validate(need(isTruthy(SelMods()$mod1), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$mod1$output_unadj$par[2], 3), subtitle = "Unadjusted Estimate", color = "light-blue")
})

output$modone_perc <- renderValueBox({validate(need(isTruthy(SelMods()$mod1), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$mod1$output_unadj$par[2]
  adj <- SelMods()$mod1$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Percent Change", color = if(perc_change < 0.2){"green"}else{"red"})
})

output$sevone <- renderValueBox({validate(need(isTruthy(SelMods()$sev1), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$sev1$output_adj$par[2], 3), subtitle = "Adjusted Estimate", color = "light-blue")
  })
output$sevone_unadj <- renderValueBox({validate(need(isTruthy(SelMods()$sev1), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$sev1$output_unadj$par[2], 3), subtitle = "Unadjusted Estimate", color = "light-blue")
})
output$sevone_perc <- renderValueBox({validate(need(isTruthy(SelMods()$sev1), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$sev1$output_unadj$par[2]
  adj <- SelMods()$sev1$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Percent Change", color = if(perc_change < 0.2){"green"}else{"red"})
})


output$modtwo <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$mod2$output_adj$par[2], 3), subtitle = "Adjusted Estimate", color = "light-blue")
  })
output$modtwo_unadj <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$mod2$output_unadj$par[2], 3), subtitle = "Unadjusted Estimate", color = "light-blue")
})
output$modtwo_perc <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$mod2$output_unadj$par[2]
  adj <- SelMods()$mod2$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Percent Change", color = if(perc_change < 0.2){"green"}else{"red"})
})

output$sevtwo <- renderValueBox({validate(need(isTruthy(SelMods()$sev2), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$sev2$output_adj$par[2], 3), subtitle = "Adjusted Estimate", color = "light-blue")
})
output$sevtwo_unadj <- renderValueBox({validate(need(isTruthy(SelMods()$sev2), "Please select the earliest study in your dataset!"))
  valueBox(round(SelMods()$sev2$output_unadj$par[2], 3), subtitle = "Unadjusted Estimate", color = "light-blue")
})
output$sevtwo_perc <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$sev2$output_unadj$par[2]
  adj <- SelMods()$sev2$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Percent Change", color = if(perc_change < 0.2){"green"}else{"red"})
})

# EXCESS OF SIGNIFICANCE TEST - Ioannidis & Trikalinos (2007) ----

# **** number of significant studies (stored in ksign of for.power object), contingent on sign of summary effect (O)

TESres <- eventReactive(input$go_tes, {
  
  req(meta_res_output())
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
  
  
  
  out <- list(
    O = O,
    E = E,
    A = A,
    res = res.it,
    for.res.it = for.res.it
  )
  return(out)
  })

output$TEShelp <- renderText(
  "The test of excess significance is a null hypothesis significance 
  test that takes a given set of studies and asks whether too many are 
  statistically significant or “positive.” The expected number of 
  positive studies is calculated based on the studies’ power, and 
  that expected number is compared with the observed number of positive 
  studies using the chi-square statistic. Guidelines for the TES indicate 
  that a p-value less than .10 should be considered significant.
  Note: this test should not be interpreted if there are fewer observed 
  than expected significant studies"
)

output$TESexp <- renderValueBox({
  valueBox(round(TESres()$E, 0), subtitle = "Expected number of significant studies", color = "light-blue")
})

output$TESobs <- renderValueBox({
  valueBox(round(TESres()$O, 0), subtitle = "Observed number of significant studies", color = "light-blue")
})

output$TESchi <- renderValueBox({
  valueBox(round(TESres()$A, 3), subtitle = "Chi-square statistic (df = 1)", color = "light-blue")
})

output$TESpval <- renderValueBox({
  valueBox(round(TESres()$res, 3), subtitle = "P-value", color = "light-blue")
})


##SUMMARY ----

##BM

output$BMsum <- renderValueBox({
  valueBox(
           paste("p = ", format.pval(BMres$res$pval, eps = 0.001, digits = 3)), 
           subtitle = "Rank Correlation", width = NULL, color = if(BMres$res$pval < input$BM_p){"red"}else{"green"})
})

##SE

output$SEsum <- renderValueBox({
  valueBox(subtitle = "Regression", 
           paste("p = ", format.pval(SEres$res$pval, eps = 0.001, digits = 3)), 
           color = if(SEres$res$pval < input$SE_p){"red"}else{"green"})
  
})

##TRFI

output$TRFIsum <- renderValueBox({
  unadj <- meta_res_output()$b
  adj <- TFres$res$beta
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), 
  subtitle = "Percent Change", color = if(perc_change < input$tf_adj){"green"}else{"red"})
})

##pcurve

##Studies contain evidential value
output$pcurvebinsum <- renderValueBox({
    valueBox(paste0("p ", pcurve()$binom.r), subtitle = "Binomial", color = if(as.numeric(substring(pcurve()$binom.r, 2)) < input$pcurve_p){"green"}else{"red"})
})

output$pcurvefullsum <- renderValueBox({
  valueBox(paste0("p ", pcurve()$p.Zppr), subtitle = "Full p-curve", color = if(as.numeric(substring(pcurve()$p.Zppr, 3)) < input$pcurve_p){"green"}else{"red"})
})

output$pcurvehalfsum <- renderValueBox({
  valueBox(paste0("p ", pcurve()$p.Zppr.half), subtitle = "Half p-curve", color = if(as.numeric(substring(pcurve()$p.Zppr.half, 3)) < input$pcurve_p){"green"}else{"red"})
})


##Studies value inadequate
output$pcurvebinsum33 <- renderValueBox({
  valueBox(paste0("p ", pcurve()$binom.33), subtitle = "Binomial", color = if(as.numeric(substring(pcurve()$binom.33, 3)) > input$pcurve_p){"green"}else{"red"})
})

output$pcurvefullsum33 <- renderValueBox({
  valueBox(paste0("p ", pcurve()$p.Zpp33), subtitle = "Full p-curve", color = if(as.numeric(substring(pcurve()$p.Zpp33, 3)) > input$pcurve_p){"green"}else{"red"})
})

output$pcurvehalfsum33 <- renderValueBox({
  valueBox(paste0("p ", pcurve()$p.Zpp33.half), subtitle = "Half p-curve", color = if(as.numeric(substring(pcurve()$p.Zpp33.half, 3)) > input$pcurve_p){"green"}else{"red"})
})

##puniform

output$punisum <- renderValueBox({
  valueBox(paste0("p = ", format.pval(PUNIres$res$pval.pb, eps = 0.0001, digits = 3)), 
  subtitle = "Publication Bias Test", color = if(PUNIres$res$pval.pb > input$puni_p){"green"}else{"red"}) 
})

output$punistar_sum <- renderValueBox({
  valueBox(paste0("p = ", format.pval(PUNISTres$res$pval.pb, eps = 0.0001, digits = 3)), 
           subtitle = "Publication Bias Test", color = if(PUNISTres$res$pval.pb > input$punistar_p){"green"}else{"red"}) 
})

##TES

output$TESsum <- renderValueBox({
  valueBox(format.pval(TESres()$res, eps = 0.001, digits = 3), subtitle = "P-value: Observed - Expected", color = if(TESres()$res < input$TES_p){"red"}else{"green"})
})

##SelMods

output$mod1sum <- renderValueBox({validate(need(isTruthy(SelMods()$mod1), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$mod1$output_unadj$par[2]
  adj <- SelMods()$mod1$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Moderate One-tailed", color = if(perc_change < input$sel_adj){"green"}else{"red"})
})

output$sev1sum <- renderValueBox({validate(need(isTruthy(SelMods()$sev1), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$sev1$output_unadj$par[2]
  adj <- SelMods()$sev1$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Severe One-tailed", color = if(perc_change < input$sel_adj){"green"}else{"red"})
})

output$mod2sum <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$mod2$output_unadj$par[2]
  adj <- SelMods()$mod2$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Moderate Two-tailed", color = if(perc_change < input$sel_adj){"green"}else{"red"})
})

output$sev2sum <- renderValueBox({validate(need(isTruthy(SelMods()$mod2), "Please select the earliest study in your dataset!"))
  unadj <- SelMods()$sev2$output_unadj$par[2]
  adj <- SelMods()$sev2$output_adj$par[2]
  perc_change <- 1/unadj*(unadj-adj)
  valueBox(percent(perc_change), subtitle = "Severe Two-tailed", color = if(perc_change < input$sel_adj){"green"}else{"red"})
})