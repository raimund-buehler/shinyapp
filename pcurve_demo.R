# P-curve

# Put calculations done inside the pcurve app into reactive
# put objects necessary for outputs (plots, table, input datastring) into list

pcurve <- reactive({
  req(meta_res_output())
  source(here("pcurve_functions.R"), local = TRUE)
  
# Generate input string for pcurve
#if summary ES = positive, first statement is called, if negative - second statement is called
  if (sign(meta_res_output()$b) == 1){
  data_dir <-data_reac$DTall[r > 0, ]
  } else if (sign(meta_res_output()$b) == -1) {
  data_dir <-data_reac$DTall[r < 0, ]
}

  datastringr<-sub("^(-?)0.", "\\1.", sprintf("%.15f", data_dir$r))
  raw <- paste0("r","(",data_dir$n-2,")","=",datastringr)
  
  ktot=length(raw)                                      #count studies
  
  #Create vector that numbers studies 1 to N,includes n.s. studies
  k=seq(from=1,to=length(raw))
  
  #1.2 Parse the entered text into usable statistical results
  #1.3 Create test type indicator
  stat=substring(raw,1,1)          #stat:   t,f,z,c,r
  test=ifelse(stat=="r","t",stat)  #test:   t,f,z,c      (r-->t)  
  
  #1.4 Create family to turn t-->F and z-->chi2
  family=test
  family=ifelse(test=="t","f",family)
  family=ifelse(test=="z","c",family)
  
  #Note on terminology:
  #Stat:   t,f,c,z,r  is what the user entered, t,f,c,z,r
  #test:   t,f,c,z    is the test statistic, same as stat but with r-->t
  #family: f,c        converting t-->f and z-->c
  
  #1.5 Find comma,parentheses,equal sign 
  par1 =str_locate(raw,"\\(")[,1]         #(  First  parenthesis
  par2 =str_locate(raw,"\\)")[,1]         #)  Second parenthesis
  comma=str_locate(raw,",")[,1]           #,  comma
  eq   =str_locate(raw,"=")[,1]           #=  equal
  
  #1.6 DF for t-tests
  df=as.numeric(ifelse(test=="t",substring(raw,par1+1,par2 -1),NA))             #t(df) later assigned to df2 in  F test with df1=1
  
  #1.7 DF1 for all tests 
  #   recall, normal=sqrt(chi(1)) so df1=1 for Normal, same f(1,df)<-t(df)
  df1=as.numeric(ifelse(test=="f",substring(raw,par1+1,comma-1),NA))            #If F test, take value after comma, NA otherwise
  df1=as.numeric(ifelse(test=="z",1,df1))                                       #If Z replace missing value with a 1
  df1=as.numeric(ifelse(test=="t",1,df1))                                       #If t, replace missing value with a 1
  df1=as.numeric(ifelse(test=="c",substring(raw,par1+1,par2 -1),df1))           #If c, replace missing value with value in ()
  
  #1.8 DF2 for F(df1,df2) tests
  df2=as.numeric(ifelse(test=="f",substring(raw,comma+1,par2-1),NA))            #F-test
  df2=as.numeric(ifelse(test=="t",df,df2))                                      #t(df) into the df2 F test
  
  #1.9 Take value after equal sign, the value of the test-statistic, and put it in vector "equal"
  equal=abs(as.numeric(substring(raw,eq+1)))  #if not a r(), take the value after the ="
  
  #1.10  Go from "equal" (the value after = sign) to F or Chi2 value,
  value=ifelse((stat=="f" | stat=="c"),equal,NA)                      #For F and Chi2 test, equal=value
  value=ifelse(stat=="r", (equal/(sqrt((1-equal**2)/df2)))**2,value)  #For correlation, first turn value (r) to t, then square t. (using t=r/sqrt(1-r**2)/DF)
  value=ifelse(stat=="t", equal**2 ,value)                            #For t and Z, square it since t(df)**2=f(1,df) and z**2=chi(1)
  value=ifelse(stat=="z", equal**2 ,value)  
  
  
  
  #1.11 Compute p-values
  p=ifelse(family=="f",1-pf(value,df1=df1,df2=df2),NA)
  p=ifelse(family=="c",1-pchisq(value,df=df1),p)
  p=pbound(p)  #Bound it to level of precision, see function 3 above 
  
  #1.12 Count  studies
  #ktot is all studies
  ksig= sum(p<.05,na.rm=TRUE)     #significant studies
  khalf=sum(p<.025,na.rm=TRUE)    #half p-curve studies
  
  ###############################################################################  
  #(2) COMPUTE PP-VALUES ----
  ##############################################################################  
  
  #2.1 Right Skew, Full p-curve
  ppr=as.numeric(ifelse(p<.05,20*p,NA))            #If p<.05, ppr is 1/alpha*p-value, so 20*pvalue, otherwise missing. 
  ppr=pbound(ppr)                                  #apply pbound function to avoid 0
  
  
  #2.2 Right Skew, half p-curve
  ppr.half=as.numeric(ifelse(p<.025,40*p,NA))    #If p<.05, ppr is 40*pvalue, otherwise missing. 
  ppr.half=pbound(ppr.half)
  
  #2.3 Power of 33%
  #2.3.1 NCP for  f,c distributions
  # NCP33 (noncentrality parameter giving each test in p-curve 33% power given the d.f. of the test)
  ncp33=mapply(getncp,df1=df1,df2=df2,power=1/3,family=family)  #See function 1 above
  
  #2.3.2 Full-p-curve
  #Using the ncp33 compute pp33
  pp33=ifelse(family=="f" & p<.05,3*(pf(value, df1=df1, df2=df2, ncp=ncp33)-2/3),NA)
  pp33=ifelse(family=="c" & p<.05,3*(pchisq(value, df=df1, ncp=ncp33)-2/3),pp33)
  pp33=pbound(pp33)
  
  #2.3.3 HALF-p-curve
  #Share of p-values expected to be p<.025 if 33% power (using Function 4 from above, prop33() )
  
  
  prop25=3*prop33(.025)
  prop25.sig=prop25[p<.05]
  # print(prop25.sig)
  
  #F-tests (& thus  t-tests)
  #prop=ifelse(family=="f" & p<.05, 1 - pf(qf(1-.025,df1=df1, df2=df2),df1=df1, df2=df2, ncp=ncp33),NA)
  #Chi2 (& thus Normal)
  #prop=ifelse(family=="c" & p<.05,1-pchisq(qchisq(1-.025,df=df1),  df=df1, ncp=ncp33),prop)
  #output it
  #prop
  
  
  #print(prop)
  #Compute pp-values for the half
  pp33.half=ifelse(family=="f" & p<.025, (1/prop25)*(    pf(value,df1=df1,df2=df2,ncp=ncp33)-(1-prop25)),NA)
  pp33.half=ifelse(family=="c" & p<.025, (1/prop25)*(pchisq(value,df=df1,         ncp=ncp33)-(1-prop25)),pp33.half)
  pp33.half=pbound(pp33.half)
  
  
  ###############################################################################  
  #(3) INFERENCE - STOUFFER & BINOMIAL ----
  ##############################################################################  
  
  #3.1 Convert pp-values to Z scores, using Stouffer function above
  Zppr =     stouffer(ppr)            #right skew  - this is a Z value from Stouffer's test
  Zpp33=     stouffer(pp33)           #33% - idem 
  Zppr.half= stouffer(ppr.half)       #right skew, half p-curve - idem 
  Zpp33.half=stouffer(pp33.half)      #33% skew, half p-curve - idem 
  
  #3.2 Overall p-values from Stouffer test
  p.Zppr =pnorm(Zppr)	
  p.Zpp33=pnorm(Zpp33)
  p.Zppr.half =pnorm(Zppr.half)
  p.Zpp33.half=pnorm(Zpp33.half)
  
  #3.3 Save results to file (STOUFFER) ----
  main.results=as.numeric(c(ktot, ksig, khalf, Zppr, p.Zppr, Zpp33, p.Zpp33, Zppr.half, p.Zppr.half, Zpp33.half, p.Zpp33.half))
  
  #3.4 BINOMIAL
  #Observed share of p<.025
  prop25.obs=sum(p<.025)/sum(p<.05)
  #3.4.1 Flat null
  binom.r=1-pbinom(q=prop25.obs*ksig- 1, p=.5, size=ksig)     #The binomial in R computes the probability of x<=xo. We want prob(x>=x0) so we subtract one from x, and 1-prob()
  #3.4.2 Power of 33% null
  binom.33=ppoibin(kk=prop25.obs*ksig,pp=prop25[p<.05])             
  
  #syntax for ppoibin():
  #   kk: is the proportion of succeses, a scalar, in this case, the share of p<.025
  #   pp: is the probabilty of success for each attempt, the number of attempts is determined
  #    by the length of the vector. For example ppoibin(kk=0,pp=c(.5,.5,.5)) is .125,
  #    if there are three attempts, each with probability .5, the odds of getting 0 succeses is .125
  #     ppoibin(kk=1,pp=c(1,.75)), in turn computes the probability of getting 1 success
  #     when one has a 100% of success, and the other 75%, and the solution is .25, since
  #     the first one succeeds for sure and the second would need to fail, with 25% chance.
  
  
  #3.4.3  Save binomial results
  binomial=c(mean(prop25.sig), prop25.obs, binom.r, binom.33)
  # write(binomial, here(pcurve.loc, filename, paste("BINOMIAL_",filek,".txt", sep="")),sep="\n")
  
  
  #3.5 Beutify results for printing in figure 
 
  
  #If there are zero p<.025, change Stouffer values for half-p-curve tests for "N/A" messages	
  if (khalf==0) {
    Zppr.half ="N/A"
    p.Zppr.half ="=N/A"
    Zpp33.half ="N/A"
    p.Zpp33.half ="=N/A"
  }
  
  
  #If there are more than 1 p<.025, round the Z and beutify the p-values
  if (khalf>0) {
    Zppr.half =round(Zppr.half,2)
    Zpp33.half =round(Zpp33.half,2)
    p.Zppr.half=cleanp(p.Zppr.half)
    p.Zpp33.half=cleanp(p.Zpp33.half)
  }
  
  #Clean  results for full test
  Zppr=round(Zppr,2)
  Zpp33=round(Zpp33,2)
  p.Zppr=cleanp(p.Zppr)
  p.Zpp33=cleanp(p.Zpp33)
  binom.r=cleanp(binom.r)
  binom.33=cleanp(binom.33)
  
  ################################################
  #(4) POWER ESTIMATE -----
  ################################################
  
  
  
  
  #4.2 COMPUTE FIT FOR EACH POWER for 5.1%, AND THEN 6-99%, AND PLOT IT. With power=5% boundary condition lead to errors
  #This becomes the diagnostic plot and gives us the best estimate, within 1%, of power.
  
  #Create image file to contain results -----
  # png(filename = "/Users/uni/Documents/FWF/pcurveplot-test/fit.)
  #  width=1200, height=1000, res=200)  
  # Fit will be evaluated at every possible value of power between 5.1% and 99% in steps of 1%, stored in fit()
  
  
  fit=c()                                          #Create empty vector
  fit=abs(powerfit(.051))                      #Start it eavaluting fit of 5.1% power
  for (i in 6:99)   fit=c(fit,abs(powerfit(i/100))) #Now do 6% to 99%
  
  # Find the minimum
  #which ith power level considered leads to best estimate
  mini=match(min(fit,na.rm=TRUE),fit)       
  #convert that into the power level, the ith value considered is (5+ith)/100
  hat=(mini+4)/100                          
  

  #4.3.2 Boundary conditions (when the end of the ci=5% or 99% we cannot use root to find it, 
  #use boundary value instead)
  
  #Boundary conditions
  p.power.05=pnorm(powerfit(.051)) #Proability p-curve would be at least at right-skewed if power=.051
  p.power.99=pnorm(powerfit(.99))  #Proability p-curve would be at least at right-skewed if power=.99
  
  #4.3.3 Find lower end of ci
  #Low boundary condition? If cannot reject 5% power, don't look for lower levels, use 5% as the end 
  if (p.power.05<=.95) power.ci.lb=.05   
  #High boundary condition? If we reject 99%, from below dont look for higher power, use 99% as the low end
  if (p.power.99>=.95) power.ci.lb=.99   
  #If low bound is higher than 5.1% power and lower than 99% power, estimate it, find interior solution
  if (p.power.05>.95 && p.power.99<.95)  power.ci.lb=get.power_pct(.95)
  
  
  #4.3.4 Higher end of CI
  #If we reject 5% power from below, 5% is above the confidence interval, use 5% as the upper end of the confidence interval
  if (p.power.05<=.05) power.ci.ub=.05
  #If we do not reject that 99% power, don't look higher, use 99% as the higher end 
  if (p.power.99>=.05) power.ci.ub=.99
  #If the the upper bound is between 5% and 99%, find it
  if (p.power.05>.05 && p.power.99<.05) power.ci.ub=get.power_pct(.05)
  
  
  #4.4 Save power fit estmiate and ci ----
  power_results=c(power.ci.lb,hat,power.ci.ub)
  
  # Save results as list ----
  
  # write(power_results, here(pcurve.loc, filename, paste("POWERHAT_",filek,".txt", sep="")),sep="\n")     
  
  #Note, I use hat as the estimate of power, with powerfit(.5) we could get a more precise best fitting 
  #level of power than the minimum in the figure above between .051 and .99, hat, but more precision than 1% in power is not informative.

  
  # source(here("pcurve_functions.R"), local = TRUE)
  

  ###############################################################################  
  #(5) MAIN FIGURE: OBSERVED P-CURVE AND EXPECTED UNDERL NULL AND 33% POWER ----
  ##############################################################################  



  
  #5.1 Green line (Expected p-curve for 33% power)
  #5.1.1 Proportion of tests expected to get <01, <02...
  #Uses FUNCTION 4, prop33() - see top of page
  gcdf1=prop33(.01)         #vector with proportion of p-values p<.01, with 33% power
  gcdf2=prop33(.02)         #              ""                   p<.02,      "
  gcdf3=prop33(.03)         #              ""                   p<.03,      "
  gcdf4=prop33(.04)         #              ""                   p<.04,      "
  #Note: for p<.05 we know it is 33% power
  
  # SAVE RESULTS ----
  outlist <- list(p = p,
                  hat = hat,
                  power.ci.lb = power.ci.lb,
                  power.ci.ub = power.ci.ub,
                  p.Zppr = p.Zppr,
                  p.Zppr.half = p.Zppr.half,
                  p.Zpp33 = p.Zpp33,
                  binom.33 = binom.33,
                  ksig = ksig,
                  khalf = khalf,
                  ktot = ktot,
                  gcdf1 = gcdf1,
                  gcdf2 = gcdf2,
                  gcdf3 = gcdf3,
                  gcdf4 = gcdf4,
                  raw = raw)
  return(outlist)
  
})
  
# Main plot ----

  output$pcurve_plot <- renderPlot({
    req(pcurve())
    source(here("pcurve_functions.R"), local = TRUE)
   
  #5.1.2 Now compute difference, and divide by 1/3 to get the share of significant p-values in each bin      
  green1=mean(pcurve()$gcdf1,na.rm=TRUE)*3        #Average of the vector p<.01
  green2=mean(pcurve()$gcdf2-pcurve()$gcdf1,na.rm=TRUE)*3  #Difference between .02 and .01
  green3=mean(pcurve()$gcdf3-pcurve()$gcdf2,na.rm=TRUE)*3  #Difference between .03 and .02
  green4=mean(pcurve()$gcdf4-pcurve()$gcdf3,na.rm=TRUE)*3  #Difference between .04 and .03
  green5=mean(1/3-pcurve()$gcdf4,na.rm=TRUE)*3    #Difference between .05 and .04
  #Because we have one row per test submitted, the average is weighted average, giving each test equal weight
  green=100*c(green1,green2,green3,green4,green5)  #The 5 values plotted in the figure for 33% power line
  
  
  #5.2 The blue line (observed p-curve)   
  #Put all significant p-values  into bins, .01 ....05
  ps=ceiling(pcurve()$p[pcurve()$p<.05]*100)/100
  #Count # of tests in each bin
  blue=c()
  #This loop creates a vector, blue, with 5 elements, with the proportions of p=.01,p=.02...p=.05
  for (i in c(.01,.02,.03,.04,.05)) blue=c(blue,sum(ps==i,na.rm=TRUE)/pcurve()$ksig*100)
  
  
  #5.3 Red line
  red=c(20,20,20,20,20)
  
  #5.4 Make the graph ----
  #Note: R_temp & filek are parameterS set at the beggining of the program for the location of files
  # png(filename = "/Users/uni/Documents/FWF/pcurveplot-test/mainfig.png", width=2600, height=2400, res=400)  
  
  #5.4.1  Define x-axis as p-values (.01, .02..)
  x = c(.01,.02,.03,.04,.05)
  
  #5.4.2 Plot the observed p-curve
  par(mar=c(6,5.5,1.5,3)) 
  #5.4.3 Does blue line cross over  68% in when p>.02?
  #Does blue line go above 68%? If yes, move up graph so that legend is not interrupted by it
  moveup=max(max(blue[2:5])-66,0)  #moveup by difference between blue and 68
  ylim=c(0,105+moveup)
  legend.top=100+moveup
  
  #5.4.4 Start the plot  
  plot(x,blue,   type='l', col='dodgerblue2',  main="",
       lwd=2, xlab="", ylab="", xaxt="n",yaxt="n", xlim=c(0.01,0.051),
       ylim=ylim, bty='L', las=1,axes=F)  	
  
  #5.4.5 x-axis value labels
  x_=c(".01",".02",".03",".04",".05")
  axis(1,at=x,labels=x_)
  #5.4.6 y-axis value labels
  y_=c("0%","25%","50%","75%","100%")
  y=c(0,25,50,75,100)
  axis(2,at=y,labels=y_,las=1,cex.axis=1.2)
  
  #5.4.7 Add y-axis label
  mtext("Percentage of test results",font=2,side=2,line=3.85,cex=1.25)
  #5.4.8 Add y-axis label
  mtext("p            ",font=4,side=1,line=2.3,cex=1.25)
  mtext(" -value",      font=2,side=1,line=2.3,cex=1.25)
  
  #5.4.9 Add little point in actual frequencies
  points(x,blue,type="p",pch=20,bg="dodgerblue2",col="dodgerblue2")
  #5.4.10 Add value-labels
  text(x+.00075,blue+3.5,percent(round(blue)/100),col='black', cex=.75)
  #5.4.11 Add red and green lines
  lines(x,red,   type='l', col='firebrick2',    lwd=1.5, lty=3)
  lines(x,green, type='l', col='springgreen4',  lwd=1.5, lty=5)
  
  #5.4.12 Legend
  #x Position of text 
  tab1=.023          #Labels for line at p=.023 in x-axis
  tab2=tab1+.0015    #Test results and power esimates at tab1+.0015
  #gaps in y positions
  gap1=9             #between labels
  gap2=4             #between lable and respective test (e.g., "OBserved p-curve" and "power estimate")
  
  #Color of font for test results
  font.col='gray44'  
  
  #Legend for blue line
  #Put together the text for power estimate in a single variable
  text.blue=paste0("Power estimate: ",percent(pcurve()$hat),", CI(",
                   percent(pcurve()$power.ci.lb),",",
                   percent(pcurve()$power.ci.ub),")")
  #Print it
  text(tab1,legend.top,     adj=0,cex=.85,bquote("Observed "*italic(p)*"-curve"))
  text(tab2,legend.top-gap2,adj=0,cex=.68,text.blue,col=font.col)
  
  #Legend for red line           
  text.red=bquote("Tests for right-skewness: "*italic(p)*""[Full]~.(pcurve()$p.Zppr)*",  "*italic(p)*""[Half]~.(pcurve()$p.Zppr.half))
  #note: .() within bquote prints the value rather than the variable name
  text(tab1,legend.top-gap1,    adj=0,cex=.85, "Null of no effect" )  
  text(tab2,legend.top-gap1-gap2,  adj=0,cex=.68, text.red, col=font.col ) 
  
  #Legend for green line           
  text.green=bquote("Tests for flatness:            "*italic(p)*""[Full]~.(pcurve()$p.Zpp33)*",  "*italic(p)*""[Binomial]~.(pcurve()$binom.33))
  text(tab1,legend.top-2*gap1,    adj=0,cex=.85,"Null of 33% power") 
  text(tab2,legend.top-2*gap1-gap2,  adj=0,cex=.68,text.green,col=font.col) 
  
  #LINES in the legend:
  segments(x0=tab1-.005,x1=tab1-.001,y0=legend.top,y1=legend.top,      col='dodgerblue2',lty=1,lwd=1.5) 
  segments(x0=tab1-.005,x1=tab1-.001,y0=legend.top-gap1,  y1=legend.top-gap1,col='firebrick2',lty=3,lwd=1.5)      
  segments(x0=tab1-.005,x1=tab1-.001,y0=legend.top-2*gap1,y1=legend.top-2*gap1,col='springgreen4',lty=2,lwd=1.5)      
  #Box for the legend
  rect(tab1-.0065,legend.top-2*gap1-gap2-3,tab1+.029,legend.top+3,border='gray85')   
  
  #NOTE AT BOTTOM   
  
  #Number of tests in p-curve      
  msgx=bquote("Note: The observed "*italic(p)*"-curve includes "*.(pcurve()$ksig)*
                " statistically significant ("*italic(p)*" < .05) results, of which "*.(pcurve()$khalf)*
                " are "*italic(p)*" < .025.")
  mtext(msgx,side=1,line=4,cex=.65,adj=0)
  #Number of n.s. results entered
  kns=pcurve()$ktot-pcurve()$ksig
  if (kns==0) ns_msg="There were no non-significant results entered." 	
  if (kns==1) ns_msg=bquote("There was one additional result entered but excluded from "*italic(p)*"-curve because it was "*italic(p)*" > .05.") 	
  if (kns>1)  ns_msg=bquote("There were "*.(kns)*" additional results entered but excluded from "*italic(p)*"-curve because they were "*italic(p)*" > .05.")
  mtext(ns_msg,side=1,line=4.75,cex=.65,adj=0)
  
  ###############################################################################  
  #(6)  SAVE PP-VALUE CALCULATIONS TO TEXT FILES ----
  ##############################################################################  
  
  #6.1 Table contains the original text enter, the corresponding p-value, the pp-values, and Z-scores for those pp-values
  # table_calc=data.frame(raw, p, ppr, ppr.half, pp33, pp33.half, 
  #                      qnorm(ppr),  qnorm(ppr.half), qnorm(pp33), qnorm(pp33.half))
  #6.2 Headers
  #headers1=c("Entered statistic","p-value", "ppr", "ppr half", "pp33%","pp33 half",
  #          "Z-R","Z-R half","Z-33","z-33 half")
  #6.3 Put headers onto table
  #table_calc=setNames(table_calc,headers1)
  
  #6.4 Save it
  # write.table(table_calc,sep="	", row.names=FALSE, 
  # file=here(pcurve.loc, filename, paste("Calculations_",filek,".txt", sep="")))
  
  #6.5 Save results behind p-curve figure
  # headers2=c("p-value","Observed (blue)","Power 33% (Green)", "Flat (Red)")
  # table_figure=setNames(data.frame(x,blue,green,red),headers2)
  #Save it to file
  # write.table(table_figure, sep="	",row.names=FALSE, 
  #file=here(pcurve.loc, filename, paste("FigNumbers_",filek,".txt", sep="")))
  
})
 
# Results Section ----
  

# Display Input Datastring ----
  
  output$pcurve_input <- renderText({
    req(pcurve())
    paste(pcurve()$raw, collapse = "\n")
  })
  
  
  