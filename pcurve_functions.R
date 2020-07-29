# Set up functions ----
##############################################
#(0) CREATE A FEW FUNCTIONS ----
##############################################
#Function 1 - functions that find non-centrality parameter for f,chi distributions that gives some level of power

#F-test 
#Note: starting with app 4.0, t() are converted to F() and Z to chisq() to avoid unnecessary repetition of code
#So we only need code to find ncp for F() and chisq()

getncp.f <- function(df1,df2, power){      
  error <- function(ncp_est, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = ncp_est) - (1-power)  
  xc=qf(p=.95, df1=df1,df2=df2) 
  return(uniroot(error, c(0, 1000), x = xc, df1 = df1,df2=df2, power=power)$root)  }


#chisq-test
getncp.c <- function(df, power){      
  xc=qchisq(p=.95, df=df) 
  error = function(ncp_est, power, x, df)      pchisq(x, df = df, ncp = ncp_est) - (1-power)   
  return(uniroot(error, c(0, 1000), x = xc, df = df, power=power)$root)   
}

#Combine both in single function
getncp <- function(family,df1,df2,power){
  if (family=="f") ncp=getncp.f(df1=df1,df2=df2,power=power)
  if (family=="c") ncp=getncp.c(df=df1,power=power)
  return(ncp)  
}

###############################################################################
#Function 2 - percent() : makes a number look like a percentage
percent <- function(x, digits = 0, format = "f", ...){
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}
###############################################################################


###############################################################################
#Function 3 - pbound: bound p-values and pp-values by precision of measurement to avoid errors
pbound <- function(p){
  pmin(pmax(p,2.2e-16),1-2.2e-16)
}


#Function 4 - prop33(pc) - Computes % of p-values that are expected to be smaller than pc, 
#for the tests submitted to p-curve, if power is 33%
prop33 <- function(pc){
  #pc: critical  p-value
  
  #Overview:
  #Creates a vector of the same length as the number of tests submitted to p-curve, significant and not,
  #    and computes the proportion of p-values expected to be smaller than {pc} given the d.f. 
  #    and outputs the entire vector, with NA values where needed
  
  #F-tests (& thus  t-tests)
  prop=ifelse(family=="f" & p<.05, 1 - pf(qf(1-pc,df1=df1, df2=df2),df1=df1, df2=df2, ncp=ncp33),NA)
  #Chi2 (& thus Normal)
  prop=ifelse(family=="c" & p<.05,1-pchisq(qchisq(1-pc,df=df1),  df=df1, ncp=ncp33),prop)
  #output it
  prop
}

#Function 5 Stouffer test for a vector of pp-values
stouffer <- function(pp){
  sum(qnorm(pp),na.rm=TRUE)/sqrt(sum(!is.na(pp)))
}



#4.1 Function powerfit(power_est) - Returns the Stouffer Z of observing at least as right skewed a p-curve if  power=power_est
#if Z=0, power_est is the best fit (p=50%). 
#if Z<0 the best fit is <power_est, 
#if Z>0 the best fit is >power_est
powerfit <- function(power_est){
  #4.1.1 Get the implied non-centrality parameters (ncps) that give power_est to each test submitted to p-curve
  ncp_est=mapply(getncp,df1=df1,df2=df2,power=power_est,family=family)
  #4.1.2 Compute implied pp-values from those ncps_est,  
  pp_est=ifelse(family=="f" & p<.05,(pf(value,df1=df1,df2=df2,ncp=ncp_est)-(1-power_est))/power_est,NA)
  pp_est=ifelse(family=="c" & p<.05,(pchisq(value,df=df1,ncp=ncp_est)-(1-power_est))/power_est,pp_est)
  pp_est=pbound(pp_est)
  #4.1.3 Aggregate pp-values for null that power=power_est via Stouffer
  return(stouffer(pp_est))   #This is a z score, so powerfit is expressed as the resulting Z score.
}

#4.3 Confidence interval for power estimate
#4.3.1 Function get.power_pct(pct) 
get.power_pct <- function(pct){   
  #Function that finds power that gives p-value=pct for the Stouffer test 
  #for example, get.power_pct(.5) returns the level of power that leads to p=.5  for the stouffer test.
  #half the time we would see p-curves more right skewed than the one we see, and half the time
  #less right-skewed, if the true power were that get.power_pct(.5). So it is the median estimate of power
  #similarliy, get.power_pct(.1) gives the 10th percentile estimate of power...
  #Obtain the normalized equivalent of pct, e.g., for 5% it is -1.64, for 95% it is 1.64
  z=qnorm(pct)  #convert to z because powerfit() outputs a z-score. 
  #Quantify gap between computed p-value and desired pct
  error = function(power_est, z)  powerfit(power_est) - z
  #Find the value of power that makes that gap zero, (root)
  return(uniroot(error, c(.0501, .99),z)$root)
}

#3.5.1 Function that processes p-values and bounds when >.999 or <.0001
cleanp <- function(p){
  p.clean=round(p,4)           #Round it
  p.clean=substr(p.clean,2,6)  #Drop the 0
  p.clean=paste0("= ",p.clean)
  if (p < .0001) p.clean= " < .0001"
  if (p > .9999) p.clean= " > .9999"
  return(p.clean)
} 

powerfit <- function(power_est){
  #4.1.1 Get the implied non-centrality parameters (ncps) that give power_est to each test submitted to p-curve
  ncp_est=mapply(getncp,df1=df1,df2=df2,power=power_est,family=family)
  #4.1.2 Compute implied pp-values from those ncps_est,  
  pp_est=ifelse(family=="f" & p<.05,(pf(value,df1=df1,df2=df2,ncp=ncp_est)-(1-power_est))/power_est,NA)
  pp_est=ifelse(family=="c" & p<.05,(pchisq(value,df=df1,ncp=ncp_est)-(1-power_est))/power_est,pp_est)
  pp_est=pbound(pp_est)
  #4.1.3 Aggregate pp-values for null that power=power_est via Stouffer
  return(stouffer(pp_est))   #This is a z score, so powerfit is expressed as the resulting Z score.
}
