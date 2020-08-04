
#5.4.1  Define x-axis as p-values (.01, .02..)
x = c(.01,.02,.03,.04,.05)

#5.4.2 Plot the observed p-curve
par(mar=c(6,5.5,1.5,3)) 
#5.4.3 Does blue line cross over  68% in when p>.02?
#Does blue line go above 68%? If yes, move up graph so that legend is not interrupted by it
moveup=max(max(pcurve_plot_params()$blue[2:5])-66,0)  #moveup by difference between blue and 68
ylim=c(0,105+moveup)
legend.top=100+moveup

#5.4.4 Start the plot  
plot(x,pcurve_plot_params()$blue,   type='l', col='dodgerblue2',  main="",
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
points(x,pcurve_plot_params()$blue,type="p",pch=20,bg="dodgerblue2",col="dodgerblue2")
#5.4.10 Add value-labels
text(x+.00075,pcurve_plot_params()$blue+3.5,percent(round(pcurve_plot_params()$blue)/100),col='black', cex=.75)
#5.4.11 Add red and green lines
lines(x,pcurve_plot_params()$red,   type='l', col='firebrick2',    lwd=1.5, lty=3)
lines(x,pcurve_plot_params()$green, type='l', col='springgreen4',  lwd=1.5, lty=5)

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
