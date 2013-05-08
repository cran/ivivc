### plot plasma conc.
### library(sciplot)
plotting.cp<-function (InVVTestindex,separateWindows=TRUE)
{
# calc SD
yy<-data.frame( formula.=InVVTestindex$formula., time=InVVTestindex$time,conc=InVVTestindex$conc.obs)
F.split<-split(yy, list(yy$formula.)) 

#auto-color
x<-NULL
for(i in 1:length(F.split)){
x[i]<-i
}
lineplot.CI(yy$time, yy$conc, group = yy$formula., cex = 1, lty=1,    ### set 'lty=1' for better looking. -YJ
            xlab = "Time", ylab = "Mean Drug Plasma Conc.",cex.lab = 1, x.leg = 12, col=x, bty="l", 
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",err.width=0.05,lwd=2
             )
            axis(1,at=c(0,2,4,6,8,10,12,14,16,18,20,24,26,28,30,32,34,36,38,40,48,72,96,120),las=0)
            axis(1,at=0:120,tcl=-.2, labels=FALSE)
            mtext("Observed Drug Plasma Conc.\n",side=3,cex=2)  #must be placed after plot()
}

 