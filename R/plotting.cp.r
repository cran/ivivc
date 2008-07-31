#plot plasma conc.
library(sciplot)
plotting.cp<-function (InVVTestindex, separateWindows=TRUE  )
{
#產出SD
yy<-data.frame( formula.=InVVTestindex$formula., time=InVVTestindex$time,conc=InVVTestindex$conc.obs)
F.split<-split(yy, list(yy$formula.)) 


#為了自動產生顏色
x<-NULL
for(i in 1:length(F.split)){
x[i]<-i
}
lineplot.CI(yy$time, yy$conc, group = yy$formula., cex = 1,
            xlab = "Time", ylab = "Plasma conc.",cex.lab = 1, x.leg = 12, col=x,bty="l", 
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",err.width=0.05
             )
            axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
            axis(1,at=0:100,tcl=-.2, labels=FALSE)
            mtext("Observed drug plasma concentration",side=3,cex=2)  #要放在plot之後

}

 