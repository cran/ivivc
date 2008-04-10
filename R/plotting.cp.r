#plot plasma conc.
library(sciplot)
plotting.cp<-function (InVVTestindex, separateWindows=TRUE  )
{
#���XSD
yy<-data.frame( formula.=InVVTestindex$formulation, time=InVVTestindex$time,conc=InVVTestindex$conc.obs)
F.split<-split(yy, list(yy$formula.)) 

if (separateWindows) {
       get(getOption("device"))()
          }

#���F�۰ʲ����C��
x<-NULL
for(i in 1:length(F.split)){
x[i]<-i
}
lineplot.CI(yy$time, yy$conc, group = yy$formula., cex = 1,
            xlab = "Time", ylab = "Plasma conc.",cex.lab = 1, x.leg = 12, col=x,bty="l", 
            font.lab=2,cex.axis=1,cex.main=1,las=1,
             )
            axis(1,at=0:50,tcl=-.5, labels=FALSE) 
            mtext("Observed plasma concentration",side=3,cex=2)  #�n��bplot����

}

 