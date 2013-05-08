#plot in vitro
plotting.vitro<-function (InVVTestindex,separateWindows=TRUE)   ### since this will genearte 1st plot. so set 'pdf_activate=FALSE' -YJ
{
y<-aggregate(InVVTestindex, by=list(pH=InVVTestindex$pH,formula.=InVVTestindex$formula.,time=InVVTestindex$time ), mean)
yy<-data.frame(pH=InVVTestindex$pH,formula.=InVVTestindex$formula.,time=InVVTestindex$time,FRD=InVVTestindex$FRD)
s<-data.frame(pH=y[1],formula.=y[2],time=y[3],meanFRD=y[9])
S.data<-data.frame(pH=s$pH,formula.=s$formula.,time=s$time, meanFRD=s$FRD)
### S.data
S.split<-split(S.data, list(S.data$pH,S.data$formula.)) 
F.split<-split(S.data, list(S.data$formula.)) 
A.split<-split(S.split, list(S.data$pH) ) 

#F.split[1]-->抓出 formulation=L
#F.split[[1]][["formula."]][1] -->抓出 formulation=L的第一行 
#A.split[1]-->抓出pH=1.2 
#A.split[[1]][[1]]--> 抓出pH=1.2 & formulation=L 
#A.split[[1]][[1]]$pH-->  抓出pH=1.2 & formulation=L 中的pH值

for(i in 1:length(A.split)){

###
### switch to using lineplot.CI() here. -YJ
### 
   lineplot.CI(yy$time, yy$FRD, group = yy$formula., cex = 1, lty=1,    ### set 'lty=1' for better looking. -YJ
       xlab = "Time", ylab = "Mean Fraction of Released Amount (%)",cex.lab = 1,x.leg = 15,y.leg=20,col=c(1:10),bty="l",  ### max. 10 different formulations are allowed.
       font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",err.width=0.05,lwd=2)
       axis(1,at=c(0,2,4,6,8,10,12,14,16,18,20,24,26,28,30,32,34,36,38,40,48,72,96,120),las=0)
       axis(1,at=0:120,tcl=-.2, labels=FALSE)
       mtext(paste(c("In-Vitro Dissolution, pH=", A.split[[i]][[i]]$pH[1],"\n"),collapse=" "),side=3,cex=2)  #must be placed after plot()
         
      ###    main<-paste(c("In Vitro Dissolution, pH=", A.split[[i]][[i]]$pH[1],"\n"),collapse=" ")
      ###    plot(0,xlim=range(y$time), ylim=c(0,100), legend=FALSE, main=main,
      ###    xlab="Time", ylab="Mean Fraction of Released Amount (%)",pch=15,col="white",bty="l",las=1,
      ###    font.lab=2,cex.lab=1,cex.axis=1,cex.main=2,lwd=2)
      ###    
      ### for( j in seq_along(F.split)){ 
      ###    # plot points
      ###    points(A.split[[i]][[j]]$time,A.split[[i]][[j]]$meanFRD,type="punkte",pch=(14+j),col=j,bty="l",las=1,
      ###    font.lab=2,cex.lab=1,cex.axis=1,cex.main=1,lwd=2)
      ###    #plot legend
      ###    lines(A.split[[i]][[j]]$time, A.split[[i]][[j]]$meanFRD,lty=1,col=j,lwd=2)
      ### 
      ###      }
      ###   temp <- legend(15,20, legend = c(unique(as.character(InVVTestindex$formula.))),
      ###                  col=c(1:10),pch=c(15:24),text.width = strwidth("1000"),lty=1,lwd=2,    ### inappropriate legend position may cause legend not showing. -YJ
      ###                  xjust=0,yjust=0,box.col="white")                                       ### max. 10 different formulations are allowed.
     }
} 


