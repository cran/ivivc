#plot in vitro
plotting.vitro<-function (InVVTestindex, separateWindows=TRUE  )
{
y<-aggregate(InVVTestindex, by=list(pH=InVVTestindex$pH,formula.=InVVTestindex$formula.,time=InVVTestindex$time ), mean)
s<-data.frame(pH=y[1],formula.=y[2],time=y[3],meanFRD=y[9])
S.data<-data.frame(pH=s$pH,formula.=s$formula.,time=s$time, meanFRD=s$FRD)
S.data
S.split<-split(S.data, list(S.data$pH,S.data$formula.)) 
F.split<-split(S.data, list(S.data$formula.)) 
A.split<-split(S.split, list(S.data$pH) ) 

#F.split[1]-->抓出 formulation=L
#F.split[[1]][["formula."]][1] -->抓出 formulation=L的第一行 
#A.split[1]-->抓出pH=1.2 
#A.split[[1]][[1]]--> 抓出pH=1.2 & formulation=L 
#A.split[[1]][[1]]$pH-->  抓出pH=1.2 & formulation=L 中的pH值 
par(mfrow=c(2,2))
 for(i in 1:length(A.split)){ 

      for( j in seq_along(F.split)){ 
         main<-paste(c("In Vitro Dissolution pH=", A.split[[i]][[i]]$pH[1], "formulation=",as.character(F.split[[j]][["formula."]][1]) ),collapse=" ")
         # plot points
         plot(A.split[[i]][[j]]$time,A.split[[i]][[j]]$meanFRD,type="punkte",main=main,
         xlab="Time", ylab="Fraction of Released (%)",pch=15,col=j,bty="l",las=1,
         font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         #plot line
         lines(A.split[[i]][[j]]$time, A.split[[i]][[j]]$meanFRD, col=j) 
   }
 } 
} 


