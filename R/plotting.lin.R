#Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route for "plot for linear"
plotting.lin <- function (InVVRefindex, fm, i, pick, coef, xaxis, yaxis,
                          separateWindows=TRUE)
{               
  #options(warn=-1)
  
  j<-1:length(InVVRefindex$time[InVVRefindex$subj==i])
  x<-InVVRefindex$time[InVVRefindex$subj==i]
  y<-InVVRefindex$conc[InVVRefindex$subj==i]
        
 #Calculated conc
  cal<-predict(fm,list(time=x))
    
 #Weighted residuals   
 if (!(pick %in% 1:3)) {
    stop("Selection is invalid.")
  }
  
 wei <- switch(pick,
               ifelse(y[j]==0.0, 0, y[j]-cal[j]),
               ifelse(y[j]==0.0, 0, sqrt(1/(y[j]))*(y[j]-cal[j])),
               ifelse(y[j]==0.0, 0, sqrt(1/((y[j])^2))*(y[j]-cal[j])))
  
 #calculate AUC and AUMC   
  add<-function(time,conc){
     auc<-0 ; aumc<-0
     for(i in 2:length(time)) {
     auc[i]<-1/2*(time[i]-time[i-1])*(conc[i]+conc[i-1])
     auc[i]<-auc[i]+auc[i-1]
     aumc[i]<-1/2*(time[i]-time[i-1])*(conc[i]*time[i]+conc[i-1]*time[i-1])
     aumc[i]<-aumc[i]+aumc[i-1]
     }
     return(list(auc=auc,aumc=aumc))
  }
  add<-add(x,y)
  AUC<-add$auc
  AUMC<-add$aumc
        
 #Output   
  cat("<< Output >>\n")  
  output<-data.frame(x,y,cal,wei,AUC,AUMC)
  colnames(output)<-list("Time","obs. conc","calc. conc.","Weigted Residuals","AUC","AUMC")
  show(output)  
  
 #AUC (0 to infinity)              
  cat("\n<< AUC (0 to infinity) computed by trapezoidal rule >>\n\n")
  auc.infinity<-y[length(y)]/coef[1,1]
  auc<-AUC[length(y)]+auc.infinity
  show(auc) 
  
 #AUMC (0 to infinity) 
  cat("\n<< AUMC (0 to infinity) computed by trapezoidal rule >>\n\n")
  aumc.infinity<-(x[length(x)]*y[length(y)])/coef[1,1]+x[length(x)]/((coef[1,1])^2)
  aumc<-AUMC[length(y)]+aumc.infinity
  show(aumc)   
  cat("\n")
         
  aicllsbc(fm)
  cat("\n\n")
    
 #Divide plot console into four parts
  ### windows(record = TRUE )     ### NOT working for linux/unix; switch to 'dev.new()'
  dev.new()
  par(mfrow=c(2,2))
  main<-paste(c("subject", i ,"plot"),collapse=" ")
     
 #Linear plot
  plot(y~x,data=InVVRefindex,type='p',main=main, 
       xlab=xaxis, ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  lines(x,predict(fm,list(time=x)),type="l",lty=1,
        col="firebrick3",lwd="2")
  mtext("Linear",side=3,cex=0.88)

 #Semi-log plot
  plot(x,y,log="y",type='p',main=main,
       xlab=xaxis, ylab=yaxis,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  lines(x,predict(fm,list(time=x)),type="l",lty=1,
        col="firebrick3",lwd="2")
  mtext("Semi-log",side=3,cex=0.88)    

 #Residual plot, time vs weighted residual
  plot(x,wei,pch=15,col="blue",bty="l",xlab=xaxis,
       ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
       cex.axis=1,cex.main=1,font.lab=2)
  abline(h=0,lwd=2,col="black",lty=2)
    
 #Residual plot, calculated concentration vs weigthed residual
  plot(cal,wei,pch=15,col="blue",bty="l",xlab="Calc Cp(i)",
       ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
       cex.axis=1,cex.main=1,font.lab=2)
  abline(h=0,lwd=2,col="black",lty=2)          
}    