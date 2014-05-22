### Step5:Calculate an IVIVC Model: Model Dependent or Independent Method  --> Model Dependent Method -->Wagner-Nelson method
### library(reshape)
### library(sciplot)

WagNel<-function(InVVTestindex,
                 InVVRefindex, 
                 keindex,
                 separateWindows=TRUE){
options(width=100)
### require(reshape)
#calculate AUC(0~t) and AUC(0~INF)
###Step1
zz <- file("ivivc_outputs.txt", open="wt")
sink(zz, split=TRUE)
description_version()
   #split dataframe into sub-dataframe
   W.data<-data.frame(pH=InVVTestindex$pH,formula.=InVVTestindex$formula.,subj=InVVTestindex$subj, 
                      time=InVVTestindex$time,conc.obs=InVVTestindex$conc.obs, FRD=InVVTestindex$FRD)
   W.data
   W.split<-split(W.data, list(W.data$pH ,W.data$formula., W.data$subj) )

cat("Enter the dose of MR formulations:\n")
Dose <- scan(nlines=1,quiet=TRUE)
cat("\n MR Dose =",Dose,"\n\n") 
cat(" Load PK parameter data file automatically as follows.\n\n")
keindex<-readRDS("ivivc_pk_values.RData")                ### use 'keindex', not 'kename'; here we will load PK parameters by default. -YJ 
### edit(keindex)
show(keindex);cat("\n\n")
###
### plot "In vitro Dissolution : Fraction of Released(%) vs. time" 
    plotting.vitro(InVVTestindex)       ### see if we can log these plots...  -YJ
###
##plot "In vivo plasma concentration (Observed): Plasma conc.vs. time"
    plotting.cp(InVVTestindex)

   AB<-NULL
   RD<-NULL
   pH<-NULL
   time<-NULL
   formu<-NULL
###
### stop computing if subj is not complete.
###
### cat("\n length of W.data =\n");show(length(unique(W.data$subj)));cat("\n\n")
### cat("\n length of keindex =\n");show(length(unique(keindex$subj)));cat("\n\n")
if(length(unique(W.data$subj))!=length(unique( keindex$subj))){
  cat("\n\n Subjects with PK parameters are not consistent\n with subjects in-vivo data.\n\n")
  readline(" ivivc for R will stop computing right now. Press Enter to stop.")
  stop(call. = FALSE)
}
###
      #calculate AUC, F(t) and Fab
      for (j in 1:length(W.split)){
           #if subject of W.split==subject of kepar, then use ke of kepar to calculate AUC(0~INF)
           for(x in 1: length(unique( keindex$subj))){
              if (W.split[[j]][["subj"]][1]==keindex$subj[[x]]){
                  ke<- keindex$ke[[x]]
                 }
               } 
             auc <- 0
             Ft<-0
             Fab<-0
             for(i in 2:length(W.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc[i]<-(W.split[[j]][["time"]][i]-W.split[[j]][["time"]][i-1])*(W.split[[j]][["conc.obs"]][i]+W.split[[j]][["conc.obs"]][i-1])* 0.5
             auc[i]<-auc[i]+auc[i-1]  
             #calculate F(t): dose of absorption
             Ft[i]<-W.split[[j]][["conc.obs"]][i]+ke*auc[i]
              }
              #calculate AUC (0~INF)
               auc.infinity<-W.split[[j]][["conc.obs"]][length(W.split[[j]][["conc.obs"]])]/ke
               aucINF<-auc[length(W.split[[j]][["conc.obs"]])]+auc.infinity         
                #calculate Fab(t): absorption fraction rate
                Fab<-0
                for(i in 2:length(W.split[[j]][["time"]])){
                Fab[i]<-(Ft[i]/(ke*aucINF))*100   
                  }
cat("****************************************************************************\n")
cat("* Next:                                                                    *\n")
cat("*      calculate AUCobs(0~t), AUCobs(0~inf), Fobs(t), FABobs               *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* AUCobs(0~t): area under the observed plasma concentration time curve     *\n")
cat("*              (time = 0 to t)                                             *\n")
cat("* AUCobs(0~inf): area under the observed plasma concentration time curve   *\n")
cat("*               (time = 0 to infinity)                                     *\n")
cat("* Fobs(t): observed absorption rate                                        *\n")
cat("* FABobs: observed cumulative absorption fraction(%)                       *\n")
cat("* FRD: cumulative released fraction(%)                                     *\n")
cat("****************************************************************************\n\n")

#Output
cat("<< Output >>\n")
output<-data.frame(W.split[[j]][["pH"]],W.split[[j]][["subj"]],W.split[[j]][["formula."]],W.split[[j]][["time"]],W.split[[j]][["conc.obs"]],auc, Ft, Fab,W.split[[j]][["FRD"]])
colnames(output)<-list("pH","subj","formula.","time","conc.obs","AUCobs(0~t)", "Fobs(t)", "FABobs","FRD")
show(output)
### write.csv(output,file="calc_ivivc_outputs.csv",append=TRUE,row.names=FALSE)
cat("\n<<AUCobs(0~inf) is computed with trapezoidal method>>\n\n")
show (aucINF)
cat("\n\n")
AB[[j]]<-c(Fab)
RD[[j]]<-c(W.split[[j]][["FRD"]])                                        
time[[j]]<-c(W.split[[j]][["time"]])
pH[[j]]<-c(W.split[[j]][["pH"]])
formu[[j]]<-c(as.character(W.split[[j]][["formula."]])) 
 }
readline(" Press Enter to continue...")               
#use "melt" function of reshape package to melt lists (Fab and FRD, respectively) from 2*3*3 dataframe
YY<-melt(AB)
XX<-melt(RD)
ZZ<-melt(time)
AA<-melt(pH)
BB<-melt(formu)
Y<-YY$value
X<-XX$value
vivo<-data.frame(pH=AA$value, formula.=BB$value, time=ZZ$value, FAB=YY$value, FRD=XX$value)

cat("****************************************************************************\n")
cat("* Step4: Develop an IVIVC Model: Model Dependent Method                    *\n")
cat("****************************************************************************\n")
cat("\n")
cat("<<Output:IVIVC model (linear regression)>>\n")

#calculate linear regression
show(lm(Y~X))
show(anova(wnlm<-lm( Y~X)))
print(summary(wnlm<-lm( Y~X)))
Intercept<-coef(lm(Y~X))[1]
Slope<-coef(lm(Y~X))[2]
summary(wnlm<-lm( Y~X))$r.sq
#plot in vitro-in vivo correlation plot

iviv<-data.frame(FAB=Y,FRD=X, formula.=BB$value)
I.split<-split(iviv, list(iviv$formula.))

par(mfrow=c(1,1), ask=TRUE)
### windows(record = TRUE )     ### NOT working for linux/unix; switch to 'dev.new()'
dev.new()
###  
### to generate color automatically
###
   x<-NULL
   for(i in 1:length(I.split)){
    x[i]<-i
         }
z <- lm(FAB~FRD, data=iviv)
plot(vivo$FRD, vivo$FAB, group=vivo$formula., xlab="Fraction Dissolved (Fabs, %)",ylab="Fraction Absorbed (Fdis, %)",
     col=x, bty="l", las=1, font.lab=2,cex.axis=1,cex.main=1,lwd=2)
mtext("Plots of Fabs (%) vs. Fdis(%) (Levy plot)\n",side=3,cex=2)  #mtext:可將文字加在圖的四周,cex為字的大小   

R.sq<-formatC(summary(wnlm<-lm(Y~X))$r.sq,format="f",digits=4)          # based on NCAreglplot() in bear -YJ
text(15,80,paste("Y=",formatC(coef(lm( iviv$FAB~iviv$FRD))[1]),"+",
     formatC(coef(lm( iviv$FAB~iviv$FRD))[2]),"X"))
### text(25,80,paste("+",formatC(coef(lm( iviv$FAB~iviv$FRD))[2]),"X"))
text(12,75,bquote(paste(R^2," = ",.(R.sq))))                        ### works great!  -YJ
## text(15,75,paste("R-squared=",formatC(summary(wnlm<-lm(Y~X))$r.sq))) #catch R-squared value; original codes
abline(z)  # equivalent to abline(reg = z) or 
### abline(coef = coef(z))
LLegend<-paste("",InVVTestindex$formula.,sep="")
LLegend<-unique(LLegend)
temp <- legend(100,40,legend = LLegend, box.col="white",                ### add formulation legend here
               text.width = strwidth("1000"),
               lty = 0, xjust = 1, yjust = 1,col=x,pch=1,lwd=2)

#split dataframe into sub-dataframe
W.data<-data.frame(pH=InVVTestindex$pH,formula.=InVVTestindex$formula.,subj=InVVTestindex$subj, time=InVVTestindex$time, conc.obs=InVVTestindex$conc.obs, FRD=InVVTestindex$FRD)
W.data
W.split<-split(W.data, list(W.data$pH ,W.data$formula., W.data$subj) )

   #calculate predicted Fab
     pH<-0
     formula.<-0
     PredCmax<-0
     ObsCmax<-0
     PEC<-0
     PEA<-0
     PredCp<-NULL
     formu<-NULL
     time<-NULL
      for (j in 1:length(W.split)){
           auc<-0
           PFab<-0
           PCp<-0
           PCmax<-0
           Cmax<-0
           PECmax <-0
           PEAUC<-0
         for(x in 1: length(unique( keindex$subj))){
              if (W.split[[j]][["subj"]][1]==keindex$subj[[x]]){
                  ke<- keindex$ke[[x]]
                 }
              if (W.split[[j]][["subj"]][1]==keindex$subj[[x]]){
                  Vd<- keindex$Vd[[x]]
               }
              }
          for(i in 2:length(W.split[[j]][["FRD"]])){
             PFab[i]<-(W.split[[j]][["FRD"]][i])*Slope+ Intercept
             }

            for(i in 2:length(W.split[[j]][["time"]])){
              #calculate predicted concentration
              PCp[i]<-((PCp[i-1]*(2-((W.split[[j]][["time"]][i]-W.split[[j]][["time"]][i-1])*ke))+(2*(PFab[i]-PFab[i-1])*1/100*Dose/Vd))/(2+(ke*(W.split[[j]][["time"]][i]-W.split[[j]][["time"]][i-1]))))
              #pick up predicted Cmax and observed Cmax
              PCmax<-max(PCp, na.rm = FALSE)
              Cmax<-max(W.split[[j]][["conc.obs"]], na.rm = FALSE)
              #calculate absolute prediction error of Cmax
              PECmax<-(abs(Cmax-PCmax))/Cmax
              }
              Pauc<-0

             for(i in 2:length(W.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc[i]<-(W.split[[j]][["time"]][i]-W.split[[j]][["time"]][i-1])*(W.split[[j]][["conc.obs"]][i]+W.split[[j]][["conc.obs"]][i-1])* 0.5
             auc[i]<-auc[i]+auc[i-1]
              }
              #calculate AUC (0~INF)
               auc.infinity<-W.split[[j]][["conc.obs"]][length(W.split[[j]][["conc.obs"]])]/ke
               aucINF<-auc[length(W.split[[j]][["conc.obs"]])]+auc.infinity

             for(i in 2:length(W.split[[j]][["time"]])){
               #calculate AUC and exclude AUC==NA (auc<-0)
               Pauc[i]<-(W.split[[j]][["time"]][i]-W.split[[j]][["time"]][i-1])*(PCp[i]+PCp[i-1])* 0.5
               Pauc[i]<-Pauc[i]+Pauc[i-1]
              }
                #calculate Predicted AUC (0~INF)
                Pauc.infinity<-PCp[length(W.split[[j]][["conc.obs"]])]/ke
                PaucINF<-Pauc[length(W.split[[j]][["conc.obs"]])]+Pauc.infinity
                  #calculate absolute prediction error of AUC
                 PEAUC<-(abs(aucINF-PaucINF))/aucINF

                pH[j]<-W.split[[j]][["pH"]][1]
                formula.[j]<-W.split[[j]][["formula."]][1]
                PredCmax[j]<-PCmax
                ObsCmax[j]<-Cmax
                PEC[j]<-PECmax
                PEA[j]<-PEAUC

cat("\n")
cat("****************************************************************************\n")
cat("* Next:                                                                    *\n")
cat("*      calculate AUCpred(0~t), AUCpred(0~inf), conc.pred, FABpred(%)       *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* AUCpred(0~t): area under the predicted plasma concentration time curve   *\n")
cat("*              (time = 0 to t)                                             *\n")
cat("* AUCpred(0~inf): area under the predicted plasma concentration time curve *\n")
cat("*                (time = 0 to infinity)                                    *\n")
cat("* conc.pred: predicted plasma concentration                                *\n")
cat("* FABpred: predicted cumulative absorption fraction(%)                     *\n")
cat("****************************************************************************\n\n")

#Output
     cat("<< Predicted Output >>\n")
     output<-data.frame(W.split[[j]][["pH"]],W.split[[j]][["subj"]],W.split[[j]][["formula."]],W.split[[j]][["time"]], PFab, PCp, Pauc )
     colnames(output)<-list("pH","subj","formula.","time","FABpred", "conc.pred", "AUCpred")
     show(output)
     ### write.csv(output,file="predicted_ivivc_outputs.csv",append=TRUE,row.names=FALSE)
     cat("\n<<AUCpred(0~inf) is computed with trapezoidal method>>\n\n")
     show(PaucINF)
     cat("\n\n")
     PredCp[[j]]<-c(PCp)                                        
     time[[j]]<-c(W.split[[j]][["time"]])
     formu[[j]]<-c(as.character(W.split[[j]][["formula."]])) 
   }
readline(" Press Enter to continue...")
#for plot predicted Cp
CC<-melt(PredCp)
DD<-melt(time)
EE<-melt(formu)
Predvivo<-data.frame(conc.pred=CC$value, formula.=EE$value, time=DD$value)
cat("****************************************************************************\n")
cat("* Step5: Evaluate an IVIVC model: Prediction Error                         *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* PE_Cmax: average absolute prediction error of Cmax (%)                   *\n")
cat("* PE_AUC: average absolute prediction error of AUC (%)                     *\n")
cat("****************************************************************************\n")
cat("\n")
cat("<<Summary: Validation report>>\n")
Y<-data.frame(pH=pH, formula.=formula., PECmax=PEC*100, PEAUC=PEA*100)
Y
XX<-(aggregate(Y, by=list(pH=Y$pH,formula.=Y$formula.), mean)  ) 
ZZ<-data.frame(pH=XX[1], formulation=XX[2],PE_Cmax=XX[5], PE_AUC=XX[6])
colnames(ZZ)<-list("pH", "Formulation", "  PE_Cmax", "PE_AUC")
show(ZZ)
write.csv(ZZ, file="validation_summary.csv",row.names=FALSE)
cat("\n")
cat("****************************************************************************\n")
cat("*<<Plots >>                                                                *\n")
cat("* - Fitting Plots                                                          *\n")
cat("* - Plots of Fabs (%) vs. Fdis(%) (Levy plot) (linear regression)          *\n")
cat("* - Fraction of in vitro Released(Fdis, %) vs. time                        *\n")
cat("* - Observed plasma concentration vs. time                                 *\n")
cat("* - Fraction of Absorption(Fabs, %) vs. time                               *\n")
cat("* - Predicted plasma concentration vs. time                                *\n")
cat("****************************************************************************\n")
cat("\n")

##plot "In vivo Absorption : Fraction of Absorption(%) vs. time"  
      y<-aggregate(vivo, by=list(pH=vivo$pH,formula.=vivo$formula.,time=vivo$time ), mean)
      yy<-data.frame(pH=vivo$pH,formula.=vivo$formula.,time=vivo$time,FAB=vivo$FAB)
      s<-data.frame(pH=y[1],formula.=y[2],time=y[3],meanFAB=y[7])
      S.data<-data.frame(pH=s$pH,formula.=s$formula.,time=s$time, meanFAB=s$FAB)
      S.split<-split(S.data, list(S.data$pH,S.data$formula.)) 
      F.split<-split(S.data, list(S.data$formula.)) 
      A.split<-split(S.split, list(S.data$pH) ) 

##plot "In vivo Absorption : Fraction of Absorption(%) vs. time"
 Predvivo<-data.frame(conc.pred=CC$value, formula.=EE$value, time=DD$value)
 P.split<-split(Predvivo, list(Predvivo$formula.)) 

### color the plots
x<-NULL
for(i in 1:length(P.split)){
   x[i]<-i
         }
    lineplot.CI(Predvivo$time, Predvivo$conc.pred, group = Predvivo$formula., cex = 1, lty=1,  ### set 'lty=1' for better looking. -YJ
    xlab = "Time", ylab = "Drug plasma conc.",cex.lab = 1, x.leg = 12, col=x,bty="l", 
    font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",err.width=0.05,lwd=2
     )
    axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
    axis(1,at=0:100,tcl=-.2, labels=FALSE) 
    mtext("Predicted Drug Plasma Conc.\n",side=3,cex=2)  ### must be placed after plot()

 for(i in 1:length(A.split)){
    lineplot.CI(yy$time, yy$FAB, group = yy$formula., cex = 1, lty=1,    ### set 'lty=1' for better looking. -YJ
       xlab = "Time", ylab = "Mean Predicted Fraction Absorption (%)",cex.lab = 1,x.leg = 15,y.leg=20,col=c(1:10),bty="l",  ### max. 10 different formulations are allowed.
       font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",err.width=0.05,lwd=2)
       axis(1,at=c(0,2,4,6,8,10,12,14,16,18,20,24,26,28,30,32,34,36,38,40,48,72,96,120),las=0)
       axis(1,at=0:120,tcl=-.2, labels=FALSE)
       mtext(paste(c("In-Vivo Absorption, pH=", A.split[[i]][[i]]$pH[1],"\n"),collapse=" "),side=3,cex=2)  #must be placed after plot()
    
      ###    main<-paste(c("In Vivo Absorption, pH=", A.split[[i]][[i]]$pH[1]),collapse=" ")
      ###    plot(0,xlim=range(y$time), ylim=c(0,max(y[7])),legend=FALSE,main=main,
      ###    xlab="Time", ylab="Mean Predicted Fraction of Absorption (%)",pch=15,col="white",bty="l",las=1,
      ###    font.lab=2,cex.lab=1,cex.axis=1,cex.main=2,lwd=2)
      ### for(j in seq_along(F.split)){ 
      ###    # plot points
      ###    points(A.split[[i]][[j]]$time,A.split[[i]][[j]]$meanFAB,type="punkte",main=main,pch=(14+j),col=j,lwd=2)
      ###    #plot line
      ###    lines(A.split[[i]][[j]]$time, A.split[[i]][[j]]$meanFAB,lty=1,col=j,lwd=2) 
      ###    ###
      ###  } 
      ###   temp <- legend(15,20, legend = c(unique(as.character(Predvivo$formula.))),
      ###                  col=c(1:10),pch=c(15:24),text.width = strwidth("1000"),lty=1,lwd=2,    ### inappropriate legend position may cause legend not showing. -YJ
      ###                  xjust=0,yjust=0,box.col="white")                                       ### max. 10 different formulations are allowed.
    }  
cat("\n")
filepath<-getwd()            
sink()
close(zz)
ivivc_all_plots(InVVTestindex, InVVRefindex, keindex, Dose, separateWindows=TRUE)
readline(" Press Enter to finish this run...")
graphics.off()
       cat("*****************************************************************************\n\n")
       cat("## Please note: output files: ivivc_outputs.txt and ivivc_plots.pdf            \n")
       cat("   have been created and placed at ",filepath,                               "\n\n")
       cat("*****************************************************************************\n\n")    
run()
}   
    

