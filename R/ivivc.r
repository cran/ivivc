#IV data for PKfit
library(reshape)
library(sciplot)
ivivc<-function()
{
###Step1
cat("****************************************************************************\n")
cat("* Step1: Input/Edit In Vivo Absorption Data: IV, Oral solution or IR drug  *\n")
cat("*       ->Input:one subject with IV data                                   *\n")
cat("****************************************************************************\n")
cat("\n\n")
cat("Dose=200 mg\n")
Dose<-200
InVVRefindex<-data.frame(subject=c(1),time=c(0,1,2,3,4,5,6,7,8),
                    concentration=c(5.71,5.24,4.81,4.41,4.05,3.71,3.40,3.12,2.87))
show(InVVRefindex)
###Step2
cat("****************************************************************************\n")
cat("* Step2: Develop an IVIVC Model: Fitting IV, Oral solution or IR drug      *\n")
cat("*       ->Fitting data with one compartment model(iv bolus)                *\n")
cat("****************************************************************************\n")
cat("\n\n")
cat("<<Output:Fitting Result>>\n")
cat("\n")       

defun<- function(time, y, parms) {
      dCpdt <- -parms["kel"] * y[1]
      list(dCpdt)
}

modfun <- function(time,kel, Vd) {
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   rtol=1e-3,atol=1e-5)
      out[-1,2]
}

objfun <- function(par) {
        out <- modfun(InVVRefindex$time, par[1], par[2])
        gift <- which(InVVRefindex$concentration!= 0 )
        sum((InVVRefindex$concentration[gift]-out[gift])^2)
}

gen<-genoud(objfun,nvars=2,max=FALSE,pop.size=30,
            max.generations=20,wait.generations=10,
            starting.value=c(0.13,20),
            BFGS=FALSE,print.level=0,boundary.enforcement=2,
            Domains=matrix(c(0.01,0.01,100,100),2,2),
            MemoryMatrix=TRUE)
namegen<-c("kel","Vd")
outgen<-c(gen$par[1],gen$par[2])
opt<-optim(c(gen$par[1],gen$par[2]),objfun,method="Nelder-Mead")
nameopt<-c("kel","Vd")
outopt<-c(opt$par[1],opt$par[2])
ke<-opt$par[1]
Vd<-opt$par[2]
fm<-nls(concentration ~ modfun(time, kel, Vd), data=InVVRefindex,
        start=list(kel=opt$par[1],Vd=opt$par[2]),trace=TRUE,
        nls.control(tol=1))
coef<-data.frame(coef(fm)["kel"])
x<-InVVRefindex$time
y<-InVVRefindex$concentration
cal<-predict(fm,list(time=x))
wei<-ifelse(y==0.0, 0, y-cal)
add<-function(time,concentration){
     auc<-0 ; aumc<-0
     for(i in 2:length(time)) {
     auc[i]<-1/2*(time[i]-time[i-1])*(concentration[i]+concentration[i-1])
     auc[i]<-auc[i]+auc[i-1]
     aumc[i]<-1/2*(time[i]-time[i-1])*(concentration[i]*time[i]+concentration[i-1]*time[i-1])
     aumc[i]<-aumc[i]+aumc[i-1]
     }
     return(list(auc=auc,aumc=aumc))
  }
add<-add(x,y)
AUC<-add$auc
AUMC<-add$aumc
output<-data.frame(x,y,cal,wei,AUC,AUMC)
colnames(output)<-list("time","Observed","Calculated","Wtd Residuals","AUC","AUMC")

auc.infinity<-y[length(y)]/coef[1,1]
auc<-AUC[length(y)]+auc.infinity

aumc.infinity<-(x[length(x)]*y[length(y)])/coef[1,1]+x[length(x)]/((coef[1,1])^2)
aumc<-AUMC[length(y)]+aumc.infinity

get(getOption("device"))()

par(mfrow=c(2,2))

plot(y~x,data=InVVRefindex,type='p',main="subject 1",
     xlab="Time (hr)", ylab="Plasma conc. (mg/L)",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
lines(x,predict(fm,list(time=x)),type="l",lty=1,
      col="firebrick3",lwd="2")
mtext("Linear",side=3,cex=0.88)

plot(x,y,log="y",type='p',main="subject 1",
     xlab="Time (hr)", ylab="Plasma conc. (mg/L)",pch=15,col="black",bty="l",
     font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
lines(x,predict(fm,list(time=x)),type="l",lty=1,
      col="firebrick3",lwd="2")
mtext("Semi-log",side=3,cex=0.88)

plot(x,wei,pch=15,col="blue",bty="l",xlab="Time (hr)",
     ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)

plot(cal,wei,pch=15,col="blue",bty="l",xlab="Calc Plasma conc.(i)",
     ylab="Weighted Residual",main="Residual Plots",cex.lab=1,
     cex.axis=1,cex.main=1,font.lab=2)
abline(h=0,lwd=2,col="black",lty=2)


output
auc
aumc
AIC(fm)
logLik(fm)
print(summary(fm))
cat("\n\n")
cat("<<Summary: Fitting result>>\n") 
keindex<-data.frame(kel=opt$par[1],Vd=opt$par[2])
show(keindex)
###Step3
cat("\n\n")  
cat("****************************************************************************\n")
cat("* Step3: Input/Edit In Vitro Dissoution Data and                           *\n")
cat("*       In Vivo absorption Data: ER drug with Different Release Rates      *\n")   
cat("*       -> Input:one subject with one formulation at pH 7.4                *\n")
cat("*--------------------------------------------------------------------------*\n") 
cat("* FRD: cumulative released fraction(%)                                     *\n")
cat("****************************************************************************\n")
cat("\n\n")
cat("Dose=200 mg\n")
Dose <-200  
InVVTestindex<-data.frame(pH=c(7.4,7.4,7.4,7.4,7.4,7.4,7.4,7.4,7.4), formula.=c(1,1,1,1,1,1,1,1,1),
                          subject=c(1,1,1,1,1,1,1,1,1),  time=c(0,1,2,3,4,5,6,7,8),
                          conc.obs=c(0,0.891,1.997,2.616,3.411,3.33,3.868,3.371,3.433),
                          FRD=c(0, 18.7, 45.3, 62.4, 77.3, 84.9, 91.5, 92.7, 95.8))
show(InVVTestindex)    

cat("\n\n")
Ft<-0
auc<-0 
for(i in 2:length(InVVTestindex$time)){
 #calculate AUC and exclude AUC==NA (auc<-0)
 auc[i]<-(InVVTestindex$time[i]-InVVTestindex$time[i-1])*(InVVTestindex$conc.obs[i]+InVVTestindex$conc.obs[i-1])* 0.5
 auc[i]<-auc[i]+auc[i-1]
   #calculate F(t): dose of absorption
    Ft[i]<-InVVTestindex$conc.obs[i]+ke*auc[i]
    }
  
     #calculate AUC (0~INF)   AUC[length(y)]+auc.infinity
     auc.infinity<-InVVTestindex$conc.obs[length(InVVTestindex$conc.obs)]/ke
     aucINF<-auc[length(InVVTestindex$conc.obs)]+auc.infinity
       #calculate Fab(t): absorption fraction rate
       Fab<-0
       for(i in 2:length(InVVTestindex$time)){
       Fab[i]<-(Ft[i]/(ke*aucINF))*100
       }

cat("****************************************************************************\n")
cat("* Next:                                                                    *\n")
cat("*      calculate AUCobs(0~t), AUCobs(0~inf), Fobs(t), FABobs(%)            *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* AUCobs(0~t): area under the observed plasma concentration time curve     *\n")
cat("*              (time = 0 to t)                                             *\n")
cat("* AUCobs(0~inf): area under the observed plasma concentration time curve   *\n")
cat("*               (time = 0 to infinity)                                     *\n")
cat("* Fobs(t): observed absorption rate                                        *\n")
cat("* FABobs: observed cumulative absorption fraction(%)                       *\n")
cat("* FRD: cumulative released fraction(%)                                     *\n")
cat("****************************************************************************\n")
cat("\n\n")
       #Output
       output1<-data.frame(InVVTestindex$pH, InVVTestindex$subject,InVVTestindex$formula., InVVTestindex$time, InVVTestindex$conc.obs,auc, Ft, Fab, InVVTestindex$FRD)
       colnames(output1)<-list("pH","subject","formula.","time","conc.obs","AUCobs(0~t)", "Fobs(t)", "FABobs","FRD")
       cat("<< Output >>\n")
       show(output1)
       cat("\n<<AUCobs(0~inf) is computed with trapezoidal method>>\n\n")
       show (aucINF)
       cat("\n\n")
       AB<-c(Fab)
       RD<-c(InVVTestindex$FRD)
       time<-c(InVVTestindex$time)
       pH<-c(InVVTestindex$pH)
       formu<-c(as.character(InVVTestindex$formula.))


#use "melt" function of reshape package to melt lists (Fab and FRD, respectively) from 2*3*3 dataframe
YY<-melt(AB)
XX<-melt(RD)
ZZ<-melt(time)
AA<-melt(pH)
BB<-melt(formu)
Y<-YY$value
X<-XX$value
vivo<-data.frame(pH=AA$value, formula.=BB$value, time=ZZ$value, FAB=YY$value, FRD=XX$value)

#calculate linear regression
cat("****************************************************************************\n")
cat("* Step4: Develop an IVIVC Model: Model Dependent Method                    *\n")
cat("****************************************************************************\n")
cat("\n")
cat("<<Output:IVIVC model (linear regression)>>\n")
show(lm(Y~X))
show(anova(wnlm<-lm( Y~X)))
print(summary(wnlm<-lm( Y~X)))
Intercept<-coef(lm(Y~X))[1]
Slope<-coef(lm(Y~X))[2]
summary(wnlm<-lm( Y~X))$r.sq
cat("\n\n")
cat("<<Summary: IVIVC model>>\n") 
cat("\nY=", coef(lm(Y~X))[1],"+",coef(lm(Y~X))[2],"X\n\n")

#plot in vitro-in vivo correlation plot
windows(record = TRUE)

iviv<-data.frame(FAB=Y,FRD=X, formula.=BB$value)
z <- lm(FAB~FRD, data=iviv)
plot(vivo$FRD, vivo$FAB, group=vivo$formula., xlab="Fraction of Released (%)",ylab="Fraction of Absorption (%)",
     bty="l", las=1, font.lab=2,cex.axis=1,cex.main=1,col="firebrick3",lwd="2")
mtext("In-vitro-in-vivo-correlation Model",side=3,cex=2)  #mtext:可將文字加在圖的四周,cex為字的大小
#text:在圖形上展現R-squared and formula
text(10,80,paste("R-squared=",formatC(summary(wnlm<-lm( Y~X))$r.sq)) ) #catch R-squared value
text(10,75,paste("Y=",formatC(coef(lm( iviv$FAB~iviv$FRD))[1])) )
text(25,75,paste("+",formatC(coef(lm( iviv$FAB~iviv$FRD))[2]),"X") )
abline(z)  # equivalent to abline(reg = z) or
abline(coef = coef(z))

##predict Fraction of absorption, plasma conc.
PFab<-0
for(i in 2:length(InVVTestindex$FRD)){
 PFab[i]<-(InVVTestindex$FRD[i])*Slope+ Intercept
 }
   PCp<-0
   for(i in 2:length (InVVTestindex$time)){
   #calculate predicted concentration
   PCp[i]<-((PCp[i-1]*(2-((InVVTestindex$time[i]-InVVTestindex$time[i-1])*ke))+(2*(PFab[i]-PFab[i-1])*1/100*Dose/Vd))/(2+(ke*(InVVTestindex$time[i]-InVVTestindex$time[i-1]))))
   #pick up predicted Cmax and observed Cmax
   PCmax<-max(PCp, na.rm = FALSE)
    Cmax<-max(InVVTestindex$conc.obs, na.rm = FALSE)
    #calculate absolute prediction error of Cmax
       PECmax<-(abs(Cmax-PCmax))/Cmax
       }
        
        Pauc<-0
        for(i in 2:length(InVVTestindex$time)){
        #calculate AUC and exclude AUC==NA (auc<-0)
        Pauc[i]<-(InVVTestindex$time[i]-InVVTestindex$time[i-1])*(PCp[i]+PCp[i-1])* 0.5
        Pauc[i]<-Pauc[i]+Pauc[i-1]
        }
         #calculate Predicted AUC (0~INF)
         Pauc.infinity<-PCp[length(InVVTestindex$conc.obs)]/ke
         PaucINF<-Pauc[length(InVVTestindex$conc.obs)]+Pauc.infinity
           #calculate absolute prediction error of AUC
           PEAUC<-(abs(aucINF-PaucINF))/aucINF
           
cat("\n")
cat("****************************************************************************\n")
cat("* Next:                                                                    *\n")
cat("*      calculate AUCpred(0~t), AUCpred(0~inf), conc.pred, FABpred          *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* AUCpred(0~t): area under the predicted plasma concentration time curve   *\n")
cat("*              (time = 0 to t)                                             *\n")
cat("* AUCpred(0~inf): area under the predicted plasma concentration time curve *\n")
cat("*                (time = 0 to infinity)                                    *\n")
cat("* conc.pred: predicted plasma concentration                                *\n")
cat("* FABpred:predicted cumulative absorption fraction(%)                      *\n")
cat("****************************************************************************\n")
cat("\n")                
                #Output
                 output2<-data.frame(InVVTestindex$pH ,InVVTestindex$subject,InVVTestindex$formula.,InVVTestindex$time, PFab, PCp, Pauc )
                 colnames(output2)<-list("pH","subject","formula.","time","FABpred", "conc.pred", "AUCpred(0~t)")
                 cat("<< Predicted Output >>\n")
                 show(output2)
                 cat("\n<<AUCpred(0~inf) is computed with trapezoidal method>>\n\n")
                 show(PaucINF)
                 cat("\n\n")
                 PredCp<-c(PCp)
                 time<-c(InVVTestindex$time)
                 formu<-c(as.character(InVVTestindex$formula.))
                 CC<-melt(PredCp)
                 DD<-melt(time)
                 EE<-melt(formu)
                 Predvivo<-data.frame(conc.pred=CC$value, formula.=EE$value, time=DD$value)
cat("****************************************************************************\n")
cat("* Step5: Evaluate an IVIVC model: Prediction Error                         *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* PECmax: average absolute prediction error of Cmax (%)                    *\n")
cat("* PEAUC: average absolute prediction error of AUC (%)                      *\n")
cat("****************************************************************************\n")
cat("\n")
cat("<<Summary: Validation report>>\n")
Y<-data.frame(pH=7.4, formulation=1, PECmax=PECmax*100, PEAUC=PEAUC*100)
XX<-(aggregate(Y, by=list(pH=Y$pH,formula.=Y$formulation), mean)) 
ZZ<-data.frame(pH=XX[1], formula.=XX[2],PECmax=XX[5], PEAUC=XX[6])
show(ZZ)  
cat("\n")
cat("****************************************************************************\n")
cat("*<<Plots >>                                                                *\n")
cat("* Fitting Plots                                                            *\n")
cat("* In-vitro-in-vivo-correlation Model (linear regression)                   *\n")
cat("* Fraction of in vitro Released(%) vs. time                                *\n")
cat("* Observed plasma concentration vs. time                                   *\n")
cat("* Fraction of Absorption(%) vs. time                                       *\n")
cat("* Predicted plasma concentration vs. time                                  *\n")
cat("****************************************************************************\n")
cat("\n")
#plot in vitro

main<-paste(c("In Vitro Dissolution pH=7.4","formulation=1"),collapse=" ")
  # plot points
  plot(InVVTestindex$time,InVVTestindex$FRD,type="punkte",main=main,
  xlab="Time (hr)", ylab="Fraction of Released (%)",pch=15,bty="l",las=1,
  font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
   #plot line
    lines(InVVTestindex$time, InVVTestindex$FRD,col="firebrick3",lwd="2") 


#plot in vivo

main<-paste(c("In vivo Absorption pH=7.4","formulation=1"),collapse=" ")
         # plot points
         plot(InVVTestindex$time,Fab,type="punkte",main=main,
         xlab="Time (hr)", ylab="Fraction of Absorption (%)",pch=15,bty="l",las=1,
         font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         #plot line
         lines(InVVTestindex$time, Fab, col="firebrick3",lwd="2",) 

#plot plasma conc (predicted)

      lineplot.CI(Predvivo$time, Predvivo$conc.pred, group = Predvivo$formula., cex = 1,
            xlab = "Time (hr)", ylab = "Plasma conc. (mg/L)",cex.lab = 1, x.leg = 12,bty="l", 
            font.lab=2,cex.axis=1,cex.main=1,las=1, pch=15, col="firebrick3",lwd="2"
             )
           axis(1,at=0:50,tcl=-.5, labels=FALSE) 
           mtext("Predicted plasma concentration ",side=3,cex=2)  #要放在plot之後

#plot plasma conc (observed)

      lineplot.CI(InVVTestindex$time, InVVTestindex$conc.obs, group = InVVTestindex$formula., cex = 1, 
            xlab = "Time (hr)", ylab = "Plasma conc. (mg/L)",cex.lab = 1, x.leg = 12, bty="l", 
            font.lab=2,cex.axis=1,cex.main=1,las=1, pch=15, col="firebrick3",lwd="2"
             )
            axis(1,at=0:50,tcl=-.5, labels=FALSE) 
            mtext("Observed plasma concentration ",side=3,cex=2)  #要放在plot之後            

run()
}
