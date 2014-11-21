#Step2-1-1-1:Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route
fbolus1 <- function(InVVRefindex,
                    Dose=NULL,
                    Vd=NULL,
                    kel=NULL)
{
   #options(warn=-1)
   modfun<-NULL
   ## Input dose and initial value for kel and Vd
   if (is.null(Dose)) {
   cat("Enter Dose value\n")
   Dose <- scan(nlines=1,quiet=TRUE)
   } 
   else {
     cat("Dose = ",Dose,"\n")
   }
   
   ## No MM elimination
      if (is.null(kel) || is.null(Vd)) {
        par<-data.frame(Parameter=c("kel","Vd"),Initial=c(0.1,10))
        par<-edit(par)
        show(par)
        ### par<-check(par)   ### check() causes error here . YJ (10/25/2013)

      }

      cat("\n")
      defun <- function(time, y, parms) { 
      dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
      } 
    
      modfun <<- function(time,kel, Vd) {  
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),rtol=1e-08,atol=1e-08) 
      out[-1,2] 
      }

### Select weighting schemes
   file.menu <- c("equal weight", 
                  "1/Cp",
                  "1/Cp^2")           
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")
   
   with(entertitle(),{  
   kel<-NULL
   Vd<-NULL
   sub<-NULL
   for( i in 1:length(unique(InVVRefindex$subj)))  {
     cat("\n\n               << subject",i,">>\n\n" ) 
     sub[i]<-i  
     objfun <- function(par) {
## No MM elimination
        out <- modfun(InVVRefindex$time[InVVRefindex$subj==i], par[1], par[2])
        gift <- which(InVVRefindex$conc[InVVRefindex$subj==i] != 0)
        switch(pick,
             sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2),
             sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2/InVVRefindex$conc[gift]),
             sum(((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])/InVVRefindex$conc[gift])^2))
        }

##fitted by Nelder-Mead Simplex algorithm      
          opt<-optimx(c(par[1,2],par[2,2]),objfun,method="Nelder-Mead")  
          nameopt<-c("kel","Vd")
          outopt<-c(opt$p1,opt$p2)
          
          if(opt$p1<0) opt$p1<-0.0001
          if(opt$p2<0) opt$p2<-0.0001
          
          conc<-InVVRefindex$conc[InVVRefindex$subj==i]
          time<-InVVRefindex$time[InVVRefindex$subj==i]
          
          if(pick==1) weights<- ifelse(conc==0.,1,1/conc^0)  ### equal weight
          if(pick==2) weights<- ifelse(conc==0.,1,1/conc^1)  ### 1/Cp
          if(pick==3) weights<- ifelse(conc==0.,1,1/conc^2)  ### 1/Cp^2

       cat("\n<< The value of parameter fitted by Nelder-Mead Simplex algorithm >>\n\n")
       print(data.frame(Parameter=nameopt,Value=outopt))
       
       ##Residuals sum-of-squares and parameter values fitted by nls
       cat("\n<< Residual sum-of-squares and parameter values fitted by nlsLM >>\n\n")
       ## No MM elimination

       fm<-nlsLM(conc ~ modfun(time, kel, Vd), data=subset(InVVRefindex,subj==i),
                 start=list(kel=opt$p1,Vd=opt$p2),weights=weights,
                 control=nls.lm.control(maxiter=500,maxfev=5000),lower=c(1e-06,1e-06))
       cat("\n")
       kel[i]<-data.frame(coef(fm)["kel"])[1,1]  ### extract kel value from fm
       Vd[i] <-data.frame(coef(fm)["Vd"])[1,1]   ### extract Vd value from fm
       coef  <-data.frame(coef(fm)["kel"])
       plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)
   }
       cat("<< Summary >>\n")
       keindex<-data.frame(subj=sub,kel=kel,Vd=Vd)
       show(keindex)
       kename<-"ivivc_pk_values.RData"                  ### however this will overwrite previously saved data. -YJ
       saveRDS(keindex,kename)   

   })
cat("\n\n")
cat("*****************************************************************\n")
cat("*                        Now, Go to Step3                       *\n")
cat("*****************************************************************\n\n")
  InVVTestdata()
}
