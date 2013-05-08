#Step2-1-1-1:Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route
fbolus1 <- function(InVVRefindex,
                    Dose=NULL,
                    Vm=NULL,Km=NULL, ## MMe=TRUE
                    Vd=NULL,
                    kel=NULL,        ## MMe=FALSE; no MMe for ivivc -- YJ
                    MMe=FALSE)
{
   #options(warn=-1)
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
        par<-data.frame(Parameter=c("kel","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)

      }

      cat("\n")
      defun <- function(time, y, parms) { 
      dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
      } 
    
      modfun <- function(time,kel, Vd) {  
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   rtol=1e-3,atol=1e-5) 
      out[-1,2] 
      }

### Select weighting schemes
   file.menu <- c("equal weight", 
                  "1/Cp",
                  "1/Cp^2")           
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")
   
   with(entertitle(),{  
   ke<-NULL
   Vd<-NULL
   Vm<-NULL
   Km<-NULL
   sub<-NULL
   for( i in 1:length(unique(InVVRefindex$subj)))  {
     cat("\n\n               << subject",i,">>\n\n" ) 
     sub[i]<-i  
     objfun <- function(par) {
## No MM elimination
        out <- modfun(InVVRefindex$time[InVVRefindex$subj==i], par[1], par[2])
       gift <- which( InVVRefindex$conc[InVVRefindex$subj==i] != 0 )
        switch(pick,
             sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2),
             sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2/InVVRefindex$conc[gift]),
             sum(((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])/InVVRefindex$conc[gift])^2))
        }
#The value of parameter obtained from genetic algorithm   
          gen<-genoud(objfun,nvars=2,max=FALSE,pop.size=30,
               max.generations=20,wait.generations=10,
               starting.values=c(par[1,2],par[2,2]),
               BFGS=FALSE,print.level=0,boundary.enforcement=2,
               Domains=matrix(c(0.01,0.01,100,100),2,2),
               MemoryMatrix=TRUE)  
## No MM elimination
          namegen<-c("kel","Vd")
          outgen<-c(gen$par[1],gen$par[2])
          F<-objfun(gen$par)
##fitted by Nelder-Mead Simplex algorithm      
          opt<-optim(c(gen$par[1],gen$par[2]),objfun,method="Nelder-Mead")  
          nameopt<-c("kel","Vd")
          outopt<-c(opt$par[1],opt$par[2])
          ke[i]<- opt$par[1]
          Vd[i]<- opt$par[2]

       cat("\n<< The value of parameter fitted by Nelder-Mead Simplex algorithm >>\n\n")
       print(data.frame(Parameter=nameopt,Value=outopt))
       
       ##Residuals sum-of-squares and parameter values fitted by nls
       cat("\n<< Residual sum-of-squares and parameter values fitted by nls >>\n\n")
       ## No MM elimination
       fm<-nls(conc ~ modfun(time, kel, Vd), data=InVVRefindex,
               start=list(kel=opt$par[1],Vd=opt$par[2]),trace=TRUE,subset=subj==i,
               nls.control(tol=1))
       cat("\n")
       coef<-data.frame(coef(fm)["kel"])
       plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)
   }
       cat("<< Summary >>\n")
       keindex<-data.frame(subj=sub,kel=ke,Vd=Vd)
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
