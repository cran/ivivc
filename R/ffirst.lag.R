#Step2-1-2-1:Fitting with 1-Compartment PK Model:  1-Compartment menu -->noniv route menu-->Extravascular, SD, & 1-Ord Abs w Tlag 
ffirst.lag<- function(InVVRefindex,
                      Dose=NULL, 
                      ka=NULL,
                      Vm=NULL,Km=NULL, # MMe=TRUE
                      Vd=NULL,
                      kel=NULL,        # MMe=FALSE
                      Tlag=TRUE,
                      MMe=FALSE) 
{
   #options(warn=-1)  此功能是將 
   ## Input dose and Tlag and initial value for ka, kel and Vd
   if (is.null(Dose)) {
     cat("Enter Dose value\n")
     Dose <- scan(nlines=1,quiet=TRUE)
   } 
   else {
     cat("Dose = ",Dose,"\n")
   }
   if ( Tlag ){
       cat("\nEnter lag time value\n")
       Tlag<-scan(nlines=1,quiet=TRUE)
       cat("\n")
   } 
     
   if (is.null(ka) || is.null(kel) || is.null(Vd) ) {
        par<-data.frame(Parameter=c("ka","kel","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)
         }
 
   cat("\n")
   
     if (Tlag){
      ## User-supplied function w/o MM elimination w lag time
      defun<- function(time, y, parms) { 
      if(time<=Tlag) {
       dy1dt<-0
       dy2dt<-0
      }
      else {
       dy1dt <- -parms["ka"]*y[1]
       dy2dt <-  parms["ka"]*y[1]/parms["Vd"]-parms["kel"]*y[2]
      }
      list(c(dy1dt,dy2dt)) 
      } 
    
      modfun <- function(time,ka,kel,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,kel=kel,Vd=Vd),
                   rtol=1e-5,atol=1e-8) 
      out[-1,3] 
      }
     }
     else{
      ## User-supplied function w/o MM elimination w/o lag time
      defun <- function(time, y, parms) { 
      dy1dt <- -parms["ka"] * y[1]
      dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["kel"] * y[2]
      list(c(dy1dt,dy2dt)) 
      } 
    
      modfun <- function(time,ka,kel,Vd) { 
      out <- lsoda(c(Dose, 0),c(0,time),defun,parms=c(ka=ka,kel=kel,Vd=Vd),
                   rtol=1e-6,atol=1e-6) 
      out[-1,3] 
      }
     }  

   ## Select weighting schemes
   file.menu <- c("equal weight", 
                  "1/Cp",
                  "1/Cp^2")           
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")

   with(entertitle(),{

   ka<-NULL
   ke<-NULL
   Vd<-NULL
   Vm<-NULL
   Km<-NULL
   sub<-NULL
   for( i in 1:length(unique(InVVRefindex$subj)))  {
      cat("\n\n               << subject",i,">>\n\n" )
      sub[i]<-i  
      objfun <- function(par) {
      out <- modfun(InVVRefindex$time[InVVRefindex$subj==i], par[1], par[2], par[3])
      gift <- which( InVVRefindex$conc[InVVRefindex$subj==i] != 0 )
      switch(pick,
            sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2),
            sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2/InVVRefindex$conc[gift]),
            sum(((InVVRefindex$conc[InVVRefindex$subj==i][gift] - out[gift])/InVVRefindex$conc[gift])^2))
      }
#### The value of parameter obtained from genetic algorithm 
###            gen<-genoud(objfun,nvars=3,max=FALSE,pop.size=20,max.generations=15,
###                wait.generations=10,starting.values=c(par[1,2],par[2,2],par[3,2]),
###                BFGS=FALSE,print.level=0,boundary.enforcement=2,
###                Domains=matrix(c(0.01,0.01,1,10,1,100),3,2),
###                MemoryMatrix=TRUE)

     
## No MM elimination
###         namegen<-c("ka","kel","Vd")
###         outgen<-c(gen$par[1],gen$par[2],gen$par[3])
### 
###       
###       F<-objfun(gen$par) 

## fitted by Nelder-Mead Simplex algorithm
        opt<-optim(c(par[1,2],par[2,2],par[3,2]),objfun, method="Nelder-Mead") 
        nameopt<-c("ka","kel","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3])
        ka[i]<- opt$par[1]
        ke[i]<- opt$par[2]
        Vd[i]<- opt$par[3]

      cat("\n<< The value of parameter fitted by Nelder-Mead Simplex algorithm >>\n\n")
      print(data.frame(Parameter=nameopt,Value=outopt))
      
##Residuals sum-of-squares and parameter values fitted by nls 
      cat("\n<< Residual sum-of-squares and parameter values fitted by nls >>\n\n")
## No MM elimination
        fm <-nls(conc ~ modfun(time, ka, kel, Vd), data=InVVRefindex,subset=subj==i,
             start=list(ka=opt$par[1],kel=opt$par[2],Vd=opt$par[3]),trace=TRUE,
             nls.control(tol=1))
        cat("\n")
        coef<-data.frame(coef(fm)["kel"])
        plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)

 }        
          cat("<< Summary >>\n")
          keindex<-data.frame(subj=sub,ka=ka, kel=ke,Vd=Vd)
          show(keindex)
          kename<-"ivivc_pk_values.RData"                  ### however this will overwrite previously saved data. -YJ
          saveRDS(keindex,kename)
}) 
cat("\n\n")
cat("*****************************************************************\n")
cat("*                        Now, Go to Step3                       *\n")
cat("*****************************************************************\n\n")
readline(" Press Enter to continue...")
  InVVTestdata(keindex)     
} 