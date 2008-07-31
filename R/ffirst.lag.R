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
     cat("Dose from arguments is = ",Dose,"\n")
   }
   if ( Tlag ){
       cat("\nEnter lag time value\n")
       Tlag<-scan(nlines=1,quiet=TRUE)
       cat("\n")
   } 
     
   if (MMe){
      if (is.null(ka) || is.null(Vm) || is.null(Km) || is.null(Vd) ) {
        par<-data.frame(Parameter=c("ka","Vm","Km","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)
         }
   } 
   else{
      if (is.null(ka) || is.null(kel) || is.null(Vd) ) {
        par<-data.frame(Parameter=c("ka","kel","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)
         }
   }
   cat("\n")
   
   if ( MMe ) {
    if (Tlag){
     
      ## User-supplied function w Michaelis-Mention elimination & w lag time
      defun<- function(time, y, parms) { 
      if(time <= Tlag) {
        dy1dt<-0
        dy2dt<-0
      }
      else  {
        dy1dt <- -parms["ka"] * y[1]
        dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - (parms["Vm"]/parms["Vd"])*y[2]/(parms["Km"]/parms["Vd"]+y[2])
      }
      list(c(dy1dt,dy2dt)) 
      } 
    
      modfun1 <- function(time,ka,Vm,Km,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,Vm=Vm,Km=Km,Vd=Vd),
                   rtol=1e-8,atol=1e-8) 
      out[-1,3] 
      }
     }
     else{
       ## User-supplied function with MM elimination w/o lag time
      defun<- function(time, y, parms) { 
      dy1dt <- -parms["ka"] * y[1]
      dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - (parms["Vm"]/parms["Vd"])*y[2]/(parms["Km"]/parms["Vd"]+y[2])
      list(c(dy1dt,dy2dt)) 
      } 
    
      modfun1 <- function(time,ka,Vm,Km,Vd) { 
      out <- lsoda(c(Dose,0),c(0,time),defun,parms=c(ka=ka,Vm=Vm,Km=Km,Vd=Vd),
                   rtol=1e-5,atol=1e-7) 
      out[-1,3] 
      }
     } 
   } 
   else {
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
                   rtol=1e-5,atol=1e-7) 
      out[-1,3] 
      }
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
        if (MMe) {
           out <- modfun1(InVVRefindex$time[InVVRefindex$subj==i], par[1], par[2],par[3],par[4])
        } 
        else  {
           out <- modfun(InVVRefindex$time[InVVRefindex$subj==i], par[1], par[2], par[3])
        }
     gift <- which( InVVRefindex$conc[InVVRefindex$subj==i] != 0 )
     switch(pick,
            sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2),
            sum((InVVRefindex$conc[InVVRefindex$subj==i][gift]-out[gift])^2/InVVRefindex$conc[gift]),
            sum(((InVVRefindex$conc[InVVRefindex$subj==i][gift] - out[gift])/InVVRefindex$conc[gift])^2))
     }
#The value of parameter obtained from genetic algorithm 
      if (MMe) {
          gen<-genoud(objfun,nvars=4,max=FALSE,pop.size=30,max.generations=20,
               wait.generations=10,starting.value=c(par[1,2],par[2,2],par[3,2],par[4,2]),
               BFGS=FALSE,print.level=0,boundary.enforcement=2,
               Domains=matrix(c(0.01,1,1,1,10,100,100,100),4,2),
               MemoryMatrix=TRUE) 
      } 
      else {
          gen<-genoud(objfun,nvars=3,max=FALSE,pop.size=20,max.generations=15,
               wait.generations=10,starting.value=c(par[1,2],par[2,2],par[3,2]),
               BFGS=FALSE,print.level=0,boundary.enforcement=2,
               Domains=matrix(c(0.01,0.01,1,10,1,100),3,2),
               MemoryMatrix=TRUE)
      }
     
     if (MMe) {
        namegen<-c("ka","Vm","Km","Vd")
        outgen<-c(gen$par[1],gen$par[2],gen$par[3],gen$par[4])
      } 
      else {
        ## No MM elimination
        namegen<-c("ka","kel","Vd")
        outgen<-c(gen$par[1],gen$par[2],gen$par[3])
      }
      
      F<-objfun(gen$par) 
  ##fitted by Nelder-Mead Simplex algorithm
      if (MMe) {
        opt<-optim(c(gen$par[1],gen$par[2],gen$par[3],gen$par[4]),objfun, method="Nelder-Mead")
        nameopt<-c("ka","Vm","Km","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3],opt$par[4])
        ka[i]<- opt$par[1]
        Vm[i]<- opt$par[2]
        Km[i]<- opt$par[3]
        Vd[i]<- opt$par[4]
      }
      else {
        opt<-optim(c(gen$par[1],gen$par[2],gen$par[3]),objfun, method="Nelder-Mead") 
        nameopt<-c("ka","kel","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3])
        ka[i]<- opt$par[1]
        ke[i]<- opt$par[2]
        Vd[i]<- opt$par[3]
      }
      cat("\n<< The value of parameter fitted by Nelder-Mead Simplex algorithm >>\n\n")
      print(data.frame(Parameter=nameopt,Value=outopt))
      
 ##Residuals sum-of-squares and parameter values fitted by nls 
      cat("\n<< Residual sum-of-squares and parameter values fitted by nls >>\n\n")
      if (MMe) {
        fm<-nls(conc ~ modfun1(time, ka, Vm, Km, Vd), data=InVVRefindex,subset=subj==i,
            start=list(ka=opt$par[1],Vm=opt$par[2],Km=opt$par[3],Vd=opt$par[4]),trace=TRUE,
            nls.control(maxiter=500,tol=1.0))
        cat("\n")
        plotting.non(InVVRefindex, fm, i, pick, xaxis, yaxis)
      } 
      else {
        ## No MM elimination
        fm <-nls(conc ~ modfun(time, ka, kel, Vd), data=InVVRefindex,subset=subj==i,
             start=list(ka=opt$par[1],kel=opt$par[2],Vd=opt$par[3]),trace=TRUE,
             nls.control(tol=1))
        cat("\n")
        coef<-data.frame(coef(fm)["kel"])
        plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)
      }
 }        
       if (MMe) {
          cat("<< Summary >>\n")
          keindex<-data.frame(subj=sub,ka=ka,Vm=Vm,Km=Km, Vd=Vd)
          show(keindex)
              cat("\nSave data (y/n) ?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(PKfit(InVVRefindex))
                      }
              else {
               cat("Enter name you want to call this data\n")
               kename <-readline() 
               kename<-paste(kename,".RData",sep="")      
                 if(file.exists(kename)){
                   cat("\n")
                   cat("*****************************************\n")
                   cat("* The file name have been existed.      *\n")
                   cat("* Would you want to overwrite it ? (y/n)*\n")
                   cat("*****************************************\n")
                   ans<-readline()
                      if (ans == "y" | ans == "Y"){
                      save(keindex,file=kename)
                      cat("\n")
                              }
                      else{
                      cat("\nEnter name you want to call this data\n")
                      kename <-readline() 
                      kename<-paste(kename,".RData",sep="") 
                        repeat{
                        if(file.exists(kename)){
                        cat("\n")
                        cat("***********************************\n")
                        cat("* The file name have been existed *\n")
                        cat("* Enter name again, OK.           *\n")
                        cat("***********************************\n")
                      kename<-readline()
                      kename<-paste(kename,".RData",sep="") 
                         }
                        else{
                         break                       
                         }
                        }        
                      }   
              save(keindex,file=kename)   
                }
                else{
                 save(keindex,file=kename)
                  }                            
            }
    }   
       else {
          cat("<< Summary >>\n")
          keindex<-data.frame(subj=sub,ka=ka, kel=ke,Vd=Vd)
          show(keindex)   
            cat("\nSave data (y/n) ?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(PKfit(InVVRefindex))
                      }
              else {
               cat("Enter name you want to call this data\n")
               kename <-readline() 
               kename<-paste(kename,".RData",sep="")      
                 if(file.exists(kename)){
                   cat("\n")
                   cat("*****************************************\n")
                   cat("* The file name have been existed.      *\n")
                   cat("* Would you want to overwrite it ? (y/n)*\n")
                   cat("*****************************************\n")
                   ans<-readline()
                      if (ans == "y" | ans == "Y"){
                      save(keindex,file=kename)
                      cat("\n")
                              }
                      else{
                      cat("\nEnter name you want to call this data\n")
                      kename <-readline() 
                      kename<-paste(kename,".RData",sep="") 
                        repeat{
                        if(file.exists(kename)){
                        cat("\n")
                        cat("***********************************\n")
                        cat("* The file name have been existed *\n")
                        cat("* Enter name again, OK.           *\n")
                        cat("***********************************\n")
                      kename<-readline()
                      kename<-paste(kename,".RData",sep="") 
                         }
                        else{
                         break                       
                         }
                        }        
                      }   
              save(keindex,file=kename)   
                }
                else{
                 save(keindex,file=kename)
                  }                            
            }
    }
}) 
cat("\n\n")
cat("****************************************************************************\n")
cat("*                        Now, Go to Step3                                  *\n")
cat("****************************************************************************\n\n") 
  InVVTestdata(keindex)     
} 