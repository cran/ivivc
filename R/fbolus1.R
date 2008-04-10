#Step2-1-1-1:Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route
fbolus1 <- function(InVVRefindex,
                    Dose=NULL,
                    Vm=NULL,Km=NULL, ## MMe=TRUE
                    Vd=NULL,
                    kel=NULL,        ## MMe=FALSE
                    MMe=FALSE)
{
   #options(warn=-1)
   ## Input dose and initial value for kel and Vd
   if (is.null(Dose)) {
   cat("Enter Dose value\n")
   Dose <- scan(nlines=1,quiet=TRUE)
   } 
   else {
     cat("Dose from arguments is = ",Dose,"\n")
   }
   
   if (MMe){
      if (is.null(Vm) || is.null(Km) || is.null(Vd) ) {
        par<-data.frame(Parameter=c("Vm","Km","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)
      }
   } 
   else {
      ## No MM elimination
      if (is.null(kel) || is.null(Vd)) {
        par<-data.frame(Parameter=c("kel","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)

      }
   }
   
   cat("\n")
   
   if (!MMe) {
      ## User-supplied function w/o Michaelis-Mention elimination
      defun <- function(time, y, parms) { 
      dCpdt <- -parms["kel"] * y[1] 
      list(dCpdt) 
      } 
    
      modfun <- function(time,kel, Vd) {  
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(kel=kel,Vd=Vd),
                   rtol=1e-3,atol=1e-5) 
      out[-1,2] 
      }
   } 
   else {
      ## User-supplied function with MM elimination
      defun<- function(time, y, parms) { 
      dCpdt <- -(parms["Vm"]/parms["Vd"])*y[1]/(parms["Km"]/parms["Vd"]+y[1]) 
      list(dCpdt)
      }

      modfun1 <- function(time,Vm,Km,Vd) { 
      out <- lsoda(Dose/Vd,c(0,time),defun,parms=c(Vm=Vm,Km=Km,Vd=Vd),
                   rtol=1e-3,atol=1e-5)
      out[-1,2] 
      }
   }
   ## Select weighting schemes
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
   for( i in 1:length(unique(InVVRefindex$subject)))  {
     cat("\n\n               << subject",i,">>\n\n" ) 
      sub[i]<-i  
     objfun <- function(par) {
        if (MMe) {
           out <- modfun1(InVVRefindex$time[InVVRefindex$subject==i], par[1], par[2],par[3])
        } 
        else {
           ## No MM elimination
           out <- modfun(InVVRefindex$time[InVVRefindex$subject==i], par[1], par[2])
        }
        gift <- which( InVVRefindex$concentration[InVVRefindex$subject==i] != 0 )
        switch(pick,
             sum((InVVRefindex$concentration[InVVRefindex$subject==i][gift]-out[gift])^2),
             sum((InVVRefindex$concentration[InVVRefindex$subject==i][gift]-out[gift])^2/InVVRefindex$concentration[gift]),
             sum(((InVVRefindex$concentration[InVVRefindex$subject==i][gift]-out[gift])/InVVRefindex$concentration[gift])^2))
        }
#The value of parameter obtained from genetic algorithm   
        if (MMe) {
          gen<-genoud(objfun,nvars=3,max=FALSE,pop.size=30,
               max.generations=20,wait.generations=10,
               starting.value=c(par[1,2],par[2,2],par[3,2]),
               BFGS=FALSE,print.level=0,boundary.enforcement=2,
               Domains=matrix(c(1,1,1,100,100,100),3,2),
               MemoryMatrix=TRUE) 
        }
        else {
          gen<-genoud(objfun,nvars=2,max=FALSE,pop.size=30,
               max.generations=20,wait.generations=10,
               starting.value=c(par[1,2],par[2,2]),
               BFGS=FALSE,print.level=0,boundary.enforcement=2,
               Domains=matrix(c(0.01,0.01,100,100),2,2),
               MemoryMatrix=TRUE)  
         }
     
       if (MMe) {
          namegen<-c("Vm","Km","Vd")
          outgen<-c(gen$par[1],gen$par[2],gen$par[3])
       } 
       else {
          ## No MM elimination
          namegen<-c("kel","Vd")
          outgen<-c(gen$par[1],gen$par[2])
       }
      
       F<-objfun(gen$par)
##fitted by Nelder-Mead Simplex algorithm      
       if (MMe) {
        opt<-optim(c(gen$par[1],gen$par[2],gen$par[3]),objfun, method="Nelder-Mead")
        nameopt<-c("Vm","Km","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3])
        Vm[i]<- opt$par[1]
        Km[i]<- opt$par[2]
        Vd[i]<- opt$par[3]
       }
       else {
        opt<-optim(c(gen$par[1],gen$par[2]),objfun,method="Nelder-Mead")  
        nameopt<-c("kel","Vd")
        outopt<-c(opt$par[1],opt$par[2])
        ke[i]<- opt$par[1]
        Vd[i]<- opt$par[2]
       }
       cat("\n<< The value of parameter fitted by Nelder-Mead Simplex algorithm >>\n\n")
       print(data.frame(Parameter=nameopt,Value=outopt))
       
       ##Residuals sum-of-squares and parameter values fitted by nls
       cat("\n<< Residual sum-of-squares and parameter values fitted by nls >>\n\n")
       if (MMe) {
         fm<-nls(concentration~modfun1(time,Vm,Km,Vd),data=InVVRefindex,subset=subject==i,
                 start=list(Vm=opt$par[1],Km=opt$par[2],Vd=opt$par[3]),trace=TRUE,
                 nls.control(tol=1))
         cat("\n")
         plotting.non(InVVRefindex, fm, i, pick, xaxis, yaxis)
       
       } 
       else {
        ## No MM elimination
         fm<-nls(concentration ~ modfun(time, kel, Vd), data=InVVRefindex,
                 start=list(kel=opt$par[1],Vd=opt$par[2]),trace=TRUE,subset=subject==i,
                 nls.control(tol=1))
         cat("\n")
         coef<-data.frame(coef(fm)["kel"])
         plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)
       }
   }
 if (MMe) {
          cat("<< Summary >>\n")
          keindex<-data.frame(subject=sub,Vm=Vm,Km=Km, Vd=Vd)
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
          keindex<-data.frame(subject=sub,kel=ke,Vd=Vd)
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
  InVVTestdata()  
}
