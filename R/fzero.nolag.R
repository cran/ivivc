#Step2-1-2-3:Fitting with 1-Compartment PK Model:  1-Compartment menu -->noniv route menu-->Extravascular, SD, & Zero-Ord Abs without Tlag
fzero.nolag <- function(InVVRefindex,
                        Dose=NULL, 
                        Tabs=NULL,
                        Vm=NULL,Km=NULL, ## MMe=TRUE
                        Vd=NULL,
                        kel=NULL,        ## MMe=FALSE
                        MMe=FALSE) 
{ 
   #options(warn=-1)
        
   ## Input dose and initial value for Tabs, kel and Vd
   
   if (is.null(Dose)) {
     cat("Enter Dose value\n")
     Dose <- scan(nlines=1,quiet=TRUE)
   } 
   else {
     cat("Dose from arguments is = ",Dose,"\n")
   }
   
   if (MMe){
      if ( is.null(Tabs) || is.null(Vm) || is.null(Km) || is.null(Vd) ) {
        par<-data.frame(Parameter=c("Tabs","Vm","Km","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)
        
      }
   } 
   else {
      ## No MM elimination
      if ( is.null(Tabs) || is.null(kel) || is.null(Vd)) {
        par<-data.frame(Parameter=c("Tabs","kel","Vd"),Initial=c(0))
        par<-edit(par)
        par<-check(par)

      }
   }

   cat("\n")
   
   if (!MMe) {
      ## User-supplied function w/o Michaelis-Mention elimination
      defun<- function(time, y, parms) { 
      if(time<=parms["Tabs"]) 
        dCpdt <- (Dose/parms["Tabs"])/parms["Vd"] - parms["kel"] * y[1]
      else
        dCpdt <- - parms["kel"] * y[1]
      list(dCpdt) 
      } 
   
      modfun <- function(time,Tabs,kel,Vd) { 
         out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,kel=kel,Vd=Vd),
                      rtol=1e-8,atol=1e-8) 
         out[-1,2]
      } 
   } 
   else {
     ## User-supplied function with MM elimination
      defun<- function(time, y, parms) { 
      if(time<=parms["Tabs"]) 
        dCpdt <- (Dose/parms["Tabs"])/parms["Vd"]-(parms["Vm"]/parms["Vd"])*y[1]/(parms["Km"]/parms["Vd"]+y[1])            
      else
        dCpdt <- -(parms["Vm"]/parms["Vd"])*y[1]/(parms["Km"]/parms["Vd"]+y[1])            
      list(dCpdt) 
      }   
   
      modfun1 <- function(time,Tabs,Vm,Km,Vd) { 
      out <- lsoda(0,c(0,time),defun,parms=c(Tabs=Tabs,Vm=Vm,Km=Km,Vd=Vd),
                   rtol=1e-6,atol=1e-8) 
      out[-1,2]
      } 
    }
    
   ## Select weighting schemes
   file.menu <- c("equal weight", 
                  "1/Cp",
                  "1/Cp^2")           
   pick <- menu(file.menu, title = "<< Weighting Schemes >>")

   with(entertitle(),{
 Tabs<-NULL  
 Vd<-NULL
 Vm<-NULL
 Km<-NULL
 ke<-NULL 
 sub<-NULL
   for( i in 1:length(unique(InVVRefindex$subj)))  {
     cat("\n\n               << subject",i,">>\n\n" )  
     sub[i]<-i  
     objfun <- function(par) {
        if (MMe) {
           out<-modfun1(InVVRefindex$time[InVVRefindex$subj==i],par[1],par[2],par[3],par[4])
        } 
        else {
           ## No MM elimination
           out<-modfun(InVVRefindex$time[InVVRefindex$subj==i],par[1],par[2],par[3])
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
              wait.generations=10,
              starting.values=c(par[1,2],par[2,2],par[3,2],par[4,2]),
              BFGS=FALSE,print.level=0,boundary.enforcement=2,
              Domains=matrix(c(1,1,1,1,50,100,100,100),4,2),
              MemoryMatrix=TRUE)
      } 
      else {
         gen<-genoud(objfun,nvars=3,max=FALSE,pop.size=30,max.generations=20,
              wait.generations=10,
              starting.values=c(par[1,2],par[2,2],par[3,2]),
              BFGS=FALSE,print.level=0,boundary.enforcement=2,
              Domains=matrix(c(1,0.01,1,50,1,100),3,2),
              MemoryMatrix=TRUE)
      }
      
    
     if (MMe) {
        namegen<-c("Tabs","Vm","Km","Vd")
        outgen<-c(gen$par[1],gen$par[2],gen$par[3],gen$par[4])
     } 
     else {
        ## No MM elimination
        namegen<-c("Tabs","kel","Vd")
        outgen<-c(gen$par[1],gen$par[2],gen$par[3])
     }
    
     F<-objfun(gen$par)  
      ##fitted by Nelder-Mead Simplex algorithm  
     if (MMe) {
        opt<-optim(c(gen$par[1],gen$par[2],gen$par[3],gen$par[4]),objfun, method="Nelder-Mead")  
        nameopt<-c("Tabs","Vm","Km","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3],opt$par[4])
        Tabs[i]<- opt$par[1]
        Vm[i]<- opt$par[2]
        Km[i]<- opt$par[3]
        Vd[i]<- opt$par[4]
     }
     else {
        opt<-optim(c(gen$par[1],gen$par[2],gen$par[3]),objfun, method="Nelder-Mead")  
        nameopt<-c("Tabs","kel","Vd")
        outopt<-c(opt$par[1],opt$par[2],opt$par[3])
        Tabs[i]<- opt$par[1]
        ke[i]<- opt$par[2]
        Vd[i]<- opt$par[3]
     }
     cat("\n<< The value of parameter fitted by Nelder-Mead Simplex slgorithm >>\n\n")
     print(data.frame(Parameter=nameopt,Value=outopt))
     
    ##Residuals sum-of-squares and parameter values fitted by nls
     cat("\n<< Residual sum-of-squares and parameter values fitted by nls >>\n\n")
     if (MMe) {
        fm<-nls(conc~modfun1(time,Tabs,Vm,Km,Vd),data=InVVRefindex, algorithm="default",subset=subj==i,
            start=list(Tabs=opt$par[1],Vm=opt$par[2],Km=opt$par[3],Vd=opt$par[4]),trace=TRUE,
            nls.control(maxiter=50,tol=10,minFactor=1/1024))
        cat("\n")
        plotting.non(InVVRefindex, fm, i, pick,xaxis,yaxis)
     } 
     else {
        ## No MM elimination
        fm<-nls(conc~modfun(time,Tabs,kel,Vd,1),data=InVVRefindex, algorithm="default",subset=subj==i,
            start=list(Tabs=opt$par[1],kel=opt$par[2],Vd=opt$par[3]),trace=TRUE,
            nls.control(tol=1))
        cat("\n")
        coef<-data.frame(coef(fm)["kel"])
        plotting.lin(InVVRefindex, fm, i, pick, coef, xaxis, yaxis)
     }
  }
   if (MMe) {
          cat("<< Summary >>\n")
          keindex<-data.frame(subj=sub,Vm=Vm,Km=Km, Vd=Vd)
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
          keindex<-data.frame(subj=sub,Tabs=Tabs, kel=ke,Vd=Vd)
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