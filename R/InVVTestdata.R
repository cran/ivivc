#Step3: edit In Vivo raw data for extended release formulations with different rates and Edit In Vitro Dissoution Data
InVVTestdata<-function(InVVTestindex,keindex)
 { 
  cat("\n")
  file.menu <- c("Input/Edit Raw Data from keyboard",
                 "Load Data Files (.CSV)",
                 "Load Data Files (.RData)",
                 "Go Back to PK parameter menu",
                 "Quit")
   cat("\n")
pick <- menu(file.menu, title = " <<Step3:In Vitro Dissolution Data and In Vivo absorption Data: ER drug with Different Release Rates >>")
  if (pick == 1){
cat("\n")
cat("****************************************************************************\n")
cat("*Input/Edit Basic Data                                                     *\n")
cat("*   ->pH media of dissolution tests                                        *\n")
cat("*   ->formulations with different release rates(character or numeric)      *\n")
cat("*   ->subjects no./formulation                                             *\n")
cat("*   ->time                                                                 *\n")   
cat("*   ->observed concentration (conc.obs)                                    *\n")
cat("*   ->cumulative dissolved fraction (%)(FRD)                               *\n")
cat("****************************************************************************\n")
cat("\n")
     InVVTestindex<-data.frame (pH=c(0), formulation=c(0), subject=c(1), time=(0),conc.obs=c(0),FRD=c(0)) 
     InVVTestindex<-edit(InVVTestindex)
     show(InVVTestindex)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return(InVVTestdata())
        }
     else {
        cat("Enter name you want to call this data\n")
        InVVTestname <-readline() 
        InVVTestname<-paste(InVVTestname,".RData",sep="")      
           if(file.exists(InVVTestname)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(InVVTestindex,file=InVVTestname)
                cat("\n")
                }
                else{
                cat("\nEnter name you want to call this data\n")
                InVVTestname <-readline() 
                InVVTestname<-paste(InVVTestname,".RData",sep="") 
                repeat{
                    if(file.exists(InVVTestname))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed *\n")
                      cat("* Enter name again, OK.           *\n")
                      cat("***********************************\n")
                      InVVTestname<-readline()
                      InVVTestname<-paste(InVVTestname,".RData",sep="") 
                      }
                       else{
                       break                       
                           }
                    }        
             }   
              save(InVVTestindex,file=InVVTestname)   
           }
        else{
           save(InVVTestindex,file=InVVTestname)
          }                            
cat("\n\n")
cat("****************************************************************************\n")
cat("*                        Now, Go to Step4 and Step5                        *\n")
cat("****************************************************************************\n\n")   
        return(SelModel(InVVTestindex,keindex))
      }      
    }     
  else {   
  if (pick == 2){
cat("\n\n")
cat("****************************************************************************\n")
cat("*Input/Edit Basic Data                                                     *\n")
cat("*   ->pH media of dissolution tests                                        *\n")
cat("*   ->formulations with different release rates(character or numeric)      *\n")
cat("*   ->subjects no./formulation                                             *\n")
cat("*   ->time                                                                 *\n")   
cat("*   ->observed concentration(conc.obs)                                     *\n")
cat("*   ->cumulative dissolved fraction%(FRD)                                  *\n")
cat("****************************************************************************\n")
cat("\n")
     InVVTest.file <-readline()
     InVVTest.file<-paste(InVVTest.file,".csv",sep="")
     cnames<-c("pH", "formulation", "subject", "time", "conc.obs", "FRD")
     InVVTestindex<-read.csv(InVVTest.file,header=TRUE,sep=",",row.names=NULL,col.names=cnames)
     InVVTestindex<-edit(InVVTestindex)
     cat("\n\n")
     show(InVVTestindex)
     cat("\n\n")
cat("\n\n")
cat("****************************************************************************\n")
cat("*                        Now, Go to Step4 and Step5                        *\n")
cat("****************************************************************************\n\n") 
     return(SelModel(InVVTestindex,keindex))   
                }  
  else 
  if (pick == 3){
     cat("\nEnter data file name\n") 
     InVVTestname <-readline()
     InVVTestname<-paste(InVVTestname,".RData",sep="")
     load(InVVTestname)
     InVVTestindex<-edit(InVVTestindex)
     colnames(InVVTestindex)<-list("pH", "formulation", "subject", "time", "conc.obs","FRD")
     cat("\n\n")
     show(InVVTestindex)
     save(InVVTestindex,file=InVVTestname)
     cat("\n\n")
cat("\n\n")
cat("****************************************************************************\n")
cat("*                        Now, Go to Step4 and Step5                        *\n")
cat("****************************************************************************\n\n")  
     return(SelModel(InVVTestindex,keindex))   
                }  
 else {   
  if (pick == 4){
     cat("\n\n")
     PKvalue()
                }
  else {
  if (pick == 5){
      cat("\nBye~~ \n\n")
                }  
       
   }
  }
 }
}                           