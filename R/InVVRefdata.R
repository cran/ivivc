#Step1: edit In Vivo raw data for IV, oral solution or IR drugs
InVVRefdata<-function(InVVRefindex)
 { 
  cat("\n")
  file.menu <- c("Input/Edit Raw Data from keyboard",
                 "Load Data Files (.CSV)",
                 "Load Data Files (.RData)",   
                 "Go Back to PK parameter menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " <<Step1:In Vivo Absorption Data: IV, Oral solution or IR drug >> ")
  if (pick == 1){
     cat("\n")
cat("****************************************************************************\n")
cat("*Input/Edit Basic Data                                                     *\n")
cat("*   ->subjects no.(subj)                                                   *\n")
cat("*   ->time                                                                 *\n")   
cat("*   ->concentration (conc)                                                 *\n")
cat("****************************************************************************\n")
     cat("\n")
     InVVRefindex<-data.frame (subj=c(1), time=(0), conc=c(0)) 
     InVVRefindex<-edit(InVVRefindex)
     InVVRefindex<- na.omit(InVVRefindex)
     show(InVVRefindex)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return(InVVRefdata())
        }
     else {
        cat("Enter name you want to call this data\n")
        InVVRefname <-readline() 
        InVVRefname<-paste(InVVRefname,".RData",sep="")      
           if(file.exists(InVVRefname)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(InVVRefindex,file=InVVRefname)
                cat("\n")
                }
                else{
                cat("\nEnter name you want to call this data\n")
                InVVRefname <-readline() 
                InVVRefname<-paste(InVVRefname,".RData",sep="") 
                repeat{
                    if(file.exists(InVVRefname))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed *\n")
                      cat("* Enter name again, OK.           *\n")
                      cat("***********************************\n")
                      InVVRefname<-readline()
                      InVVRefname<-paste(InVVRefname,".RData",sep="") 
                      }
                       else{
                       break                       
                           }
                    }        
             }   
              save(InVVRefindex,file=InVVRefname)   
           }
        else{
           save(InVVRefindex,file=InVVRefname)
          }                            
cat("\n\n")
cat("****************************************************************************\n")
cat("*                         Now, Go to Step2                                 *\n")
cat("****************************************************************************\n\n")   
        return(PKfit(InVVRefindex))
      }      
    }     
  else {   
  if (pick == 2){
cat("\n\n")
cat("****************************************************************************\n")
cat("* Enter data file name(.csv)                                               *\n")
cat("* Data should consist of                                                   *\n")
cat("*       ->subjects no.(subj)                                               *\n")
cat("*       ->time                                                             *\n")
cat("*       ->concentration (conc)                                             *\n")
cat("****************************************************************************\n\n")
cat("\n")
    filepath<-getwd()
    cat("R will import your data from the directory of \n")
    cat("",filepath,"\n")
        return(InVVRefcsv())     
      }  
  else {
  if (pick == 3){
     cat("\n\n")
cat("****************************************************************************\n")
cat("* Enter data file name(.RData)                                             *\n")
cat("* Data should consist of                                                   *\n")
cat("*       ->subjects no.(subj)                                               *\n")
cat("*       ->time                                                             *\n")
cat("*       ->concentration (conc)                                             *\n")
cat("****************************************************************************\n\n")
cat("\n\n")
filepath<-getwd()
cat("R will load your data from the directory of \n")
cat("",filepath,"\n")
cat("\n")     
     InVVRefname <-readline()
     InVVRefname<-paste(InVVRefname,".RData",sep="")
     load(InVVRefname)
     InVVRefindex<-edit(InVVRefindex)
     InVVRefindex<- na.omit(InVVRefindex)
     colnames(InVVRefindex)<-list("subj", "time","conc")
     cat("\n\n")
     show(InVVRefindex)
     save(InVVRefindex,file=InVVRefname)
cat("\n\n")
cat("****************************************************************************\n")
cat("*                         Now, Go to Step2                                 *\n")
cat("****************************************************************************\n\n") 
     return(PKfit(InVVRefindex))   
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
} 
                           