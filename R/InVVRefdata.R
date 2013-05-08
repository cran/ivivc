#Step1: edit In Vivo raw data for IV, oral solution or IR drugs
InVVRefdata<-function(InVVRefindex)
 { 
  cat("\n")
  file.menu <- c("Input/Edit raw data from keyboard",
                 "Load data from a .csv file",
                 "Autoload data from a previously saved .RData file",
                 "Go back to previous menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " <<Step1:In-vivo Data: IV, Oral solution or IR formulation >> ")
  if (pick == 1){
     cat("\n")
cat("*****************************************************************\n")
cat("*Input/Edit Basic Data                                          *\n")
cat("*   -> subject no.(subj)                                        *\n")
cat("*   -> sampling time                                            *\n")   
cat("*   -> drug plasma/serum/blood concentration (conc)             *\n")
cat("*****************************************************************\n")
     cat("\n")
     InVVRefindex<-data.frame (subj=c(1), time=(0), conc=c(0)) 
     InVVRefindex<-edit(InVVRefindex)
     InVVRefindex<- na.omit(InVVRefindex)
     cat("\n\n");show(InVVRefindex);cat("\n\n")
     saveRDS(InVVRefindex,"ivivc_ref_data.RData")     ### for later use. -YJ                       
cat("\n\n")
cat("*****************************************************************\n")
cat("*                       Now, Go to Step2                        *\n")
cat("*****************************************************************\n\n")
readline(" Press Enter to continue...")   
return(PKfit(InVVRefindex))
      } 
  else {   
  if (pick == 2){
cat("\n\n")
cat("*****************************************************************\n")
cat("* Load data file from a .csv file                               *\n")
cat("* Data should consist of                                        *\n")
cat("*       -> subjects no.(subj)                                   *\n")
cat("*       -> time                                                 *\n")
cat("*       -> concentration (conc)                                 *\n")
cat("*****************************************************************\n\n")
readline(" Press Enter to continue...")
return(InVVRefcsv())     
      }  
  else {
  if (pick == 3){
     cat("\n\n")
cat("*****************************************************************\n")
cat("* Autoload data file from a previously saved .RData file        *\n")
cat("* Data should consist of                                        *\n")
cat("*       -> subjects no.(subj)                                   *\n")
cat("*       -> time                                                 *\n")
cat("*       -> concentration (conc)                                 *\n")
cat("*****************************************************************\n\n")
readline(" Press Enter to continue...")
     InVVRefindex<-readRDS("ivivc_ref_data.RData")
     InVVRefindex<-edit(InVVRefindex)
     InVVRefindex<- na.omit(InVVRefindex)
     colnames(InVVRefindex)<-list("subj", "time","conc")
     cat("\n\n")
     show(InVVRefindex)
cat("\n\n")
cat("*****************************************************************\n")
cat("*                      Now, Go to Step2                         *\n")
cat("*****************************************************************\n\n")
readline(" Press Enter to continue...")
return(PKfit(InVVRefindex))   
                }  
 
  else {   
  if (pick == 4){
     cat("\n\n")
    PKvalue()
                }
  else {
  if (pick == 5){
      cat("\n Thanks for using ivivc for R.  Bye now. \n\n");graphics.off()
                }  
     } 
    }
   }
  }
} 
                           