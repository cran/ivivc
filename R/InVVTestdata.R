#Step3: edit In Vivo raw data for extended release formulations with different rates and Edit In Vitro Dissoution Data
InVVTestdata<-function(InVVTestindex,keindex)
 { 
  cat("\n")
  file.menu <- c("Input/Edit raw data from keyboard",
                 "Load a .csv data file",
                 "Autoload a previously saved .RData data file",
                 "Go back to previous menu",
                 "Quit")
   cat("\n")
pick <- menu(file.menu, title = " <<Step3: In-Vitro Dissolution Data and In-Vivo Absorption Data: ER Formulaitons with Different Release Rates >>")
  if (pick == 1){
cat("\n")
cat("*****************************************************************\n")
cat("* Input/Edit Basic Data                                         *\n")
cat("*   -> pH media of dissolution tests                            *\n")
cat("*   -> formulations with different release rates(formula.)      *\n")
cat("*   -> subjects no./formulation (subj)                          *\n")
cat("*   -> time                                                     *\n")
cat("*   -> observed concentration (conc.obs)                        *\n")
cat("*   -> cumulative dissolved fraction (%)(FRD)                   *\n")
cat("*****************************************************************\n")
cat("\n")
     InVVTestindex<-data.frame (pH=c(0), formula.=c(0), subj=c(1), time=(0),conc.obs=c(0),FRD=c(0)) 
     InVVTestindex<-edit(InVVTestindex)
     InVVTestindex<- na.omit(InVVTestindex)
     show(InVVTestindex)
     saveRDS(InVVTestindex,"ivivc_test_data.RData")         ### save InVVTestindex as "ivivc_test_data.RData" by default. --YJ
                       
cat("\n\n")
cat("******************************************************************\n")
cat("*                   Now, Go to Step4 and Step5                   *\n")
cat("******************************************************************\n\n") 
readline(" Press Enter to continue...")
        return(SelModel(InVVTestindex,keindex))
      }      

else {   
  if (pick == 2){
cat("\n\n")
cat("******************************************************************\n")
cat("* Load data file from a .csv file                                *\n")
cat("*   -> pH media of dissolution tests                             *\n")
cat("*   -> formulations with different release rates(formula.)       *\n")
cat("*   -> subjects no./formulation (subj)                           *\n")
cat("*   -> time                                                      *\n")
cat("*   -> observed concentration (conc.obs)                         *\n")
cat("*   -> cumulative dissolved fraction (%)(FRD)                    *\n")
cat("******************************************************************\n\n")
readline(" Press Enter to continue...")
return(InVVTestcsv(InVVTestindex,keindex))
   } 
  else 
  if (pick == 3){
cat("******************************************************************\n")
cat("* Autoload data file from a previously save .RData file          *\n")
cat("*   -> pH media of dissolution tests                             *\n")
cat("*   -> formulations with different release rates(formula.)       *\n")
cat("*   -> subjects no./formulation (subj)                           *\n")
cat("*   -> time                                                      *\n")
cat("*   -> observed concentration (conc.obs)                         *\n")
cat("*   -> cumulative dissolved fraction (%)(FRD)                    *\n")
cat("******************************************************************\n\n")
readline(" Press Enter to continue...")
InVVTestindex<-readRDS("ivivc_test_data.RData")
InVVTestindex<-edit(InVVTestindex)
colnames(InVVTestindex)<-list("pH", "formula.", "subj", "time", "conc.obs","FRD")
InVVTestindex<- na.omit(InVVTestindex)
cat("\n\n")
show(InVVTestindex)
cat("\n\n")
cat("\n\n")
cat("******************************************************************\n")
cat("*                     Now, Go to Step4 and Step5                 *\n")
cat("******************************************************************\n\n")
readline(" Press Enter to continue...")
return(SelModel(InVVTestindex,keindex))   
                }  
 else {   
  if (pick == 4){
     cat("\n\n")
     PKvalue()
                }
  else {
  if (pick == 5){
      cat("\n  Thanks for using ivivc for R.  Bye now. \n\n");graphics.off()
                }  
       
   }
  }
 }
}                           