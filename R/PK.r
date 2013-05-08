# List of PK value from references Menu
### changes: use saveRDS() since v0.1.7
PK<-function(keindex)
{
 cat("\n")
  file.menu <- c("Use previously saved PK Data Files(.RData) ",
                 "Enter PK value from references",
                 "Back to previous menu",
                 "Quit")
 cat("\n")
pick <- menu(file.menu, title = " << Menu: Enter PK parameter>> ")
 if (pick == 1){
cat("****************************************************************************\n")
cat("*                        Now, Go to Step3                                  *\n")
cat("****************************************************************************\n\n")
    keindex<-readRDS("ivivc_pk_values.RData")  ### use 'keindex', not 'kename'; here we will load PK parameters by default. -YJ 
    edit(keindex)
    show(keindex)
    return(InVVTestdata(keindex))}
else {
 if (pick == 2){
cat("\n")
cat("****************************************************************************\n")
cat("*3: Input/Edit in-vitro dissoution data and                                *\n")
cat("*   in-vivo absorption Data: ER formulations with Different Release Rates  *\n")
cat("*4: Develop an IVIVC Model: model dependent or independent method          *\n")
cat("*5: Evaluate an IVIVC model: prediction error                              *\n")
cat("****************************************************************************\n")
cat("\n")
cat("Enter ke and Vd values from reference now.\n\n")
 
      ##Input Ke value from reference or from previous PKfit
      keindex<-data.frame(subj=c(1), ke=c(0), Vd=c(0))
      keindex<-edit(keindex)
      keindex<- na.omit(keindex) 
      kename<-"ivivc_pk_values.RData"                         ### however this will overwrite previously saved data. -YJ
      saveRDS(keindex,kename)
      cat("\n\n");show(keindex);cat("\n\n")
                
cat("****************************************************************************\n")
cat("*                          Now, Go to Step3                                *\n")
cat("****************************************************************************\n\n")
readline(" Press Enter to continue...")
     return(InVVTestdata(keindex))}

else {
 if (pick == 3){
     cat("\n")
     PKvalue()}
 else {
 if (pick == 4){
     cat("\n  Thanks for using ivivc for R.  Bye now. \n\n")}
      }
   }
 }
}   