# List of PK value from references Menu
PK<-function(keindex)
{
 cat("\n")
  file.menu <- c("Use an existed PK Data Files(.RData) ",
                 "Enter PK value from references",
                 "Back to PK parameter menu",
                 "Quit")
 cat("\n")
pick <- menu(file.menu, title = " << Enter PK parameter menu >> ")
 if (pick == 1){
cat("****************************************************************************\n")
cat("*                        Now, Go to Step3                                  *\n")
cat("****************************************************************************\n\n")
     return(InVVTestdata(keindex))}
 else {
 if (pick == 2){
cat("\n")
cat("****************************************************************************\n")
cat("*3: Input/Edit In Vitro Dissoution Data and                                *\n")
cat("*   In Vivo absorption Data: ER Drug with Different Release Rates          *\n")
cat("*4: Develop an IVIVC Model: Model Dependent or Independent Method          *\n")
cat("*5: Evaluate an IVIVC model: Prediction Error                              *\n")
cat("****************************************************************************\n")
cat("\n")
     cat("Enter ke and Vd value from references\n") 
      ##Input Ke value from reference or from previous PKfit
      keindex<-data.frame(subj=c(1), ke=c(0), Vd=c(0))
      keindex<-edit(keindex)
      keindex<- na.omit(keindex) 
       cat("\nSave data (y/n) ?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(PKvalue())
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
cat("****************************************************************************\n")
cat("*                          Now, Go to Step3                                *\n")
cat("****************************************************************************\n\n")
     return(InVVTestdata())}

   else {
    if (pick == 3){
        cat("\n")
        PKvalue()}
    else {
    if (pick == 4){
        cat("\nThanks for using ivivc for R.  Bye now. \n\n")}
         }
       }
   }
}   