# List of IVIVC Menu
PKvalue<-function()
{

  cat("\n")
  file.menu <- c("PK parameters from curve fitting (PKfit)",
                 "PK parameters from literature",
                 "Back to ivivc menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = " << PK parameter menu >> ")
    if (pick == 1){
     cat("\n")

cat("****************************************************************************\n")
cat("* In vitro-in vivo correlation (IVIVC) is defined as the correlation       *\n")
cat("* between in-vitro drug dissolution and in-vivo drug absorption.           *\n")
cat("* This package is used to develop and validate an IVIVC model.             *\n")                
cat("* The following steps will be conducted:                                   *\n")
cat("* -> 1: Input/Edit In-vivo absorption data: IV, oral solution or IR drug    *\n")
cat("* -> 2: Develop an IVIVC model: Fitting IV, Oral solution or IR drug        *\n")
cat("* -> 3: Input/Edit In-vitro dissolution data and in-Vivo absorption         *\n")
cat("*       Data: ER drug with different release rates                          *\n")   
cat("* -> 4: Develop an IVIVC model: Model Dependent Method                      *\n")
cat("* -> 5: Evaluate an IVIVC model: Prediction error                           *\n")
cat("****************************************************************************\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           Now, go to Step 1                               *\n")
cat("****************************************************************************\n\n")
     return(InVVRefdata())}
     else {
    if (pick == 2){
       cat("\n")
        PK()}
    else {
    if (pick == 3){
        cat("\n")
        run()}
    else {
    if (pick == 4){
        cat("\nThanks for using ivivc for R. Bye now. \n\n")}
         }
       }
  }
}
