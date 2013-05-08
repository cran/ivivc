# List of IVIVC Menu
PKvalue<-function()
{

  cat("\n")
  file.menu <- c("PK parameters from PK model fitting (PKfit)",
                 "PK parameters from previously saved .RData or literature",
                 "Back to top menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = " << Menu: Get PK parameters>> ")
    if (pick == 1){
     cat("\n")

cat("****************************************************************************\n")
cat("* In-vitro in-vivo correlation (IVIVC) is defined as the correlation       *\n")
cat("* between in-vitro drug dissolution and in-vivo drug absorption.           *\n")
cat("* This package is used to develop and validate an IVIVC model.             *\n")                
cat("* The following steps will be conducted:                                   *\n")
cat("* -> 1: Input/Edit In-vivo data: such as IV, oral solution or IR           *\n")
cat("* -> 2: Calculate required PK parameters: fitting IV, oral solution or IR  *\n")
cat("* -> 3: Input/Edit in-vitro dissolution data and in-vivo absorption        *\n")
cat("*       data: ER formulatons with different release rates                  *\n")
cat("* -> 4: Develop an IVIVC model: model dependent method                     *\n")
cat("* -> 5: Evaluate an IVIVC model: prediction error                          *\n")
cat("****************************************************************************\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           Now, go to Step 1                               *\n")
cat("****************************************************************************\n\n")
readline(" Press Enter to continue...")
return(InVVRefdata())}
     else {
    if (pick == 2){
       cat("\n")
        PK()}
    else {
    if (pick == 3){
        cat("\n")
        graphics.off()
        run()}
    else {
    if (pick == 4){
        cat("\n  Thanks for using ivivc for R. Bye now. \n\n")
        graphics.off()}
         }
       }
  }
}
