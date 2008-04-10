# List of IVIVC Menu
PKvalue<-function()
{

  cat("\n")
  file.menu <- c("PK parameters from curve fitting (PKfit)",
                 "PK parameters from Literatures",
                 "Back to ivivc menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = " << PK parameter menu >> ")
    if (pick == 1){
     cat("\n")

cat("****************************************************************************\n")
cat("*1: Input/Edit In Vivo Absorption Data: IV, Oral solution or IR drug       *\n")
cat("*2: Develop an IVIVC Model: Fitting IV, Oral solution or IR drug           *\n")
cat("*3: Input/Edit In Vitro Dissoution Data and                                *\n")
cat("*   In Vivo absorption Data: ER drug with Different Release Rates          *\n")   
cat("*4: Develop an IVIVC Model: Model Dependent                                *\n")
cat("*5: Evaluate an IVIVC model: Prediction Error                              *\n")
cat("****************************************************************************\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           Now, Go to Step1                               *\n")
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
        cat("\nBye~~ \n\n")}
         }
       }
  }
}
