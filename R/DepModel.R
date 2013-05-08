#Step4:Develop an IVIVC Model: Model Dependent or Independent Method  --> Model Dependent Method
DepModel<-function(InVVTestindex, keindex)
{ 
  cat("\n")
  file.menu <- c("1-compartment model: Wagner-Nelson method",
                 "Go back to Upper Level", 
                 "Go back to PK parameter menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = "<<Step4-1: Develop an IVIVC Model: Model Dependent Method>>")
  if (pick == 1){
     cat("\n\n")  
     WagNel(InVVTestindex,keindex)
  }
  else {   
  if (pick == 2){
     cat("\n\n")
     SelModel(InVVTestindex,keindex)
                }
  else {   
  if (pick == 3){
     cat("\n\n")
     PKvalue()
                }
  else {
  if (pick == 4){
      cat("\n Thanks for using ivivc for R.  Bye now. \n\n");graphics.off()
                }  
      }
     }
    }
 }     