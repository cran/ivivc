#Step2-1-2:Fitting with 1-Compartment PK Model:  1-Compartment menu -->noniv route menu
noniv.route <- function(InVVRefindex)
{
  
  file.menu <- c("Extravascular, single-dose, 1-Ordered absorption wihtout lag time",
                 "Go back One Upper Level")
  cat("\n")
  pick <- menu(file.menu, title = "<< Non IV Route >>")
  if (pick == 1){
     cat("\n\n") 
     ffirst.nolag(InVVRefindex)
  }
  else if (pick == 2){
     one.list(InVVRefindex)
  }
  
}