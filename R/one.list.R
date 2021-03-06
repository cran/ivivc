#Step2-1:Fitting with 1-Compartment PK Model:  1-Compartment menu
one.list <- function(InVVRefindex)
{
  cat("\n")
  file.menu <- c("IV Route", 
                 "Non IV Route (solutions/IR)",
                 "Go Back One Upper Level")
  cat("\n")
  pick <- menu(file.menu, title = "<< 1-Compartment PK Model >>")
  if (pick == 1){
     cat("\n\n")  
     iv.route(InVVRefindex)
  }
  else if (pick == 2){
     cat("\n\n")
     noniv.route(InVVRefindex)
  }
  else if (pick == 3){
     cat("\n\n")
     PKfit(InVVRefindex)
  }              
}