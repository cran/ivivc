#Step2-1-1:Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu
iv.route <- function(InVVRefindex)
{
  cat("\n")
  file.menu <- c("IV bolus & single-dose",  
                 "Go back to upper level")
  cat("\n")
  pick <- menu(file.menu, title = "<< IV Route >>")
  if (pick ==1){
     cat("\n\n")
     fbolus1(InVVRefindex)
  }
  else if (pick == 2){
     cat("\n\n") 
     one.list(InVVRefindex)
  }
}