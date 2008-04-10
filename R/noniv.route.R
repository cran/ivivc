#Step2-1-2:Fitting with 1-Compartment PK Model:  1-Compartment menu -->noniv route menu
noniv.route <- function(InVVRefindex)
{
  cat("***************************************\n")
  cat(" SD: Single-Dose                       \n")
  cat(" 1st-Ord: First-Ordered                \n")
  cat(" Zero-Ord: Zero-Ordered                \n")
  cat(" Abs: Absorption                       \n")
  cat(" w: with                               \n")
  cat(" w/o: without                          \n")
  cat(" Tlag: Lag Time                        \n")
  cat("***************************************\n\n")
  cat("\n")
  file.menu <- c("Extravascular, SD, & 1-Ord Abs w Tlag",
                 "Extravascular, SD, & 1-Ord Abs w/o Tlag",
                 "Extravascular, SD, & Zero-Ord Abs w/o Tlag",
                 "Go Back One Upper Level")
  cat("\n")
  pick <- menu(file.menu, title = "<< Non IV Route >>")
  if (pick == 1){
     cat("\n\n") 
     ffirst.lag(InVVRefindex)
  }
  else if (pick == 2){
     cat("\n\n") 
     ffirst.nolag(InVVRefindex)
  }
  else if (pick == 3){
     cat("\n\n") 
     fzero.nolag(InVVRefindex)
  }
  else if (pick == 4){
     one.list(InVVRefindex)
  }
}