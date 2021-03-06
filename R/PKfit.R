#Step2: PK fitting for IV, IR, oral solution data: PK fitting menu 
PKfit<-function(InVVRefindex)
 { 
  cat("\n")
  file.menu <- c("1-compartment PK model", 
                 "Go back to PK parameter menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = "<<Step2: Fitting IV, Oral solution or IR formulation --> Select a PK model >>")
  if (pick== 1){
     cat("\n\n")  
     one.list(InVVRefindex)
  }     
  else if (pick == 2){
     cat("\n\n")
     PKvalue()
  }        
   else if (pick == 3){
     cat("\n\n")
     cat("\nThanks for using ivivc for R.  Bye now. \n\n");graphics.off()
  }        
}