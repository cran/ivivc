#Step4:Develop an IVIVC Model: Model Dependent or Independent Method  
SelModel<-function(InVVTestindex,keindex)
{ 
  cat("\n")
  file.menu <- c("Model Dependent Method",
                 "Go Back to PK parameter menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = "<<Step4:Develop an IVIVC Model: Model Dependent Method>>")
  if (pick == 1){
     cat("\n\n")  
     DepModel(InVVTestindex,keindex)
  }
  else {   
  if (pick == 2){
     cat("\n\n")
     PKvalue()
                }
  else {
  if (pick == 3){
      cat("\nBye~~ \n\n")
                }  
   }
  }
}   