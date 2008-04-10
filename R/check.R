#Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route for "check function"
check<-function(par)
{
   repeat{
     if ( par[1,2] == 0 ){
       cat("\n")
       cat("**********************************\n")
       cat(" Parameter value can not be zero. \n")
       cat(" Press enter to continue.         \n")
       cat("**********************************\n\n")
       readline()
       cat("\n")
       par<-edit(par)
       }   
     else{
       break
       return(edit(par))
      }
  } 
  cat("\n")       
  show(par)     
}