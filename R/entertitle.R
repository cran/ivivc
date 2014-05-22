#Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route for "enter names of x,y-axis function"
entertitle<-function()
{
  cat("\nEnter the title of x-axis(time)\n")
  cat("(or press Enter to use default)\n\n") 
  xaxis<-readline()
  if (substr(xaxis, 1, 1) == "")  xaxis<-"Time after dosing (hr)"  else xaxis<-xaxis
  cat("\nEnter the title of y-axis(Cp)\n")
  cat("(or press Enter to use default)\n\n") 
  yaxis<-readline()
  #cat("\n\n Please Wait.  Data is Processing. \n")
  if (substr(yaxis, 1, 1) == "")  yaxis<-"DrugX Plasma Conc. (ng/mL)"  else yaxis<-yaxis
  #cat("\n\n Please Wait.  Data is Processing. \n")
  return(list(xaxis=xaxis,yaxis=yaxis))
}