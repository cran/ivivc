#Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route for "plot for linear"
##Estimate model fitting: AIC, Log likelihood, SBC
aicllsbc <- function(fm)
{   
  cat("\n") 
  cat("<< Akaike's Information Criterion (AIC) >>\n\n")
  show(AIC(fm))
    
  cat("\n<< Log likelihood >>\n\n")
  show(logLik(fm))
    
  if (!require(stats4)) {
    ## BIC is belong to stats4 package
    stop("Package stats4 not found.")
  }  
    
  cat("\n<< Schwarz's Bayesian Criterion (SBC) >>\n\n")
  show(BIC(fm))
  cat("\n")     
  
  #Summary the results of nls
  print(summary(fm))  
  cat("\n")   
  cat(date(),"\n\n")     
}  