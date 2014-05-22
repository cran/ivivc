#Fitting with 1-Compartment PK Model:  1-Compartment menu -->iv route menu --> iv bolus route for "plot for linear"
##Estimate model fitting: AIC, Log likelihood, SBC
aicllsbc <- function(fm)
{   
  #Summary the results of nls
  print(summary(fm))
  ModelSelect<-data.frame(Model_Select=c("AIC","Log Likelihood","SBC/BIS"),
                             Values=c(AIC(fm),logLik(fm),BIC(fm)))
  show(ModelSelect);cat("\n")
  
  cat("<< Variance-Covariance Matrix >>\n")
  print(vcov(fm))
    
  cat("\n<< weights >>\n")  ### for debugging purpose. -YJ
  print(weights(fm))
}  