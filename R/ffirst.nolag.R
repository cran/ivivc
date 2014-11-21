#Step2-1-2-2:Fitting with 1-Compartment PK Model:  1-Compartment menu -->noniv route menu-->Extravascular, SD, & 1-Ord Abs without Tlag

ffirst.nolag <- function(InVVRefindex,...) {
  ffirst.lag(InVVRefindex,...,Tlag = FALSE)
}