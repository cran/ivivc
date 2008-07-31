# List of IVIVC Menu
run<-function()
{
options(warn=-1)
#prevent warning message 
  if (!require(rgenoud)) {
    ## genoud is belong to rgenoud package
    stop("Package rgenoud not found.")
  }   
  
  if (!require(odesolve)) {
    ## lsoda is belong to odesolve package
    stop("Package odesolve not found.")
  }
 
  if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }   
  cat("\n")
  file.menu <- c("Start a new project",
                 "Demo (Wagner-Nelson method)", 
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << IVIVC menu >> ")
    if (pick == 1){
      cat("\n")
      PKvalue()}
    
    else {
    if (pick == 2){
        cat("\n")
        ivivc()
       }
    
    else {
    if (pick == 3){
        cat("\nBye~~ \n\n")}
         }
   } 
}