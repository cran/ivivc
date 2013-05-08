# List of IVIVC Menu
run<-function()
{
options(warn=-1)
 
  if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }   
  cat("\n")
  file.menu <- c("Start a new project",
                 "Tutorial (Wagner-Nelson method)", 
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << IVIVC menu >> ")
    if (pick == 1){
      cat("\n")
      PKvalue()}
    
    else {
    if (pick == 2){
        cat("\n")
        ivivc_demo()
       }
    
    else {
    if (pick == 3){
        cat("\n Thanks for using ivivc for R. Bye now. \n\n")
        graphics.off()}
         }
   } 
}