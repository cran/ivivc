bye<-function()
{
cat("\n")
  file.menu <- c("Back to ivivc menu",
                 "Quit")
  cat("\n")
  pick <- menu(file.menu, title = " << Try again or bye~ >> ")
    if (pick == 1){
     cat("\n")
     run()}
    else {
     if (pick == 2){
        cat("\nBye~~ \n\n")}
        }
  }