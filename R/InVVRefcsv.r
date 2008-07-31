#choose separator and decimal type
InVVRefcsv<-function()
{
cat("\n")
file.menu <- c("sep = comma (,) &  dec= point (.)",
               "sep = semicolon (;) &  dec= comma (,)",
               "sep = semicolon (;) &  dec= point (.)",
               "sep = {space} &  dec= comma (,)",
               "sep = {space} &  dec= point (.)",
               "sep = {tab} &  dec= comma (,)",
               "sep = {tab} &  dec= point (.)",
               "sep = colon (:) &  dec= comma (,)",
               "sep = colon (:) &  dec= point (.)",
               "Go Back to PK parameter menu")
cat("\n")
pick <- menu(file.menu, title = " << Separator type and Decimal type >> ")
if (pick == 1){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of .csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
     }

 else {
  if (pick == 2){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
 else {
  if (pick == 3){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
else {
  if (pick == 4){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
else {
  if (pick == 5){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))

          }
else {
  if (pick == 6){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
else {
  if (pick == 7){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
else {
  if (pick == 8){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
else {
  if (pick == 9){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        InVVRef.file <-readline()
        InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("subj", "time","conc")
        InVVRefindex<-read.csv(InVVRef.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        InVVRefindex<-edit(InVVRefindex)
        InVVRefindex<- na.omit(InVVRefindex)
        cat("\n\n")
        show(InVVRefindex)
        cat("\n\n")
        cat("****************************************************************************\n")
        cat("*                         Now, Go to Step2                                 *\n")
        cat("****************************************************************************\n\n")
        return(PKfit(InVVRefindex))
          }
 else {
  if (pick == 10){
      return (PKvalue())
                }
          }
         }
        }
       }
      }
     }
    }
  }
 }
}