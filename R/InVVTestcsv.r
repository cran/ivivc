#choose separator and decimal type
InVVTestcsv<-function(InVVTestindex,keindex)
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
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
     }

 else {
  if (pick == 2){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
 else {
  if (pick == 3){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 4){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 5){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 6){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 7){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 8){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
          }
else {
  if (pick == 9){
  cat("\n\n")
        ### cat("\nEnter Data file name(without file extention of .csv)\n")
        ### InVVRef.file <-readline()
        ### InVVRef.file<-paste(InVVRef.file,".csv",sep="")
        cnames<-c("pH", "formula.", "subj", "time", "conc.obs", "FRD")
        InVVTestindex<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        InVVTestindex<-edit(InVVTestindex)
        InVVTestindex<- na.omit(InVVTestindex)
        cat("\n\n")
        show(InVVTestindex)
        saveRDS(InVVTestindex,"ivivc_test_data.RData")
        cat("\n\n")
        cat("*****************************************************************\n")
        cat("*                      Now, Go to Step4 and Step5               *\n")
        cat("*****************************************************************\n\n")
        return(SelModel(InVVTestindex,keindex))
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