description_version<-function(){
cat("                                        \n")
cat(" 888 888  888 888 888  888  .d8888b     \n")
cat(" 888 888  888 888 888  888 d88P         \n")
cat(" 888 Y88  88P 888 Y88  88P 888          \n")
cat(" 888  Y8bd8P  888  Y8bd8P  Y88b.        \n")
cat(" 888   Y88P   888   Y88P     Y8888P   \n\n")
cat("..........................................................\n")
cat(" This report was generated using ivivc for R v0.2.2\n")
cat(" on:-",date(),"\n\n")
username<-Sys.info()[['user']]
osname_version<-c(paste(Sys.info()[['sysname']],"-",Sys.info()[['version']],"\n",
                  Sys.info()[['release']],",",Sys.info()[['machine']]))
cat(" R version:",gsub("R version ","",R.Version()[['version.string']],fixed=TRUE),"\n")
cat(" running on:",osname_version,"\n")
cat(" user id:",username,"\n\n")
cat(" ivivc for R was developed by Hsin-ya Lee & Yung-jin Lee.\n")
cat(" contact: Yung-jin Lee <mobilepk at gmail.com> \n\n")
cat(" ivivc for R is under license of GPL-2|GPL-3.\n")
cat("..........................................................\n\n")
}