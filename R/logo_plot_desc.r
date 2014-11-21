logo_plot_desc<-function()
{
## pdf("test.pdf", paper = "a4", bg = "white") ## for test purposes
### require(png)
### require(grid)
par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
logo<-readPNG(system.file("img","ivivc_logo.png",package="ivivc"),TRUE)
grid.raster(logo,width=unit(1,"npc"),height=unit(1,"npc"))     ### this one works great without showing border. -YJ
text(100,180,"This file was generated using ivivc for R v0.2.2",cex = 1.2)
text(70,170,"on:",cex=1.2)
text(110,170,Sys.time(),cex = 1.2)
text(100,150,
"ivivc for R was developed by Hsin-ya Lee (HY) & Yung-jin Lee (YJ).",cex = 1.2)
par(mai=c(0.9,0.9,0.9,0.9)) 
## dev.off()  ## remarked this after testing
}