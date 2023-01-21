rm(list=ls())
memory.limit(size=40000)
library(raster)
library(ncdf4)
library(RcppCNPy)
library(R.matlab)
library(maps)
library(maptools)
library(animation)
library(fields)
library(abind)

worldmap <- map("world",fill=F,interior=F,plot=F)
worldmap <- map2SpatialLines(worldmap,proj4string=CRS("+proj=longlat"))
lakemap <- map("lakes",fill=F,interior=F,plot=F)
lakemap <- map2SpatialLines(lakemap,proj4string=CRS("+proj=longlat"))
colorbar <- colorRampPalette(c("antiquewhite3","beige","aquamarine3"))
colors1 <- colorbar(5)
colorbar <- colorRampPalette(c("deepskyblue2","white","deeppink2"))
colors2 <- colorbar(10)
colorbar <- colorRampPalette(c("beige","brown4"))
colors3 <- colorbar(6)

load("C:\\Users\\zhyxi\\Documents\\UTrack\\ERA3_data.RData")

rhs_mean <- array(NA,dim=c(12,360,180))
rhs_mean[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean01.npy")
rhs_mean[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean02.npy")
rhs_mean[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean03.npy")
rhs_mean[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean04.npy")
rhs_mean[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean05.npy")
rhs_mean[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean06.npy")
rhs_mean[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean07.npy")
rhs_mean[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean08.npy")
rhs_mean[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean09.npy")
rhs_mean[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean10.npy")
rhs_mean[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean11.npy")
rhs_mean[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs_mean12.npy")

sst_mean <- array(NA,dim=c(12,360,180))
sst_mean[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean01.npy")
sst_mean[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean02.npy")
sst_mean[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean03.npy")
sst_mean[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean04.npy")
sst_mean[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean05.npy")
sst_mean[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean06.npy")
sst_mean[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean07.npy")
sst_mean[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean08.npy")
sst_mean[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean09.npy")
sst_mean[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean10.npy")
sst_mean[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean11.npy")
sst_mean[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst_mean12.npy")

operc <- array(NA,dim=c(12,360,180))
operc[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc01.npy")
operc[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc02.npy")
operc[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc03.npy")
operc[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc04.npy")
operc[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc05.npy")
operc[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc06.npy")
operc[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc07.npy")
operc[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc08.npy")
operc[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc09.npy")
operc[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc10.npy")
operc[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc11.npy")
operc[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc12.npy")

rhs1c_mean <- array(NA,dim=c(12,360,180))
rhs1c_mean[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean01.npy")
rhs1c_mean[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean02.npy")
rhs1c_mean[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean03.npy")
rhs1c_mean[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean04.npy")
rhs1c_mean[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean05.npy")
rhs1c_mean[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean06.npy")
rhs1c_mean[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean07.npy")
rhs1c_mean[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean08.npy")
rhs1c_mean[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean09.npy")
rhs1c_mean[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean10.npy")
rhs1c_mean[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean11.npy")
rhs1c_mean[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\rhs1c_mean12.npy")

sst1c_mean <- array(NA,dim=c(12,360,180))
sst1c_mean[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean01.npy")
sst1c_mean[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean02.npy")
sst1c_mean[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean03.npy")
sst1c_mean[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean04.npy")
sst1c_mean[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean05.npy")
sst1c_mean[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean06.npy")
sst1c_mean[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean07.npy")
sst1c_mean[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean08.npy")
sst1c_mean[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean09.npy")
sst1c_mean[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean10.npy")
sst1c_mean[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean11.npy")
sst1c_mean[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\sst1c_mean12.npy")

operc1c <- array(NA,dim=c(12,360,180))
operc1c[1,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c01.npy")
operc1c[2,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c02.npy")
operc1c[3,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c03.npy")
operc1c[4,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c04.npy")
operc1c[5,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c05.npy")
operc1c[6,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c06.npy")
operc1c[7,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c07.npy")
operc1c[8,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c08.npy")
operc1c[9,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c09.npy")
operc1c[10,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c10.npy")
operc1c[11,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c11.npy")
operc1c[12,,] <- npyLoad("C:\\Users\\zhyxi\\Documents\\UTrack\\operc1c12.npy")

land_filter <- sst_12mean_regrid[1,,]
land_filter[!is.na(land_filter)] <- 999
land_filter[is.na(land_filter)] <- 1
land_filter[land_filter==999] <- NA

operc_season <- array(NA,dim=c(4,360,180))
operc_season[1,,] <- (operc[12,,]*precip_12mean_regrid[12,,]+operc[1,,]*precip_12mean_regrid[1,,]+operc[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
operc_season[2,,] <- (operc[3,,]*precip_12mean_regrid[3,,]+operc[4,,]*precip_12mean_regrid[4,,]+operc[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
operc_season[3,,] <- (operc[6,,]*precip_12mean_regrid[6,,]+operc[7,,]*precip_12mean_regrid[7,,]+operc[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
operc_season[4,,] <- (operc[9,,]*precip_12mean_regrid[9,,]+operc[10,,]*precip_12mean_regrid[10,,]+operc[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

rhs_mean_season <- array(NA,dim=c(4,360,180))
rhs_mean_season[1,,] <- (rhs_mean[12,,]*precip_12mean_regrid[12,,]+rhs_mean[1,,]*precip_12mean_regrid[1,,]+rhs_mean[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
rhs_mean_season[2,,] <- (rhs_mean[3,,]*precip_12mean_regrid[3,,]+rhs_mean[4,,]*precip_12mean_regrid[4,,]+rhs_mean[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
rhs_mean_season[3,,] <- (rhs_mean[6,,]*precip_12mean_regrid[6,,]+rhs_mean[7,,]*precip_12mean_regrid[7,,]+rhs_mean[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
rhs_mean_season[4,,] <- (rhs_mean[9,,]*precip_12mean_regrid[9,,]+rhs_mean[10,,]*precip_12mean_regrid[10,,]+rhs_mean[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

sst_mean_season <- array(NA,dim=c(4,360,180))
sst_mean_season[1,,] <- (sst_mean[12,,]*precip_12mean_regrid[12,,]+sst_mean[1,,]*precip_12mean_regrid[1,,]+sst_mean[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
sst_mean_season[2,,] <- (sst_mean[3,,]*precip_12mean_regrid[3,,]+sst_mean[4,,]*precip_12mean_regrid[4,,]+sst_mean[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
sst_mean_season[3,,] <- (sst_mean[6,,]*precip_12mean_regrid[6,,]+sst_mean[7,,]*precip_12mean_regrid[7,,]+sst_mean[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
sst_mean_season[4,,] <- (sst_mean[9,,]*precip_12mean_regrid[9,,]+sst_mean[10,,]*precip_12mean_regrid[10,,]+sst_mean[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

operc1c_season <- array(NA,dim=c(4,360,180))
operc1c_season[1,,] <- (operc1c[12,,]*precip_12mean_regrid[12,,]+operc1c[1,,]*precip_12mean_regrid[1,,]+operc1c[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
operc1c_season[2,,] <- (operc1c[3,,]*precip_12mean_regrid[3,,]+operc1c[4,,]*precip_12mean_regrid[4,,]+operc1c[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
operc1c_season[3,,] <- (operc1c[6,,]*precip_12mean_regrid[6,,]+operc1c[7,,]*precip_12mean_regrid[7,,]+operc1c[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
operc1c_season[4,,] <- (operc1c[9,,]*precip_12mean_regrid[9,,]+operc1c[10,,]*precip_12mean_regrid[10,,]+operc1c[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

rhs1c_mean_season <- array(NA,dim=c(4,360,180))
rhs1c_mean_season[1,,] <- (rhs1c_mean[12,,]*precip_12mean_regrid[12,,]+rhs1c_mean[1,,]*precip_12mean_regrid[1,,]+rhs1c_mean[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
rhs1c_mean_season[2,,] <- (rhs1c_mean[3,,]*precip_12mean_regrid[3,,]+rhs1c_mean[4,,]*precip_12mean_regrid[4,,]+rhs1c_mean[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
rhs1c_mean_season[3,,] <- (rhs1c_mean[6,,]*precip_12mean_regrid[6,,]+rhs1c_mean[7,,]*precip_12mean_regrid[7,,]+rhs1c_mean[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
rhs1c_mean_season[4,,] <- (rhs1c_mean[9,,]*precip_12mean_regrid[9,,]+rhs1c_mean[10,,]*precip_12mean_regrid[10,,]+rhs1c_mean[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

sst1c_mean_season <- array(NA,dim=c(4,360,180))
sst1c_mean_season[1,,] <- (sst1c_mean[12,,]*precip_12mean_regrid[12,,]+sst1c_mean[1,,]*precip_12mean_regrid[1,,]+sst1c_mean[2,,]*precip_12mean_regrid[2,,])/
  (precip_12mean_regrid[12,,]+precip_12mean_regrid[1,,]+precip_12mean_regrid[2,,])
sst1c_mean_season[2,,] <- (sst1c_mean[3,,]*precip_12mean_regrid[3,,]+sst1c_mean[4,,]*precip_12mean_regrid[4,,]+sst1c_mean[5,,]*precip_12mean_regrid[5,,])/
  (precip_12mean_regrid[3,,]+precip_12mean_regrid[4,,]+precip_12mean_regrid[5,,])
sst1c_mean_season[3,,] <- (sst1c_mean[6,,]*precip_12mean_regrid[6,,]+sst1c_mean[7,,]*precip_12mean_regrid[7,,]+sst1c_mean[8,,]*precip_12mean_regrid[8,,])/
  (precip_12mean_regrid[6,,]+precip_12mean_regrid[7,,]+precip_12mean_regrid[8,,])
sst1c_mean_season[4,,] <- (sst1c_mean[9,,]*precip_12mean_regrid[9,,]+sst1c_mean[10,,]*precip_12mean_regrid[10,,]+sst1c_mean[11,,]*precip_12mean_regrid[11,,])/
  (precip_12mean_regrid[9,,]+precip_12mean_regrid[10,,]+precip_12mean_regrid[11,,])

operc_season_rt <- abind(operc_season[,181:360,],operc_season[,1:180,],along=2)
rhs_mean_season_rt <- abind(rhs_mean_season[,181:360,],rhs_mean_season[,1:180,],along=2)
sst_mean_season_rt <- abind(sst_mean_season[,181:360,],sst_mean_season[,1:180,],along=2)
operc1c_season_rt <- abind(operc1c_season[,181:360,],operc1c_season[,1:180,],along=2)
rhs1c_mean_season_rt <- abind(rhs1c_mean_season[,181:360,],rhs1c_mean_season[,1:180,],along=2)
sst1c_mean_season_rt <- abind(sst1c_mean_season[,181:360,],sst1c_mean_season[,1:180,],along=2)
land_filter_rt <- abind(land_filter[181:360,],land_filter[1:180,],along=1)

# left
plt <- t((operc_season_rt[1,,]+(1-operc_season_rt[1,,])*operc1c_season_rt[1,,])*land_filter_rt)*100
par(mar=c(28,1.5,1,35.5))
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=seq(0,100,20),col=colors1,zlim=c(0,100))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
text(-148,-50,"DJF",cex=0.9,pos=4)
rect(-140,55,-120,73,col="white",border=NA)
text(-130,64,"(a)",cex=0.9)
msk1 <- plt

plt <- t((operc_season_rt[2,,]+(1-operc_season_rt[2,,])*operc1c_season_rt[2,,])*land_filter_rt)*100
par(mar=c(20,1.5,9,35.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=seq(0,100,20),col=colors1,zlim=c(0,100))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
text(-148,-50,"MAM",cex=0.9,pos=4)
msk2 <- plt

plt <- t((operc_season_rt[3,,]+(1-operc_season_rt[3,,])*operc1c_season_rt[3,,])*land_filter_rt)*100
par(mar=c(12,1.5,17,35.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=seq(0,100,20),col=colors1,zlim=c(0,100))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
text(-148,-50,"JJA",cex=0.9,pos=4)
msk3 <- plt

plt <- t((operc_season_rt[4,,]+(1-operc_season_rt[4,,])*operc1c_season_rt[4,,])*land_filter_rt)*100
par(mar=c(4,1.5,25,35.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=seq(0,100,20),col=colors1,zlim=c(0,100))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
text(-148,-50,"SON",cex=0.9,pos=4)
msk4 <- plt

par(mar=c(2.5,4.5,33.1,38.5),new=T)
image(x=0:5,z=t(t(1:5)),col=colors1,xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",axes=F)
axis(1,seq(0,5,1),labels=seq(0,100,20),tck=-0.4,padj=-2.7,cex.axis=0.7,lwd=0.5)
mtext(side=1,text="OMS (direct + 1st cascading) ratio (%)",cex=0.7,line=0.6)
box(lwd=0.5)

# right
plt <- t((operc_season_rt[1,,]*rhs_mean_season_rt[1,,]+(1-operc_season_rt[1,,])*operc1c_season_rt[1,,]*rhs1c_mean_season_rt[1,,])/(operc_season_rt[1,,]+(1-operc_season_rt[1,,])*operc1c_season_rt[1,,])*land_filter_rt)
par(mar=c(28,36.5,1,0.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(0,seq(50,90,5),110),col=colors2,zlim=c(0,110))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
rect(-140,55,-120,73,col="white",border=NA)
text(-130,64,"(c)",cex=0.9)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk1)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[2,,]*rhs_mean_season_rt[2,,]+(1-operc_season_rt[2,,])*operc1c_season_rt[2,,]*rhs1c_mean_season_rt[2,,])/(operc_season_rt[2,,]+(1-operc_season_rt[2,,])*operc1c_season_rt[2,,])*land_filter_rt)
par(mar=c(20,36.5,9,0.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(0,seq(50,90,5),110),col=colors2,zlim=c(0,110))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk2)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[3,,]*rhs_mean_season_rt[3,,]+(1-operc_season_rt[3,,])*operc1c_season_rt[3,,]*rhs1c_mean_season_rt[3,,])/(operc_season_rt[3,,]+(1-operc_season_rt[3,,])*operc1c_season_rt[3,,])*land_filter_rt)
par(mar=c(12,36.5,17,0.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(0,seq(50,90,5),110),col=colors2,zlim=c(0,110))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk3)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[4,,]*rhs_mean_season_rt[4,,]+(1-operc_season_rt[4,,])*operc1c_season_rt[4,,]*rhs1c_mean_season_rt[4,,])/(operc_season_rt[4,,]+(1-operc_season_rt[4,,])*operc1c_season_rt[4,,])*land_filter_rt)
par(mar=c(4,36.5,25,0.5),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(0,seq(50,90,5),110),col=colors2,zlim=c(0,110))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk4)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

par(mar=c(2.5,37.5,33.1,1.5),new=T)
image(x=0:10,z=t(t(1:10)),col=colors2,xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",axes=F)
axis(1,seq(1,9,1),labels=seq(50,90,5),tck=-0.4,padj=-2.7,cex.axis=0.7,lwd=0.5)
mtext(side=1,text="OMS (direct + 1st cascading) RH (%)",cex=0.7,line=0.6)
box(lwd=0.5)

# middle
plt <- t((operc_season_rt[1,,]*sst_mean_season_rt[1,,]+(1-operc_season_rt[1,,])*operc1c_season_rt[1,,]*sst1c_mean_season_rt[1,,])/(operc_season_rt[1,,]+(1-operc_season_rt[1,,])*operc1c_season_rt[1,,])*land_filter_rt)
par(mar=c(28,19,1,18),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(-5,seq(5,25,5),35),col=colors3,zlim=c(-5,35))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
rect(-140,55,-120,73,col="white",border=NA)
text(-130,64,"(b)",cex=0.9)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk1)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[2,,]*sst_mean_season_rt[2,,]+(1-operc_season_rt[2,,])*operc1c_season_rt[2,,]*sst1c_mean_season_rt[2,,])/(operc_season_rt[2,,]+(1-operc_season_rt[2,,])*operc1c_season_rt[2,,])*land_filter_rt)
par(mar=c(20,19,9,18),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(-5,seq(5,25,5),35),col=colors3,zlim=c(-5,35))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk2)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[3,,]*sst_mean_season_rt[3,,]+(1-operc_season_rt[3,,])*operc1c_season_rt[3,,]*sst1c_mean_season_rt[3,,])/(operc_season_rt[3,,]+(1-operc_season_rt[3,,])*operc1c_season_rt[3,,])*land_filter_rt)
par(mar=c(12,19,17,18),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(-5,seq(5,25,5),35),col=colors3,zlim=c(-5,35))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk3)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

plt <- t((operc_season_rt[4,,]*sst_mean_season_rt[4,,]+(1-operc_season_rt[4,,])*operc1c_season_rt[4,,]*sst1c_mean_season_rt[4,,])/(operc_season_rt[4,,]+(1-operc_season_rt[4,,])*operc1c_season_rt[4,,])*land_filter_rt)
par(mar=c(4,19,25,18),new=T)
plot(NA,NA,xlim=c(-140,160),ylim=c(-60,75),xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",asp=1)
plot(raster(plt,xmn=-180,xmx=180,ymn=-90,ymx=90),
     add=T,legend=F,breaks=c(-5,seq(5,25,5),35),col=colors3,zlim=c(-5,35))
axis(1,at=seq(-120,120,60),labels=parse(text=degreeLabelsEW(seq(-120,120,60))),padj=-4,cex.axis=0.5,tck=-0.02)
axis(2,at=seq(-50,70,20),labels=parse(text=degreeLabelsNS(seq(-50,70,20))),hadj=0.2,cex.axis=0.5,tck=-0.02,las=2)
plot(worldmap,add=T,col="black",lwd=0.5);plot(lakemap,add=T,col="black",lwd=0.5)
box(lwd=1)
for (j in seq(-179,269,5)){
  x <- seq(j,-180,-1)
  y <- seq(90,90-length(x)+1,-1)
  cut <- which((x<= 179)&(y>= -89))
  x <- x[cut]
  y <- y[cut]
  z <- rep(NA,length(x))
  for (xx in 1:length(x)){
    z[xx] <- t(msk4)[which(seq(-180,179,1)==x[xx]),which(seq(90,-89,-1)==y[xx])]
  }
  cut <- which((z>40)|(is.na(z)))
  y[cut] <- NA
  lines(x,y)
}

par(mar=c(2.5,22,33.1,21),new=T)
image(x=0:6,z=t(t(1:6)),col=colors3,xaxt="n",yaxt="n",xaxs="i",yaxs="i",xlab="",ylab="",axes=F)
axis(1,seq(1,5,1),labels=seq(5,25,5),tck=-0.4,padj=-2.7,cex.axis=0.7,lwd=0.5)
mtext(side=1,text="OMS (direct + 1st cascading) SST (\u00B0C)",cex=0.7,line=0.6)
box(lwd=0.5)
save.image("~/UTrack/plot_1c.RData")