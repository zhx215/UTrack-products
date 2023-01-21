rm(list=ls())
memory.limit(size=40000) # R version 2021.09.0
library(raster)
library(ncdf4)

#
nc_data <- nc_open("C:\\Users\\zhyxi\\Documents\\UTrack\\era_sst.nc")
lon <- ncvar_get(nc_data,"longitude") # 0 ~ 359.75
lat <- ncvar_get(nc_data,"latitude") # 90 ~ -90
sst.array <- ncvar_get(nc_data,"sst")
nc_data <- nc_open("C:\\Users\\zhyxi\\Documents\\UTrack\\era_dew.nc")
d2m.array <- ncvar_get(nc_data,"d2m")
nc_data <- nc_open("C:\\Users\\zhyxi\\Documents\\UTrack\\era_precip.nc")
precip.array <- ncvar_get(nc_data,"tp")
nc_data <- nc_open("C:\\Users\\zhyxi\\Documents\\UTrack\\era_evap.nc")
evap.array <- ncvar_get(nc_data,"e")

# sst
sst_12mean <- array(NA,dim=c(12,1440,721))
for (i in 1:12){
  sst_12mean[i,,] <- rowMeans(sst.array[,,seq(i,120,12)],dims=2)-273.15
}

# d2m
d2m_12mean <- array(NA,dim=c(12,1440,721))
for (i in 1:12){
  d2m_12mean[i,,] <- rowMeans(d2m.array[,,seq(i,120,12)],dims=2)-273.15
}

# rhs
get_rh <- function(td,t){
  return(exp(17.67*td/(td+243.5)-17.67*t/(t+243.5))*100)
}
rhs_12mean <- get_rh(d2m_12mean,sst_12mean)
rhs_12mean[!is.na(rhs_12mean)&(rhs_12mean>110)] <- 110

# precip
precip_12mean <- array(NA,dim=c(12,1440,721))
for (i in 1:12){
  precip_12mean[i,,] <- rowMeans(precip.array[,,seq(i,120,12)],dims=2)
}

# evap
evap_12mean <- array(NA,dim=c(12,1440,721))
for (i in 1:12){
  evap_12mean[i,,] <- -rowMeans(evap.array[,,seq(i,120,12)],dims=2)
}
evap_12mean[evap_12mean<0] <- 0

#
sst_12mean_regrid <- rhs_12mean_regrid <- 
  precip_12mean_regrid <- evap_12mean_regrid <- array(NA,dim=c(12,360,180))

for (i in 1:12){
  for (j in 1:360){
    for (k in 1:180){
      id <- which(!is.na(as.vector(sst_12mean[i,((j-1)*4+1):(j*4),((k-1)*4+1):(k*4)])))
      if (length(id) >= 8){
        sst_12mean_regrid[i,j,k] <- 
          mean(as.vector(sst_12mean[i,
                                    ((j-1)*4+1):(j*4),
                                    ((k-1)*4+1):(k*4)])[id])
        rhs_12mean_regrid[i,j,k] <- 
          mean(as.vector(rhs_12mean[i,
                                    ((j-1)*4+1):(j*4),
                                    ((k-1)*4+1):(k*4)])[id])
        precip_12mean_regrid[i,j,k] <- 
          mean(as.vector(precip_12mean[i,
                                       ((j-1)*4+1):(j*4),
                                       ((k-1)*4+1):(k*4)])[id])
        evap_12mean_regrid[i,j,k] <- 
          mean(as.vector(evap_12mean[i,
                                     ((j-1)*4+1):(j*4),
                                     ((k-1)*4+1):(k*4)])[id])
      }
      else {
        sst_12mean_regrid[i,j,k] <- 
          mean(as.vector(sst_12mean[i,
                                    ((j-1)*4+1):(j*4),
                                    ((k-1)*4+1):(k*4)]))
        rhs_12mean_regrid[i,j,k] <- 
          mean(as.vector(rhs_12mean[i,
                                    ((j-1)*4+1):(j*4),
                                    ((k-1)*4+1):(k*4)]))
        precip_12mean_regrid[i,j,k] <- 
          mean(as.vector(precip_12mean[i,
                                       ((j-1)*4+1):(j*4),
                                       ((k-1)*4+1):(k*4)]))
        evap_12mean_regrid[i,j,k] <- 
          mean(as.vector(evap_12mean[i,
                                     ((j-1)*4+1):(j*4),
                                     ((k-1)*4+1):(k*4)]))
      }
    }
  }
}

#
rm(list=setdiff(ls(),
                c("sst_12mean_regrid",
                  "rhs_12mean_regrid",
                  "precip_12mean_regrid",
                  "evap_12mean_regrid")))
save.image(file="C:\\Users\\zhyxi\\Documents\\UTrack\\ERA3_data.RData")