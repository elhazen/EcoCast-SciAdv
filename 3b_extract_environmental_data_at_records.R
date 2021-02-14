## Extracts downloaded environmental data to species records
# load functions in 3a_extract_functions.R

## Variable metadata and name structure
# name: jplL4Avhrr, variable: SST, metadata: GHRSST Level 4 AVHRR_OI Global Blended Sea Surface Temperature Analysis, Global, 0.25 Degree, Daily
# name: erdSWchla, variable: CHL-A, metadata: Chlorophyll-a, Orbview-2 SeaWiFS, Global (8 Day Composite)
# name: erdMHchla, variable: CHL-A, metadata: Chlorophyll-a, Aqua MODIS, NPP, Global, Science Quality (8 Day Composite)
# name: MADTu, variable: eastward water velocity, metadata: DT merged Global Ocean Gridded Geostrophic Velocities SSALTO/Duacs L4 product
# name: MADTv, variable: northward water velocity, metadata: DT merged Global Ocean Gridded Geostrophic Velocities SSALTO/Duacs L4 product
# name: MADTh, variable: SSH, metadata: DT merged Global Ocean Gridded sea Absolute Dynamic Topography SSALTO/Duacs L4 product
# name: ncdcOwDly, variable: SSW, metadata: NOAA/NCDC Blended Daily 0.25-degree Sea Surface Winds
# name: erdPHsst, variable: SST, metadata: SST, Pathfinder Ver 5.0, Day and Night, 4.4 km, Global, Science Quality (8 Day Composite)
# name: sla, variable: SSH, metadata: Delayed-Time Level-4 sea surface height above Mean Sea Surface products from multi-satellite observations
# name: fsle_max, variable: FSLE, metadata: Backward-in-time FSLE product deduced from DT merged Global Ocean Gridded Absolute Geostrophic Velocities SSALTO/Duacs L4 product (version 2014)
# name: z, variable: bathymetry, metadata: Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), (Ice Sheet Surface)
# name: zsd, variable: bathymetry (sd), metadata: Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), (Ice Sheet Surface)
# name: blendchla, variable: CHL-A, metadata: gaps in erdMHchla filled with erdSWchla
# name: l.blendchla, variable: CHL-A, metadata: log of blendchla
# name: eke, variable: eddy kinetic energy, metadata: 1/2*(AVISOu^2+AVISOv^2)
# name: eke_mean, variable: eddy kinetic energy, metadata: 1/2*(AVISOu_mean^2+AVISOv_mean^2)
# name: l.eke_mean, variable: eddy kinetic energy, metadata: log of eke_mean


## import species records
setwd("record_directory")
master=read.csv("species_records.csv").   ### species_records to have at least 3 columns - lat, lon, dt.
head(master)
summary(master$presabs) # 0s and 1s
master$dt=as.Date(master$dt) ## make sure date is in date format

## points for extraction, with latitude longitude and date as lat lon dt
obsdata <- master
obsdata$X <- seq(1, nrow(obsdata),by=1)
head(obsdata)

## list NetCDF files
ncpath <- "ncdf_path"  ## change for each user
list.ncs <- list.files(ncpath, full.names=T)

### REYNOLDS SST
filenames<-grep("jpl",list.ncs)
fl<-list.ncs[filenames]
ptradius=0.25
alt<-NA
varname<-"analysed_sst" 
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

jplL4Avhrr <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius),fill=TRUE)
obsdata$jplL4Avhrr<-rep(NA, nrow(obsdata))
obsdata$jplL4Avhrr[jplL4Avhrr$X] <- jplL4Avhrr$analysed_sst # at point location
obsdata$jplL4Avhrr_mean<-rep(NA, nrow(obsdata))
obsdata$jplL4Avhrr_mean[jplL4Avhrr$X] <- jplL4Avhrr$analysed_sst_mean # mean calculated across a .25 radius
obsdata$jplL4Avhrr_sd<-rep(NA, nrow(obsdata))
obsdata$jplL4Avhrr_sd[jplL4Avhrr$X] <- jplL4Avhrr$analysed_sst_sd # sd calculated across a .25 radius

rm(jplL4Avhrr,fl)

### SeaWIFS chlorophyll
filenames<-grep("erdSWchla",list.ncs)
fl<-list.ncs[filenames]
ptradius<-0.25
alt<-1
varname<-"chlorophyll"
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

seawifs <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$erdSWchla<-rep(NA, length(obsdata[,1]))
obsdata$erdSWchla[seawifs$X]<-seawifs$chlorophyll
obsdata$erdSWchla_mean<-rep(NA, length(obsdata[,1])) 
obsdata$erdSWchla_mean[seawifs$X]<-seawifs$chlorophyll_mean
obsdata$erdSWchla_sd<-rep(NA, length(obsdata[,1])) 
obsdata$erdSWchla_sd[seawifs$X]<-seawifs$chlorophyll_sd

rm(seawifs,fl)

### MODIS
filenames<-grep("erdMHchla",list.ncs)
fl<-list.ncs[filenames]
ptradius<-0.25
alt<-1
varname<-"chlorophyll"
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

modis <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$erdMHchla<-rep(NA, length(obsdata[,1]))
obsdata$erdMHchla[modis$X]<-modis$chlorophyll
obsdata$erdMHchla_mean<-rep(NA, length(obsdata[,1])) 
obsdata$erdMHchla_mean[modis$X]<-modis$chlorophyll_mean
obsdata$erdMHchla_sd<-rep(NA, length(obsdata[,1])) 
obsdata$erdMHchla_sd[modis$X]<-modis$chlorophyll_sd

rm(modis,fl)

### AVISO u&v
filenames<-grep("aviso_delayed_time_madt_u",list.ncs)
fl<-list.ncs[filenames]
ptradius<-0.25 
alt<-NA 
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

varname<-"u"
AVISOu <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

varname<-"v"
AVISOv <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$MADTu<-rep(NA, nrow(obsdata))  ## don't need sd
obsdata$MADTu[AVISOu$X] <- AVISOu$u 
obsdata$MADTu_mean<-rep(NA, nrow(obsdata))
obsdata$MADTu_mean[AVISOu$X] <- AVISOu$u_mean

obsdata$MADTv<-rep(NA, nrow(obsdata))
obsdata$MADTv[AVISOv$X] <- AVISOv$v
obsdata$MADTv_mean<-rep(NA, nrow(obsdata))
obsdata$MADTv_mean[AVISOv$X] <- AVISOv$v_mean

rm(AVISOu,AVISOv,fl)

## AVISO ssh  
filenames<-grep("aviso_delayed_time_madt_h",list.ncs)
fl<-list.ncs[filenames]
ptradius<-0.25
alt<-1
varname<-"adt"
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

AVISOh <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$MADTh<-rep(NA, length(obsdata[,1]))
obsdata$MADTh[AVISOh$X]<-AVISOh$adt
obsdata$MADTh_mean<-rep(NA, length(obsdata[,1]))
obsdata$MADTh_mean[AVISOh$X]<-AVISOh$adt_mean
obsdata$MADTh_sd<-rep(NA, length(obsdata[,1]))
obsdata$MADTh_sd[AVISOh$X]<-AVISOh$adt_sd

rm(AVISOh,fl)

### QSCAT BLENDED WIND
filenames<-grep("ncdcOwDly",list.ncs)
fl<-list.ncs[filenames]
ptradius<- 0.25 
alt<-1
varname<-"v"
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

ywind <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$ncdcOwDly<-rep(NA, length(obsdata[,1])) 
obsdata$ncdcOwDly[ywind$X]<-ywind$v
obsdata$ncdcOwDly_mean <-rep(NA, length(obsdata[,1])) 
obsdata$ncdcOwDly_mean[ywind$X]<-ywind$v_mean

obsdata$ncdcOwDly[is.nan(obsdata$ncdcOwDly)] <- NA  #nan to NA
obsdata$ncdcOwDly_mean[is.nan(obsdata$ncdcOwDly_mean)] <- NA #nan to NA

rm(ywind,fl)

### Pathfinder SST
filenames=grep("erdPHsst",list.ncs)
fl<-list.ncs[filenames]
ptradius<- 0.25 
alt<-1
varname<-"sst"
fl <- getDateRange(inpts=obsdata,ncIn=fl)

path <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$erdPHsst<-rep(NA, length(obsdata[,1])) 
obsdata$erdPHsst[path$X]<-path$sst
obsdata$erdPHsst_mean <-rep(NA, length(obsdata[,1])) 
obsdata$erdPHsst_mean[path$X]<-path$sst_mean
obsdata$erdPHsst_sd <-rep(NA, length(obsdata[,1])) 
obsdata$erdPHsst_sd[path$X]<-path$sst_mean

obsdata$erdPHsst[is.nan(obsdata$erdPHsst)] <- NA  #nan to NA
obsdata$erdPHsst_mean[is.nan(obsdata$erdPHsst_mean)] <- NA #nan to NA
obsdata$erdPHsst_sd[is.nan(obsdata$erdPHsst_sd)] <- NA  #nan to NA

rm(path,fl)

### SEA LEVEL ANOMALY
fl <- list.files(ncpath,all.files=TRUE,include.dirs=TRUE,recursive=TRUE,full.names=TRUE)
fl <- fl[grep('msla',fl)]
varname='sla'
ptradius=0.25  
alt=NA
fl <- getDateRange(inpts=obsdata,ncIn=fl) 

sla <- rbindlist(lapply(fl,FUN=getvar,varname=varname,ptsfile=obsdata,pt.radius=ptradius,alt=alt),fill=TRUE)

obsdata$sla<-rep(NA, nrow(obsdata))
obsdata$sla[sla$X]<-sla$sla
obsdata$sla_mean<-rep(NA, nrow(obsdata))
obsdata$sla_mean[sla$X]<-sla$sla_mean
obsdata$sla_sd<-rep(NA, nrow(obsdata))
obsdata$sla_sd[sla$X]<-sla$sla_sd

rm(sla,fl)

### ETOPO1 BATHYMETRY
obsdata$z <- c()
obsdata$zsd <- c()
rastIn <- "/Volumes/SeaGate/ERD_DOM/ETOPO1_bathy/ETOPO1_Bed_g_geotiff.tif" ## change for each user
obsdata <- getZ(inpts=obsdata,rastIn=rastIn)
rastIn <- '/Volumes/SeaGate/ERD_DOM/ETOPO1_bathy/ETOPO1_CCS_bathySD.grd' ## change for each user
obsdata <- getZsd(inpts=obsdata,rastIn=rastIn)

### lunillum
obsdata$lunillum <- lunar.illumination(obsdata$dt)

### l.blendChl
obsdata$blendChl <- obsdata$erdMHchla
obsdata$blendChl[which(is.na(obsdata$erdMHchla) & !is.na(obsdata$erdSWchla))] <- obsdata$erdSWchla[which(is.na(obsdata$erdMHchla) & !is.na(obsdata$erdSWchla))]
obsdata$l.blendChl <- log(obsdata$blendChl + 0.001)

obsdata$blendChl_mean <- obsdata$erdMHchla_mean
obsdata$blendChl_mean[which(is.na(obsdata$erdMHchla_mean) & !is.na(obsdata$erdSWchla_mean))] <- obsdata$erdSWchla_mean[which(is.na(obsdata$erdMHchla_mean) & !is.na(obsdata$erdSWchla_mean))]
obsdata$l.blendChl_mean <- log(obsdata$blendChl_mean + 0.001)

### l.eke_mean
obsdata$eke<-1/2*(obsdata$MADTu^2+obsdata$MADTv^2)
obsdata$l.eke <- log(obsdata$eke + 0.001)
obsdata$eke_mean=1/2*(obsdata$MADTu_mean^2+obsdata$MADTv_mean^2)
obsdata$l.eke_mean <- log(obsdata$eke_mean + 0.001)

head(obsdata)
summary(obsdata)
saveRDS(obsdata, "outdir/obsdata.rds") ## change for each user
