## Functions to support 3_extract_environmental_data_at_records.R

# convert -180, 180 --> 0, 360
make360 <- function(lon){
  isnot360<-min(lon)<0
   if(isnot360){
    ind<-which(lon<0)
    lon[ind]<-lon[ind]+360
  }  
  return(lon)
}

# get dates with data in a list of netcdfs
getDateRange <- function(inpts, ncIn){
  parsedt <- vector(mode='character', length(ncIn))
  for (j in 1:length(ncIn)){
    if (varname=='sla'){
      nc <- ncIn[j]
      parsename <- unlist(strsplit(nc,'_'))
      dtname <- substr(parsename[7],1,8)
      parsedt[j] <- as.character(strptime(dtname,format='%Y%m%d',tz='UTC')) 
    } else {
      nc <- ncIn[j]
      parsename <- unlist(strsplit(nc,'_'))
      parsedt[j] <- substr(parsename[abs(length(parsename))],1,10) 
      }
    }
  startD <- format(as.POSIXct(min(inpts$dt), tz='UTC'), '%Y-%m-%d')
  print(paste('Ptsfile start', startD))
  endD <- format(as.POSIXct(max(inpts$dt), tz='UTC'), '%Y-%m-%d')
  print(paste('Ptsfile end', endD))
  if (varname=='sla'){
    startfile <- which(abs(as.Date(parsedt)-as.Date(startD)) == min(abs(as.Date(parsedt)-as.Date(startD))))[1]
    endfile <- which(abs(as.Date(endD)-as.Date(parsedt)) == min(abs(as.Date(endD)-as.Date(parsedt))))[1]
  } else {
    startfile <- which(abs(as.Date(parsedt)-as.Date(startD)) == min(abs(as.Date(parsedt)-as.Date(startD))))[1]-1
    endfile <- which(abs(as.Date(endD)-as.Date(parsedt)) == min(abs(as.Date(endD)-as.Date(parsedt))))[1]
  }
  dtIn <- ncIn[startfile:endfile]
  return(dtIn)
}

# extract data in netcdf list to species records
getvar <- function(nc,varname,ptsfile,pt.radius,alt){
  inpts <- ptsfile
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d', tz='UTC')
  if (varname=='sla'){  ### AVISO SLA SPECIFIC
    nc.data <- nc_open(nc, write=FALSE)
    lat <- ncvar_get(nc.data,'lat')
    lon <- ncvar_get(nc.data,'lon')
    tim <- ncvar_get(nc.data,'time')
    day <- as.Date(tim,origin='1950-01-01',tz= "UTC")
    startdt <- as.POSIXct(as.character(day),format='%Y-%m-%d',tz='UTC')
    enddt <- seq(startdt,length=2,by='1 day')[2]
  } else {
    nc.data <- nc_open(nc, write=FALSE)
    parsename <- unlist(strsplit(nc,'_'))
    parsedt <- substr(parsename[abs(length(parsename))],1,10)
    startdt <- as.POSIXct(parsedt,'%Y-%m-%d',tz='UTC')
    lat <- ncvar_get(nc.data,'latitude')
    lon <- ncvar_get(nc.data,'longitude')
    tim <- ncvar_get(nc.data,'time')
    day <- as.POSIXlt(tim,origin='1970-01-01',tz= "UTC")
    enddt <- as.Date(day[length(day)])
  }
  if (max(lon)>180) inpts$lon<-inpts$lon+360 
  data.var <-  ncvar_get(nc.data,varname)
  nrows <- length(lon); ncols <- length(lat)
  res <- ncatt_get(nc.data,varid=0)
  if (varname=='sea_surface_temperature') {
    pixel.radius <- ceiling(pt.radius/as.numeric(unlist(strsplit(res$spatial_resolution,' '))[1]))
    } else {
    pixel.radius <- ceiling(pt.radius/as.numeric(res$geospatial_lon_resolution))
  } 
  xtract <- inpts[which(as.Date(inpts$dt) >= as.Date(startdt) & as.Date(inpts$dt) < as.Date(enddt)),]
  xtract$lon[xtract$lon<=-180] <- NA; xtract$lat[is.na(xtract$lon)] <- NA        
  if (length(xtract$lat)>0) {
    for(i in 1:nrow(xtract)) {           
      print(Sys.time()) 
      xdate <- xtract$dt[i] 
      index1 <- which(abs(as.Date(xdate)-as.Date(day)) == min(abs(as.Date(xdate)-as.Date(day))))[1]
        if (length(index1)==0) {
          print('No matching NetCDF') 
          } else {
            c<-which(abs(lat-xtract$lat[i])==min(abs(lat-xtract$lat[i])))
            r<-which(abs(lon-xtract$lon[i])==min(abs(lon-xtract$lon[i])))
            row1 <- ceiling(max(r-pixel.radius,1))
            numrows <- ceiling(min(2*+pixel.radius+1,nrows-row1+1))        
            col1 <- ceiling(max(c-pixel.radius,1))                          
            numcols <- ceiling(min(2*+pixel.radius+1,1+ncols-col1+1)) 
            newlon <- lon[row1:(row1+numrows-1)]
            newlat <- lat[col1:(col1+numcols-1)]
          }          
        if ((nrows > row1+numrows) && (nrows > r+1) && (ncols > col1+numcols) && (ncols > c+1)) {
          if (nc.data$ndims == 3 || varname=='sla') {        
            data.var <-  ncvar_get(nc.data,varname,start=c(row1,col1,index1),count=c(numrows,numcols,1),verbose=FALSE) 
            } else {
            data.var <- ncvar_get(nc.data,varname,start=c(row1,col1,alt,index1),count=c(numrows,numcols,alt,1),verbose=FALSE)
          }
          var.mat <- matrix(data.var, numrows, numcols, dimnames=list(newlon,newlat))  
          xtract[i,varname] <- var.mat[ceiling(numrows/2),ceiling(numcols/2)]
          xtract[i,paste(varname,'_mean', sep='')]<- mean(var.mat, na.rm=TRUE) ## mean over whole radius
          xtract[i,paste(varname,'_sd', sep='')]<- sd(var.mat, na.rm=TRUE) ## sd over whole radius
        } else {
          xtract[i,varname] <- NA
          xtract[i,paste(varname,'_mean', sep='')]<- NA 
          xtract[i,paste(varname,'_sd', sep='')]<- NA        
        }
        if (is.na(xtract$lat[i]) & is.na(xtract$lon[i])) {
            xtract[i,varname] <- NA
            xtract[i,paste(varname,'_mean', sep='')]<- NA  
            xtract[i,paste(varname,'_sd', sep='')]<- NA 
        }
      }
    } 
    nc_close(nc.data)
    return(xtract)
}   

# extract depth in raster to species records
getZ <- function(inpts,rastIn){
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  inpts2$lon[inpts2$lon<=-180]<- -180
  coordinates(inpts2) <- c('lon','lat')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  z <- extract(rastIn,cbind(inpts2$lon,inpts2$lat),method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,z)
  EOVout$z[EOVout$lon<=-180] <- NA
  rm(inpts2,inpts); return(EOVout)
}

# extract the spatial standard devaition of depth to species records
getZsd <- function(inpts,rastIn){
  rastIn <- raster(rastIn)
  inpts2 <- inpts
  inpts2$lon[inpts2$lon<=-180]<- -180
  coordinates(inpts2) <- c('lon','lat')
  proj4string(inpts2) <- CRS("+proj=longlat +ellps=WGS84")
  zsd <- extract(rastIn,inpts2@coords,method='simple',na.rm=F,cellnumbers=F)
  EOVout <- cbind(inpts,zsd)
  EOVout$zsd[EOVout$lon<=-180] <- NA
  rm(inpts2,inpts); return(EOVout)
}
