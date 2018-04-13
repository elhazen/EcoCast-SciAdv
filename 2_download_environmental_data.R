## Downloads relevant environmental data as netcdfs from CoastWatch-West Coast ERDDAP: https://coastwatch.pfeg.noaa.gov/erddap/index.html
# note: this script only contains code to download a subset of environmental products

waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
}


# download netcdfs

#Pathfinder 4k - http://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.html
dates<-seq(as.Date("1991/10/01"), as.Date("2010/7/31"), by = "month",format="%Y/%mm/%dd")
i<-1
waitsecs<-2

while (i < length(dates)){
	startdate<-dates[i]
	enddate<-dates[i+1]
	filenm<-paste("nodcPH2sstd1day_",startdate,".nc",sep="")
	url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nodcPH2sstd1day.nc?sea_surface_temperature[(",startdate,"):1:(",enddate,")][(60):1:(10)][(-150):1:(-110)]",sep="")
	print(startdate)
	f = CFILE(filenm,mode="wb")
	curlPerform(url=url,writedata=f@ref) 
 	close(f)
	i<-i+1
	if (is.na(file.info(filenm)$size)) {
		i<-i-1
		waitfor(waitsecs)
		waitsecs<-waitsecs+2
	}
	else if (file.info(filenm)$size < 2000){
		i<-i-1
		waitfor(waitsecs)
		waitsecs<-waitsecs+2
	}
	else waitsecs<-2
	if (waitsecs > 90) waitsecs <- 30
}

#GHRSST - http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.html
dates<-seq(as.Date("2010/07/01"), as.Date("2015/12/31"), by = "week",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
	startdate<-dates[i]
	enddate<-dates[i+1]
	filenm<-paste("jplG1SST_",startdate,".nc",sep="")
	url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(",startdate,"):1:(",enddate,")][(10):1:(60)][(-150):1:(-110)]",sep="")
	print(startdate)
	f = CFILE(filenm,mode="wb")
	curlPerform(url=url,writedata=f@ref) 
	close(f)
		i<-i+1
	if (is.na(file.info(filenm)$size)) {
		i<-i-1
	}
	else if (file.info(filenm)$size < 2000){
		i<-i-1
	}
}

#MODIS chla - http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day.nc?chlorophyll[(2014-05-01):1:(2016-06-05T00:00:00Z)][(0.0):1:(0.0)][(10):1:(60)][(210):1:(250)]
dates<-seq(as.Date("2014/05/01"), as.Date("2015/12/31"), by = "week",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
  startdate<-dates[i]
  enddate<-dates[i+1]
  filenm<-paste("erdMBchla8day_",startdate,".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day.nc?chlorophyll[(",startdate,"):1:(",enddate,")][(0.0):1:(0.0)][(10):1:(60)][(210):1:(250)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
    i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}

#Wind - http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwind8day.nc?y_wind[(2016-06-05T00:00:00Z):1:(2016-06-05T00:00:00Z)][(10.0):1:(10.0)][(10):1:(60)][(210):1:(250)]
dates<-seq(as.Date("2009/11/01"), as.Date("2016/05/31"), by = "month",format="%Y/%mm/%dd")
i<-1
while (i < length(dates)){
  startdate<-dates[i]
  enddate<-dates[i+1]
  filenm<-paste("erdQAwind8day_",startdate,".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwind8day.nc?y_wind[(",startdate,"):1:(",enddate,")][(10.0):1:(10.0)][(10):1:(60)][(210):1:(250)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
    i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}




