## Checks if all required packages are installed. If not, packages and all dependencies will be installed from the default CRAN mirror.

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


pkgTest("gmt")
pkgTest("SDMTools")
pkgTest("ncdf4")
pkgTest("RCurl")
pkgTest("raster")
pkgTest("data.table")
pkgTest("lunar")
pkgTest("R.utils")
pkgTest("RColorBrewer")
pkgTest("gbm")
pkgTest("colorRamps")
pkgTest("maps")
pkgTest("mapdata")
pkgTest("tweedie")
pkgTest("mgcv")
pkgTest("gamm4")
pkgTest("sp")
pkgTest("rgdal")
pkgTest("fields")
pkgTest("maptools")
pkgTest("adehabitatHR")
pkgTest("adehabitatHS")
pkgTest("adehabitatLT")
pkgTest("adehabitatMA")
pkgTest("tidyverse")
pkgTest("magrittr")
pkgTest("dismo")
pkgTest("caret")
pkgTest("mlbench")


