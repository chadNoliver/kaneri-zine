# api data
# load.dependencies
library(tidyverse)
library(arcpullr)
library(leaflet)
library(lubridate)

TOKEN <- read.delim(file="private/IRWIN Api Key.txt",header=FALSE)
startDate=as.POSIXct("2020-12-31 23:59:59", tz="GMT","%Y-%m-%d %H:%M:%S");
startDateMs=as.double(startDate)*1000
URL <- "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/IRWIN_Incidents/FeatureServer/0/"
irwin <- get_spatial_layer(url=URL,
                        token = TOKEN,
                        where = paste0("CreatedOnDateTime > ",startDateMs)
)
save(irwin, file="irwin.RData")
