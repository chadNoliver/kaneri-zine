#@title access-data.R
#@author Chad Oliver
#@created 2021.01.17

#Accesses data: Assumes data stored in ~/private directory

#load dependencies
library("readxl")
library("tibble")
library("sf")
library("tidycensus")
library("dplyr")
library("leaflet")
library("arcpullr")
options(tigris_use_cache = TRUE)
key <- read.delim(file="private/Census Api Key.txt",header=FALSE)
Sys.setenv(census_api_key=key[[1]])


DIR <-  "./private/FOIA-20220118T034550Z-001/FOIA/FA-FOIA-2021-002_Final Response/Responsive Records";
F1 <-  "InFORM_BLM_FireReports_2020.xlsx";
F2 <-  "InFORM_BLM_Protecting_FireReports_2020.xlsx";
F3 <-  "WFMI_BLM_FireReports_2010_2019.xlsx";
#print(exists(DIR))


df1 <-  as.data.frame(readxl::read_excel(path=paste(DIR,F1,sep="/")))
df2 <-  as.data.frame(readxl::read_excel(path=paste(DIR,F2,sep="/")))
df3 <-  as.data.frame(readxl::read_excel(path=paste(DIR,F3,sep="/")))
#df1_st <- st_as_sf(df1,coords = c("Latitude", "Longitude"),remove=FALSE)
#df2_st <- st_as_sf(df2,coords = c("Latitude", "Longitude"),remove=FALSE)

library(tidyverse)

geo <- st_as_sf(tidycensus::get_estimates(year=2019, geography="county", geometry=TRUE, product="population")) %>%
  filter(variable=="POP") %>%
  st_transform(2163)

m <- df3 %>%
  drop_na(`Latitude NAD83`,`Longitude NAD83`) %>%
  st_as_sf(coords = c("Longitude NAD83", "Latitude NAD83"),crs = 4269,remove=FALSE) %>%
  st_transform(2163)%>%
  mutate(intersection = as.integer(st_intersects(geometry, geo)),
  geo_name = if_else(is.na(intersection), '', geo$NAME[intersection]),
  GEOID = if_else(is.na(intersection), '', geo$GEOID[intersection]),
  geo_geometry = (geo$geometry[intersection])) %>%
  st_transform(crs=4326) %>%
  st_set_geometry("geo_geometry")%>%
  st_transform(crs=4326)

counts <- m %>%
  group_by(GEOID) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::summarize(count = n()
            ,
            sum_contained_acres = sum(`Contained Acres`, na.rm=FALSE),
            sum_attack_acres = sum(`Initial Attack Acres`, na.rm=FALSE),
            sum_start_acres = sum(`Start Acres`, na.rm=FALSE)
  ) %>% ungroup();
m <- m %>%
  st_set_geometry("geometry")
map_set <- left_join(geo, counts, by="GEOID") %>%
  st_transform(4326) %>%
  st_set_geometry("geometry") %>%
  st_simplify(dTolerance=10^3, preserveTopology=TRUE)

p_popup <- paste0("<strong>",map_set$NAME,"</></br>","<strong>Fires: ", map_set$count,"</strong>");
points_popup <- paste0("<strong>Fire Name: ",m$`Fire Name`,"</></br>","<strong>Start Time: ", m$StartTime,"</strong>","</></br>","<strong>Control Time: ", m$`Control Time`,"</strong>",
                  "</></br>","<strong>Start Acres: ", m$`Start Acres`,"</strong>", "</></br>","<strong>Control Acres: ", m$`Control Acres`,"</strong>");
pal_fun <- colorQuantile("YlOrRd", NULL, n = 9)
save (m, file = "m.RData")
save (pal_fun, file = "pal_fun.RData")
save (p_popup, file = "p_popup.RData")
leaflet(map_set) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data=map_set,
    stroke = FALSE,
    fillColor = ~pal_fun(map_set$count),
    fillOpacity = 0.6, smoothFactor = 0.5,
    popup = p_popup,
    group="Counties") %>%
  addMarkers(data=m,group="Points",clusterOptions = markerClusterOptions(),icon =  awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion'), popup = points_popup) %>%
  addLayersControl(overlayGroups = c("Counties", "Points"),
                   options = layersControlOptions(collapsed = FALSE))


  save(map_set, p_popup, pal_fun, points_popup, m, file = "map_set.RData")
