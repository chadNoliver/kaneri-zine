#stats-by-geo

library("dplyr")
library("magrittr")
library("sf")
library("tidyr")
library("readxl")
library("tibble")
library("sf")
library("tidycensus")
library("dplyr")
library("leaflet")
library("arcpullr")
load("irwin.RData")
load("census.RData")


m <- irwin %>%
  drop_na(`geoms`) %>%
  st_as_sf(coords = geoms,crs = 4269,remove=FALSE) %>%
  st_transform(2163)%>%
  mutate(intersection = as.integer(st_intersects(geoms, census)),
         geo_name = if_else(is.na(intersection), '', census$NAME[intersection]),
         GEOID = if_else(is.na(intersection), '', census$GEOID[intersection]),
         geo_geometry = (census$geometry[intersection]),
         FireDiscoveryDateTime = (as.POSIXct(FireDiscoveryDateTime/1000,origin="1970-01-01")),
         ContainmentDateTime = (as.POSIXct(ContainmentDateTime/1000,origin="1970-01-01"))
         ) %>%
  st_transform(crs=4326)%>%
    st_set_geometry("geo_geometry")%>%
    st_transform(crs=4326)

counts <- m %>%
  group_by(GEOID) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::summarize(count = n()
                   ,
                   sum_final_acres = sum(`FinalAcres`, na.rm=FALSE),
                   sum_daily_acres = sum(`DailyAcres`, na.rm=FALSE),
                   sum_discovery_acres = sum(`DiscoveryAcres`, na.rm=FALSE)
  ) %>% ungroup();

View(m)
m <- m %>%
  st_set_geometry("geoms")

map_set <- left_join(census, counts, by="GEOID") %>%
  st_transform(4326) %>%
  st_set_geometry("geometry") %>%
  st_simplify(dTolerance=10^3, preserveTopology=TRUE)

p_popup <- paste0("<strong>",map_set$NAME,"</></br>","<strong>Fires: ", map_set$count,"</strong>");
points_popup <- paste0("<strong>Incident Name: ",m$`IncidentName`,"</></br>","<strong>Discovery Time: ", m$FireDiscoveryDateTime,"</strong>","</></br>","<strong>Containment Time: ", m$`ContainmentDateTime`,"</strong>",
                       "</></br>","<strong>Discovery Acres: ", m$`DiscoveryAcres`,"</strong>", "</></br>","<strong>Daily Acres: ", m$`DailyAcres`,"</strong>");
pal_fun <- colorQuantile("YlOrRd", NULL, n = 2)
save (m, file = "m.RData")
save (pal_fun, file = "pal_fun.RData")
save (p_popup, file = "p_popup.RData")

save(map_set, p_popup, pal_fun, points_popup, m, file = "map_set.RData")
