#Access Census API
library("tidycensus")
library("dplyr")
options(tigris_use_cache = TRUE)
key <- read.delim(file="private/Census Api Key.txt",header=FALSE)
Sys.setenv(census_api_key=key[[1]])

census <- st_as_sf(tidycensus::get_estimates(year=2019, geography="county", geometry=TRUE, product="population")) %>%
  filter(variable=="POP") %>%
  st_transform(2163)

save(census, file="census.RData")
