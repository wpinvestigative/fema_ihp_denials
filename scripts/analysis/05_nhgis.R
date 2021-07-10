library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

old1 <- read_csv("data/raw_data/nhgis0006_csv/nhgis0006_ds13_1860_county.csv")
old2 <- read_csv("data/raw_data/nhgis0006_csv/nhgis0006_ds14_1860_county.csv")
old3 <- read_csv("data/raw_data/nhgis0006_csv/nhgis0006_ts_nominal_county.csv")

old3_1860 <- filter(old3, YEAR==1860) %>% 
  mutate(fips=paste0(STATEFP, COUNTYFP))

old_count <- old3_1860 %>% 
  count(fips)

US_county_1860_conflated.shp

old_shape <- "data/raw_data/nhgis0006_shape/US_county_1860_conflated.shp"
old_shapes <- st_read(old_shape)

old_shapes %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~COUNTY)

counties <- st_read("data/clean_data/shapefiles/counties_reprojected.shp")

counties %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~name)


# hale slave population

hale_counties <- c("G0100630", "G0100910", "G0101050", "G0101250")

old1_hale <- filter(old1, GISJOIN %in% hale_counties)
old1_hale <- old1_hale %>% left_join(old2)

old1_hale <- old1_hale %>% 
  summarize(AGU001=sum(AGU001),
            AG3001=sum(AG3001)) %>% 
  mutate(percent_slaves=AGU001/AG3001*100)
