# LIBRARIES ----
library(tidyverse)
#install.packages("tidygeocoder")
library(tidygeocoder)
#install.packages("sf")
library(sf)
#install.packages("mapview")
library(mapview)
library(readxl)

# DATA ----
# (will show how to get this from an API in R-Tip 60)

setwd("C:/Users/pc/Desktop/Diseños Experimentales/Scripts")

pittsburgh_pharmacies_tbl <- read_excel("Geo_data.xlsx")
pittsburgh_pharmacies_tbl


# 1.0 GEOCODING -----
# - Have address, want Lat/Long
# - Important for geospatial analysis

geo_code_tbl <- pittsburgh_pharmacies_tbl %>%
  
  # Pretend we dont have lat/lon...
  slice(1:10) %>%
  select(-lat, -lon) %>%
  
  # Geocode Address to Lat/Lon
  tidygeocoder::geocode(
    address = address,
    method = "osm"
  )

# 2.0 REVERSE GEOCODING -----
# - Have Lat/Long, Want Physical Address
# - Important for Sales People that Travel

geo_reverse_tbl <- pittsburgh_pharmacies_tbl %>%
  
  # Pretend we don't have the address...
  slice(1:10) %>%
  select(-address) %>%
  
  # Go from Lat/Lon to Address
  tidygeocoder::reverse_geocode(
    lat    = lat,
    long   = lon,
    method = "osm"
  )


# BONUS: MAPPING WITH SIMPLE FEATURES ----

pittsburgh_pharmacies_sf <- pittsburgh_pharmacies_tbl %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs    = 4326
  )

pittsburgh_pharmacies_sf %>% mapview()

### Example ###

# charge data

data <- read_excel("Prueba_geodata.xlsx")

# Have Lat/Long, Want Physical Address

data_adrees <- data %>% 
  tidygeocoder::reverse_geocode(
    lat    = lat,
    long   = long,
    method = "osm"
  )

# MAPPING WITH SIMPLE FEATURES

data_sf <- data %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs    = 4326
  )

data_sf %>% mapview()
