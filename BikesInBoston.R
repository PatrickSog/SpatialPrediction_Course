# Author: Patrick Sogno
# E-mail: patrick.sogno@stud-mail.uni-wuerzburg.de
# Date: 05 June 2019
# RStudio version: 1.1.456
# R version: 3.5.1

#--------------------------------------------------#
# Background: This code is part of a university
# project on spatial modeling and prediction.
# The objective to spatially model user types for
# rental bikes in Boston, Massachusetts, USA has
# been developed by Ronja Lappe and me.
# This code and Ronja's are part of the same work.
# For any questions regarding her code, please
# contact her directly:
# ronja.lappe@stud-mail.uni-wuerzburg.de
#--------------------------------------------------#

# ==> Hypothesis: Based on the bike data of Boston for [DATE], [TIMESPAN] we can:
#                 1. Find the spatial distribution at that point in time for different user types
#                 2. Build an occurence model that is transferable to other times.
#                 3. We will find that bike usage is not correlated to seasons.
#                 4. We will find that bike usage is not correlated to specific demographic user types
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Needed data:
#
# 1 Sentinel-2 Satellite Image of the research area, L2A.
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#



######----------------------------------- 0. Set environment -----------------------------------######

## Paths:

ipath = "D:/Patrick/Documents/Dokumente/SoSe2019/MET/SpatialModeling/BikeData" # this 
opath = getwd()
Sentinel2Image <- "D:/Patrick/Documents/Dokumente/SoSe2019/MET/SpatialModeling/get_data/SENTINEL/S2A_MSIL2A_20180704T153601_N0206_R111_T19TCG_20180704T220529.SAFE/GRANULE/L2A_T19TCG_A015838_20180704T154031/IMG_DATA/R10m"

bikeDir <- paste0(opath, "/", "BikeData_bo")

## Packages:

#install.packages("bikedata")
#install.packages("dodgr")
#install.packages("osmdata")
#install.packages("osmplotr")
#install.packages("OpenStreetMap")
library('bikedata')
library('rgdal')
library('dodgr')
library('tidyverse')
library('osmdata')
library('osmplotr')
library('OpenStreetMap')
library('raster')
library('ellipse')



######------------------------------------ End of section 0 ------------------------------------######



######----------------------------- 1. Find, download, & load data -----------------------------######

## Let's look at the bike data before we download it:
demo <- bike_demographic_data()
demo[which(demo$demographic_data == T),] # Boston seems like a nice option

# Old Boston data
head(bike_test_data$bo17)
names(bike_test_data$bo17)

# New Boston data
head(bike_test_data$bo18)
names(bike_test_data$bo18)


# --> Both datasets seem useful (lat/lon, demographics, times stamps)
# --> Download into a folder named "BikeData_bo" (is produced if it doesn't exist already).

if (dir.exists(bikeDir) == T) {
  if (length(list.files(bikeDir) != 0)) {
    break()
  } else {
    dl_bikedata(city = "bo", data_dir = bikeDir, dates = 201701:201712, quiet = T)
  }
} else {
  dir.create(bikeDir)
  dl_bikedata(city = "bo", data_dir = bikeDir, dates = 201701:201712, quiet = T)
}




######## unzipping data, but not in R....





## Load data for May 2017:
bikefiles <- list.files(bikeDir, pattern = "*.csv$", full.names = T)
bikefiles




## Extract functions from bike data by using the bikedata R package.... (not part of this code)





# Loading tripdata of May, 2017:
tripMay <- read.csv(bikefiles[grep("201705-*", bikefiles)])
head(tripMay)

# Loading stations of 2017:
stations <- read.csv(bikefiles[grep("/Hubway_Stations_as_of_July_2017.csv$", bikefiles)])
head(stations)


## Converting stations of 2017 to spdf:
coordinates(stations) <- c("Longitude", "Latitude")
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")
class(stations)


## Plotting stations on OSM cycle map:
minlat <- min(stations$Latitude)
maxlat <- max(stations$Latitude)
minlon <- min(stations$Longitude)
maxlon <- max(stations$Longitude)

map <- openmap(upperLeft = c(maxlat, minlon), lowerRight = c(minlat, maxlon), type = "opencyclemap", mergeTiles = T)
plot(map)

stations <- spTransform(stations, osm()) # Don't forget to reproject the data to osm projection
plot(stations, add = T)


## Clean data to only show subscribers with valid demographic info
tripMay = tripMay[which(tripMay$birth.year != "\\N"),]
which(tripMay$usertype == "Customer")



######------------------------------------ End of section 1 ------------------------------------######



######--------------- 2. Find characterizing environmental influences & land use ---------------######

## Land use from osm to drive:
q <- opq(bbox = 'boston') %>%                     # Variety of streets and pathways
  add_osm_feature(key = "highway") %>%
  osmdata_xml(file = "OSMstreets.osm", quiet = F)

c1 <- opq(bbox = 'boston') %>%                    # Important places for commuters
  add_osm_feature(key = "public_transport") %>%
  add_osm_feature(key = "railway") %>%
  osmdata_xml(file = "OSMcommuter.osm", quiet = F)

c2<- opq(bbox = 'boston') %>%                     # All places for commuters
  add_osm_feature(key = "public_transport") %>%
  osmdata_xml(file = "OSMcommuter2.osm", quiet = F)

t <- opq(bbox = 'boston') %>%                     # Tourist-specific places
  add_osm_feature(key = "tourism") %>%
  osmdata_xml(file = "OSMtourist.osm", quiet = F)

l <- opq(bbox = 'boston') %>%                     # Leisure-related places
  add_osm_feature(key = "leisure") %>%
  osmdata_xml(file = "OSMleisure.osm", quiet = F)


## Load street data into r as spdf:

q <- sf::st_read('OSMstreets.osm', layer = 'lines', quiet = F)
q_sp <- as(q, 'Spatial')
class(q_sp)
q_sp$highway
plot(q_sp)
names(q_sp)


## Now for the guess work: Let's subset streets that are possible to be used by bike

footway <- q_sp[which(q_sp$highway == "footway"),]  # Biking on a footway? - No problem!
plot(footway)

cycleway <- q_sp[which(q_sp$highway == "cycleway"),]  # Biking on a dedicated cycle lane? - Sure!
plot(cycleway, col = "#009900", add = T)

residential <- q_sp[which(q_sp$highway == "residential"),]  # Biking on a residential street? - Not so sure, but let's say yes!
plot(residential, col = "#CC0033", add = T)

tertiary <- q_sp[which(q_sp$highway == "tertiary"),]  # Biking on a tertiary street? - If it is absolutely necessary!
plot(tertiary, col = "#FFCC00", add = T)


## Same for commuter places:

c1 <- sf::st_read('OSMcommuter.osm', layer = 'points', quiet = F)   # Railway stops
c1_sp <- as(c1, 'Spatial')
class(c1_sp)
names(c1_sp)
plot(c1_sp)

c2 <- sf::st_read('OSMcommuter2.osm', layer = 'points', quiet = F)   # Bus stops
c2_sp <- as(c2, 'Spatial')
class(c2_sp)
names(c2_sp)
plot(c2_sp, col = "#FFCC00", add = T)


## Same for tourist-related places:

t <- sf::st_read('OSMtourist.osm', layer = 'points', quiet = F)   # Railway stops
t_sp <- as(t, 'Spatial')
class(t_sp)
names(t_sp)
plot(t_sp)


## Same for leisure-related places:

l <- sf::st_read('OSMleisure.osm', layer = 'points', quiet = F)   # Railway stops
l_sp <- as(l, 'Spatial')
class(l_sp)
names(l_sp)
plot(l_sp)


## Rasterize land use:

# Prepare extent:
e <- extent(c(-71.2, -70.9, 42.2, 42.45))

# Distance from points
rc1 <- raster(e, ncols = 100, nrows = 100) # For commuter land use 1
crs(rc1) <- osm()
distances <- distanceFromPoints(object = rc1, xy = c1_sp)
distances
plot(distances)
class(distances)
#
rc2 <- raster(e, ncols = 100, nrows = 100) # For commuter land use 2
distances <- stack(distances, distanceFromPoints(object = rc2, xy = c2_sp))
distances
plot(distances)
#
rt <- raster(e, ncols = 100, nrows = 100) # For tourist land use
distances <- stack(distances, distanceFromPoints(object = rt, xy = t_sp))
distances
plot(distances)
#
rl <- raster(e, ncols = 100, nrows = 100) # For leisure land use
distances <- stack(distances, distanceFromPoints(object = rl, xy = l_sp))
distances
plot(distances)
#
rs <- raster(e, ncols = 100, nrows = 100) # For distance to bike stations
disStations <- distanceFromPoints(object = rs, xy = stations)
crs(disStations) <- osm()
plot(disStations)
#
writeRaster(distances, paste0(bikeDir,"/", "distanceStack.tif"))
writeRaster(disStations, paste0(bikeDir,"/", "distanceStation.tif"))
bikeDir


## Ndvi from sentinel 2017 (May):

# Load raster:
Sentinel2Image <- stack(list.files(Sentinel2Image, pattern = "*_B0*", full.names = T)[c(3,4)])

# Produce extent:
stations <- spTransform(stations, Sentinel2Image@crs)
bbox(stations)
e2 <- extent(bbox(stations))
e2 <- extent(c(318368.44, 343757.43, 4674325.33, 4701490.03))

# Crop image:
Sentinel2Image <- crop(Sentinel2Image, e2)

# Produce NDVI:
NDVI <- function(Red, NIR) {
  (NIR-Red)/(NIR+Red)
}
NDVI_BO <- NDVI(Sentinel2Image[[1]], Sentinel2Image[[2]])
plot(NDVI_BO)

# Write raster:
writeRaster(NDVI_BO, paste0(bikeDir, "/", "NDVI.tif"))



######------------------------------------ End of section 2 ------------------------------------######