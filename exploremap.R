########################################
#### County level map :)

library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(raster)
library(sp)
library(rasterVis)
library(htmltools)
library(RColorBrewer)
library(geosphere)

library(tigris)
#######################################
#### From Joe's gis example. mapping each county
tn <- read_csv('https://github.com/databrew/intro-to-data-science/raw/main/data/tnzips.csv')

temp <- tigris::zctas(starts_with = 370:385)
options(tigris_use_cache = TRUE)
tnz <-  as(temp, 'Spatial')
tng <- fortify(tnz, region = 'GEOID20') %>%
  mutate(zip = id)

library(rgeos)
tns<- gSimplify(tnz, tol = 0.1, topologyPreserve = TRUE)
plot(tns)

tng <- left_join(tng, tnz@data, by = c('zip' = 'GEOID20'))
plot(tng) 

# set to monteagle, trying to find a way to include multiple cities
citymap <- tnz[tnz@data$GEOID20 %in% tn$zip[tn$city == 'Tracy City' |
                 tn$city == 'Monteagle'| tn$city == 'Coalmont'| tn$city == 'Winchester'|
                   tn$city == 'Decherd'| tn$zip == 'Chattanooga'],]
# can change up the esri theme, still trying to find new ones
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addPolygons(data = areamap,
              weight = 2,
              color = 'blue',
              fillOpacity = .1,
              fillColor = 'grey')

zipmap <- tnz[tnz@data$GEOID20 %in% c(37324, 37387, 37313, 37356, 
                                      37398, 37339, 37357, 37301, 47411, 37110),]
# cashexpress<- c(-85.739667, 35.26025)
# Title cash = (-86.09522, 35.1992)
# Title max = (-86.103218, 35.222679)
# advance financial = (-86.100580 , 35.216940)
# Mariner financer  = (-86.100690, 35.218420)
# cash express2 = (-86.0979138 , 35.2138122)
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addPolygons(data = zipmap,
              weight = 1,
              color = 'blue',
              fillOpacity = .1,
              fillColor = 'grey') %>% 
addMarkers(lng = c(-85.739667, -86.09522, -86.103218, -86.100580, -86.100690, -86.0979138),
                 lat = c(35.26025, 35.1992, 35.222679, 35.216940, 35.218420, 35.2138122)) 
  

#zips <- new.client %>% 
 # dplyr::select(streetzip) %>% 
 # group_by(streetzip)


# When I get back, merge popultion with selected counties, 


##############################################################
##############################################################
# What eric gave as an example... like Joe's example better tho unless you 
# like this better. He did it where it shows water coverage per county

# Load packages
library(shiny)
library(leaflet)
library(sf)

# Download county data (will take a few minutes)
options(tigris_use_cache = TRUE)
shape <- tigris::zctas(state = "TN", class = "sf", year = 2010)

# Check out dataset
shape %>% head

shape$

# Add a column with the fraction water coverage in  each county
shape$percent_water <- shape$AWATER10 / (shape$AWATER10 + shape$ALAND10)

# Build a chloroplet color palette according to percent_water
bins <- c(0, .1, .2, .3, .4, .5)
pal <- colorBin("YlOrRd", domain = shape$percent_water, bins = bins)

# Build leaflet map!
leaflet(shape) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(shape$percent_water),
    weight = 2,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.7)
