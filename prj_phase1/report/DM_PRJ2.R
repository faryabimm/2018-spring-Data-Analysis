library(dplyr)
# install.packages("leaflet")
library(leaflet)
library(KernSmooth)

#default_map
addTiles()
#bright_map
addProviderTiles(providers$CartoDB.Positron)
#bw_map
addProviderTiles(providers$Stamen.Toner)

print_blue_marker <- function(data) {
  leaflet(data) %>% addTiles() %>% addCircleMarkers(~lng, ~lat,radius = 1)
}
print_heatmap <- function(data) {
  ## MAKE CONTOUR LINES
  ## Note, bandwidth choice is based on MASS::bandwidth.nrd()
  kde <- bkde2D(data %>% select(lng, lat),
                bandwidth=c(.0055, .0055), gridsize = c(200,200))
  CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
  
  ## EXTRACT CONTOUR LINE LEVELS
  LEVS <- as.factor(sapply(CL, `[[`, "level"))
  NLEV <- length(levels(LEVS))
  
  ## CONVERT CONTOUR LINES TO POLYGONS
  pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
  spgons = SpatialPolygons(pgons)
  
  ## Leaflet map with polygons
  leaflet(spgons) %>% addTiles() %>% 
    addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
}
print_vs_2 <- function(data, names) {
  color_factor <- colorFactor(c("blue", "red"), domain = names)
  leaflet(data %>% filter(amenity %in% names)) %>% addTiles() %>%
    addCircleMarkers(
      ~lng, ~lat,
      radius = ~ifelse(amenity == names[1], 8, 5),
      color = ~color_factor(amenity),
      stroke = FALSE, fillOpacity = 0.5
    )
}
print_vs_3 <- function(data, names) {
  leaflet(data %>% filter(amenity %in% names)) %>% addTiles() %>%
    addCircleMarkers(
      ~lng, ~lat,
      radius = ~ifelse(amenity == names[1],4 , ifelse(amenity == names[2], 3, 5)),
      color = ~ifelse(amenity == names[1], 'red', ifelse(amenity == names[2],'green', 'blue')),
      stroke = FALSE, fillOpacity = 0.5
    )
}

load('Desktop/cleanPins')

##### Appendix 1

cleanPins %>% filter(amenity %in% c('university', 'cafe')) %>% print_vs_2(names = c('university', 'cafe'))
cleanPins %>% filter(amenity %in% c('university', 'cafe', 'school')) %>% print_vs_3(names = c('university', 'cafe', 'school'))


library(ggplot2)
library(stringr)
cleanPins %>% select(amenity, lat, lng) %>% filter(amenity %in% c("university")) -> edu.pins
cleanPins %>% select(amenity, lat, lng, name) %>% filter(amenity %in% c("cafe")) -> cafe.pins

ggplot(edu.pins)+geom_density(aes(x = lat) ,fill = "green", alpha = 0.6)+
  geom_density(data = cafe.pins, aes(x = lat), fill = "orange", alpha = 0.4)
ggplot(edu.pins)+geom_density(aes(x = lng) ,fill = "green", alpha = 0.6)+
  geom_density(data = cafe.pins, aes(x = lng), fill = "orange", alpha = 0.4)



##### Appendix 2


cleanPins %>% filter(amenity %in% c('restaurant', 'fast_food', 'food_court')) %>% print_blue_marker
cleanPins %>% filter(amenity %in% c('restaurant', 'fast_food', 'food_court')) %>% heatmap_map




##### Appendix 3

library(geosphere)  
meanLat = mean(cleanPins$lat, na.rm = TRUE)
meanLng = mean(cleanPins$lng, na.rm = TRUE)
dist<-sapply(1:nrow(cleanPins), function(x){
  distm(c(cleanPins$lat[x], cleanPins$lng[x]), c(meanLat, meanLng), fun = distHaversine)
})
cleanPins$dsit <- dist
ggplot(cleanPins)+geom_density(aes(x = dist), fill = "blue", alpha = 0.6)





