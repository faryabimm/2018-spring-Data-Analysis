library(dplyr)
library(readr)
library(stringr)
library(tidyr)

all_data <- read_csv('final_crawler_result.csv')

all_data[is.na(all_data)] <- ''

all_data %>% select(-X1, -X5) %>% mutate(website = paste0(.[[34]], .[[24]], .[[17]], .[[13]])) %>% 
  mutate(telephone = paste0(.[[9]], .[[10]], .[[27]])) %>% 
  mutate(email = paste0(.[[26]], .[[30]], .[[37]])) %>% 
  mutate(fax = paste0(.[[7]], .[[18]], .[[29]])) %>% 
  mutate(description = paste(
    "آدرس: ", 
    .[[33]] ,
    "| کد پستی: ",
    .[[11]],
    "| ایمیل: ",
    email,
    "| تلفن: ",
    telephone,
    "| فکس: ",
    fax , sep = ' ')) -> data_step_1

data_step_1 %>% select(c(1:6, 8, 12, 14:16, 19:23, 25, 28, 31, 32, 35, 36, 38:42)) -> data_step_2
colnames(data_step_2) <- c("latitude", "longitude", "type", "school sex type", "price range",
                           "WIFI", "especiality1", "sports", "selling for", "additional info",
                           "languages", "especiality2", "name", "branch", "men/momen",
                           "service hours", "mall type", "has parking", "food type",
                           "introduction", "parking capacity", "activities", "website",
                           "telephone", "email", "fax", "description")
data_step_2 %>% mutate(speciality = paste0(especiality1, especiality2)) %>%
  select(-especiality1, -especiality2) %>% select("latitude", "longitude", "type", "name", "description",
                                                  "school sex type", "price range", "WIFI", "speciality",
                                                  "sports", "selling for", "additional info", "languages",
                                                  "branch", "men/momen", "service hours", "mall type",
                                                  "has parking", "food type", "introduction", "parking capacity",
                                                  "activities", "website", "telephone", "email", "fax") -> tidy_data



tidy_data %>% select(type) %>% unique %>% View
# tidy_data$type <- as.factor(tidy_data$type)
tidy_data$latitude <- as.numeric(tidy_data$latitude)
tidy_data$longitude <- as.numeric(tidy_data$longitude)


tidy_data[str_detect(tidy_data$type, pattern = regex('bank$', ignore_case = TRUE)),]$type <- 'Bank'
tidy_data %>% filter(latitude < 36) %>% filter(latitude > 35) -> tidy_data

tidy_data<- read_csv('Desktop/tidy_data.csv')




##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


min_lat <- tidy_data$latitude %>% min(na.rm = TRUE)
max_lat <- tidy_data$latitude %>% max(na.rm = TRUE)
min_lon <- tidy_data$longitude %>% min(na.rm = TRUE)
max_lon <- tidy_data$longitude %>% max(na.rm = TRUE)

ver_segments <- 20
hor_segments <- 20

hor_step <- (max_lon - min_lon) / hor_segments
ver_step <- (max_lat - min_lat) / ver_segments

tidy_data$hor_segment <- (tidy_data$longitude - min_lon) %/% hor_step
tidy_data$ver_segment <- (tidy_data$latitude - min_lat) %/% ver_step
tidy_data$segment_number <- tidy_data$ver_segment * hor_segments + tidy_data$hor_segment

tidy_data %>% select(segment_number, type) %>% table %>% as.data.frame %>% filter(Freq != 0) -> business_per_region
business_per_region %>% spread(key = type, value = Freq) -> transactions

transactions %>% select(-segment_number) %>% as.matrix -> transactions_matrix

transactions_matrix[is.na(transactions_matrix)] <- 0

library(arules)
library(arulesViz)
library(colorspace)

transactions_obj <- as(transactions_matrix, 'transactions')
grules <- apriori(transactions_obj, parameter = list(support = 0.009, confidence = 0.25, minlen = 2, maxlen = 4))

show_regions <- function(candidate_results) {
  candidate_results %>% .$segment_number %>% as.vector %>% as.integer -> candidate_regions
  candidate_results %>% .$Freq -> candidate_Freqs  
  max_freq <- candidate_Freqs %>% max(na.rm = TRUE)
  min_freq <- candidate_Freqs %>% min(na.rm = TRUE)
  
  map <- leaflet() %>% addTiles()
  
  lat1 = c()
  lat2 = c()
  lon1 = c()
  lon2 = c()
  color = c()
  for (index in 1:length(candidate_regions)) {
    region <- candidate_regions[index]
    Freq <- candidate_Freqs[index]
    hor_index <- candidate_regions %% hor_segments
    ver_index <- candidate_regions %/% hor_segments
    lat1 = c(lat1, (min_lat + (ver_index * ver_step)))
    lat2 = c(lat2, (min_lat + ((ver_index + 1) * ver_step)))
    lon1 = c(lon1, (min_lon + (hor_index * hor_step)))
    lon2 = c(lon2, (min_lon + ((hor_index + 1) * hor_step)))
    color = c(color, map_freq_to_range(Freq, max_freq, min_freq))
  }
  map <- map %>% add_rect(lat1, lat2, lon1, lon2, color)
  return(map)
}
map_freq_to_range <- function(freq, max_freq, min_freq) {
  freq = (freq - min_freq)/(max_freq - min_freq)
  
  if (freq < 0.33) {
    return('#00FF00')
  } else if (freq < 0.66) {
    return('#0000FF')
  } else {
    return('#FF0000')
  }
}
add_rect <- function(pipe_in, lat1, lat2, lon1, lon2, color) {
  print('#####')
  print(color)
  pipe_out <- pipe_in %>% addRectangles(
    lng1=lon1, lat1=lat1,
    lng2=lon2, lat2=lat2,
    fillColor = "transparent",
    color = color
  )
  
  return(pipe_out)
}
propse_location <- function(target_type) {
  grules %>% subset(subset = rhs %in% c(target_type)) -> kebab_grules
  
  rule_lhs <- kebab_grules %>% sort(by = 'lift') %>% head(10) %>% lhs %>% as('list')
  
  rhs <- c(target_type)
  tmp <- transactions %>% select(-segment_number)
  tmp[!is.na(tmp)] <- TRUE
  tmp[is.na(tmp)] <- FALSE
  
  first_iter <- TRUE
  
  for (lhs in rule_lhs) {
    if (first_iter) {
      first_iter <- FALSE
      cbind(
        transactions %>% select(segment_number),
        tmp %>% select_(.dots = lhs) %>% mutate(candidate = rowSums(.) == (length(colnames(.)))) %>% select(candidate)
      ) %>% filter(candidate == TRUE) %>% left_join(business_per_region %>% filter(type == rhs), by = 'segment_number') -> candidate_results
    } else {
      cbind(
        transactions %>% select(segment_number),
        tmp %>% select_(.dots = lhs) %>% mutate(candidate = rowSums(.) == (length(colnames(.)))) %>% select(candidate)
      ) %>% filter(candidate == TRUE) %>% left_join(business_per_region %>% filter(type == rhs), by = 'segment_number') -> result_increament
      
      rbind(candidate_results, result_increament) -> candidate_results
    }
  }
  
  candidate_results <- candidate_results %>% unique
  show_regions(candidate_results)
}

propse_location('KababShop')



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################


tidy_data %>% select(lat = latitude, lng = longitude, type) -> map_test_data
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
  leaflet(data %>% filter(type %in% names)) %>% addTiles() %>%
    addCircleMarkers(
      ~lng, ~lat,
      radius = ~ifelse(type == names[1], 8, 5),
      color = ~color_factor(type),
      stroke = FALSE, fillOpacity = 0.5
    )
}
print_vs_3 <- function(data, names) {
  leaflet(data %>% filter(type %in% names)) %>% addTiles() %>%
    addCircleMarkers(
      ~lng, ~lat,
      radius = ~ifelse(type == names[1],4 , ifelse(type == names[2], 3, 5)),
      color = ~ifelse(type == names[1], 'red', ifelse(type == names[2],'green', 'blue')),
      stroke = FALSE, fillOpacity = 0.5
    )
}


map_test_data %>% print_blue_marker

