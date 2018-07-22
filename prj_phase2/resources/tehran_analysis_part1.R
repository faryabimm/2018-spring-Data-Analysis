library(readr)
library(dplyr)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(stringr)
library(plotly)
library(tidyr)
library(highcharter)
library(leaflet)
# read the data
tehran.data <- read_csv('tidy_data.csv')
tehran.data$latitude <- as.numeric(tehran.data$latitude)
tehran.data$longitude <- as.numeric(tehran.data$longitude)
# analysing the name of down town companies and up town companies
tehran.data %>% filter(latitude<35.7) -> down.town
tehran.data %>% filter(latitude>35.7) -> up.town
down.town %>% .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "")  -> down.names
up.town %>% .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "")  -> up.names
tehran.data %>% .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "")  -> tehran.names

count_it <- function(names){
  wstone = names %>% 
    str_replace_all("\"","") %>% 
    str_replace_all("[[:punct:]]","") %>% 
    str_split(pattern = "\\s") %>% 
    unlist() %>% 
    str_trim() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F)
  
  colnames(wstone) = c("word","count")
  wstone = wstone %>% arrange(desc(count)) %>% 
    filter(nchar(word)>2)
  wstone %>% mutate(count  = sqrt(count)) -> wstone
  return(wstone)
}
cloud_it <- function(wstone, name){
  number = min(100, nrow(wstone))
  pngname = paste("./results/", name,".png")
  png(pngname, width=1000,height=800)
  wordcloud(wstone$word[1:number],wstone$count[1:number],
            c(6,1), random.order = FALSE,fixed.asp = T, colors=brewer.pal(8, "Dark2"), min.freq = 0 )
  dev.off()
}


# below this functions will draw the clouds and they automatically store them in the directory
cloud_it(count_it(up.names) %>% mutate(count = sqrt(count)), "North of Tehran")
cloud_it(count_it(down.names) %>% mutate(count = sqrt(count)), "South of Tehran")
count_it(tehran.names) %>% slice(1:200) %>% wordcloud2::wordcloud2(size = 0.3, figPath = "Milad-Tower.png")

#but this functions create interactive things 
tehran.data[str_detect(tehran.data$type, regex("restaurant",ignore_case = T)),] %>% 
  .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "") %>% 
  count_it() %>% slice(1:50) %>% wordcloud2::wordcloud2(size = 0.5, figPath = "restaurant.png", ellipticity = 1)

tehran.data[str_detect(tehran.data$type, regex("school",ignore_case = T)),] %>% 
  .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "") %>% 
  count_it() %>% slice(1:200) %>% wordcloud2::wordcloud2(size = 0.5, figPath = "book.jpg")

tehran.data[str_detect(tehran.data$type, regex("(hospital|medical|medicine|health)",ignore_case = T)),] %>% 
  .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "") %>% 
  count_it() %>% slice(1:200) %>% wordcloud2::wordcloud2(size = 0.4, figPath = "health.png")

tehran.data[str_detect(tehran.data$type, regex("(coffee)",ignore_case = T)),] %>% 
  .$name %>% sub(pattern = "^[[:alpha:]]*", replacement = "") %>% paste(collapse = "") %>% 
  count_it() %>% slice(1:200) %>% wordcloud2::wordcloud2(size = 1, figPath = "coffee.png")

########## find differenct cultures of tehran(part_part+PCA+Kmean) ############
min_lat <- tehran.data$latitude %>% min(na.rm = TRUE)
max_lat <- tehran.data$latitude %>% max(na.rm = TRUE)
min_lon <- tehran.data$longitude %>% min(na.rm = TRUE)
max_lon <- tehran.data$longitude %>% max(na.rm = TRUE)

ver_segments <- 15
hor_segments <- 15

hor_step <- (max_lon - min_lon) / hor_segments
ver_step <- (max_lat - min_lat) / ver_segments

tehran.data$hor_segment <- (tehran.data$longitude - min_lon) %/% hor_step
tehran.data$ver_segment <- (tehran.data$latitude - min_lat) %/% ver_step
tehran.data$segment_number <- tehran.data$ver_segment * hor_segments + tehran.data$hor_segment
# Now find the number of things

tehran.data %>% group_by(segment_number) %>% summarise(lat = median(latitude), lng = median(longitude)) -> seg.centers
tehran.data %>% group_by(segment_number, type) %>% summarise(count = n()) %>%
  ungroup() %>% spread(key = type, value = count) -> seg.data 
seg.data[is.na(seg.data)]<-0
# cor matrix
library(Hmisc)
library(corrplot)
#the chart of correlations
seg.data %>% select(-segment_number) %>% cor %>% hchart
for(i in 2:ncol(seg.data)){
  m <- (seg.data[,i]) %>% colMeans(na.rm = T) %>% as.numeric
  if(is.nan(m)){
    m <- 0
  }
  seg.data[is.na(seg.data[,i]), i] <- m
  coldata <- seg.data[[i]]
  s <- sd(coldata)
  if(s == 0){
    s <- 1
  }
  seg.data[,i]<-(coldata-coldata%>% mean)/s
}
seg.data %>% full_join(seg.centers) -> seg.data
seg.data %>% select(-segment_number, -lat, -lng)%>% prcomp() -> prc
seg.data$PC1<-prc$x[,"PC1"]  
seg.data$PC2<-prc$x[,"PC2"]  
center_number <- 4
seg.data %>% select(PC1, PC2) %>% kmeans(center = center_number) -> kcl
seg.data$cluster <- kcl$cluster
qpal <- colorQuantile("Dark2", seg.data$cluster %>% unique, n = center_number)
seg.data %>% leaflet %>% addTiles %>% addCircles(~lng, ~lat,radius = 1000,  color = ~qpal(cluster))
seg.data %>% View

############## plot the frequency of different jobs #####################
tehran.data %>% group_by(type) %>% summarise(n = n()) %>%
  hchart("treemap", hcaes(x = type, value = n, color = n))



