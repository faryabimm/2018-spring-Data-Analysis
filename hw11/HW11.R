library(readr)
library(dplyr)
library(tidyr)
library(highcharter)
library(plotly)
library(stringr)


hist_data <- read_rds('data/historical_web_data_26112015.rds')
disaster <- read_delim("data/disaster.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
iran_eqrthquake<- read_rds('data/iran_earthquake.rds')
myMap = read_rds("data/Tehrn_map_6.rds")
worldwide <- read_csv('data/worldwide.csv')


################################################################################################
## Q1

hist_data %>% plot_ly(type = 'scatter3d', mode='markers', x = ~Latitude, y = ~Longitude, z = ~-Depth, size = ~Magnitude,
                      marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1,20),
                      text = ~paste('Time:', Time, '<br>Province:', Province, '<br>City:', City,'<br>Magnitude:', Magnitude)) %>%
                              layout(title = 'Iran Earthquakes Data Visualization',
                                     scene = list(xaxis = list(title = 'Lattitude'#,
                                                               # gridcolor = 'rgb(255, 255, 255)',
                                                               # range = c(2.003297660701705, 5.191505530708712),
                                                               # type = 'log',
                                                               # zerolinewidth = 1,
                                                               # ticklen = 5,
                                                               # gridwidth = 2
                                                               ),
                                                  yaxis = list(title = 'Longitude'#,
                                                               # gridcolor = 'rgb(255, 255, 255)',
                                                               # range = c(36.12621671352166, 91.72921793264332),
                                                               # zerolinewidth = 1,
                                                               # ticklen = 5,
                                                               # gridwith = 2
                                                               ),
                                                  zaxis = list(title = 'Depth'#,
                                                               # gridcolor = 'rgb(255, 255, 255)',
                                                               # type = 'log',
                                                               # zerolinewidth = 1,
                                                               # ticklen = 5,
                                                               #gridwith = 2
                                                               )
                                                  ),
                                        paper_bgcolor = 'rgb(243, 243, 243)',
                                        plot_bgcolor = 'rgb(243, 243, 243)'
                                     )
# 
# 
# df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
# df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
#                            "Fruits", total.fruits, "Veggies", total.veggies,
#                            "<br>", "Wheat", wheat, "Corn", corn))
# # give state boundaries a white border
# l <- list(color = toRGB("white"), width = 2)
# # specify some map projection/options
# g <- list(
#   scope = 'iran',
#   projection = list(type = 'iran'),
#   showlakes = TRUE,
#   lakecolor = toRGB('white')
# )
# 
# p <- plot_geo(df, locationmode = 'Iran') %>%
#   add_trace(
#     z = ~total.exports, text = ~hover, locations = ~code,
#     color = ~total.exports, colors = 'Purples'
#   ) %>%
#   colorbar(title = "Millions USD") %>%
#   layout(
#     title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
#     geo = g
#   )
# p
# 
# 
# library(mapdata)
# 
# map_data("world", "iran") %>%
#   group_by(group) %>%
#   plot_geo(x = ~long, y = ~lat, locationmode = 'iran') %>%
#   add_markers(size = I(1)) %>% layout(geo = g)

################################################################################################
## Q2

disaster %>% filter(FLAG_TSUNAMI == 'Tsu') %>%
  select(year = YEAR, mag = EQ_PRIMARY, lat = LATITUDE, lng = LONGITUDE) -> q2_data

min_year <- min(q2_data$year, na.rm = TRUE)
max_year <- max(q2_data$year, na.rm = TRUE)

q2_data_acc <- q2_data %>% filter(year == min_year)
for (year_iter in (min_year+1):max_year) {
  q2_data_acc <- rbind(
    q2_data_acc,
    q2_data_acc %>% filter(year == year_iter-1) %>% mutate(year = year_iter),
    q2_data %>% filter(year == year_iter)
  )
}


map_data("world") %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_markers(size = ~mag, x = ~lng, y = ~lat, data = q2_data, frame = ~year) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

# accumulated - too slow
# map_data("world") %>%
#   plot_geo(locationmode = 'USA-states') %>%
#   add_markers(size = ~mag, x = ~lng, y = ~lat, data = q2_data_acc, frame = ~year) %>% 
#   animation_opts(
#     frame = 100, 
#     transition = 0, 
#     redraw = FALSE
#   ) %>%
#   animation_slider(
#     hide = T
#   ) %>%
#   animation_button(
#     x = 1, xanchor = "right", y = 0, yanchor = "bottom"
#   )

################################################################################################
## Q3

library(ggmap)


iran_eqrthquake$Latitude = as.numeric(iran_eqrthquake$Lat)
iran_eqrthquake$Longitude = as.numeric(iran_eqrthquake$Long)
ggmap(myMap) + geom_point(aes(x = Long, y = Lat), data = iran_eqrthquake, alpha = 0.5, size = 0.2) +
  stat_density_2d(aes(x = Long, y = Lat), data = iran_eqrthquake)

################################################################################################
## Q4

iran_eqrthquake %>% View

iran_eqrthquake %>% select(OriginTime, Mag) -> q4_data

q4_data %>% mutate(year = format(OriginTime,'%Y'), month = format(OriginTime,'%m')) %>% filter(year > 2000) %>% 
  mutate(month_count = (year %>% as.integer - 2006) * 12 + month %>% as.integer) %>% select(-year, -month) %>% 
  select(-OriginTime) %>% mutate(disaster = Mag >= 7) %>% group_by(month_count) %>% summarise(disaster = any(disaster)) %>% 
  ungroup %>% group_by(disaster) %>% summarise(frequency = n()) -> q4_disaster_prob


false <- q4_disaster_prob %>% filter(disaster == FALSE) %>% .$frequency
true <- q4_disaster_prob %>% filter(disaster == TRUE) %>% .$frequency
prob <- true / (true + false)

# next 5 years is 60 months we calculate probablility that no earthquake with Mag > 7 would happen in next 60 months.

final_prob_not <- (1 - prob) ^ 60

# the the probabiliry that in upcomming 60 months there is at least one earthquake with Mag > 7 is:

final_prob <- 1 - final_prob_not
final_prob %>% print

# of course this value has no significat meaning asd is this large because i've selected my baskets as months,
# selecting daily baskets would result in a much smaller number
# at last the calculated probability would pretty much dedpend on our definition of the question.

################################################################################################
## Q5

disaster %>% select(country = COUNTRY, deaths = DEATHS) %>% group_by(country) %>%
  summarise(tot_deaths = sum(deaths, na.rm = TRUE), mean_deaths = mean(deaths, na.rm = TRUE)) -> q5_data


library(rworldmap)

matched <- joinCountryData2Map(q5_data %>% select(country, value = tot_deaths), joinCode="NAME", nameJoinColumn="country")
mapCountryData(matched, nameColumnToPlot="value", mapTitle="Total deaths in earthquakes", catMethod = "quantiles", colourPalette = "heat")

################################################################################################
## Q6

disaster %>%
  select(lat = LATITUDE, lng = LONGITUDE, mag = EQ_PRIMARY, depth = FOCAL_DEPTH, deaths = TOTAL_DEATHS) %>%
  .[complete.cases(.),] -> q6_data

library(h2o)

glm(deaths~., data = q6_data, family = gaussian(link = 'identity')) %>% summary.glm()

################################################################################################
## Q7

find_place <- function(string) {
  tmp <- gregexpr(',', string)
  start_index <- tmp[[length(string)]][1] + 2
  result <- substr(string, start_index, nchar(string))
  return(result)
}

q7_data <- worldwide
q7_data$place <- q7_data$place %>% sapply(find_place) %>% str_replace(., regex('region$', ignore_case = TRUE), '') %>% 
  str_replace(., regex('of the [^\\s]* coast of', ignore_case = TRUE), '') %>%
  str_replace(., regex('[^\\s]* of (the)?', ignore_case = TRUE), '') %>%
  str_replace(., regex('.* of', ignore_case = TRUE), '') %>%
  str_replace(., regex('^off the ', ignore_case = TRUE), '') %>%
  str_replace(., regex('islands?$', ignore_case = TRUE), '') %>%
  str_trim(., side = 'both') %>% tolower(.)

q7_data %>% select(place, time, mag) %>% mutate(time = time %>% as.integer) -> q7_data
q7_data %>% arrange(place, time) -> q7_data
q7_data$time_diff <- q7_data$time - (q7_data$time %>% lag)
q7_data %>% View


# according to wikipedia there is no currently known model for matching pre earthquakes to main earthquakes
# for example its proposed that earthquake of indonesia in 2002 was a pre earthquake for earthquake and tsunami
# in indian oscean in 2004!
# for the sake of this question, we consider a chain of earth rattles as a pre-main-post earthquake chain if
# time difference between no pair of them is more than 5 days.


q7_data %>% mutate(new_chain = time_diff > 5*24*3600) -> q7_data
q7_data$new_chain[1] <- FALSE

q7_data %>% mutate(chain_number = cumsum(new_chain)) %>% select(-new_chain, -time_diff) -> q7_ready_data

q7_ready_data %>% group_by(chain_number) %>% summarise(max_rank = which.max(mag), chain_lenght = n()) %>% 
  mutate(max_rel_pos = max_rank/chain_lenght) -> q7_ready_data_summarised

hchart(density(q7_ready_data_summarised$max_rel_pos), type = 'area', color = "#B71C1C", name='Distribution of main earthquake position in chain')

hchart(density(q7_ready_data_summarised$chain_lenght), type = 'area', color = "#3BD641", name='Distribution of chain length')
hchart(density(q7_ready_data_summarised %>% filter(chain_lenght < 620) %>% .$chain_lenght), type = 'area', color = "#3BD641", name='Distribution of chain length')
hchart(density(q7_ready_data_summarised %>% filter(chain_lenght < 20) %>% .$chain_lenght), type = 'area', color = "#3BD641", name='Distribution of chain length')

# as we can see with the really strong assumption that the consecutive durations of a chain are not longer than
# 5 days, we can see that for most of the chains, the biggest quake is the last one,
# but there is no clear understanding of how long a chain would actually be!
# perhaps with more information and a complex learnging model we can overcome this and predict chain length but
# the strong assumption we made in the first place makes it unachievable to predict the main quake from pre ones.

################################################################################################
## Q8

worldwide %>% select(depth, mag) %>% .[complete.cases(.),] %>% filter(depth >= 0) -> q8_data
chisq.test(q8_data)
cor.test(q8_data$depth, q8_data$mag, method = 'spearm')

################################################################################################
## Q9

q9_data <- worldwide
q9_data$place <- q9_data$place %>% sapply(find_place) %>% str_replace(., regex('region$', ignore_case = TRUE), '') %>% 
  str_replace(., regex('of the [^\\s]* coast of', ignore_case = TRUE), '') %>%
  str_replace(., regex('[^\\s]* of (the)?', ignore_case = TRUE), '') %>%
  str_replace(., regex('.* of', ignore_case = TRUE), '') %>%
  str_replace(., regex('^off the ', ignore_case = TRUE), '') %>%
  str_replace(., regex('islands?$', ignore_case = TRUE), '') %>%
  str_trim(., side = 'both') %>% tolower(.)

q9_data %>% mutate(year = format(time, '%Y') %>% as.integer) -> q9_data
q9_data %>% select(year, place, mag) %>% group_by(place) %>%
  summarise(count = n(), mean_mag = mean(mag, na.rm = TRUE)) %>% arrange(desc(count)) %>% slice(1:5) %>% 
  hchart(type = 'column', hcaes(x = place, y = count, color = mean_mag))


q9_data %>% select(year, place, mag) %>%
  filter(place %in% c('alaska', 'indonesia', 'oklahama', 'japan', 'puerto rico')) %>% group_by(place, year) %>%
  summarise(count = n(), mean_mag = mean(mag, na.rm = TRUE)) %>% arrange(desc(count)) %>% 
  hchart(type = 'column', hcaes(x = place, y = count, group = year))


# harp project base is located in alaska! :D

################################################################################################
## Q10


# the pocture of earth crust pieces can be generated useing a simple earthquake visualizasion
bbox <- c(left = -170, bottom = -60, right = 170, top = 80)
# World_map <- get_stamenmap(bbox, zoom = 3, maptype="terrain")
# save(World_map, file='map_data/all_world_map.RData')
load(file = 'map_data/all_world_map.RData')
World_map %>% ggmap(extent = "device") + geom_point(data = worldwide, aes(y = latitude, x = longitude), alpha = 0.5, color = 'red', size = 0.1)


# Brazil, Russia and Canada are earthquake safe countries
# look at the island of hawaii! :D
# as described in question 10 alaska has the most number of earthquakes in recent years. it has 3 of 6 biggest
# recorded earthquakes in history




