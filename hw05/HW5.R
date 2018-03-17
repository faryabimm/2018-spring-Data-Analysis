library(readr)
library(dplyr)
library(tidyr)
library(coin)
library(highcharter)
library(magrittr)
library(ggplot2)
library(ggmosaic)


############################################################################################################
# Q1

stores <- c(102, 300, 102, 100, 205, 105, 71, 92)
ks.test(stores, rep(1/8, 8))
ks.test(stores, 'pnorm')

############################################################################################################
# Q2

q2_classical <- c(50, 50, 60, 70, 75, 80, 90, 85)
q2_modern <- c(55, 75, 80, 90, 105, 65)


data.frame(sellings = c(q2_classical, q2_modern), type = c(rep('CLASSIC', 8), rep('MODERN', 6))) -> q2_full_data

coin::oneway_test(sellings~type, data=q2_full_data, distribution="exact")


oldwarn <- getOption('warn')
options(warn = -1)
wilcox.test(sellings~type, data=q2_full_data, distribution="exact")
options(warn = oldwarn)

############################################################################################################
# Q3

data.frame(
  before = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904),
  after  = c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)
  ) %>% mutate(index = 1:n()) -> q3_full_data

q3_full_data %$% wilcox.test(.$before, .$after, paired = TRUE)
q3_full_data %$% t.test(.$before, .$after, paired = TRUE)

q3_full_data %>% gather(after, before, key = 'when', value = 'score') %>%
  hchart(type = 'column', hcaes(x = index, y = score, group = when))


############################################################################################################
# Q4

q4_data <- data.frame(row.names = 1:5,
           White = c(510, 720, 930, 754, 105),
           Blue  = c(925, 735, 753, 685, NA ),
           Red   = c(730, 745, 875, 610, NA ))

q4_data %>% as.matrix() %>% friedman.test()

q4_data %>% mutate(store_index = rownames(.)) %>%
  gather(White, Blue, Red, key = 'color', value = 'sellings') %>%
  ggplot() + geom_mosaic(aes(x = product(store_index, color), weight = sellings)) + 
  labs(x = 'color of product', y = 'store (1, 2, 3, 4, 5)')

q4_data %>% plot()

############################################################################################################
# Q5

q5_data <- read_csv('Desktop/hw_05/data/tv.csv')
q5_data %>% as.matrix() %>% friedman.test()

############################################################################################################
# Q6

data.frame(Always = c(151, 802, 753), Sometimes = c(252, 603, 55), Never = c(603, 405, 408),
                      row.names = c('Small', 'Medium', 'Large')) -> q6_full_data

q6_full_data %>% chisq.test()
q6_full_data %>% mutate(city_size = rownames(.)) %>%
  gather(Always, Sometimes, Never, key = 'usage_frequency', value = 'score') %>%
  ggplot() + geom_mosaic(aes(x = product(city_size, usage_frequency), weight = score)) + 
  labs(x = 'usage frequency', y = 'city size (Small, Medium, Large)')

q6_full_data %>% plot()

############################################################################################################
# Q7

q7_data <- read_csv('Desktop/hw_05/data/consumption.csv')
q7_data %$% cor.test(.$A, .$B, method = 'spearman')
q7_data %>% hchart(type = 'point', color = 'red', hcaes(x = A, y = B))

############################################################################################################
# Q8

data.frame(row.names = c('Price', 'Design', 'Color'),
                      Male   = c(301, 353, 558),
                      Female = c(502, 155, 153)) -> q8_full_data

q8_full_data %>% chisq.test()

q8_full_data %>% mutate(product_factor = rownames(.)) %>%
  gather(Male, Female, key = 'gender', value = 'count') %>%
  ggplot() + geom_mosaic(aes(x = product(gender, product_factor), weight = count)) + 
  labs(y = 'customer gender (Male, Female)', x = 'product factor')
