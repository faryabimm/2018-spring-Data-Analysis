library(readr)
library(dplyr)
library(highcharter)
library(stringr)
library(tidyr)


constituents <- read_csv('class_data/constituents.csv')
file_names <- list.files('class_data/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('class_data/stock_dfs/', full.names = TRUE)

# stocks_data <- data.frame()
# for (i in 1:length(file_names)) {
#   company_data <- read_csv(file_paths[i]) %>%
#     select(Date, Open, Close, Volume) %>% mutate(Symbol = file_names[i])
#   rbind(stocks_data, company_data) -> stocks_data
#   print(i)
# }

# save(stocks_data, file = 'saved_data/stocks_data.RData')
load('saved_data/stocks_data.RData')


# stocks_data_full <- data.frame()
# for (i in 1:length(file_names)) {
#   company_data <- read_csv(file_paths[i]) %>% mutate(Symbol = file_names[i])
#   rbind(stocks_data_full, company_data) -> stocks_data_full
#   print(i)
# }

# save(stocks_data_full, file = 'saved_data/stocks_data_full.RData')
load('saved_data/stocks_data_full.RData')


########################################################################################
#### Q1

get_company_revenues <- function(i) {
  company_data <- read_csv(file_paths[i])
  company_name <- file_names[i]
  
  first_date <- company_data %>% arrange(Date) %>% summarise(date = first(Date))
  first_date <- first_date$date[[1]]
  
  last_date <- company_data %>% arrange(Date) %>% .[complete.cases(.),] %>% summarise(date = last(Date))
  last_date <- last_date$date[[1]]
  
  all_dates <- seq(from=first_date, to=last_date, by = 'days') %>% as.data.frame() %>% rename(Date = '.')
  
  
  all_dates %>% full_join(company_data, by = 'Date') %>%
    mutate(OneYearRev = NA, TwoYearRev = NA, FiveYearRev = NA) -> company_data_all_dates
  
  for (i in 1:nrow(company_data_all_dates)) {
    if (company_data_all_dates[i,]$Open %>% is.na) {
      company_data_all_dates[i,2:7] <- company_data_all_dates[i-1,2:7]
    }
    if (i > 365) {
      company_data_all_dates$OneYearRev[i] <- company_data_all_dates$Volume[i] - company_data_all_dates$Volume[(i-365)]
    }
    if (i > (365*2)) {
      company_data_all_dates$TwoYearRev[i] <- company_data_all_dates$Volume[i] - company_data_all_dates$Volume[(i-(365*2))]
    }
    if (i > (365*5)) {
      company_data_all_dates$FiveYearRev[i] <- company_data_all_dates$Volume[i] - company_data_all_dates$Volume[(i-(365*5))]
    }
    # print(i)
  }
  
  rbind(
    company_data_all_dates %>% arrange(desc(OneYearRev)) %>% slice(1) %>% select(Date, Revenue = OneYearRev) %>% mutate(Duration = 1),
    company_data_all_dates %>% arrange(desc(TwoYearRev)) %>% slice(1) %>% select(Date, Revenue = TwoYearRev) %>% mutate(Duration = 2),
    company_data_all_dates %>% arrange(desc(FiveYearRev)) %>% slice(1) %>% select(Date, Revenue = FiveYearRev) %>% mutate(Duration = 5)
  ) %>% mutate(Symbol = company_name) -> result
  
  return(result)
}

# company_revenue_data <- data.frame()
# n <- length(file_names)
# for (i in 1:n) {
#   rbind(company_revenue_data, get_company_revenues(i)) -> company_revenue_data
#   print(paste(i, 'out of', n))
# }


# save(company_revenue_data, file = 'saved_data/company_revenue_data.RData')
load('saved_data/company_revenue_data.RData')

company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
  filter(Duration == 1) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue)) %>% .[complete.cases(.),] %>%
  hchart(type = 'column', hcaes(x =  paste(Name, ':' , Sector), y = Revenue)) %>% hc_title(text = 'Revenue over 1 year')

company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
  filter(Duration == 2) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue)) %>% .[complete.cases(.),] %>%
  hchart(type = 'column', hcaes(x = paste(Name, ':' , Sector), y = Revenue)) %>% hc_title(text = 'Revenue over 2 years')

company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
  filter(Duration == 5) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue)) %>% .[complete.cases(.),] %>%
  hchart(type = 'column', hcaes(x = paste(Name, ':' , Sector), y = Revenue)) %>% hc_title(text = 'Revenue over 5 years')


rbind(
  company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
    filter(Duration == 1) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue)),
  company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
    filter(Duration == 2) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue)),
  company_revenue_data %>% full_join(constituents, by = 'Symbol') %>%
    filter(Duration == 5) %>% group_by(Sector) %>% top_n(1, Revenue) %>% arrange(desc(Revenue))
) %>% .[complete.cases(.),] %>%
  hchart(type = 'column', hcaes(x = paste(Name, ':' , Sector), y = Revenue, group = Duration))





########################################################################################
#### Q2

library(ggplot2)

stocks_data %>% filter((Date %>% gdata::getDay()) == 13) %>% mutate(Revenue = Close - Open) %>% 
  ggplot(aes(x = Revenue)) + geom_density()

stocks_data %>% filter((Date %>% gdata::getDay()) == 13) %>% mutate(Revenue = Close - Open) %>% filter(abs(Revenue) < 5) %>% 
  ggplot(aes(x = Revenue)) + geom_density()

stocks_data %>% mutate(is13th = (Date %>% gdata::getDay()) == 13) %>% mutate(Revenue = Close - Open) -> q2_revenue_data
t.test(Revenue~is13th, data = q2_revenue_data)


########################################################################################
#### Q3

get_company_voldiffs <- function(i) {
  company_data <- read_csv(file_paths[i])
  company_name <- file_names[i]
  
  company_data %>% mutate(RevenueDiff = NA) -> company_data
  
  for (i in 2:nrow(company_data)) {
    company_data$RevenueDiff[i] <- company_data$Volume[i] - company_data$Volume[i-1]
  }
  
  company_data %>% mutate(Symbol = company_name) -> company_data
  
  return(company_data)
}

# company_data_voldiffs <- data.frame()
# n <- length(file_names)
# for (i in 1:n) {
#   rbind(company_data_voldiffs, get_company_voldiffs(i)) -> company_data_voldiffs
#   print(paste(i, 'out of', n))
# }
# save(company_data_voldiffs, file = 'saved_data/company_data_voldiffs.RData')
load('saved_data/company_data_voldiffs.RData')

company_data_voldiffs %>% mutate(AbsRevenue = abs(RevenueDiff)) %>% group_by(Date) %>%
  summarise(Turnover = sum(as.numeric(AbsRevenue))) %>% arrange(desc(Turnover)) %>% ungroup %>% slice(1:10) -> q3_chart_data

q3_chart_data %>% hchart(type = 'column', hcaes(x = Date, y = Turnover))

q3_chart_data$Date <- q3_chart_data$Date %>% as.factor
q3_chart_data %>% hchart(type = 'column', hcaes(x = Date, y = Turnover))

########################################################################################
#### Q4

stocks_data %>% filter(Symbol == 'AAPL') %>% arrange(desc(Date)) %>% select(Open) %>% rename(Price = Open) -> apple_open_prices

mse <- function(model) {
  result = mean(model$residuals^2)
  return(result)
}

get_model_data <- function(k) {
  cbind(
    apple_open_prices,
    apple_open_prices %>% .$Price %>% lead %>% as.data.frame %>% dplyr::rename(Day1 = '.')
  ) -> model_data
  column_names <- c('Day1')
  if (k > 1) {
    for (i in 2:k) {
      cbind(
        model_data,
        model_data %>% .$Day1 %>% lead %>% as.data.frame
      ) -> model_data
      column_names <- c(paste('Day', i, sep = ''), column_names)
      colnames(model_data) <- c('Price', column_names)
    }
  }
  return(model_data)
}

find_mse_for_kday_model <- function(k) {
  model_data <- k %>% get_model_data  
  model <- lm(Price~., data = model_data)
  result <- model %>% mse
  return(result)
}


x = 1:300
y = sapply(x, find_mse_for_kday_model)

data.frame(number_of_days = x, mean_square_error = y) %>%
  hchart(type = 'line', hcaes(x = number_of_days, y = mean_square_error))

# minimum error is for 119 days of lookback


library(h2o)
h2o.init()
h2o_model_data <- 119 %>% get_model_data %>% as.h2o
h2o.glm(y = "Price", x = colnames(h2o_model_data), training_frame = h2o_model_data, family="gaussian" ,nfolds = 5)

########################################################################################
#### Q5

# q5_data <- data.frame()
# for (i in 1:length(file_names)) {
#   data_ext <- read_csv(file_paths[i]) %>% select(Date, Close)
#   colnames(q5_data)[colnames(q5_data)=="Close"] <- file_names[i]
#   if (i == 1) {
#     data_ext -> q5_data
#   } else {
#     full_join(q5_data, data_ext, by = 'Date') -> q5_data
#   }
# }
# 
# save(q5_data, file = 'saved_data/q5_data.RData')
load('saved_data/q5_data.RData')


q5_data_original <- q5_data


# filling NAs with mean of the column

for (i in 1:ncol(q5_data)) {
  if ((q5_data[which(is.na(q5_data[i])),i] %>% nrow) > 0) {
    q5_data[which(is.na(q5_data[i])),i] <- q5_data[i] %>% na.omit %>% unlist %>% mean
  }
}

#### WITH DATE

# q5_data %>% mutate(Date = as.numeric(Date)) %>% prcomp() -> q5_pca
# q5_pca %>% plot
# 
# biplot(q5_pca, cex = 0.8)
# library(ggbiplot) 
# q5_pca %>% ggbiplot(obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
# 
# 
# 
# eigv = round(q5_pca$sdev^2/sum(q5_pca$sdev^2)*100, 2)[1:10]
# eigv = data.frame(c(1:10),eigv)
# names(eigv) = c('PCs','Variance')
# plot(eigv,type = "b",col = "darkblue",lwd = 2);grid()
# 
# 
# library(qcc)
# PCA <- (q5_pca$sdev^2)[1:10]
# names(PCA) = paste0('PC', eigv$PCs)
# qcc::pareto.chart(PCA)
# 
# 
# first_3_comps <- (round(q5_pca$sdev^2, 2) %>% as.data.frame() %>% slice(1:3) %>% sum())/
#   (round(q5_pca$sdev^2, 2) %>% as.data.frame() %>% sum())
# first_3_comps %>% print



#### WITHOUT DATE

q5_data %>% select(-Date) %>% prcomp() -> q5_pca

cbind(
  q5_data %>% select(Date),
  (q5_data %>% select(-Date) %>% as.matrix) %*% (q5_pca$rotation %>% as.matrix) %>% .[,1:10]
) -> q8_10_pca_data

q5_pca %>% plot

biplot(q5_pca, cex = 0.8)
library(ggbiplot) 
q5_pca %>% ggbiplot(obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)



eigv = round(q5_pca$sdev^2/sum(q5_pca$sdev^2)*100, 2)[1:10]
eigv = data.frame(c(1:10),eigv)
names(eigv) = c('PCs','Variance')
plot(eigv,type = "b",col = "darkblue",lwd = 2);grid()


library(qcc)
PCA <- (q5_pca$sdev^2)[1:10]
names(PCA) = paste0('PC', eigv$PCs)
qcc::pareto.chart(PCA)


first_3_comps <- (round(q5_pca$sdev^2, 2) %>% as.data.frame() %>% slice(1:3) %>% sum())/
  (round(q5_pca$sdev^2, 2) %>% as.data.frame() %>% sum())
first_3_comps %>% print


########################################################################################
#### Q6

stocks_data %>% full_join(constituents, by = 'Symbol') %>% group_by(Sector, Date) %>%
  summarise(OpenMean = mean(Open, na.rm = TRUE)) %>% spread(key = 'Sector', value = 'OpenMean') -> sectors_daily_data

indexes_data <- read_csv('class_data/indexes.csv')


full_join(
  sectors_daily_data,
  indexes_data,
  by = 'Date'
) -> q6_data

#### first approach, removing incomplete records

q6_data %>% .[complete.cases(.),] %>% select(-Date) %>% prcomp -> q6_pca_first_approach

q6_pca_first_approach %>% ggbiplot(obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)

#### second approach, filling NAs with mean of the column

q6_data_second_approach <- q6_data %>% select(-Date)

for (i in 1:ncol(q6_data_second_approach)) {
  if ((q6_data_second_approach[which(is.na(q6_data_second_approach[i])),i] %>% nrow) > 0) {
    q6_data_second_approach[which(is.na(q6_data_second_approach[i])),i] <- q6_data_second_approach[i] %>% na.omit %>% unlist %>% mean
  }
}

q6_data_second_approach %>% prcomp -> q6_pca_second_approach
q6_pca_second_approach %>% ggbiplot(obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)


########################################################################################
#### Q7

stocks_data_full %>% filter(Symbol == 'AAPL') %>% arrange(desc(Date)) %>% select(-Symbol) -> apple_stocks_data

apple_stocks_data %>% select(-Date) %>% prcomp -> q7_pca


cbind(
  apple_stocks_data %>% select(Open) %>% dplyr::rename(Price = Open),
  (apple_stocks_data %>% select(-Date) %>% as.matrix) %*% (q7_pca$rotation %>% as.matrix) %>% as.data.frame
) %>% select(Price, PC1) %>% dplyr::rename(Day1 = PC1) -> apple_open_prices

get_model_data <- function(k) {
  cbind(
    apple_open_prices,
    apple_open_prices %>% .$Day1 %>% lead %>% as.data.frame %>% dplyr::rename(Day2 = '.')
  ) -> model_data
  column_names <- c('Day2', 'Day1')
  if (k > 1) {
    for (i in 2:k) {
      cbind(
        model_data,
        model_data %>% .$Day1 %>% lead %>% as.data.frame
      ) -> model_data
      column_names <- c(paste('Day', (i+1), sep = ''), column_names)
      colnames(model_data) <- c('Price', column_names)
    }
  }
  return(model_data)
}

library(h2o)
h2o.init()
find_mse_for_kday_model <- function(k) {
  model_data <- k %>% get_model_data %>% as.h2o
  model <- h2o.glm(y = "Price", x = colnames(model_data), training_frame = model_data, family="gaussian" ,nfolds = 5)
  result <- model %>% h2o.performance(model_data) %>% h2o.mse
  return(result)
}

# renaming the column and dataframe in order to use it with the same methods from Q4


# x = 1:100
# y = sapply(x, find_mse_for_kday_model)
# q7_data <- data.frame(number_of_days = x, mean_square_error = y)
# save(q7_data, file = 'Desktop/saved_data/q7_data.RData')

load('Desktop/saved_data/q7_data.RData')

 q7_data %>% hchart(type = 'line', hcaes(x = number_of_days, y = mean_square_error))

########################################################################################
#### Q8

cbind(
  indexes_data %>% select(Date, SP500),
  indexes_data %>% .$SP500 %>% lead %>% as.data.frame %>% dplyr::rename(Before = '.')
) %>% mutate(relative_revenue = (SP500 - Before)/SP500) %>% .[complete.cases(.),] -> sp500_rel_rev_data

sp500_rel_rev_data %>% .$relative_revenue %>% hcdensity()

library(nortest)

pearson.test(sp500_rel_rev_data %>% .$relative_revenue)
chisq.test(pearson.test(sp500_rel_rev_data %>% .$relative_revenue), 'normal')
ks.test(sp500_rel_rev_data %>% .$relative_revenue, 'pnorm')


car::qqp(sp500_rel_rev_data %>% .$relative_revenue)


q8_10_pca_data %>% head %>% View

full_join(
  sp500_rel_rev_data %>% mutate(Profit = relative_revenue > 0) %>% select(Date, Profit),
  q8_10_pca_data,
  by = 'Date'
) %>% .[complete.cases(.),] %>% select(-Date) -> q8_model_data

q8_h2o_model_data <- as.h2o(q8_model_data)
h2o.glm(y = "Profit", x = colnames(q8_h2o_model_data), training_frame = q8_h2o_model_data, family="binomial" ,nfolds = 5)


########################################################################################
#### Q9

library(EBImage)
image <- readImage('hw_09/images/stock.jpg') %>% flip

compress_image <- function(image, comp_count){
  red.weigth   <- 0.2989
  green.weigth <- 0.587
  blue.weigth  <- 0.114
  image <- red.weigth * imageData(pic)[,,1] +
           green.weigth * imageData(pic)[,,2] +
           blue.weigth  * imageData(pic)[,,3]
  pca <- image %>% prcomp(scale=TRUE)
  
  feature_vector <- pca$rotation[,1:comp_count]
  compact_data <- t(feature_vector) %*% t(image)
  approximate_image <- t(feature_vector %*% compact_data)
  return(approximate_image)
}

save_image <- function(comp_count) {
  pic %>% compress_image(comp_count) -> compressed_image
  compressed_image %>% flip %>% 
    writeImage(paste0("hw_09/images/image_", str_pad(as.character(comp_count), 3, 'left','0'), ".jpg"))
}
  
# image has 412 principal components
for (i in 1:412) {
  i %>% save_image
}

image_names <- list.files(path="hw_09/images", pattern="image_", full.names=T, recursive=FALSE) %>% sort
image_sizes <- image_names %>% file.info %>% .$size

file.info("hw_09/images/stock.jpg")$size -> main_image_size

data.frame(number_of_pcs = 1:412, image_sizes = image_sizes) %>%
  hchart(type = 'line', hcaes(x = number_of_pcs, y = image_sizes))


stocks_data %>% filter(Symbol == "AAPL") %>% select(Date, Open) %>% plot()
