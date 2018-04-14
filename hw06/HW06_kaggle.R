#------------------------------           In the name of God, the merciful           ------------------------------#
#------------------------------   script by Mohammadmahdi Faryabi -- 13 April 2018   ------------------------------#
#------------------------------          Data Analysis course - spring 2018          ------------------------------#
#------------------------------           Sharif  University of Technology           ------------------------------#

# loading required libraries and data
library(readr)
library(dplyr)

test_data <- read_csv('../input/test.csv')
train_data <- read_csv('../input/train.csv')

# removing Id column and finding correlations between factors
train_data %>% select_if(is.numeric) %>% select(-Id) %>% cor(use = "pairwise.complete.obs") -> train_data_numeric_corrs

# finding 7 suitable factors to use in regression model
train_data_numeric_corrs %>% as.data.frame() %>% select(SalePrice) %>%
  cbind(Factor=rownames(.)) %>% arrange(desc(SalePrice)) %>% slice(2:11) -> top_factors
top_factors %>% select(Factor) %>% .$Factor %>% as.character() -> top_factor_names

# extracting data for model
train_data %>% select(c(top_factor_names, 'SalePrice')) %>%
  select(-GarageArea, -TotRmsAbvGrd, -FullBath, -`1stFlrSF`) %>%
  mutate(SalePriceLog = log(SalePrice)) -> model_data

# creating regression model
model <- lm(SalePriceLog ~ OverallQual + GrLivArea + GarageCars +
              YearRemodAdd + YearBuilt + I(sqrt(TotalBsmtSF)), model_data)

# cleaning up test data
test_data[which(is.na(test_data$GarageCars)),]$GarageCars <- 0
test_data[which(is.na(test_data$GarageArea)),]$GarageArea <- 0
test_data[which(is.na(test_data$TotalBsmtSF)),]$TotalBsmtSF <- 0


# turning off scientific notation, computing and saving submission
options(scipen = 5)

cbind(
  test_data %>% select(Id),
  SalePrice = predict(model, test_data) %>% exp()
) -> submissin_data

submissin_data$Id <- as.integer(submissin_data$Id)

write_csv(submissin_data, './submission.csv')
