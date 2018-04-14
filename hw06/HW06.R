
library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)
library(tidyr)

test_data <- read_csv('house/test.csv')
train_data <- read_csv('house/train.csv')


################################################################################################
#### Q1

train_data %>% select_if(is.numeric) %>% select(-Id) -> q1_data
q1_data %>% cor(use = "pairwise.complete.obs") -> q1_cor
q1_cor %>% as.data.frame() %>% select(SalePrice) %>% cbind(Factor=rownames(.)) %>%
  arrange(desc(SalePrice)) %>% slice(2:11) -> q1_top_factors


q1_top_factors %>% select(Factor) %>% .$Factor %>% as.character() -> top_factors
q1_top_factors %>% print()
q1_cor %>% hchart()

library(psych)
q1_data %>% corr.test() %>% .$p %>% print()

library(corrplot)
q1_data %>% cor.mtest(conf.level = .95) -> res1
q1_cor %>% corrplot(type = 'upper')
q1_cor %>% corrplot(type = 'upper', p.mat = res1$p, sig.level = 1e-90, pch.cex = 1.5)




################################################################################################
#### Q2

train_data %>% select(c(top_factors, 'SalePrice')) %>% plot()

################################################################################################
#### Q3

library(magrittr)

train_data %>% select(c(top_factors, 'SalePrice')) %$% lm(SalePrice ~ ., .) -> q3_lm
q3_lm %>% summary() %>% print()

################################################################################################
#### Q4

cbind(
  PredictedPrice = predict.lm(q3_lm, train_data %>% select(c(top_factors))),
  train_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(x = PredictedPrice, y = SalePrice))


cbind(
  Residuals = residuals(q3_lm),
  train_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(y = Residuals, x = SalePrice), color = 'red')


###############################################################################################
#### Q5

# q3_lm %>% summary() %>% print()

q3_lm %>% summary() %>% .$fstatistic
q3_lm %>% summary() %>% .$r.squared
q3_lm %>% summary() %>% .$adj.r.squared

###############################################################################################
#### Q6

train_data %>% select(c(top_factors, 'SalePrice')) %>%
  select(-GarageArea, -TotRmsAbvGrd, -FullBath, -`1stFlrSF`) -> q6_data

q6_data %$% lm(SalePrice ~ ., .) -> q6_lm

q6_lm %>% summary() %>% print()

cbind(
  PredictedPrice = predict.lm(q6_lm, train_data %>% select(c(top_factors))),
  train_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(x = PredictedPrice, y = SalePrice))


cbind(
  Residuals = residuals(q6_lm),
  train_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(y = Residuals, x = SalePrice), color = 'red')


###############################################################################################
#### Q7


#### Constant variance

q6_data %>% mutate(SalePriceLog = log(SalePrice)) -> q7_data
lm(SalePriceLog ~ . - SalePrice, q7_data) -> q7_lm

q7_lm %>% summary() %>% print()

cbind(
  PredictedPrice = fitted.values(q7_lm) %>% exp(),
  q7_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(x = PredictedPrice, y = SalePrice))

cbind(
  ((fitted.values(q7_lm) %>% exp()) - (q7_data %>% select(SalePrice))) %>% rename(Residuals = SalePrice),
  q7_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(y = Residuals, x = SalePrice), color = 'red')

#### using VIF for independence test
library(car)
q6_lm %>% vif() %>% print()
#### using qqPlot for normality test
car::qqPlot(q7_lm, id.method="identify", simulate = TRUE, main="Q-Q Plot")


###############################################################################################
#### Q8

n <- nrow(q7_data)
q8_train_index <- sample(1:n, 0.8*n)
q8_test_index <- setdiff(1:n, q8_train_index)

q8_train_data <- q7_data[q8_train_index,]
q8_test_data <- q7_data[q8_test_index,]

lm(SalePriceLog ~ . - SalePrice, q8_train_data) -> q8_lm


cbind(
  PredictedPrices = predict.lm(q8_lm, q8_test_data) %>% exp(),
  q8_test_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(x = PredictedPrices, y = SalePrice))


cbind(
  ((predict(q8_lm, q8_test_data) %>% exp()) - (q8_test_data %>% select(SalePrice))) %>% rename(Residuals = SalePrice),
  q8_test_data %>% select(SalePrice)
) -> q8_residual_data

q8_residual_data %>% hchart(type = 'point', hcaes(y = Residuals, x = SalePrice), color = 'red')

q8_residual_data_vector <- q8_residual_data %>% select(Residuals) %>% .$Residuals

q8_mu <- mean(q8_residual_data_vector)
q8_sd <- sd(q8_residual_data_vector)

q8_mu %>% print()
q8_sd %>% print()


###############################################################################################
#### Q9


# I will compare model variables agains one another in order to 
# observe curve similarities to non linear functions.

q7_data %>% plot()

# TotalBsmtSF vs Sale price is a sqrt-like function
# I think there is no more obvios non linearity evidences.

# Now I recreate the mlodel and use this non linear relations instead:

# q7_data[which(q7_data$TotalBsmtSF == 0),]$TotalBsmtSF = 1
q9_lm <- lm(SalePriceLog ~ I(OverallQual^1.5) + I(log(GrLivArea)) +
              GarageCars + YearRemodAdd + YearBuilt + I(sqrt(TotalBsmtSF)), q7_data)

q9_lm %>% summary()



cbind(
  PredictedPrices = predict.lm(q9_lm, q8_test_data) %>% exp(),
  q8_test_data %>% select(SalePrice)
) %>% hchart(type = 'point', hcaes(x = PredictedPrices, y = SalePrice))


cbind(
  ((predict(q9_lm, q8_test_data) %>% exp()) - (q8_test_data %>% select(SalePrice))) %>% rename(Residuals = SalePrice),
  q8_test_data %>% select(SalePrice)
) -> q9_residual_data

q9_residual_data %>% hchart(type = 'point', hcaes(y = Residuals, x = SalePrice), color = 'red')

q9_residual_data_vector <- q9_residual_data %>% select(Residuals) %>% .$Residuals

q9_mu <- mean(q9_residual_data_vector)
q9_sd <- sd(q9_residual_data_vector)

q9_mu %>% print()
q9_sd %>% print()


###############################################################################################
#### Q10

predict(q9_lm, test_data) %>% exp() %>% View()

# The kernel is built, and the result submitted.
# The 'Public Score' is 0.16548
# The rank is 2960 out of 4795 at the time of writing this report
# Link to public kernel page: https://www.kaggle.com/faryabimm/kernela5b4857479/
# Kaggle kernel script file is also included with the report. please look for the file 'HW6_kaggle_version.R'


