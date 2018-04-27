library(readr)
library(dplyr)
library(highcharter)
library(ggplot2)

source('Desktop/unbalanced_functions.R')

data <- read_csv('Desktop/data/murder_suicide.csv')

###############################################################################################
## Q1

data %>% colnames -> colnames
data %>% filter(AgeType == 1) -> data

colnames %>% setdiff(c('EducationReportingFlag', 'AgeType', 'AgeRecode12',
                       'AgeRecode27', 'AgeRecode52', 'InfantAgeRecode22',
                       'RaceImputationFlag', 'RaceRecode3', 'RaceRecode5',
                       'BridgedRaceFlag', 'CurrentDataYear', 'AgeSubstitutionFlag',
                       'CauseRecode358', 'CauseRecode113', 'InfantCauseRecode130',
                       'HispanicOrigin', 'Id')) -> non_redundant_colnames

non_redundant_colnames %>% setdiff(c('HispanicOriginRaceRecode', 'NumberOfEntityAxisConditions',
                                     'NumberOfRecordAxisConditions', 'DayOfWeekOfDeath', 'Race',
                                     'MonthOfDeath', 'PlaceOfDeathAndDecedentsStatus')) -> minified_non_redundant_colnames

data %>% select(non_redundant_colnames) %>%
  mutate_if(is.character, factor) %>% mutate_if(is.factor, as.numeric) -> q1_nr_data

data %>% select(minified_non_redundant_colnames) %>%
  mutate_if(is.character, factor) %>% mutate_if(is.factor, as.numeric) -> minified_q1_nr_data


minified_q1_nr_data %>% cor(use = 'pairwise.complete.obs') -> q1_cor
q1_cor %>% hchart()

library(corrplot)
q1_nr_data %>% cor.mtest(conf.level = .95) -> res1
q1_cor %>% corrplot(type = 'upper')
q1_cor %>% corrplot(type = 'upper', p.mat = res1$p, sig.level = 1e-90, pch.cex = 1.5)

minified_q1_nr_data %>% slice(1:500) %>% plot()


###############################################################################################
## Q2

# cbind(
#   data %>% select(MethodOfDisposition),
#   data %>% mutate_if(is.character, factor) %>% mutate_if(is.factor, as.numeric) %>% select(MethodOfDisposition)
# ) %>% View()
# q1_nr_data %>% mutate(ms = ifelse(MannerOfDeath %in% c(2, 6), 'Suicide',
#                                   ifelse(MannerOfDeath == 3, 'Murder', 'Other'))) -> q2_data

############################

male_count <- q2_data %>% filter(Sex == 2) %>% count() %>% as.integer()
female_count <- q2_data %>% filter(Sex != 2) %>% count() %>% as.integer()

q2_data %>% group_by(Sex, ms) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(Sex = ifelse(Sex == 2, 'Male', 'Female')) %>%
  mutate(ratio = ifelse(Sex == 'Male', count/male_count, count/female_count)) %>% 
  hchart(type = 'column', hcaes(x = ms, y = ratio, group = Sex))

kruskal.test(ms ~ Sex, data = q2_data) %>% print

############################

race_counts <- q2_data %>% group_by(Race) %>% summarise(race_count = n())
race_names <- read_csv('Desktop/data/Race.csv')

full_join(
  race_counts,
  race_names %>% rename(Race = Code),
  by = 'Race'
) %>% full_join(
  q2_data,
  by = 'Race'
) %>% group_by(Race, ms, race_count, Description) %>% summarise(count = n()) %>% ungroup() %>%
  mutate(ratio = count/race_count) %>% .[complete.cases(.),] %>% 
  hchart(type = 'column', hcaes(x = Description, y = ratio, group = ms))

kruskal.test(ms ~ Race, data = q2_data) %>% print

############################

q2_data_1989 <- q2_data %>% filter(Education1989Revision != 0)
q2_data_2003 <- q2_data %>% filter(Education2003Revision != 0)


edu_1989_names <- read_csv('Desktop/data/Education1989Revision.csv')
edu_1989_names %>% mutate(Description = ifelse(Description == 'Years of elementary school', paste(Code, Description, sep = ' '), Description)) -> edu_1989_names

full_join(
  q2_data %>% group_by(Education1989Revision) %>% summarise(edu_count = n()) %>% filter(Education1989Revision != 0),
  edu_1989_names %>% rename(Education1989Revision = Code),
  by = 'Education1989Revision'
) %>% full_join(
  q2_data_1989,
  by = 'Education1989Revision'
) %>% group_by(Education1989Revision, ms, edu_count, Description) %>% summarise(count = n()) %>% ungroup() %>%
  mutate(ratio = count/edu_count) %>% .[complete.cases(.),] %>% 
  hchart(type = 'column', hcaes(x = Description, y = ratio, group = ms))


kruskal.test(ms ~ Education1989Revision, data = q2_data_1989) %>% print


############################

edu_2003_names <- read_csv('Desktop/data/Education2003Revision.csv')

full_join(
  q2_data %>% group_by(Education2003Revision) %>% summarise(edu_count = n()) %>% filter(Education2003Revision != 0),
  edu_2003_names %>% rename(Education2003Revision = Code),
  by = 'Education2003Revision'
) %>% full_join(
  q2_data_2003,
  by = 'Education2003Revision'
) %>% group_by(Education2003Revision, ms, edu_count, Description) %>% summarise(count = n()) %>% ungroup() %>%
  mutate(ratio = count/edu_count) %>% .[complete.cases(.),] %>% 
  hchart(type = 'column', hcaes(x = Description, y = ratio, group = ms))


kruskal.test(ms ~ Education2003Revision, data = q2_data_2003) %>% print

############################

full_join(
  q2_data %>% filter(Age < 250) %>% group_by(Age) %>% summarise(age_count = n()),
  q2_data,
  by = 'Age'
) %>% 
  group_by(Age, ms, age_count) %>% summarise(count = n()) %>% ungroup() %>%
  mutate(ratio = count/age_count) %>% .[complete.cases(.),] %>%
  hchart(type = 'spline', hcaes(x = Age, y = ratio, group = ms))
  
q2_data %>% filter(Age < 250) %>%
  ggplot(aes(y = Age, x = ms)) + geom_boxplot(notch=TRUE, outlier.colour="red", outlier.shape=8)
 

kruskal.test(ms ~ Age, data = q2_data) %>% print

############################

disposition_names <- read_csv('Desktop/data/MethodOfDisposition-custom.csv')
disposition_count <- q2_data %>% group_by(MethodOfDisposition) %>% summarise(dispo_count = n())

cbind(
  disposition_names,
  disposition_count
) %>% full_join(
  q2_data,
  by = 'MethodOfDisposition'
) %>% group_by(ms, Description, dispo_count) %>% summarise(count = n()) %>% ungroup() %>% 
  mutate(ratio = count/dispo_count) -> q2_dispo_data

q2_dispo_data %>% hchart(type = 'column', hcaes(x = Description, y = ratio, group = ms))
q2_dispo_data %>% hchart(type = 'column', hcaes(x = Description, y = count, group = ms))

kruskal.test(ms ~ MethodOfDisposition, data = q2_data) %>% print


###############################################################################################
## Q3

minified_q1_nr_data %>% mutate(MannerOfDeath = MannerOfDeath %in% c(2, 6)) -> q3_data
q3_data %>% filter(Age < 250) -> q3_data

glm(MannerOfDeath~., data = q3_data, family = binomial(link = 'logit')) -> q3_logit_model_1
q3_logit_model_1 %>% summary.glm() %>% print


glm(MannerOfDeath~.-InjuryAtWork-Education1989Revision, data = q3_data, binomial(link = 'logit')) -> q3_logit_model_2
q3_logit_model_2 %>% summary.glm() %>% print

###############################################################################################
## Q4

q3_data %>% mutate(prediction = fitted(q3_logit_model_2)) %>%
  ggplot(aes(x = Age, y = prediction, color = MannerOfDeath)) + geom_point()


q3_data %>% mutate(prediction = predict(q3_logit_model_2, type = 'response')) %>%
  mutate(MannerOfDeath = as.integer(MannerOfDeath)) %>%
  ggplot(aes(x = Age, y = MannerOfDeath)) + 
  geom_point(aes(x =  Age, y = prediction), color = 'red', alpha = 0.2) + 
  geom_point(alpha = 0.005, size = 3)

library(ggthemes)

q3_data %>% mutate(prediction = predict(q3_logit_model_2, type = 'response')) %>%
  ggplot(aes(fitted(q3_logit_model_2), color = as.factor(MannerOfDeath))) + 
  geom_density(size = 0.5) +
  ggtitle("Training Set's Predicted Score") + 
  scale_color_economist(name = "data", labels = c("negative", "positive"))


table(q3_data$MannerOfDeath,ifelse(fitted(q3_logit_model_2)>0.3,1,0)) %>% plot()

###############################################################################################
## Q5
q3_data %>% nrow -> n_data
q5_sample <- sample(1:n_data, size = 0.8*n_data, replace = FALSE)

q3_data %>% mutate(MannerOfDeath = as.integer(MannerOfDeath)) %>% select(-Education1989Revision,-InjuryAtWork) -> q5_data



q4_model_data <- q5_data %>% .[q5_sample,]
q4_test_data <- q5_data %>% .[-q5_sample,]

glm(MannerOfDeath~., data = q4_model_data, family = 'binomial') -> q4_logit_model
q4_test_data$prediction   <- predict.glm(q4_logit_model, newdata = q4_test_data  , type = "response")
q4_model_data$prediction  <- predict.glm(q4_logit_model, newdata = q4_model_data , type = "response")
q4_logit_model %>% summary.glm()

# suicide is TRUE, murder is FALSE

q4_test_data %>% mutate(prediction = predict.glm(q4_logit_model,q4_test_data ,type = 'response')) %>%
  select(prediction, MannerOfDeath) %>% mutate(prediction_end = prediction > 0.5) -> q4_calc_data

q4_calc_data %>% summarise(P = sum(prediction_end),
                           N = n() -  sum(prediction_end),
                           TP = sum(prediction_end & MannerOfDeath),
                           TN = sum(!prediction_end & !MannerOfDeath),
                           FP = sum(prediction_end & !MannerOfDeath),
                           FN = sum(!prediction_end & MannerOfDeath)) %>% 
  mutate(ACC = (TP+TN)/(P+N),
         FPR = 1- (TN/N),
         TPR = TP/P) %>% print

table(q4_test_data$MannerOfDeath, ifelse(predict.glm(q4_logit_model, q4_test_data, type = 'response')>0.5,1,0)) %>%
  plot()

cm_info <- ConfusionMatrixInfo(data = q4_test_data, predict = "prediction",actual = "MannerOfDeath", cutoff = .5 )
cm_info$plot


accuracy_info <- AccuracyCutoffInfo(train = q4_model_data, test = q4_test_data, predict = "prediction", actual = "MannerOfDeath")
accuracy_info$plot

calculate_acc <- function(data, cutoff) {
  P <- sum(data$prediction > cutoff)
  N <- sum(data$prediction <= cutoff)
  TP <- sum((data$prediction > cutoff) & (data$MannerOfDeath == 1))
  TN <- sum(!(data$prediction > cutoff) & !(data$MannerOfDeath == 1))
  
  ACC <- (TP+TN)/(P + N)
  
  return(ACC)
}
draw_acc_diag <- function(gaps) {
  sapply(seq(0, 1, gaps), function(x) {calculate_acc(q4_test_data, x)}) -> all_test_acc_vals
  sapply(seq(0, 1, gaps), function(x) {calculate_acc(q4_model_data, x)}) -> all_model_acc_vals
  rbind(
    cbind(
      seq(0, 1, gaps) %>% as.data.frame %>% rename(cutoff = '.'),
      accuracy = all_test_acc_vals,
      data = 'test'
    )
    ,
    cbind(
      seq(0, 1, gaps) %>% as.data.frame %>% rename(cutoff = '.'),
      accuracy = all_model_acc_vals,
      data = 'train'
    )
  ) %>% hchart(type = 'line', hcaes(x = cutoff, y = accuracy, group = data))
}

draw_acc_diag(0.005)

q5_data %>% colnames


###############################################################################################
## Q6

# draw_acc_diag(0.0005)

###############################################################################################
## Q7

cost_fp = 100
cost_fn = 200
roc_info = ROCInfo(data = cm_info$data, predict = "predict", 
                    actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn)
grid.draw(roc_info$plot)

###############################################################################################
## Q8

library(h2o)

h2o.init()

q8_data <- as.h2o(q5_data)

h2o_model <- h2o.glm(y = "MannerOfDeath", x= colnames(q8_data),
                training_frame = q8_data, family="binomial" ,nfolds = 5)

h2o_model %>% print
