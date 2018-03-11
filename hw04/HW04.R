library(readr)
library(dplyr)
library(tidyr)
library(highcharter)
library(ggplot2)

tims_2015 <- read_rds('timss15_grade_8/timss_2015.rds')
bcg <- read_rds('timss15_grade_8/data/bcg.rds')
btm <- read_rds('timss15_grade_8/data/btm.rds')
bts <- read_rds('timss15_grade_8/data/bts.rds')
bsg <- read_rds('timss15_grade_8/data/bsg.rds')
bsr <- read_rds('timss15_grade_8/data/bsr.rds')
bst <- read_rds('timss15_grade_8/data/bst.rds')
bsa <- read_rds('timss15_grade_8/data/bsa.rds')


################################################################################################################
# Q1

stud_teacher_country <- bst %>% select(idcntry, idtealin, idstud) # no unique needed.
stud_math_score <- bsa %>% select(idcntry, idstud, idschool, bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05) %>%
  group_by(idcntry, idstud, idschool) %>% summarise(math_score = (bsmmat01+bsmmat02+bsmmat03+bsmmat04+bsmmat05)/5)
teacher_sat_inv_score <- btm %>% select(idcntry, idtealin, btbg10b)


q1_full_data <- full_join(stud_teacher_country, stud_math_score) %>% right_join(teacher_sat_inv_score)

aov(math_score~btbg10b, data = q1_full_data) %>% summary.aov()

q1_full_data %>% select(teacher_satisfaction = btbg10b, math_score) %>% group_by(teacher_satisfaction) %>% 
  summarise(mean_math_score = mean(math_score)) %>% hchart(type = 'line', hcaes(x = teacher_satisfaction, y = mean_math_score))

################################################################################################################
# Q2

# bsa %>% select(idcntry, idstud, bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05) %>% View()
# btm %>% select(idcntry, idtealin, btbg10a, btbg10b, btbg10c, btbg10d, btbg10e, btbg10f, btbg10g) %>%  View()

bsg %>% select(idcntry, idstud, bsbg07a, bsbg07b)%>% filter(bsbg07a != 8 & bsbg07b != 8) %>%
  mutate(parent_edu = bsbg07a+bsbg07b) %>% 
  select(idcntry, idstud, parent_edu) -> parent_edu

full_join(parent_edu, stud_math_score) %>% .[complete.cases(.),]-> stud_parent
stud_parent$math_score <- as.numeric(stud_parent$math_score)
ggplot(stud_parent, aes(x = math_score, y = parent_edu))+geom_hex()
stud_parent %>% group_by(parent_edu) %>% summarise(mean_math_score = mean(math_score)) %>%
  hchart(type = 'line', hcaes(x = parent_edu, y = mean_math_score))

aov(math_score~parent_edu, stud_parent)->fit_q2
summary.aov(fit_q2)

################################################################################################################
# Q3

# in order to calculate a factor for home amenities, I've used the following factors:
# * numbrt of books (as a number ranging from 0 to 4
# * number of digital information devices (as a number ranging from 0 to 4)
# * 11 factors mentioned in student question 6 (as a number ranging from 1 to 11)
# then summed them up and calculated a numeric factor indicating home amenities level for student.


bsg %>% select(idcntry, idstud, book = bsbg04, comp = bsbg05, a = bsbg06a, b = bsbg06b, c = bsbg06c, d = bsbg06d,
                e = bsbg06e, f = bsbg06f, g = bsbg06g, h = bsbg06h, i = bsbg06i, j = bsbg06j, k = bsbg06k) %>% 
  group_by(idcntry, idstud) %>% summarise(home_emnities = book+comp+a+b+c+d+e+f+g+h+i+j+k-13) %>%
  .[complete.cases(.),] -> student_home_amnities

full_join(stud_math_score, student_home_amnities) %>% .[complete.cases(.),] -> q3_full_data

q3_full_data %>% ggplot(aes(x = home_emnities, y = math_score)) + geom_point()
q3_full_data %>% ggplot(aes(x = home_emnities, y = math_score)) + geom_hex()

lm(math_score~home_emnities, data = q3_full_data) -> fit_q3
summary.lm(fit_q3)


################################################################################################################
# Q4

################################################################################################################
# Q5

btm %>% select(idcntry, idtealin, teacher_edu_level = btbg04, experience = btbg01) -> teacher_edu_level

full_join(stud_teacher_country, stud_math_score) %>% right_join(teacher_edu_level) %>% .[complete.cases(.),] -> q5_full_data
aov(math_score~experience*teacher_edu_level, data = q5_full_data) %>% summary.aov()


q5_full_data %>% group_by(teacher_edu_level) %>% summarise(mean_math_score = mean(math_score)) %>%
  hchart(type = 'line', hcaes(x = teacher_edu_level, y = mean_math_score))

q5_full_data %>% mutate(experience = (experience %/% 5) * 5) %>% group_by(experience) %>% summarise(mean_math_score = mean(math_score)) %>%
  hchart(type = 'line', hcaes(x = experience, y = mean_math_score))

################################################################################################################
# Q6

tims_2015 %>%
  select(question, students_per_question_male,students_per_question_female, cognitive_domain, content_domain,
         correct_ratio_per_question_female, correct_ratio_per_question_male) %>%
  filter(cognitive_domain == 'Applying', content_domain == 'Geometry') %>% group_by(question) %>%
  mutate(female_cnt = students_per_question_female*correct_ratio_per_question_female, 
         male_cnt = students_per_question_male*correct_ratio_per_question_male) %>% 
  summarise(female = sum(female_cnt), male = sum(male_cnt)) %>% select(question, female, male) %>%
  tidyr::gather('male', 'female', key = 'gender', value = 'score') -> q6_full_data

t.test(score~gender, data = q6_full_data, alt = 'greater') # boys better than girls
t.test(score~gender, data = q6_full_data, alt = 'less') # girls better than boys
t.test(score~gender, data = q6_full_data, alt = 'two.sided') # one is better than other

q6_full_data %>% hchart(type = 'column', hcaes(group = gender, y = score))
q6_full_data %>% group_by(gender) %>% summarise(score = sum(score)) %>%
  hchart(type = 'column', hcaes(group = gender, y = score))

################################################################################################################
# Q7

bsg %>% select(idcntry, idstud, breakfast = bsbg12) %>% full_join(stud_math_score) %>%
  .[complete.cases(.),] -> q7_full_data

aov(math_score~breakfast, data = q7_full_data) -> fit_q7
summary.aov(fit_q7)

q7_full_data %>% group_by(breakfast) %>% summarise(mean_math_score = mean(math_score)) %>%
  hchart(type = 'line', hcaes(x = breakfast, y = mean_math_score))


################################################################################################################
# Q8

bcg %>% select(idcntry, idschool, bcbg08a:bcbg13ce) %>% mutate(bcbg08b = 3 - bcbg08a) %>%
  mutate(bcbg08b = ifelse(is.na(bcbg08b), 0, bcbg08b)) %>% mutate(bcbg12ab = ifelse(is.na(bcbg12ab), 0, bcbg12ab)) %>%
  mutate(bcbg12aa = ifelse(is.na(bcbg12aa), 0, bcbg12aa)) %>%
  mutate(school_facil = (bcbg08a*5+bcbg08b+bcbg09a+bcbg09b+bcbg10*5+bcbg11a*5+bcbg11b+bcbg12*5+bcbg12aa+bcbg12ab+bcbg12ba+
                           bcbg12bb+bcbg13aa+bcbg13ab+bcbg13ac+bcbg13ad+bcbg13ae+bcbg13af+bcbg13ag+bcbg13ah+
                           bcbg13ai+bcbg13ba+bcbg13bb+bcbg13bc+bcbg13bd+bcbg13be+bcbg13ca+bcbg13cb+bcbg13cc+
                           bcbg13cd+bcbg13cd)) %>% select(idcntry, idschool, school_facil) %>%
  .[complete.cases(.),] %>% full_join(stud_math_score) %>% .[complete.cases(.),] -> q9_full_data

lm(math_score~school_facil, data = q9_full_data) %>% summary.lm()
q9_full_data %>% ggplot(aes(x = school_facil, y = math_score)) + geom_hex()


################################################################################################################
# Q9

# Academic failure should be measured over time!
# these question implies that we should test the hypothesis that student academic performance is not related to 
# his/her presense in class and prove its wrong.

bsg %>% select(idcntry, idstud, presence_level = bsbg11) %>% full_join(stud_math_score) %>%
  .[complete.cases(.),] -> q9_data

aov(math_score~presence_level, data = q9_data) -> fit_q9
summary.aov(fit_q9)

q9_data %>% group_by(presence_level) %>% summarise(mean_math_score = mean(math_score)) %>%
  hchart(type = 'line', hcaes(x = presence_level, y = mean_math_score))

################################################################################################################
# Q10

################################################################################################################
# Q11-1


# immigrant students or those who has born in a foreign country perform better than local born students.

bsg %>% select(idcntry, idstud, abroad = bsbg10a) %>% full_join(stud_math_score) %>% .[complete.cases(.),] -> q11_1_data

t.test(math_score~abroad, data = q11_1_data)

q11_1_data %>% group_by(abroad) %>% summarise(mean_score = mean(math_score)) %>%
  hchart(type = 'column', hcaes(x = abroad, y = mean_score))


################################################################################################################
# Q11-2

# speaking mother tongue is better for good education

bsg %>% select(idcntry, idstud, home_lang = bsbg03) %>% full_join(stud_math_score) %>% .[complete.cases(.),] -> q11_2_data
aov(math_score~home_lang, data = q11_2_data) %>% summary.aov()

q11_2_data %>% group_by(home_lang) %>% summarise(mean_score = mean(math_score)) %>%
  hchart(type = 'column', hcaes(x = home_lang, y = mean_score))
################################################################################################################
# Q11-3

# having many electronic it devices has a bad impact but in general it and technology can be a help to education

bsg %>% select(idcntry, idstud, devices = bsbg05) %>% full_join(stud_math_score) %>% .[complete.cases(.),] -> q11_3_data
aov(math_score~devices, data = q11_3_data) %>% summary.aov()

q11_3_data %>% group_by(devices) %>% summarise(mean_score = mean(math_score)) %>%
  hchart(type = 'column', hcaes(x = devices, y = mean_score))








