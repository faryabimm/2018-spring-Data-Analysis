library(WDI)
library(dplyr)
library(readr)
library(highcharter)
library(tidyr)

country_series <- read_csv('WDI_csv/WDICountry-Series.csv')
country <- read_csv('WDI_csv/WDICountry.csv')
data <- read_csv('WDI_csv/WDIData.csv')
footnote <- read_csv('WDI_csv/WDIFootNote.csv')
series_time <- read_csv('WDI_csv/WDISeries-Time.csv')
series <- read_csv('WDI_csv/WDISeries.csv')

################################################################################
#### Q1

days_per_year <- 365

data %>% filter(`Indicator Code` == 'NY.GDP.PCAP.CD') %>%
  select('Country Name', 'Country Code', gdp_pcap = '2016') %>%
  arrange(gdp_pcap) %>% .[complete.cases(.),] %>% slice(1:10) -> q1_poor_data

q1_poor_data %>% hchart(type = 'column', hcaes(x = `Country Name`, y = gdp_pcap/days_per_year)) %>% 
  hc_yAxis(title = list(text = 'GDP per capita per day'))
  
q1_poor_data %>% .$`Country Code` %>% as.vector -> poor_country_codes

data %>% filter(`Indicator Code` == 'SI.POV.NAHC', `Country Code` %in% poor_country_codes) %>%
  gather(key = 'year', value = 'pbpl', `2000`:`2016`) %>%
  select(`Country Name`, `Country Code`, year, pbpl) %>%
  group_by(`Country Name`, `Country Code`) %>% summarise(pbpl_mean = mean(pbpl, na.rm = TRUE)) %>% 
  arrange(pbpl_mean) %>% hchart(type = 'column', color = 'red', hcaes(x = `Country Name`, y = pbpl_mean)) %>%
  hc_yAxis(title = list(text = 'Average population percentage below national powerty line in 21st century'))


data %>% filter(`Indicator Code` == 'SP.DYN.LE00.IN', `Country Code` %in% poor_country_codes) %>% 
  gather(key = 'year', value = 'pbpl', `2000`:`2016`) %>% select(`Country Name`, `Country Code`, year, pbpl) %>%
  group_by(`Country Name`, `Country Code`) %>% summarise(pbpl_mean = mean(pbpl, na.rm = TRUE)) %>% 
  arrange(pbpl_mean) %>% hchart(type = 'column', color = 'pink', hcaes(x = `Country Name`, y = pbpl_mean)) %>%
  hc_yAxis(title = list(text = 'Average Life Expectancy in 21st century'))

################################################################################
#### Q2

data %>% filter(`Indicator Code` == 'SP.DYN.LE00.IN') %>%
  gather(key = 'year', value = 'lexp', `1960`:`2016`) %>%
  select(`Country Name`, `Country Code`, year, lexp) -> q2_data


rwanda_data <- q2_data %>% filter(`Country Name` == 'Rwanda')

hcboxplot(x = q2_data$lexp, var = q2_data$year, outliers = FALSE) %>% hc_chart(type = "column") %>%
  hc_add_series(type = 'line', rwanda_data, color = 'red', hcaes(x = year, y = lexp)) %>% 
  hc_yAxis(title = list(text = 'Life Expectancy in years')) %>% 
  hc_xAxis(title = list(text = 'Year'))


data %>% filter(`Indicator Code` == 'SP.POP.TOTL', `Country Name` == 'Rwanda') %>%
  gather(key = 'year', value = 'population', `1960`:`2016`) %>% select(year, population) -> rwanda_population_data

rwanda_population_data %>% hchart(type = 'line', color = 'darkblue', hcaes(x = year, y = population))



regression_model <- lm(population~year, data = rwanda_population_data %>%
                         filter(year <= 1994) %>% mutate(year = as.numeric(year)))

df <- data.frame(year = 1960:2016)
cbind(
  df,
  predict.lm(regression_model, df, interval = 'prediction') %>% as.data.frame %>% select(population_pred = fit),
  population = rwanda_population_data$population
) %>% mutate(pop_diff = population_pred - population) %>% hchart(type = 'line', hcaes(x = year, y = pop_diff))


################################################################################
#### Q3

data %>% filter(`Indicator Code` %in% c('SP.DYN.LE00.IN', 'SH.XPD.CHEX.PC.CD')) %>%
  gather(key = 'year', value = 'value', `1960`:`2016`) %>% select(-'X63', -'2017', -'Indicator Name') %>% 
  spread(key = `Indicator Code`, value = value) %>% .[complete.cases(.),] -> q3_data

q3_data %>% hchart(type = 'scatter', hcaes(y = SP.DYN.LE00.IN, x = SH.XPD.CHEX.PC.CD, group = `Country Name`, alpha = 0.3))


################################################################################
#### Q4

data %>% filter(`Indicator Code` == 'NY.GDP.PCAP.PP.CD', `Country Code` == 'IRN') %>%
  gather(key = 'year', value = 'purchase power', `1960`:`2016`) %>%
  select(year, `purchase power`) %>% .[complete.cases(.),] %>% 
  hchart(type = 'line', color = 'red', hcaes(x = year, y = `purchase power`))

################################################################################
#### Q5


compare_iran_with_others <- function (indicator_code, indicator_name) {
  data %>% filter(`Indicator Code` == indicator_code) %>%
    gather(key = 'year', value = 'lexp', `1960`:`2016`) %>%
    select(`Country Name`, `Country Code`, year, lexp) %>% .[complete.cases(.),] -> all_country_data
  
  
  iran_data <- all_country_data %>% filter(`Country Code` == 'IRN')
  
  hcboxplot(x = all_country_data$lexp, var = all_country_data$year, outliers = FALSE) %>% hc_chart(type = "column") %>%
    hc_add_series(type = 'line', iran_data, color = 'red', hcaes(x = year, y = lexp)) %>% 
    hc_yAxis(title = list(text = indicator_name)) %>% 
    hc_xAxis(title = list(text = 'Year'))
}

compare_iran_with_others('NE.EXP.GNFS.ZS', 'Exports of goods and services (% of GDP)')
compare_iran_with_others('NE.IMP.GNFS.ZS', 'Imports of goods and services (% of GDP)')
compare_iran_with_others('FP.CPI.TOTL.ZG', 'Inflation, consumer prices (annual %)')
compare_iran_with_others('NY.GDP.DEFL.KD.ZG', 'Inflation, GDP deflator (annual %)')
compare_iran_with_others('MS.MIL.MPRT.KD', 'Arms imports (SIPRI trend indicator values)')
compare_iran_with_others('MS.MIL.XPRT.KD', 'Arms exports (SIPRI trend indicator values)')
compare_iran_with_others('TX.MNF.TECH.ZS.UN', 'Medium and high-tech exports (% manufactured exports)')
compare_iran_with_others('FI.RES.TOTL.CD', 'Total reserves (includes gold, current US$)')
compare_iran_with_others('GC.TAX.TOTL.GD.ZS', 'Tax revenue (% of GDP)')
compare_iran_with_others('GC.TAX.EXPT.ZS', 'Taxes on exports (% of tax revenue)')
compare_iran_with_others('BX.KLT.DINV.CD.WD', 'Foreign direct investment, net inflows (BoP, current US$)')
compare_iran_with_others('BX.KLT.DINV.WD.GD.ZS', 'Foreign direct investment, net inflows (% of GDP)')
compare_iran_with_others('NY.GDP.MKTP.KD', 'GDP (constant 2010 US$)')
compare_iran_with_others('NY.GDP.MKTP.KD.ZG', 'GDP growth (annual %)')
compare_iran_with_others('SI.POV.GAPS', 'Poverty gap at $1.90 a day (2011 PPP) (%)')
compare_iran_with_others('SI.POV.LMIC.GP', 'Poverty gap at $3.20 a day (2011 PPP) (%)')
compare_iran_with_others('SI.POV.UMIC.GP', 'Poverty gap at $5.50 a day (2011 PPP) (%)')
compare_iran_with_others('BN.CAB.XOKA.GD.ZS', 'Current account balance (% of GDP)')
compare_iran_with_others('NV.AGR.TOTL.ZS', 'Agriculture, value added (% of GDP)')
compare_iran_with_others('SH.XPD.CHEX.GD.ZS', 'Current health expenditure (% of GDP)')
compare_iran_with_others('SE.XPD.PRIM.PC.ZS', 'Government expenditure per student, primary (% of GDP per capita)')
compare_iran_with_others('NV.IND.TOTL.ZS', 'Industry, value added (% of GDP)')

################################################################################
#### Q6

q5_series <- c('NE.EXP.GNFS.ZS', 'NE.IMP.GNFS.ZS', 'FP.CPI.TOTL.ZG', 'NY.GDP.DEFL.KD.ZG',
               'MS.MIL.MPRT.KD', 'MS.MIL.XPRT.KD', 'TX.MNF.TECH.ZS.UN', 'FI.RES.TOTL.CD',
               'GC.TAX.TOTL.GD.ZS', 'GC.TAX.EXPT.ZS', 'BX.KLT.DINV.CD.WD', 'BX.KLT.DINV.WD.GD',
               'NY.GDP.MKTP.KD', 'NY.GDP.MKTP.KD.ZG', 'SI.POV.GAPS', 'SI.POV.LMIC.GP',
               'SI.POV.UMIC.GP', 'BN.CAB.XOKA.GD.ZS', 'NV.AGR.TOTL.ZS', 'SH.XPD.CHEX.GD.ZS',
               'GC.DOD.TOTL.GD.ZS', 'SE.XPD.PRIM.PC.ZS', 'NV.IND.TOTL.ZS')

normalize <- function(column) {
  result <- (column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE)
  return(result)
}

data %>% select(-X63) %>% filter(`Indicator Code` %in% q5_series) %>%
  gather(key = 'year', value = 'value', `1960`:`2017`) %>% 
  group_by(`Indicator Code`) %>% mutate(value = normalize(value)) %>% 
  ungroup %>% group_by(`Country Name`, `Country Code`, `Indicator Code`) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) -> q6_data

q6_data %>% spread(key = `Indicator Code`, value = mean_value) -> q6_data

q6_data %>% select(-GC.DOD.TOTL.GD.ZS) %>% ungroup -> q6_data

# filling NAs with mean of the column

for (i in 1:ncol(q6_data)) {
  if ((q6_data[which(is.na(q6_data[i])),i] %>% nrow) > 0) {
    q6_data[which(is.na(q6_data[i])),i] <- q6_data[i] %>% na.omit %>% unlist %>% mean
  }
}

q6_data %>% select(BN.CAB.XOKA.GD.ZS:TX.MNF.TECH.ZS.UN) %>% kmeans(centers = 3) -> kmeans_data

cbind(
  q6_data %>% select(`Country Name`, `Country Code`),
  cluster = kmeans_data$cluster
  ) -> q6_table_data


library(knitr)

kable(q6_table_data %>% arrange(cluster))


################################################################################
#### Q7

all_data <- q6_data %>% select(-`Country Name`, -`Country Code`)
all_data %>% prcomp -> q7_pca
biplot(q7_pca, cex = 0.8)


chosen_components <- 1:2
compact_data <- q7_pca$x[,chosen_components]

# feature_vector <- q7_pca$rotation[,chosen_components]
# compact_data <- t(feature_vector) %*% t(all_data)
# compact_result <- t(feature_vector %*% compact_data)

library(ggbiplot)

compact_data %>% prcomp %>% ggbiplot(obs.scale = 1, var.scale = 1,
         groups = kmeans_data$cluster, ellipse = TRUE, labels = q6_data$`Country Name`, labels.size = 1)


################################################################################
#### Q8

data %>% filter(`Indicator Code` == 'NY.GDP.MKTP.KD.ZG', `Country Code` == 'IRN') %>%
  gather(key = 'year', value = 'gdp_growth', `1960`:`2016`) %>%
  select(year, gdp_growth) %>% .[complete.cases(.),] -> q8_data

q8_data$year <- as.integer(q8_data$year)

plot(type = 'l', q8_data$year, q8_data$gdp_growth)

library(h2o)
h2o.init()

q8_data_h2o <- as.h2o(q8_data)

q8_model <- h2o.deeplearning(y = 'gdp_growth', x = colnames(q8_data_h2o),  q8_data_h2o)

q8_prediction <- h2o.predict(q8_model, as.h2o(data.frame(year = 2017)))




q8_data %>% mutate(type = 'observation') %>% rbind(
  q8_data %>% filter(year == 2016) %>% mutate(type = 'predict') %>%
    rbind(cbind(
      data.frame(year = 2017, type = 'predict'),
      q8_prediction %>% as.data.frame %>% dplyr::rename(gdp_growth = 'predict')
    )) %>% select(year, gdp_growth, type)
  ) %>% hchart(type = 'line', hcaes(x = year, y = gdp_growth, group = type))


################################################################################
#### Q9-5-Health

compare_iran_with_others('SH.DYN.MORT', 'Mortality rate, under-5 (per 1,000 live births)')
compare_iran_with_others('SH.DYN.NCOM.ZS', 'Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)')
compare_iran_with_others('SH.MED.BEDS.ZS', 'Hospital beds (per 1,000 people)')
compare_iran_with_others('SH.STA.ANVC.ZS', 'Pregnant women receiving prenatal care (%)')
compare_iran_with_others('SH.STA.SUIC.P5', 'Suicide mortality rate (per 100,000 population)')
compare_iran_with_others('SH.STA.TRAF.P5', 'Mortality caused by road traffic injury (per 100,000 people)')
compare_iran_with_others('SP.DYN.LE00.FE.IN', 'Life expectancy at birth, female (years)')
compare_iran_with_others('SP.DYN.LE00.MA.IN', 'Life expectancy at birth, male (years)')
compare_iran_with_others('SP.DYN.TO65.FE.ZS', 'Survival to age 65, female (% of cohort)')
compare_iran_with_others('SP.DYN.TO65.MA.ZS', 'Survival to age 65, male (% of cohort)')
compare_iran_with_others('SP.POP.0014.TO.ZS', 'Population ages 0-14 (% of total)')
compare_iran_with_others('SP.POP.1564.TO.ZS', 'Population ages 15-64 (% of total)')
compare_iran_with_others('SP.POP.65UP.TO.ZS', 'Population ages 65 and above (% of total)')
compare_iran_with_others('SH.IMM.IDPT', 'Immunization, DPT (% of children ages 12-23 months)')
compare_iran_with_others('SH.IMM.MEAS', 'Immunization, measles (% of children ages 12-23 months)')
compare_iran_with_others('SH.XPD.CHEX.GD.ZS', 'Current health expenditure (% of GDP)')
compare_iran_with_others('SH.MMR.DTHS', 'Number of maternal deaths')
compare_iran_with_others('SH.H2O.BASW.ZS', 'People using at least basic drinking water services (% of population)')
compare_iran_with_others('SH.H2O.SMDW.ZS', 'People using safely managed drinking water services (% of population)')
compare_iran_with_others('SH.HIV.1524.FE.ZS', 'Prevalence of HIV, female (% ages 15-24)')
compare_iran_with_others('SH.HIV.1524.MA.ZS', 'Prevalence of HIV, male (% ages 15-24)')

################################################################################
#### Q9-5-Education

compare_iran_with_others('SE.ADT.1524.LT.FE.ZS', 'Literacy rate, youth female (% of females ages 15-24)')
compare_iran_with_others('SE.ADT.1524.LT.MA.ZS', 'Literacy rate, youth male (% of males ages 15-24)')
compare_iran_with_others('SE.ADT.LITR.FE.ZS', 'Literacy rate, adult female (% of females ages 15 and above)')
compare_iran_with_others('SE.ADT.LITR.MA.ZS', 'Literacy rate, adult male (% of males ages 15 and above)')
compare_iran_with_others('SE.COM.DURS', 'Compulsory education, duration (years)')
compare_iran_with_others('SE.PRM.NENR', 'School enrollment, primary (% net)')
compare_iran_with_others('SE.XPD.PRIM.PC.ZS', 'Government expenditure per student, primary (% of GDP per capita)')
compare_iran_with_others('SE.XPD.SECO.PC.ZS', 'Government expenditure per student, secondary (% of GDP per capita)')
compare_iran_with_others('SE.XPD.TERT.PC.ZS', 'Government expenditure per student, tertiary (% of GDP per capita)')
compare_iran_with_others('SE.XPD.PRIM.ZS', 'Expenditure on primary education (% of government expenditure on education)')
compare_iran_with_others('SE.XPD.SECO.ZS', 'Expenditure on secondary education (% of government expenditure on education)')
compare_iran_with_others('SE.XPD.TERT.ZS', 'Expenditure on tertiary education (% of government expenditure on education)')
compare_iran_with_others('IP.JRN.ARTC.SC', 'Scientific and technical journal articles')
compare_iran_with_others('GB.XPD.RSDV.GD.ZS', 'Research and development expenditure (% of GDP)')
compare_iran_with_others('SE.PRE.ENRL.TC.ZS', 'Pupil-teacher ratio, preprimary')
compare_iran_with_others('SE.PRE.ENRR.FE', 'School enrollment, preprimary, female (% gross)')
compare_iran_with_others('SE.PRE.ENRR.MA', 'School enrollment, preprimary, male (% gross)')
compare_iran_with_others('SE.PRM.AGES', 'Primary school starting age (years)')
compare_iran_with_others('SE.PRM.CMPT.FE.ZS', 'Primary completion rate, female (% of relevant age group)')
compare_iran_with_others('SE.PRM.CMPT.MA.ZS', 'Primary completion rate, male (% of relevant age group)')


# library(stringr)
# 
# series %>% filter(str_detect(Topic, 'Education')) %>% View



################################################################################
#### Q9-6-Health

q96H_series <- c('SH.DYN.MORT', 'SH.DYN.NCOM.ZS', 'SH.MED.BEDS.ZS', 'SH.STA.ANVC.ZS',
                 'SH.STA.SUIC.P5', 'SH.STA.TRAF.P5', 'SP.DYN.LE00.FE.IN', 'SP.DYN.LE00.MA.IN',
                 'SP.DYN.TO65.FE.ZS', 'SP.DYN.TO65.MA.ZS', 'SP.POP.0014.TO.ZS', 'SP.POP.1564.TO.ZS',
                 'SP.POP.65UP.TO.ZS', 'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.XPD.CHEX.GD.ZS', 'SH.MMR.DTHS',
                 'SH.H2O.BASW.ZS', 'SH.H2O.SMDW.ZS', 'SH.HIV.1524.FE.ZS', 'SH.HIV.1524.MA.ZS')

data %>% select(-X63) %>% filter(`Indicator Code` %in% q96H_series) %>%
  gather(key = 'year', value = 'value', `1960`:`2017`) %>% 
  dplyr::group_by(`Indicator Code`) %>% dplyr::mutate(value = normalize(value)) %>% 
  ungroup %>% dplyr::group_by(`Country Name`, `Country Code`, `Indicator Code`) %>% 
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) -> q96H_data

q96H_data %>% spread(key = `Indicator Code`, value = mean_value) -> q96H_data

q96H_data %>% ungroup -> q96H_data

# filling NAs with mean of the column

for (i in 1:ncol(q96H_data)) {
  if ((q96H_data[which(is.na(q96H_data[i])),i] %>% nrow) > 0) {
    q96H_data[which(is.na(q96H_data[i])),i] <- q96H_data[i] %>% na.omit %>% unlist %>% mean
  }
}

q96H_data %>% select(SH.DYN.MORT:SP.POP.65UP.TO.ZS) %>% kmeans(centers = 3) -> q96H_kmeans_data

cbind(
  q96H_data %>% select(`Country Name`, `Country Code`),
  cluster = q96H_kmeans_data$cluster
) -> q96_health_table_data

library(knitr)

kable(q96_health_table_data)

################################################################################
#### Q9-7-Health

all_data <- q96H_data %>% select(-`Country Name`, -`Country Code`)
all_data %>% prcomp -> q97H_pca
biplot(q97H_pca, cex = 0.8)


chosen_components <- 1:2
compact_data <- q97H_pca$x[,chosen_components]

compact_data %>% prcomp %>% ggbiplot(obs.scale = 1, var.scale = 1,
                                     groups = q96H_kmeans_data$cluster, ellipse = TRUE, labels = q96H_data$`Country Name`, labels.size = 1)


################################################################################
#### Q9-6-Education

q96E_series <- c('SE.ADT.1524.LT.FE.ZS', 'SE.ADT.1524.LT.MA.ZS', 'SE.ADT.LITR.FE.ZS', 'SE.ADT.LITR.MA.ZS',
                 'SE.COM.DURS', 'SE.PRM.NENR', 'SE.XPD.PRIM.PC.ZS', 'SE.XPD.SECO.PC.ZS', 'SE.XPD.TERT.PC.ZS',
                 'SE.XPD.PRIM.ZS', 'SE.XPD.SECO.ZS', 'SE.XPD.TERT.ZS', 'IP.JRN.ARTC.SC', 'GB.XPD.RSDV.GD.ZS',
                 'SE.PRE.ENRL.TC.ZS', 'SE.PRE.ENRR.FE', 'SE.PRE.ENRR.MA', 'SE.PRM.AGES', 'SE.PRM.CMPT.FE.ZS',
                 'SE.PRM.CMPT.MA.ZS')

data %>% select(-X63) %>% filter(`Indicator Code` %in% q96E_series) %>%
  gather(key = 'year', value = 'value', `1960`:`2017`) %>% 
  dplyr::group_by(`Indicator Code`) %>% dplyr::mutate(value = normalize(value)) %>% 
  ungroup %>% dplyr::group_by(`Country Name`, `Country Code`, `Indicator Code`) %>% 
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) -> q96E_data

q96E_data %>% spread(key = `Indicator Code`, value = mean_value) -> q96E_data

q96E_data %>% ungroup -> q96E_data

# filling NAs with mean of the column

for (i in 1:ncol(q96E_data)) {
  if ((q96E_data[which(is.na(q96E_data[i])),i] %>% nrow) > 0) {
    q96E_data[which(is.na(q96E_data[i])),i] <- q96E_data[i] %>% na.omit %>% unlist %>% mean
  }
}

q96E_data %>% select(GB.XPD.RSDV.GD.ZS:SE.XPD.TERT.ZS) %>% kmeans(centers = 3) -> q96E_kmeans_data

cbind(
  q96E_data %>% select(`Country Name`, `Country Code`),
  cluster = q96E_kmeans_data$cluster
) %>% View

################################################################################
#### Q9-7-Education

all_data <- q96E_data %>% select(-`Country Name`, -`Country Code`)
all_data %>% prcomp -> q97E_pca
biplot(q97E_pca, cex = 0.8)


chosen_components <- 1:2
compact_data <- q97E_pca$x[,chosen_components]

compact_data %>% prcomp %>% ggbiplot(obs.scale = 1, var.scale = 1,
                                     groups = q96E_kmeans_data$cluster, ellipse = TRUE, labels = q96E_data$`Country Name`, labels.size = 1)


################################################################################
#### Q10

q10_indexes <- c(q5_series, q96E_series, q96H_series)

data %>% select(-X63) %>% filter(`Indicator Code` %in% q10_indexes) %>%
  gather(key = 'year', value = 'value', `1960`:`2017`) %>% 
  dplyr::group_by(`Indicator Code`) %>% dplyr::mutate(value = normalize(value)) %>% 
  ungroup %>% dplyr::group_by(`Country Name`, `Country Code`, `Indicator Code`) %>% 
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) -> q10_data

q10_data %>% spread(key = `Indicator Code`, value = mean_value) %>% ungroup -> q10_data

# filling NAs with mean of the column

for (i in 1:ncol(q10_data)) {
  if ((q10_data[which(is.na(q10_data[i])),i] %>% nrow) > 0) {
    q10_data[which(is.na(q10_data[i])),i] <- q10_data[i] %>% na.omit %>% unlist %>% mean
  }
}

q10_data %>% select(-`Country Code`, -`Country Name`) -> q10_dist_data
rownames(q10_dist_data) <- q10_data$`Country Name`

q10_dist <- stats::dist(q10_dist_data,method = "euclidean")


q10_clus <- hclust(q10_dist, method = "complete")

plot(q10_clus, hang = -1, cex = 0.4)
rect.hclust(q10_clus, 3)

################################################################################
#### Q11


# iran hsa no hiv prevention program in comparison to rest of the world :(
compare_iran_with_others('SH.HIV.1524.FE.ZS', 'Prevalence of HIV, female (% ages 15-24)')
compare_iran_with_others('SH.HIV.1524.MA.ZS', 'Prevalence of HIV, male (% ages 15-24)')


# economics of germany and japan overtime after world war II
compare_country_with_others <- function (indicator_code, indicator_name, country_code) {
  data %>% filter(`Indicator Code` == indicator_code) %>%
    gather(key = 'year', value = 'lexp', `1960`:`2016`) %>%
    select(`Country Name`, `Country Code`, year, lexp) %>% .[complete.cases(.),] -> all_country_data
  
  
  iran_data <- all_country_data %>% filter(`Country Code` == country_code)
  
  hcboxplot(x = all_country_data$lexp, var = all_country_data$year, outliers = FALSE) %>% hc_chart(type = "column") %>%
    hc_add_series(type = 'line', iran_data, color = 'red', hcaes(x = year, y = lexp)) %>% 
    hc_yAxis(title = list(text = indicator_name)) %>% 
    hc_xAxis(title = list(text = 'Year'))
}



compare_country_with_others('NY.GDP.PCAP.CD', 'GDP per capita (current US$)', 'JPN')
compare_country_with_others('NY.GDP.PCAP.CD', 'GDP per capita (current US$)', 'DEU')

# economics vs child per woman in USA

data %>% filter(`Country Code` == 'USA') %>% filter(`Indicator Code` %in% c('NY.GDP.PCAP.CD', 'SP.DYN.TFRT.IN')) %>% 
  gather(key = 'year', value = 'value', `1960`:`2016`) %>% select(-`2017`, -X63) %>%
  select(`Indicator Code`, year, value) %>% spread(key = `Indicator Code`, value = value) %>% select(-year) -> q11_3_data

q11_3_data %>% View

cor.test(q11_3_data$NY.GDP.PCAP.CD, q11_3_data$SP.DYN.TFRT.IN) %>% print
cor.test(q11_3_data$NY.GDP.PCAP.CD, q11_3_data$SP.DYN.TFRT.IN, method = 'spearm') %>% print


# impact of 2008 economical crisis on life expectancy in USA
compare_country_with_others('NY.GDP.PCAP.CD', 'GDP per capita (current US$)', 'USA')
compare_country_with_others('NY.GDP.MKTP.KD.ZG', 'GDP growth (annual %)', 'USA')
compare_country_with_others('CM.MKT.TRAD.CD', 'Stocks traded, total value (current US$)', 'USA')


# number of iranian journal articles is mindblowing
compare_iran_with_others('IP.JRN.ARTC.SC', 'Scientific and technical journal articles')






