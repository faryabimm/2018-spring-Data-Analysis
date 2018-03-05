library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
old_theme <- theme_set(theme_solarized())
mobile <- read_csv('Desktop/mobile_data.csv') # for auto path completion press tab
  
# Q1
mobile %>% group_by(company ) %>% summarise(n = n()) %>%  arrange(desc(n))  %>% slice(1:20) -> data_1
data_1$company <- factor(data_1$company, levels = data_1$company)
ggplot(data = data_1) + geom_col(aes(x = company, y = n)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'companies with most phone models', y = 'phone model count')

# Q2_1
mobile %>% group_by(year) %>% summarise(length = mean(dim_length, na.rm = TRUE)) -> data_2_1
coef_2_1 <- coef(lm(length ~ year, data = data_2_1))
q2_1 <- ggplot(data = data_2_1, aes(x = year, y = length)) + geom_point(na.rm = TRUE, colour = 'red') + geom_line(na.rm = TRUE) +
  geom_abline(intercept = coef_2_1[1], slope = coef_2_1[2]) + labs(title = 'average length over time')
  

# Q2_2
mobile %>% group_by(year) %>% summarise(breadth = mean(dim_breadth, na.rm = TRUE)) -> data_2_2
coef_2_2 <- coef(lm(breadth ~ year, data = data_2_2))
q2_2 <- ggplot(data = data_2_2, aes(x = year, y = breadth)) + geom_point(na.rm = TRUE, colour = 'red') + geom_line(na.rm = TRUE) +
  geom_abline(intercept = coef_2_2[1], slope = coef_2_2[2]) + labs(title = 'average breadth over time')

#Q2_3
mobile %>% group_by(year) %>% summarise(thickness = mean(dim_thickness, na.rm = TRUE)) -> data_2_3
coef_2_3 <- coef(lm(thickness ~ year, data = data_2_3))
q2_3 <- ggplot(data = data_2_3, aes(x = year, y = thickness)) + geom_point(na.rm = TRUE, colour = 'red') + geom_line(na.rm = TRUE) +
  geom_abline(intercept = coef_2_3[1], slope = coef_2_3[2]) + labs(title = 'average thickness over time')

#Q2_4
mobile %>% group_by(year) %>% summarise(camera = mean(cam_px, na.rm = TRUE)) -> data_2_4
coef_2_4 <- coef(lm(camera ~ year, data = data_2_4))
q2_4 <- ggplot(data = data_2_4, aes(x = year, y = camera)) + geom_point(na.rm = TRUE, colour = 'red') + geom_line(na.rm = TRUE) +
  geom_abline(intercept = coef_2_4[1], slope = coef_2_4[2]) + labs(title = 'average camera mpx over time')

# Q2 SUMMARIZATION
library(cowplot)
plot_grid(q2_1, q2_2, q2_3, q2_4) 

#Q3
mobile %>% group_by(LTE, sim_no) %>% summarise(value = mean(price, na.rm = TRUE)) -> data_3
data_3$sim_no <- factor(data_3$sim_no)
ggplot(data = data_3) +
  geom_col(aes(y = value, x = sim_no, color = LTE, fill = LTE), na.rm = TRUE, position = 'dodge') +
  labs(title = 'average phone price vs number of sims and LTE support', x = 'number of sims', y = 'average price') +
  scale_colour_solarized("blue")



#Q4
mobile %>% filter(as.integer(year) == 2017) -> data_4
ggplot(data = data_4) + geom_boxplot(aes(y = dim_thickness, x = audio_jack), na.rm = TRUE) +
  labs(title = 'device thickness', y = 'thickness', x = 'audio jack presense')

#Q5_1
mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>% filter(ppi < 700) -> data_5_1
plot_5_1 <- ggplot(data = data_5_1) + geom_histogram(aes(x = ppi), binwidth = 10, na.rm = TRUE) +
  labs(title = 'histogram of device display ppis')

#Q5_2
data_5_1 %>% group_by(year) %>% summarise(mean_ppi = mean(ppi, na.rm = TRUE)) -> data_5_2
coef_5_2 <- coef(lm(mean_ppi~year, data = data_5_2))

plot_5_2 <- ggplot(data = data_5_2, aes(x = year, y = mean_ppi)) + geom_point(colour = 'red', na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  geom_abline(intercept = coef_5_2[1], slope = coef_5_2[2]) +
  labs(title = 'average display ppi over time', y = 'average display ppi')

plot_grid(plot_5_1, plot_5_2)

#Q6
mobile %>% filter(year < 2010 | display_size > 2) %>%
  filter(display_size < 7) %>% 
  mutate(volume = dim_breadth * dim_length * dim_thickness) %>% 
  mutate(hardness = 1000 * weight / volume * dim_thickness) %>% filter(hardness < Inf) %>% 
  select(company, device, year, hardness) %>% arrange(desc(hardness)) %>% slice(1:10) %>%
  mutate(full_name = paste(company, device, year)) %>% select(full_name, hardness) -> data_6

data_6$full_name <- factor(data_6$full_name, levels = data_6$full_name)
ggplot(data = data_6) + geom_col(aes(x = full_name, y = hardness, fill = hardness)) + coord_flip() +
  labs(title = 'hardest devices', x = 'device full name')

#Q7

# mobile %>% mutate(volume = dim_breadth * dim_length * dim_thickness) %>% mutate(density = 1000 * weight/volume) %>% 
#   filter((year <= 2010 | year > 2010 & display_size > 2) & display_size < 7) %>% arrange(density) %>% View()

mobile %>% mutate(volume = dim_breadth * dim_length * dim_thickness) %>% mutate(dent = 1000 * weight/volume) %>%
  filter((year <= 2010 | year > 2010 & display_size > 2) & display_size < 7) -> data_7

plot_7_1 <- ggplot(data = data_7) +
  geom_histogram(aes(x = dent, fill = ifelse(dent < 1, 'red', 'blue')), bins = 1000, na.rm = TRUE) +
  guides(fill = FALSE) + labs(title = 'histogram of device density', x = 'density', y = 'device count')

#Q7_2
data_7 %>% mutate(x_pos = 0, full_name = paste(company, device)) %>% filter(dent < 2) -> data_7
data_7$x_pos <- jitter(data_7$x_pos)

plot_7_2 <- ggplot() + geom_point(data = data_7[which(data_7$dent >= 1),], colour = 'blue', alpha = 0.2, aes(x = x_pos, y = dent)) +
  geom_point(data = data_7[which(data_7$dent < 1),], colour = 'red', alpha = 0.8, aes(x = x_pos, y = dent)) + 
  geom_text(data = data_7[which(data_7$dent < 1),], aes(x = x_pos, y = dent, label = full_name), size = 1) +
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  labs(y = 'density', title = 'distribution of device densities')

plot_grid(plot_7_1, plot_7_2)

#Q8_1
mobile %>% .[complete.cases(.$weight), ] %>% select(weight) %>% filter(weight < 800) -> data_8_1
plot_8_1 <- ggplot(data = data_8_1) + geom_density(aes(x = weight), colour = 'red')

#Q8_2
mobile %>% .[complete.cases(.$battery_mah), ] %>% select(battery_mah) -> data_8_2
plot_8_2 <- ggplot(data = data_8_2) + geom_density(aes(x = battery_mah), colour = 'red') + labs(x = 'battery capacity (mah)')

plot_grid(plot_8_1, plot_8_2)

#Q8_3
mobile %>% .[complete.cases(.$battery_mah),] %>% .[complete.cases(.$weight),] %>% select(battery_mah, weight) -> data_8_3
cor(data_8_3[1], data_8_3[2]) # 0.8086273

#Q9_ESHTEBAHI
mobile %>% filter(price < 1500 & display_size < 7 & display_size > 2) %>% group_by(company) %>%
  filter(price == max(price)) %>% mutate(full_name = paste(company, device)) %>% arrange(year) -> data_9

data_9$price <- jitter(data_9$price, factor = 50)

data_9$year <- factor(data_9$year, levels = data_9$year %>% unique())
ggplot(data = data_9, aes(x = year, y = price)) + geom_point(colour = 'red') + 
  geom_text(aes(label = full_name), hjust=0, vjust=0, size = 3) + labs(title = 'flagship devices over time')

#Q9_solution_1
mobile %>% filter(company == 'Samsung') %>% group_by(year) %>% top_n(1, price) %>% slice(1) -> data_9_1
ggplot(data = data_9_1, aes(x = year, y= price)) + geom_point(na.rm = TRUE, colour = 'red') + 
  geom_line(na.rm = TRUE) + geom_text(aes(label = device), hjust=0, vjust=0, na.rm = TRUE) + 
  labs(title = 'Samsung flagship devices and their price over time')

#Q9_solution_2
mobile %>% filter(company == 'Samsung' & (year <= 2010 | year > 2010 & display_size > 2) & display_size < 7) %>%
  group_by(year) %>% summarise(avg = mean(price, na.rm = TRUE), var = sd(price, na.rm = TRUE)) %>% 
  .[complete.cases(.$year,.$avg,.$var),] -> Q9_stats
min_var_coef <- 1
max_var_coef <- 7
mobile %>% filter(company == 'Samsung' & (year <= 2010 | year > 2010 & display_size > 2) & display_size < 7) %>%
  full_join(Q9_stats) %>% filter(price > (avg + min_var_coef * var) & price < (avg + max_var_coef * var)) %>%
  group_by(year) %>%  top_n(1, price) %>% slice(1) -> data_9_2

ggplot(data = data_9_2, aes(x = year, y= price)) + geom_point(colour = 'red') + geom_line() +
  geom_text(aes(label = device), hjust=0, vjust=0) +
  labs(title = 'Samsung flagship devices and their price over time')

#Q10_1

# best phones to buy each year
# high ppi # large ram # large battery * battrty generation # more thinness # camera px + selfie px

mean_ppi <- mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>% summarise(mean(ppi, na.rm = TRUE)) %>% as.numeric()
mean_ram <- mobile %>% summarise(mean(ram, na.rm = TRUE)) %>% as.numeric()
mean_bat <- mobile %>% summarise(mean(battery_mah, na.rm = TRUE)) %>% as.numeric()
mean_btv <- mobile %>% summarise(mean(bt_v, na.rm = TRUE)) %>% as.numeric()
mean_pri_cam <- mobile %>% summarise(mean(cam_px, na.rm = TRUE)) %>% as.numeric()
mean_sec_cam <- mobile %>% summarise(mean(sel_px, na.rm = TRUE)) %>% as.numeric()
mean_thinness <- mobile %>% summarise(mean(dim_thickness, na.rm = TRUE)) %>% as.numeric()
mean_thinness <- 1 / mean_thinness
mean_price <- mobile %>% summarise(mean(price, na.rm = TRUE)) %>% as.numeric()
mean_price <- 1 / mean_price
mean_weight <- mobile %>% summarise(mean(weight, na.rm = TRUE)) %>% as.numeric()
mean_weight <- 1 / mean_weight

f <- 5 # importance factor
min_spec <- 70

mobile %>% .[complete.cases(.$price), ] %>%  mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>%
  mutate(hardware_spec = ppi/mean_ppi*f + ram/mean_ram*f + battery_mah/mean_bat*f + bt_v/mean_btv + cam_px/mean_pri_cam*f +
           sel_px/mean_sec_cam + 1/dim_thickness/mean_thinness*f + 1/weight/mean_weight*f +
           (LTE == 'Yes')*f + (gps == 'Yes') + (radio == 'Yes') + (nfc == 'Yes') + (wlan == 'Yes') + (audio_jack == 'Yes')) %>% 
  arrange(desc(hardware_spec)) -> data_10_1

data_10_1 %>% mutate(x_pos = 0, full_name = paste(company, device)) -> data_10_1
data_10_1$x_pos <- jitter(data_10_1$x_pos)

 ggplot(data = (data_10_1 %>% filter(hardware_spec > min_spec)), aes(x = x_pos, y = hardware_spec)) + geom_point(alpha = 0.8, aes(color = hardware_spec)) + 
  geom_text(aes(label = full_name), size = 1) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = 'potential value', title = 'distrinution of devices and specs') -> plot_10_1_1

data_10_1 %>% filter(price < 1500) %>% ggplot(.) + 
  geom_hex(aes(x = hardware_spec, y = price, color = price/hardware_spec)) +
  labs(x = 'hardware specs', title = 'hardware specifications vs price') -> plot_10_1_2

plot_grid(plot_10_1_1, plot_10_1_2) 


# Q10_2
# incrase in peoples interest in taking selfies
mobile %>% group_by(year) %>% summarise(mean_selfie = mean(sel_px, na.rm = TRUE)) %>% 
  ggplot(., aes(x = year, y = mean_selfie)) + geom_line(na.rm = TRUE) + geom_point(na.rm = TRUE, colour = 'red') + 
  labs(y = 'average selfie cam quality (MegaPixels)', title = 'increase in market\'s request for better selfies')

# Q10_3
# battery size vs ppi and display size
mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>% ggplot(., aes(x = ppi, y = battery_mah)) +
  geom_hex(na.rm = TRUE) +
  labs(y = 'battery capacity (mah)', title = 'battery capacity vs ppi') -> plot_10_3_1

mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>% ggplot(., aes(x = display_size, y = battery_mah)) +
  geom_hex(na.rm = TRUE) +
  labs(y = 'battery capacity (mah)', title = 'battery capacity vs display size', x = 'display size') -> plot_10_3_2

plot_grid(plot_10_3_1, plot_10_3_2)

library(magrittr)
mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>%
  .[complete.cases(.$battery_mah, .$ppi),] %$% cor(.$battery_mah, .$ppi) # 0.4474414
mobile %>% .[complete.cases(.$battery_mah, .$display_size),] %$%
  cor(.$battery_mah, .$display_size) # 0.8855491

# Q10_4
# hardware price changes over time
mobile %>% mutate(ppi = sqrt(px_row ^ 2 + px_col ^ 2) / display_size) %>%  group_by(year) %>%
  summarise(hw_spec = (mean(cam_px, na.rm = T)/mean_pri_cam + mean(sel_px, na.rm = T)/mean_sec_cam +
                       mean(battery_mah, na.rm = T)/mean_bat + mean(ram, na.rm = T)/mean_ram +
                       mean(bt_v, na.rm = T)/mean_btv + mean(ppi, na.rm = T)/mean_ppi) + 
                       1/mean(dim_thickness, na.rm = T)/mean_thinness + 1/mean(weight, na.rm = T)/mean_weight,
            price = mean(price, na.rm = TRUE)) %>% filter(price < 1500) -> data_10_3
data_10_3 %>% ggplot(., aes(y = price, x = hw_spec)) + geom_line(na.rm = T) + geom_point(na.rm = T, colour = 'red') +
  labs(title = 'average device specs vs price', x = 'hardware specs', y = 'price') -> plot_10_3_1
data_10_3 %>% ggplot(., aes(y = price, x = year)) + geom_line(na.rm = T) + geom_point(na.rm = T, colour = 'red') +
  labs(title = 'average prices vs time') -> plot_10_3_2
data_10_3 %>% ggplot(., aes(y = hw_spec, x = year)) + geom_line(na.rm = T) + geom_point(na.rm = T, colour = 'red') +
  labs(title = 'average device specs vs time', y = 'hardware specs') -> plot_10_3_3

plot_grid(plot_10_3_1, plot_10_3_2, plot_10_3_3)

# Q10_5
# camera vs price
mobile %>% filter(price < 1500) %>% mutate(cam_tot_px = cam_px + sel_px) %>% group_by(year) %>%
  summarise(cam = mean(cam_tot_px, na.rm = T), price = mean(price, na.rm = T)) -> data_10_5
coef_10_5 <- coef(lm(price~cam, data = data_10_5))
data_10_5 %>% 
  ggplot(., aes(x = cam, y = price)) + geom_line(na.rm = T) + geom_point(na.rm = T, colour = 'red') +
  labs(title = 'camera quality vs price', x = 'total camera qualities') + geom_abline(intercept = coef_10_5[1], slope = coef_10_5[2])
