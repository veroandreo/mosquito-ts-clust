########################################################################
# Script to obtain statistics regarding onset, offset and duration
# of season as well as max, mean(max) and date of max egg counts
#
# Author: Veronica Andreo
# Last modified: November 2020
########################################################################


#
# Load libraries
#


library(dplyr)


#
# Max egg count and when
#


max(ovis_long$eggs) # 615
ovis_long[which.max(ovis_long$eggs),] # 2019-01-28


#
# Max egg count per year (mean max, max max, median date of max values)
#


max_egg_1718 <- 
  ovis_long %>%
  filter(weeks_date < '2018-06-30') %>%
  group_by(V1) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = V1) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1718$eggs) # 236
mean(max_egg_1718$eggs) # 70
median(max_egg_1718$weeks_date) # 2018-01-29

max_egg_1819 <- 
  ovis_long %>%
  filter(weeks_date > '2018-06-30' & 
           weeks_date < '2019-06-30') %>%
  group_by(V1) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = V1) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1819$eggs) # 615
mean(max_egg_1819$eggs) # 185
median(max_egg_1819$weeks_date) # 2019-01-21


#
# Earliest onset
#


# earliest onset 2017
ovis_long %>%
  filter(eggs > 0) %>%
  arrange(by_group = weeks_date) # 2017-10-16

# earliest onset 2018
ovis_long %>%
  filter(eggs > 0) %>%
  filter(weeks_date > '2018-08-01') %>%
  arrange(by_group = weeks_date) # 2018-09-17

# earliest onset 2019
ovis_long %>%
  filter(eggs > 0) %>%
  filter(weeks_date > '2019-08-01') %>%
  arrange(by_group = weeks_date) # 2019-09-23


#
# Latest offset
#


# latest offset 2018
ovis_long %>% 
  filter(eggs > 0) %>%
  filter(weeks_date < '2018-07-01') %>%
  arrange(by_group = weeks_date) %>%
  arrange(desc(weeks_date)) # 2018-05-28

# latest offset 2019
ovis_long %>% 
  filter(eggs > 0) %>%
  filter(weeks_date < '2019-07-01') %>%
  arrange(by_group = weeks_date) %>%
  arrange(desc(weeks_date)) # 2019-06-17


#
# Median start and end date of female activity per group in each period
#


ovis_long2 <- 
  ovi_clus_sum %>% 
  select(!starts_with("sum")) %>%
  pivot_longer(cols = all_of(gathercols),
               names_to = keycol, 
               values_to = valuecol)

weeks_date2 <- rep(weeks_date, 143)
ovis_long3 <- cbind(ovis_long2,weeks_date2)


# median start date

### 2017 - 2018 ###

filt_1718_1 <- 
  ovis_long3 %>%
  filter(eggs > '0' & series_17_18 == '1') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_1$weeks_date2) #2017-11-20

filt_1718_2 <- 
  ovis_long3 %>%
  filter(eggs > '0' & series_17_18 == '2') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_2$weeks_date2) #2017-12-04

filt_1718_3 <- 
  ovis_long3 %>%
  filter(eggs > '0' & series_17_18 == '3') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_3$weeks_date2) # 2017-11-27


### 2018 - 2019 ###

filt_1819_1 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01' & eggs > '0' & series_18_19 == '1') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_1$weeks_date2) # 2018-10-25

filt_1819_2 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01' & eggs > '0' & series_18_19 == '2') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_2$weeks_date2) # 2018-11-05

filt_1819_3 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01' & eggs > '0' & series_18_19 == '3') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == min(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_3$weeks_date2) # 2018-10-29 


# median end date

### 2017 - 2018 ###

filt_1718_1 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-01' & eggs > '0' & 
           series_17_18 == '1') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_1$weeks_date2) # 2018-05-07

filt_1718_2 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-01' & eggs > '0' & 
           series_17_18 == '2') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_2$weeks_date2) # 2018-05-07

filt_1718_3 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-01' & eggs > '0' & 
           series_17_18 == '3') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1718_3$weeks_date2) # 2018-04-30

### 2018 - 2019 ###

filt_1819_1 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01' & 
           weeks_date2 < '2019-06-01' & 
           eggs > '0' & series_18_19 == '1') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_1$weeks_date2) # 2019-04-29

filt_1819_2 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01' & 
           weeks_date2 < '2019-06-01' & 
           eggs > '0' & series_18_19 == '2') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_2$weeks_date2) # 2019-04-29

filt_1819_3 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-01'  & 
           weeks_date2 < '2019-06-01'& 
           eggs > '0' & series_18_19 == '3') %>%
  select(weeks_date2, vivienda) %>%
  group_by(vivienda) %>%
  filter(weeks_date2 == max(weeks_date2)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
median(filt_1819_3$weeks_date2) # 2019-04-29


#
# Median duration
#


start_dates <- as.Date(c("2017-11-20", "2017-12-04", "2017-11-27",
                         "2018-10-25", "2018-11-05", "2018-10-29"), 
                       format = "%Y-%m-%d")
end_dates <- as.Date(c("2018-05-07", "2018-05-07", "2018-04-30",
                       "2019-04-29", "2019-04-29", "2019-04-29"), 
                     format = "%Y-%m-%d")
duration <- end_dates - start_dates
duration


#
# Median date of max egg count per year and cluster
#


## 2017 - 2018 ##

max_egg_1718_c1 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-30' &
           series_17_18 == '1') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1718_c1$eggs) # 205
mean(max_egg_1718_c1$eggs) # 73
median(max_egg_1718_c1$weeks_date2) # 2018-02-01

max_egg_1718_c2 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-30' &
           series_17_18 == '2') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1718_c2$eggs) # 155
mean(max_egg_1718_c2$eggs) # 55
median(max_egg_1718_c2$weeks_date2) # 2018-01-15

max_egg_1718_c3 <- 
  ovis_long3 %>%
  filter(weeks_date2 < '2018-06-30' &
           series_17_18 == '3') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1718_c3$eggs) # 196
mean(max_egg_1718_c3$eggs) # 74
median(max_egg_1718_c3$weeks_date2) # 2018-01-29

## 2018 - 2019 ##

max_egg_1819_c1 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-30' & 
           weeks_date2 < '2019-06-30' &
           series_18_19 == '1') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1819_c1$eggs) # 274
mean(max_egg_1819_c1$eggs) # 127
median(max_egg_1819_c1$weeks_date2) # 2019-02-11

max_egg_1819_c2 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-30' & 
           weeks_date2 < '2019-06-30' &
           series_18_19 == '2') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1819_c2$eggs) # 594
mean(max_egg_1819_c2$eggs) # 190
median(max_egg_1819_c2$weeks_date2) # 2019-01-21

max_egg_1819_c3 <- 
  ovis_long3 %>%
  filter(weeks_date2 > '2018-06-30' & 
           weeks_date2 < '2019-06-30' &
           series_18_19 == '3') %>%
  group_by(vivienda) %>%
  filter(eggs == max(eggs)) %>%
  arrange(by_group = vivienda) %>%
  arrange(desc(eggs)) %>%
  slice(1)
max(max_egg_1819_c3$eggs) # 615
mean(max_egg_1819_c3$eggs) # 206
median(max_egg_1819_c3$weeks_date2) # 2019-02-04


#
# Eggs total counts per group per best clustering
#


# matrix to df
ovis_filled_clean_df <- as.data.frame(ovis_filled_clean)
ovis_filled_clean_df <- cbind(vivienda = cluster_result$vivienda,
                              ovis_filled_clean_df)

# sum egg counts per group per series
ovi_clus_sum <- 
  ovis_filled_clean_df %>%
  mutate(sum_full_series = 
           select(., starts_with("W")) %>%
           rowSums(na.rm = TRUE),
         sum_17_18 =
           select(., c(W402017:W392018)) %>%
           rowSums(na.rm = TRUE),
         sum_18_19 =
           select(., c(W402018:W392019)) %>%
           rowSums(na.rm = TRUE)
         ) %>%
  left_join(cluster_result, by = "vivienda")


#
# Test egg counts differences among groups in each clustering
#


# Serie full
GSF <- as.factor(ovi_clus_sum$series_full)
summary(gsf_aov <- aov(sum_full_series ~ GSF, ovi_clus_sum))
TukeyHSD(gsf_aov, 'GSF', conf.level = 0.95)

promedio_gsf <- aggregate( sum_full_series ~ GSF, 
                           ovi_clus_sum, mean )
desvio_gsf <- aggregate( sum_full_series ~ GSF, 
                         ovi_clus_sum, sd )

# Serie 2017-2018
GS1718 <- as.factor(ovi_clus_sum$series_17_18)
summary(gs1718_aov <- aov(sum_17_18 ~ GS1718, ovi_clus_sum))
TukeyHSD(gs1718_aov, 'GS1718', conf.level = 0.95)

promedio_gs1718 <- aggregate( sum_17_18 ~ GS1718, 
                           ovi_clus_sum, mean )
desvio_gs1718 <- aggregate( sum_17_18 ~ GS1718, 
                         ovi_clus_sum, sd )

# Serie 2018-2019
GS1819 <- as.factor(ovi_clus_sum$series_18_19)
summary(gs1819_aov <- aov(sum_18_19 ~ GS1819, ovi_clus_sum))
TukeyHSD(gs1819_aov, 'GS1819', conf.level = 0.95)

promedio_gs1819 <- aggregate( sum_18_19 ~ GS1819, 
                           ovi_clus_sum, mean )
desvio_gs1819 <- aggregate( sum_18_19 ~ GS1819, 
                         ovi_clus_sum, sd )


#
# Overall table & boxplot
#

clus <- c(1,2,3)
eggs_group <- 
  data.frame(Clustering = rep(c("Series_full","Series_17_18",
                                "Series_18_19"), 
                              each = 3),
             Group = rep(clus,3),
             Average = c(promedio_gsf[,2], promedio_gs1718[,2],
                         promedio_gs1819[,2]), 
             SD = c(desvio_gsf[,2], desvio_gs1718[,2],
                    desvio_gs1819[,2]))

eggs_group_long <- 
  ovi_clus_sum %>%
  select(series_full, series_17_18, series_18_19) %>%
  pivot_longer(starts_with("series"),
               names_to = "Clustering", 
               values_to = "Group") 

eggs_group_long2 <- 
  ovi_clus_sum %>%
  select(sum_full_series, sum_17_18, sum_18_19) %>%
  pivot_longer(starts_with("sum"), 
               names_to = "sumsbla", 
               values_to = "Sum") 

eggs_clus_group <- cbind(eggs_group_long, eggs_group_long2)
eggs_clus_group

boxplot_total_eggs <- 
  ggplot(eggs_clus_group, 
         aes(x=as.factor(Group), y=Sum, fill = as.factor(Group))) +
  geom_boxplot() +
  labs(title="",x="Group", y = "Egg count") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~Clustering, dir = "v")
boxplot_total_eggs

ggsave("boxplot.pdf", width = 100, height = 180, 
       units = "mm", dpi = "print")

