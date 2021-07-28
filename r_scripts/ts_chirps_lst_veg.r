##############################################################
# Script to:
#
# Extract LST from MYD11A2.006 product and vegetation 
# indices from MOD13Q1 obtained through AppEAARS. 
# Extract precipitation data from CHIRPS (done in GEE).
# Process data and create plots
#
# Author: Veronica Andreo
# Last modified: June 2021
##############################################################


#
# Load libraries
#


library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ISOweek)
library(lubridate)
library(dplyr)
library(zoo)
library(imputeTS)


#
# Import data
#


chirps <- read.table("chirps_daily_2017_2020.csv",
                     header = TRUE,
                     sep = ",")
mod11a2 <- read.table("cordoba-final-2-MOD11A2-006-results.csv",
                          header = TRUE,
                          sep = ",")
mod13q1 <- read.table("cordoba-final-2-MOD13Q1-006-results.csv",
                          header = TRUE,
                          sep = ",")


#
# Process CHIRPS
#


# character to date in chirps
chirps$system.time_start <- as.Date(chirps$system.time_start, 
                                    format = "%b %d, %Y")


# aggregate
chirps$week <- floor_date(chirps$system.time_start, 
                          "week", week_start = 1)
chirps_weekly <- 
  chirps %>%
  group_by(week) %>%
  summarize(precip_sum = sum(precipitation))


#
# Process MODIS data
#


## LST ##

# replace 0 by NA in LST Day and Night
mod11a2$MOD11A2_006_LST_Day_1km <- 
  ifelse(mod11a2$MOD11A2_006_LST_Day_1km == 0.0, 
         NA, mod11a2$MOD11A2_006_LST_Day_1km)

mod11a2$MOD11A2_006_LST_Night_1km <- 
  ifelse(mod11a2$MOD11A2_006_LST_Night_1km == 0.0,
         NA, mod11a2$MOD11A2_006_LST_Night_1km)

# aggregate by date and convert to celsius
lst <- 
  mod11a2 %>%
  select(ID, Date, MOD11A2_006_LST_Day_1km, 
         MOD11A2_006_LST_Night_1km) %>%
  group_by(Date) %>%
  summarise(lst_day = mean(MOD11A2_006_LST_Day_1km, 
                           na.rm = TRUE),
            lst_night = mean(MOD11A2_006_LST_Night_1km,
                             na.rm = TRUE)) %>%
  mutate(lst_day_celsius = lst_day - 273.15,
         lst_night_celsius = lst_night - 273.15)


## Vegetation ##

veg <- 
  mod13q1 %>%
  select(ID, Date, MOD13Q1_006__250m_16_days_EVI,
         MOD13Q1_006__250m_16_days_NDVI,
         MOD13Q1_006__250m_16_days_NIR_reflectance,
         MOD13Q1_006__250m_16_days_MIR_reflectance) %>%
  mutate(MOD13Q1_006__250m_16_days_NDWI = 
           (MOD13Q1_006__250m_16_days_NIR_reflectance - 
           MOD13Q1_006__250m_16_days_MIR_reflectance) /
           (MOD13Q1_006__250m_16_days_NIR_reflectance + 
           MOD13Q1_006__250m_16_days_MIR_reflectance)) %>%
  select(ID, Date, 
         MOD13Q1_006__250m_16_days_EVI,
         MOD13Q1_006__250m_16_days_NDVI,
         MOD13Q1_006__250m_16_days_NDWI) %>%
  group_by(Date) %>%
  summarise(evi = mean(MOD13Q1_006__250m_16_days_EVI,
                       na.rm = TRUE),
            ndvi = mean(MOD13Q1_006__250m_16_days_NDVI, 
                        na.rm = TRUE),
            ndwi = mean(MOD13Q1_006__250m_16_days_NDWI, 
                        na.rm = TRUE))


#
# Seasonal aggregations / nov to march
#


lst_1718 <- 
  lst %>%
  filter(Date >= "2017-11-01" & Date <= "2018-03-30") %>%
  summarise(mean_lst_day = mean(lst_day_celsius), # 34.8
            mean_lst_night = mean(lst_night_celsius)) # 20.1

lst_1819 <- 
  lst %>%
  filter(Date >= "2018-11-01" & Date <= "2019-03-30") %>%
  summarise(mean_lst_day = mean(lst_day_celsius), # 32.1
            mean_lst_night = mean(lst_night_celsius)) # 19.8

veg_1718 <- 
  veg %>%
  filter(Date >= "2017-11-01" & Date <= "2018-03-22") %>%
  summarise(mean_ndvi = mean(ndvi), # 0.275
            mean_evi = mean(evi), # 0.193
            mean_ndwi = mean(ndwi)) # 0.0967 

veg_1819 <- 
  veg %>%
  filter(Date >= "2018-11-01" & Date <= "2019-03-22") %>%
  summarise(mean_ndvi = mean(ndvi), # 0.306
            mean_evi = mean(evi), # 0.209
            mean_ndwi = mean(ndwi)) # 0.141


#
# Time series plots
#


chirps_p <- 
  chirps_weekly %>% 
  filter(week <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(week), y = precip_sum)) + 
  geom_line() + ylab("Precipitation (mm)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

lst_day_p <- 
  lst %>% 
  filter(Date <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(Date), y = lst_day_celsius)) + 
  geom_line() + ylab("LST Day (°C)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

lst_night_p <- 
  lst %>% 
  filter(Date <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(Date), y = lst_night_celsius)) + 
  geom_line() + ylab("LST Night (°C)") + xlab("Date")

evi_p <-
  veg %>% 
  filter(Date <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(Date), y = evi)) + 
  geom_line() + ylab("EVI") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

ndvi_p <- 
  veg %>% 
  filter(Date <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(Date), y = ndvi)) + 
  geom_line() + ylab("NDVI") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

ndwi_p <- 
  veg %>% 
  filter(Date <= "2020-10-01") %>%
  ggplot(aes(x = as.Date(Date), y = ndwi)) + 
  geom_line() + ylab("NDWI") + xlab("Date")

ts <- grid.arrange(chirps_p, evi_p, lst_day_p, ndvi_p, lst_night_p, ndwi_p, ncol = 2)
ggsave("time_series.png",
       ts,
       width = 210,
       height = 200,
       units = "mm")


#
# Boxplots comparing 2017-2018 vs 2018-2019
#


chirps_b <- 
  chirps_weekly %>% 
  mutate(season = ifelse(
    week >= "2017-10-30" & week <= "2018-04-02", "2017-2018",
    ifelse(week >= "2018-10-29" & week <= "2019-04-01", "2018-2019",
           ifelse(week >= "2019-10-28" & week <= "2020-03-30", "2019-2020", NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = precip_sum, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylab("Precipitation (mm)") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

lst_day_b <- 
  lst %>% 
  mutate(season = ifelse(Date >= "2017-10-30" & Date <= "2018-04-02", "2017-2018",
                         ifelse(Date >= "2018-10-29" & Date <= "2019-04-01", "2018-2019",
                                ifelse(Date >= "2019-11-01" & Date <= "2020-03-30", "2019-2020",
                                NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = lst_day_celsius, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylab("LST Day (°C)") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

lst_night_b <- 
  lst %>% 
  mutate(season = ifelse(Date >= "2017-10-30" & Date <= "2018-04-02", "2017-2018",
                         ifelse(Date >= "2018-10-29" & Date <= "2019-04-01", "2018-2019", 
                                ifelse(Date >= "2019-11-01" & Date <= "2020-03-30", "2019-2020",
                                NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = lst_night_celsius, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylab("LST Night (°C)") + xlab("Season")

evi_b <- 
  veg %>% 
  mutate(season = ifelse(Date >= "2017-10-30" & Date <= "2018-04-02", "2017-2018",
                         ifelse(Date >= "2018-10-29" & Date <= "2019-04-01", "2018-2019",
                                ifelse(Date >= "2019-11-01" & Date <= "2020-03-30", "2019-2020",
                                NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = evi, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylab("EVI") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

ndvi_b <- 
  veg %>% 
  mutate(season = ifelse(Date >= "2017-10-30" & Date <= "2018-04-02", "2017-2018",
                         ifelse(Date >= "2018-10-29" & Date <= "2019-04-01", "2018-2019",
                                ifelse(Date >= "2019-11-01" & Date <= "2020-03-30", "2019-2020",
                                NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = ndvi, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  ylab("NDVI") + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

ndwi_b <- 
  veg %>% 
  mutate(season = ifelse(Date >= "2017-10-30" & Date <= "2018-04-02", "2017-2018",
                         ifelse(Date >= "2018-10-29" & Date <= "2019-04-01", "2018-2019",
                                ifelse(Date >= "2019-11-01" & Date <= "2020-03-30", "2019-2020",
                                NA)))) %>% 
  filter(!is.na(season)) %>% 
  ggplot(aes(x = season, y = ndwi, fill = season)) + 
  geom_boxplot(show.legend = FALSE) + 
  xlab("Season") + ylab("NDWI")

bp <- grid.arrange(chirps_b, evi_b, lst_day_b, ndvi_b, lst_night_b, ndwi_b)
ggsave("boxplots_seasons.png",
       bp,
       width = 200,
       height = 200,
       units = "mm")
