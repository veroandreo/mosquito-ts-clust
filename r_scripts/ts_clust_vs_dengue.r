########################################################################
# Dengue incidence vs proportion of pixels of different temporal 
# patterns in each neighbourhood
#
# Author: Veronica Andreo
# Last modified: June, 2021
########################################################################


#
# Load libraries
#


library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)


#
# Data
#


# Note: these tables were obtained in GRASS and exported from there
tabla_casos_dengue_por_barrio <- 
  read.csv2("tabla_casos_dengue_por_barrio.csv")

class_prop <- read_delim("~/class_prop.csv", "|", 
                         escape_double = FALSE, 
                         col_types = cols(cat = col_integer(), 
                                          mode = col_integer()), 
                         trim_ws = TRUE)

tabla_poblacion_por_barrio <- read_delim("tabla_poblacion_por_barrio.csv", 
                                         "|", escape_double = FALSE, 
                                         col_types = cols(cat = col_integer()), 
                                         trim_ws = TRUE)


#
# Join tables and estimate incidence per neighbourhood
#


dengue_cba <- 
  left_join(tabla_poblacion_por_barrio,
            tabla_casos_dengue_por_barrio,
            by = "cat") %>% 
  mutate(incidence = (10000 * count_dengue) / ppp_sum) %>% 
  select(!contains("area")) %>% 
  filter(incidence > 0)


#
# Plots
#


dengue_1 <- 
  left_join(dengue_cba,class_prop,by = "cat") %>% 
  filter(incidence < 200) %>%
  ggplot(aes(x=prop_1, y=incidence)) +
  geom_point() + geom_smooth(method=lm)
dengue_1

dengue_2 <- 
  left_join(dengue_cba,class_prop,by = "cat") %>% 
  filter(incidence < 200) %>%
  ggplot(aes(x=prop_2, y=incidence)) +
  geom_point() + geom_smooth(method=lm)
dengue_2

dengue_3 <-   
  left_join(dengue_cba,class_prop,by = "cat") %>% 
  filter(incidence < 200) %>%
  ggplot(aes(x=prop_3, y=incidence)) +
  geom_point() + geom_smooth(method=lm)
dengue_3

# figure dengue incidence vs proportion of clustering groups
plot_grid(dengue_1,dengue_2,dengue_3, 
          align = "h", 
          ncol = 3, 
          labels = c("a","b","c"), 
          label_size = 14)

ggsave("incidence_vs_ovi_clus_prop.png", 
       width = 180, height = 120, 
       units = "mm", dpi = "print")


#
# Spearman correlation tests
#


dengue <- left_join(dengue_cba,class_prop,by = "cat") %>% 
  filter(incidence < 200)

cor.test(dengue$incidence,dengue$prop_1, use = "complete.obs", method = "spearman")
cor.test(dengue$incidence,dengue$prop_2, use = "complete.obs", method = "spearman")
cor.test(dengue$incidence,dengue$prop_3, use = "complete.obs", method = "spearman")
