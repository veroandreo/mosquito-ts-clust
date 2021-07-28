########################################################################
# Script to obtain different plots for the manuscript
#
# Author: Veronica Andreo
# Last modified: November 2020
########################################################################


#
# Load libraries
#


library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tmap)
library(tidyr)


#
# Time series plots before interpolation
#


# read ovitraps' data (average of ovitraps A & B, per week)
ovis <- read.table("ovis_avg_2017_2019.csv", 
                   header = TRUE, sep = ",")

# remove variable called sector
ovis <- ovis[,-2]

# vivienda (house number) as factor
ovis$vivienda <- as.factor(ovis$vivienda)

weeks <- c("2017-W40-1","2017-W41-1","2017-W42-1","2017-W43-1","2017-W44-1","2017-W45-1",
           "2017-W46-1","2017-W47-1","2017-W48-1","2017-W49-1","2017-W50-1","2017-W51-1",
           "2017-W52-1",
           "2018-W01-1","2018-W02-1","2018-W03-1","2018-W04-1","2018-W05-1","2018-W06-1",
           "2018-W07-1","2018-W08-1","2018-W09-1","2018-W10-1","2018-W11-1","2018-W12-1",
           "2018-W13-1","2018-W14-1","2018-W15-1","2018-W16-1","2018-W17-1","2018-W18-1",
           "2018-W19-1","2018-W20-1","2018-W21-1","2018-W22-1","2018-W23-1","2018-W24-1",
           "2018-W25-1","2018-W26-1","2018-W27-1","2018-W28-1","2018-W29-1","2018-W30-1",
           "2018-W31-1","2018-W32-1","2018-W33-1","2018-W34-1","2018-W35-1","2018-W36-1",
           "2018-W37-1","2018-W38-1","2018-W39-1","2018-W40-1","2018-W41-1","2018-W42-1",
           "2018-W43-1","2018-W44-1","2018-W45-1","2018-W46-1","2018-W47-1","2018-W48-1",
           "2018-W49-1","2018-W50-1","2018-W51-1","2018-W52-1",
           "2019-W01-1","2019-W02-1","2019-W03-1","2019-W04-1","2019-W05-1","2019-W06-1",
           "2019-W07-1","2019-W08-1","2019-W09-1","2019-W10-1","2019-W11-1","2019-W12-1",
           "2019-W13-1","2019-W14-1","2019-W15-1","2019-W16-1","2019-W17-1","2019-W18-1",
           "2019-W19-1","2019-W20-1","2019-W21-1","2019-W22-1","2019-W23-1","2019-W24-1",
           "2019-W25-1","2019-W26-1","2019-W27-1","2019-W28-1","2019-W29-1","2019-W30-1",
           "2019-W31-1","2019-W32-1","2019-W33-1","2019-W34-1","2019-W35-1","2019-W36-1",
           "2019-W37-1","2019-W38-1","2019-W39-1","2019-W40-1","2019-W41-1","2019-W42-1",
           "2019-W43-1","2019-W44-1","2019-W45-1","2019-W46-1","2019-W47-1","2019-W48-1",
           "2019-W49-1","2019-W50-1","2019-W51-1","2019-W52-1")

# format long 
keycol <- "weeks"
valuecol <- "eggs"
gathercols <- c("W402017", "W412017", "W422017", "W432017", "W442017", "W452017", 
                "W462017", "W472017", "W482017", "W492017", "W502017", "W512017", 
                "W522017", 
                "W012018", "W022018", "W032018", "W042018", "W052018", "W062018", 
                "W072018", "W082018", "W092018", "W102018", "W112018", "W122018", 
                "W132018", "W142018", "W152018", "W162018", "W172018", "W182018", 
                "W192018", "W202018", "W212018", "W222018", "W232018", "W242018", 
                "W252018", "W262018", "W272018", "W282018", "W292018", "W302018", 
                "W312018", "W322018", "W332018", "W342018", "W352018", "W362018", 
                "W372018", "W382018", "W392018", "W402018", "W412018", "W422018", 
                "W432018", "W442018", "W452018", "W462018", "W472018", "W482018", 
                "W492018", "W502018", "W512018", "W522018",
                "W012019", "W022019", "W032019", "W042019", "W052019", "W062019", 
                "W072019", "W082019", "W092019", "W102019", "W112019", "W122019", 
                "W132019", "W142019", "W152019", "W162019", "W172019", "W182019", 
                "W192019", "W202019", "W212019", "W222019", "W232019", "W242019", 
                "W252019", "W262019", "W272019", "W282019", "W292019", "W302019", 
                "W312019", "W322019", "W332019", "W342019", "W352019", "W362019", 
                "W372019", "W382019", "W392019", "W402019", "W412019", "W422019", 
                "W432019", "W442019", "W452019", "W462019", "W472019", "W482019", 
                "W492019", "W502019", "W512019", "W522019")

ovis_long_no_interp <-
  ovis %>%
  pivot_longer(cols = all_of(gathercols),
               names_to = keycol, 
               values_to = valuecol)

weeks_date <- ISOweek2date(weeks)
weeks_date <- rep(weeks_date, 150)
ovis_long_no_interp <- cbind(ovis_long_no_interp,weeks_date)

# time series of eggs per house
eggs_week <- 
  ggplot(ovis_long_no_interp, aes(x = weeks_date, 
                                  y = eggs, 
                                  group = vivienda)) + 
  geom_line(na.rm = FALSE) + 
  scale_x_date(date_labels = "%b", date_breaks = "9 months") + 
  scale_y_continuous(breaks = c(0,200,400,600)) +
  facet_wrap(~ vivienda, ncol = 10, strip.position = "right") +
  labs(x = "Time", y = "Egg counts") + 
  theme_gray() +
  theme(legend.position="none",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 20))
eggs_week

ggsave("eggs_week_2017_2019_no_interp.pdf", 
       width = 480, height = 350, 
       units = "mm", dpi = "print")

# heatmap time series of eggs per house
col1 = "#ffc100" 
col2 = "#ff0000" 

eggs_week_heatmap <- ggplot(data = ovis_long_no_interp, 
                            aes(x = weeks_date, 
                                y = as.numeric(vivienda))) +
  geom_tile(aes(fill = eggs)) +
  labs(x = "Time", y = "House number", fill = "Egg's count") +
  ylim(0,150) +
  theme_gray() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 14)) +
  scale_fill_gradient(low = col1, high = col2, na.value = NA)
eggs_week_heatmap

ggsave("eggs_week_heatmap_2017_2019_no_interp.pdf", 
       width = 480, height = 350, 
       units = "mm", dpi = "print")


#
# Plot centroids of best clusterings
#


# full series:
#   - best_dtw_pam_pp
# 2017-2018:
#   - best_dtw_pam_17_18_pp
# 2018-2019: 
#   - best_sbd_shape_18_19


full_series <- data.frame(
  time = rep(seq(1:length(best_dtw_pam_pp@centroids[[1]])), 
             times = dim(best_dtw_pam_pp@clusinfo)[1]),
  date = rep(weeks_date, 3),
  value = unlist(best_dtw_pam_pp@centroids), 
  cluster = rep(seq(1:dim(best_dtw_pam_pp@clusinfo)[1]), 
                each = length(best_dtw_pam_pp@centroids[[1]]))
  )

series_17_18 <- data.frame(
  time = rep(seq(1:length(best_dtw_pam_17_18_pp@centroids[[1]])), 
             times = dim(best_dtw_pam_17_18_pp@clusinfo)[1]),
  date = rep(weeks_date[1:52], 3),
  value = unlist(best_dtw_pam_17_18_pp@centroids), 
  cluster = rep(seq(1:dim(best_dtw_pam_17_18_pp@clusinfo)[1]), 
                each = length(best_dtw_pam_17_18_pp@centroids[[1]]))
)

series_18_19 <- data.frame(
  time = rep(seq(1:length(best_sbd_shape_18_19@centroids[[1]])), 
             times = dim(best_sbd_shape_18_19@clusinfo)[1]),
  date = rep(weeks_date[53:104], 3),
  value = unlist(best_sbd_shape_18_19@centroids), 
  cluster = rep(seq(1:dim(best_sbd_shape_18_19@clusinfo)[1]), 
                each = length(best_sbd_shape_18_19@centroids[[1]]))
)

gg_full_series <- 
  ggplot(data = full_series, aes(x = date, y = value)) + 
  geom_line(color = "red") + 
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") + 
  scale_y_continuous(breaks = c(0,2.5,5.0,7.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_text(size = 14),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16)) +
  facet_wrap(~cluster)
gg_full_series

gg_series_17_18 <- 
  ggplot(data = series_17_18, aes(x = date, y = value)) + 
  geom_line(color = "red") + 
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") + 
  scale_y_continuous(breaks = c(0,2.5,5.0,7.5)) +
  expand_limits(y = c(0, 8.0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18)) +
  facet_wrap(~cluster)
gg_series_17_18

gg_series_18_19 <- 
  ggplot(data = series_18_19, aes(x = date, y = value)) + 
  geom_line(color = "red") + 
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") + 
  scale_y_continuous(breaks = c(0,2.5,5.0,7.5)) +
  expand_limits(y = c(0, 8.0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18)) +
  facet_wrap(~cluster)
gg_series_18_19

# 3 clustering results in one plot
grid <- plot_grid(gg_full_series, gg_series_17_18, 
                  gg_series_18_19,
                  align = "v", ncol = 1, 
                  labels = c("a","b","c"), 
                  label_size = 20)
grid

ggsave("grid_clust.pdf", width = 350, height = 480, 
       units = "mm", dpi = "print")


#
# Group series per cluster in each period and plot mean and 10 & 90 perc 
#


# table with time series of all houses and their resulting group in each
# clustering

sarara <- 
  ovis_filled_clean_df %>%
  left_join(cluster_result, by = "vivienda")
  
sarara_long <- 
  sarara %>%
  pivot_longer(starts_with("W"),
               names_to = "Week_s", 
               values_to = "Eggs") 

sarara_long <- cbind(sarara_long,weeks_date2)

mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE){
  dat <- data.frame(y = mean(x, na.rm = na.rm),
                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
  return(dat)
}

gg_full_series_mean <- 
  ggplot(sarara_long, aes(weeks_date2, Eggs, group = series_full)) +
  stat_summary(geom = "line", fun = mean, color = "red") +
  stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.3) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_text(size = 14),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16)) +
  facet_wrap(~series_full)


sarara_long_1718 <- 
  subset(sarara_long, weeks_date2 >= "2017-10-02" & weeks_date2 <= "2018-09-24")

gg_series_17_18_mean <- 
  ggplot(sarara_long_1718, aes(weeks_date2, Eggs, group = series_17_18)) +
  stat_summary(geom = "line", fun = mean, color = "red") +
  stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.3) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_text(size = 14),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16)) +
  facet_wrap(~series_17_18)


sarara_long_1819 <- 
  subset(sarara_long, weeks_date2 >= "2018-10-01" & weeks_date2 <= "2019-09-30")

gg_series_18_19_mean <- 
  ggplot(sarara_long_1819, aes(weeks_date2, Eggs, group = series_18_19)) +
  stat_summary(geom = "line", fun = mean, color = "red") +
  stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.3) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        # axis.ticks.x = element_text(size = 14),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16)) +
  facet_wrap(~series_18_19)

grid <- plot_grid(gg_full_series_mean, gg_series_17_18_mean, 
                  gg_series_18_19_mean,
                  align = "v", ncol = 1, 
                  labels = c("a","b","c"), 
                  label_size = 20)

ggsave("grid_clust_mean.pdf", width = 350, height = 480, 
       units = "mm", dpi = "print")
  

#
# Flow charts/plots to see how houses move from one 
# cluster to the other in different periods
#


library(networkD3)


# from series 1718 to series 1819
nodes = data.frame("name" = 
                     c("S1718-1",  # Node 0
                       "S1718-2",  # Node 1
                       "S1718-3",  # Node 2
                       "S1819-1",  # Node 3
                       "S1819-2",  # Node 4
                       "S1819-3")) # Node 5

links = as.data.frame(matrix(c(
  0, 3, 7,  # Each row represents a link. The first number
  0, 4, 42, # represents the node being connected from. 
  0, 5, 21, # The second number represents the node connected to.
  1, 3, 0,  # The third number is the value of the node
  1, 4, 20,
  1, 5, 10,
  2, 3, 3,
  2, 4, 26,
  2, 5, 14),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 10, nodeWidth = 30)


# from series 1718 to full series
nodes = data.frame("name" = 
                     c("S1718-1",  # Node 0
                       "S1718-2",  # Node 1
                       "S1718-3",  # Node 2
                       "Full-1",   # Node 3
                       "Full-2",   # Node 4
                       "Full-3"))  # Node 5

links = as.data.frame(matrix(c(
  0, 3, 7,  # Each row represents a link. The first number
  0, 4, 12, # represents the node being connected from. 
  0, 5, 51, # The second number represents the node connected to.
  1, 3, 5,  # The third number is the value of the node
  1, 4, 7,
  1, 5, 18,
  2, 3, 4,
  2, 4, 7,
  2, 5, 32),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 10, nodeWidth = 30)


# from series 1819 to full series
nodes = data.frame("name" = 
                     c("S1819-1",  # Node 0
                       "S1819-2",  # Node 1
                       "S1819-3",  # Node 2
                       "Full-1",   # Node 3
                       "Full-2",   # Node 4
                       "Full-3"))  # Node 5

links = as.data.frame(matrix(c(
  0, 3, 4, # Each row represents a link. The first number
  0, 4, 0, # represents the node being connected from. 
  0, 5, 6, # The second number represents the node connected to.
  1, 3, 7, # The third number is the value of the node
  1, 4, 18,
  1, 5, 63,
  2, 3, 5,
  2, 4, 8,
  2, 5, 32),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 10, nodeWidth = 30)


