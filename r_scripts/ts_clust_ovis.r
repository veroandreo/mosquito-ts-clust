####################################################################
# Script to process and analyze ovitraps time series by means of
# time series clustering
#
# Author: Veronica Andreo
# Last modified: November 2020
####################################################################


#
# Load libraries
#


library(zoo) # used to interpolate
library(dtw)
library(dtwclust)
library(ISOweek)
library(tidyr)
library(ggplot2)


#
# Data preparation
#


# read ovitraps' data (average of ovitraps A & B, per week)
ovis <- read.table("ovis_avg_2017_2019.csv", 
                   header = TRUE, sep = ",")
dim(ovis)

# count NA
rowSums(is.na(ovis[,2:53]))
which(rowSums(is.na(ovis[,2:53])) >= 18)
rowSums(is.na(ovis[,54:106]))
which(rowSums(is.na(ovis[,54:106])) >= 18)

######################################################
# We discard houses with 10 or more consecutive
# NAs in one year or the other, or those that 
# present NA + 1 valid data + NA >= 10
# to avoid interpolation artifacts
# houses discarded are: 13, 19, 34, 60, 70, 131, 139
#####################################################

# remove variable called sector
ovis <- ovis[,-2]

# vivienda (house number) as factor
ovis$vivienda <- as.factor(ovis$vivienda)

# transpose
t_ovis <- t(ovis[,2:ncol(ovis)])

# set names for "viviendas"
colnames(t_ovis) <- paste0("v_",ovis[,1])

# interpolate missing data
t_ovis_zoo <- zoo(t_ovis)
t_ovis_zoo_filled <- na.fill(na.approx(t_ovis_zoo), "extend")

# back to df
t_ovis_filled <- fortify.zoo(t_ovis_zoo_filled)

# transpose again
ovis_filled <- t(t_ovis_filled[,2:ncol(t_ovis_filled)])
colnames(ovis_filled) <- names(ovis[c(2:118)])


#
# Data curation
#


# remove houses with interpolation artifacts given NA amount
ovis_filled_clean <- ovis_filled[-c(13,19,34,60,70,131,139),]

# create list of ts for clustering
ovis_ts <- tslist(ovis_filled_clean, simplify = T)
ovis_ts_17_18 <- tslist(ovis_filled_clean[,1:52], simplify = T)
ovis_ts_18_19 <- tslist(ovis_filled_clean[,53:104], simplify = T)


#
# Configurations
#


# Config DTW_basic + DBA + preprocess

cfg_dtw_pp <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10, 
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance", 
                            partitional = list(
                              dtw_basic = list(
                                window.size = seq(from = 1L, 
                                                  to = 5L, 
                                                  by = 1L),
                                norm = c("L1", "L2"))
                            )),
    centroids = pdc_configs("centroid", 
                            share.config = c("p"), 
                            dba = list(
                              window.size = seq(from = 1L, 
                                                to = 5L, 
                                                by = 1L),
                              norm = c("L1", "L2"))
    ),
    no.expand = c("window.size", "norm")
  )


# Config DTW_lb + DBA + preprocess

cfg_dtw_lb_pp <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10, 
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L,
                                          pam.precompute = FALSE)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance", 
                            partitional = list(
                              dtw_lb = list(
                                window.size = seq(from = 1L, 
                                                  to = 5L, 
                                                  by = 1L),
                                norm = c("L1", "L2"))
                            )),
    centroids = pdc_configs("centroid", 
                            share.config = c("p"), 
                            dba = list(
                              window.size = seq(from = 1L, 
                                                to = 5L, 
                                                by = 1L),
                              norm = c("L1", "L2"))
    ),
    no.expand = c("window.size", "norm")
  )


# Config DTW_basic + PAM + preprocess

cfg_dtw_pam_pp <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10, 
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance", 
                            partitional = list(
                              dtw_basic = list(
                                window.size = seq(from = 1L, 
                                                  to = 5L, 
                                                  by = 1L),
                                norm = c("L1", "L2"))
                            )),
    centroids = pdc_configs("centroid", 
                            share.config = c("p"), 
                            pam = list()
    ),
    no.expand = c("window.size", "norm")
  )


# Config DTW_lb + PAM + preprocess

cfg_dtwlb_pam_pp <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10, 
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L,
                                          pam.precompute = FALSE)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance", 
                            partitional = list(
                              dtw_lb = list(
                                window.size = seq(from = 1L, 
                                                  to = 5L, 
                                                  by = 1L),
                                norm = c("L1", "L2"))
                            )),
    centroids = pdc_configs("centroid", 
                            share.config = c("p"), 
                            pam = list()
    ),
    no.expand = c("window.size", "norm")
  )
  

# Config SBD + Shape Extraction

cfg_sbd_shape <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10,
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance",
                            partitional = list(
                              sbd = list()
                            )
    ),
    centroids = pdc_configs("centroid",
                            share.config = c("p"),
                            shape = list(znorm = TRUE)
    )
  )


# Config SBD + PAM

cfg_sbd_pam <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10,
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance",
                            partitional = list(
                              sbd = list()
                            )
    ),
    centroids = pdc_configs("centroid",
                            share.config = c("p"),
                            pam = list()
    )
  )


#
# Print all configs
#


cfg_dtw_pp
cfg_dtw_lb_pp
cfg_dtw_pam_pp
cfg_dtwlb_pam_pp
cfg_sbd_shape
cfg_sbd_pam

num_configs = sapply(cfg_dtw_pp, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

num_configs = sapply(cfg_dtw_lb_pp, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

num_configs = sapply(cfg_dtw_pam_pp, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

num_configs = sapply(cfg_dtwlb_pam_pp, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

num_configs = sapply(cfg_sbd_shape, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

num_configs = sapply(cfg_sbd_pam, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")

# define evaluation indices, score and pick
evaluators <- cvi_evaluators("internal")
score_fun <- evaluators$score
pick_fun <- evaluators$pick


#
# Clustering comparisons
# 


#########################################################
# full time series
#########################################################

data <- ovis_ts

# dtw-dba
comp_dtw_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pp, 
                      seed = 7L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_pp_config <- comp_dtw_pp$pick$config
id_best <- comp_dtw_pp$pick$config$config_id

# re-run the best
best_dtw_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 7L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_pp

# plot members and centroids separately
plot(best_dtw_pp, type = "series")
plot(best_dtw_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-dba
comp_dtwlb_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_lb_pp, 
                      seed = 7L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_pp_config <- comp_dtwlb_pp$pick$config
id_best <- comp_dtwlb_pp$pick$config$config_id

# re-run the best
best_dtwlb_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 7L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_pp

# plot members and centroids separately
plot(best_dtwlb_pp, type = "series")
plot(best_dtwlb_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw-pam
comp_dtw_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pam_pp, 
                      seed = 11L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_pam_pp_config <- comp_dtw_pam_pp$pick$config
id_best <- comp_dtw_pam_pp$pick$config$config_id

# re-run the best
best_dtw_pam_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 11L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_pam_pp

# plot members and centroids separately
plot(best_dtw_pam_pp, type = "series")
plot(best_dtw_pam_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_pam_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-pam
comp_dtwlb_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtwlb_pam_pp, 
                      seed = 4L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_pam_pp_config <- comp_dtwlb_pam_pp$pick$config
id_best <- comp_dtwlb_pam_pp$pick$config$config_id

# re-run the best
best_dtwlb_pam_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 4L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_pam_pp

# plot members and centroids separately
plot(best_dtwlb_pam_pp, type = "series")
plot(best_dtwlb_pam_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_pam_pp@cluster, ncol = 1L, byrow = TRUE)


# sbd-shape
comp_sbd_shape <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_shape, 
                      seed = 3L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_shape_config <- comp_sbd_shape$pick$config
id_best <- comp_sbd_shape$pick$config$config_id

# re-run the best
best_sbd_shape <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_shape, 
                    config_id = id_best,
                    tsclust = list(seed = 3L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_shape

# plot members and series together
plot(best_sbd_shape)

# plot members and centroids separately
plot(best_sbd_shape, type = "series")
plot(best_sbd_shape, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_shape@cluster, ncol = 1L, byrow = TRUE)


# sbd-pam
comp_sbd_pam <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_pam, 
                      seed = 4L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_pam_config <- comp_sbd_pam$pick$config
id_best <- comp_sbd_pam$pick$config$config_id

# re-run the best
best_sbd_pam <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_pam, 
                    config_id = id_best,
                    tsclust = list(seed = 4L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_pam

# plot members and series together
plot(best_sbd_pam)

# plot members and centroids separately
plot(best_sbd_pam, type = "series")
plot(best_sbd_pam, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_pam@cluster, ncol = 1L, byrow = TRUE)


#######################################################
# 2017-2018
#######################################################

data <- ovis_ts_17_18

# dtw-dba
comp_dtw_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pp, 
                      seed = 19L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_17_18_pp_config <- comp_dtw_pp$pick$config
id_best <- comp_dtw_pp$pick$config$config_id

# re-run the best
best_dtw_17_18_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 19L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_17_18_pp

# plot members and centroids separately
plot(best_dtw_17_18_pp, type = "series")
plot(best_dtw_17_18_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_17_18_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-dba
comp_dtwlb_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_lb_pp, 
                      seed = 4L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_17_18_pp_config <- comp_dtwlb_pp$pick$config
id_best <- comp_dtwlb_pp$pick$config$config_id

# re-run the best
best_dtwlb_17_18_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 4L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_17_18_pp

# plot members and centroids separately
plot(best_dtwlb_17_18_pp, type = "series")
plot(best_dtwlb_17_18_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_17_18_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw-pam
comp_dtw_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pam_pp, 
                      seed = 5L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_pam_17_18_pp_config <- comp_dtw_pam_pp$pick$config
id_best <- comp_dtw_pam_pp$pick$config$config_id

# re-run the best
best_dtw_pam_17_18_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 5L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_pam_17_18_pp

# plot members and centroids separately
plot(best_dtw_pam_17_18_pp, type = "series")
plot(best_dtw_pam_17_18_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_pam_17_18_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-pam
comp_dtwlb_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtwlb_pam_pp, 
                      seed = 4L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_pam_17_18_pp_config <- comp_dtwlb_pam_pp$pick$config
id_best <- comp_dtwlb_pam_pp$pick$config$config_id

# re-run the best
best_dtwlb_pam_17_18_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 4L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_pam_17_18_pp

# plot members and centroids separately
plot(best_dtwlb_pam_17_18_pp, type = "series")
plot(best_dtwlb_pam_17_18_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_pam_17_18_pp@cluster, ncol = 1L, byrow = TRUE)


# sbd-shape
comp_sbd_shape <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_shape, 
                      seed = 4L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_shape_17_18_config <- comp_sbd_shape$pick$config
id_best <- comp_sbd_shape$pick$config$config_id

# re-run the best
best_sbd_shape_17_18 <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_shape, 
                    config_id = id_best,
                    tsclust = list(seed = 4L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_shape_17_18

# plot members and series together
plot(best_sbd_shape_17_18)

# plot members and centroids separately
plot(best_sbd_shape_17_18, type = "series")
plot(best_sbd_shape_17_18, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_shape_17_18@cluster, ncol = 1L, byrow = TRUE)


# sbd-pam
comp_sbd_pam <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_pam, 
                      seed = 5L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_pam_17_18_config <- comp_sbd_pam$pick$config
id_best <- comp_sbd_pam$pick$config$config_id

# re-run the best
best_sbd_pam_17_18 <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_pam, 
                    config_id = id_best,
                    tsclust = list(seed = 5L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_pam_17_18

# plot members and series together
plot(best_sbd_pam_17_18)

# plot members and centroids separately
plot(best_sbd_pam_17_18, type = "series")
plot(best_sbd_pam_17_18, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_pam_17_18@cluster, ncol = 1L, byrow = TRUE)


###########################################################
# 2018-2019
###########################################################

data <- ovis_ts_18_19

# dtw-dba
comp_dtw_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pp, 
                      seed = 13L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_18_19_pp_config <- comp_dtw_pp$pick$config
id_best <- comp_dtw_pp$pick$config$config_id

# re-run the best
best_dtw_18_19_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 13L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_18_19_pp

# plot members and centroids separately
plot(best_dtw_18_19_pp, type = "series")
plot(best_dtw_18_19_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_18_19_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-dba
comp_dtwlb_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_lb_pp, 
                      seed = 5L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_18_19_pp_config <- comp_dtwlb_pp$pick$config
id_best <- comp_dtwlb_pp$pick$config$config_id

# re-run the best
best_dtwlb_18_19_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 5L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_18_19_pp

# plot members and centroids separately
plot(best_dtwlb_18_19_pp, type = "series")
plot(best_dtwlb_18_19_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_18_19_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw-pam
comp_dtw_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pam_pp, 
                      seed = 16L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_pam_18_19_pp_config <- comp_dtw_pam_pp$pick$config
id_best <- comp_dtw_pam_pp$pick$config$config_id

# re-run the best
best_dtw_pam_18_19_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 16L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtw_pam_18_19_pp

# plot members and centroids separately
plot(best_dtw_pam_18_19_pp, type = "series")
plot(best_dtw_pam_18_19_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtw_pam_18_19_pp@cluster, ncol = 1L, byrow = TRUE)


# dtw_lb-pam
comp_dtwlb_pam_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtwlb_pam_pp, 
                      seed = 5L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtwlb_pam_18_19_pp_config <- comp_dtwlb_pam_pp$pick$config
id_best <- comp_dtwlb_pam_pp$pick$config$config_id

# re-run the best
best_dtwlb_pam_18_19_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtwlb_pam_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 5L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_dtwlb_pam_18_19_pp

# plot members and centroids separately
plot(best_dtwlb_pam_18_19_pp, type = "series")
plot(best_dtwlb_pam_18_19_pp, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_dtwlb_pam_18_19_pp@cluster, ncol = 1L, byrow = TRUE)


# sbd-shape
comp_sbd_shape <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_shape, 
                      seed = 3L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_shape_18_19_config <- comp_sbd_shape$pick$config
id_best <- comp_sbd_shape$pick$config$config_id

# re-run the best
best_sbd_shape_18_19 <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_shape, 
                    config_id = id_best,
                    tsclust = list(seed = 37L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_shape_18_19

# plot members and series together
plot(best_sbd_shape_18_19)

# plot members and centroids separately
plot(best_sbd_shape_18_19, type = "series")
plot(best_sbd_shape_18_19, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_shape_18_19@cluster, ncol = 1L, byrow = TRUE)


# sbd-pam
comp_sbd_pam <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_sbd_pam, 
                      seed = 3L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_sbd_pam_18_19_config <- comp_sbd_pam$pick$config
id_best <- comp_sbd_pam$pick$config$config_id

# re-run the best
best_sbd_pam_18_19 <- 
  repeat_clustering(series = data, 
                    clusterings = comp_sbd_pam, 
                    config_id = id_best,
                    tsclust = list(seed = 3L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

best_sbd_pam_18_19

# plot members and series together
plot(best_sbd_pam_18_19)

# plot members and centroids separately
plot(best_sbd_pam_18_19, type = "series")
plot(best_sbd_pam_18_19, type = "centroids", lty=1, col="red")

# get labels for each row
matrix(best_sbd_pam_18_19@cluster, ncol = 1L, byrow = TRUE)
