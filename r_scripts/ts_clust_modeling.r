########################################################################
# Script to model the relationship of temporal pattern and remote
# sensing environmental variables by means of random forest
#
# Author: Veronica Andreo
# Last modified: June 2021
########################################################################


#
# Load libraries
#


library(rgrass7)
library(sf)
library(dplyr)
library(caret)
library(MLmetrics)


#
# Read vectors with data from GRASS
#


use_sf()

ovis_1718_50m <- readVECT("ovitraps_with_data_2017_2018_50m")
ovis_1718_100m <- readVECT("ovitraps_with_data_2017_2018_100m")

ovis_1819_50m <- readVECT("ovitraps_with_data_2018_2019_50m")
ovis_1819_100m <- readVECT("ovitraps_with_data_2018_2019_100m")


#
# Join with clustering results
#


ovis_1718_50m_clus <-  
  ovis_1718_50m %>% 
  mutate(vivienda = as.numeric(vivienda)) %>% 
  inner_join(cluster_result, by="vivienda", keep = FALSE) %>% 
  st_drop_geometry()

ovis_1718_100m_clus <- 
  ovis_1718_100m %>% 
  mutate(vivienda = as.numeric(vivienda)) %>% 
  inner_join(cluster_result, by="vivienda", keep = FALSE)

ovis_1819_50m_clus <- 
  ovis_1819_50m %>% 
  mutate(vivienda = as.numeric(vivienda)) %>% 
  inner_join(cluster_result, by="vivienda", keep = FALSE)

ovis_1819_100m_clus <-  
  ovis_1819_100m %>% 
  mutate(vivienda = as.numeric(vivienda)) %>% 
  inner_join(cluster_result, by="vivienda", keep = FALSE)


#
# Groups vs environmental variables - ML
#


###################
## 2017-2018 50m ##
###################

# create train and test partitions
set.seed(39)
inTraining <- createDataPartition(ovis_1718_50m_clus$series_17_18, 
                                  p = .7, list = FALSE) # row numbers for training

training <- ovis_1718_50m_clus[ inTraining, c(4:25,27)]
testing  <- ovis_1718_50m_clus[-inTraining, c(4:25,27)]

prop.table(table(training$series_17_18))
prop.table(table(testing$series_17_18))

# fix classes - numbers only are not accepted
training$series_17_18 <- make.names(training$series_17_18)
testing$series_17_18 <- make.names(testing$series_17_18)

# balance train and test - upsampling
training <- upSample(training[,1:22],as.factor(training$series_17_18))

# rename testing class column
names(testing)[names(testing) == 'series_17_18'] <- "Class"

# pre-process
preProcValuestr_1718_50m <- preProcess(training, method = c("center", "scale"))
trainTransformed_1718_50m <- predict(preProcValuestr_1718_50m, training)
testTransformed_1718_50m <- predict(preProcValuestr_1718_50m, testing)

# feature selection using selection by filtering (sbf) - anovaScores
ctrl <- sbfControl(functions = rfSBF,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 10,
                   verbose = TRUE)

rfProfile_1718_50m <- sbf(Class ~ .,
                          data = trainTransformed_1718_50m,
                          sbfControl = ctrl)

rfProfile_1718_50m

predictors(rfProfile_1718_50m)

# variable importance
varimp_1718_50m <- varImp(rfProfile_1718_50m$fit)
varimp_1718_50m$vars <- row.names(varimp_1718_50m)
varimp_1718_50m <- varimp_1718_50m[order(varimp_1718_50m$Overall, decreasing = TRUE),]

# prediction
predichos <- predict(rfProfile_1718_50m$fit, newdata = testTransformed_1718_50m)

# validation with test data
confusionMatrix(predichos,as.factor(testTransformed_1718_50m$Class))


####################
## 2017-2018 100m ##
####################

# create train and test partitions
set.seed(39)
inTraining <- createDataPartition(ovis_1718_100m_clus$series_17_18, 
                                  p = .7, list = FALSE) # row numbers for training

training <- ovis_1718_100m_clus[ inTraining, c(4:25,27)]
testing  <- ovis_1718_100m_clus[-inTraining, c(4:25,27)]

prop.table(table(training$series_17_18))
prop.table(table(testing$series_17_18))


# fix classes - numbers only not accepted
training$series_17_18 <- make.names(training$series_17_18)
testing$series_17_18 <- make.names(testing$series_17_18)


# balance train and test - upsampling
training <- upSample(training[,1:22],as.factor(training$series_17_18))

# rename testing class column
names(testing)[names(testing) == 'series_17_18'] <- "Class"

# pre-process
preProcValuestr_1718_100m <- preProcess(training, method = c("center", "scale"))
trainTransformed_1718_100m <- predict(preProcValuestr_1718_100m, training)
testTransformed_1718_100m <- predict(preProcValuestr_1718_100m, testing)

# feature selection using selection by filtering (sbf) - anovaScores
ctrl <- sbfControl(functions = rfSBF,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 10,
                   verbose = TRUE)

rfProfile_1718_100m <- sbf(Class ~ ., 
                           data = trainTransformed_1718_100m,
                           sbfControl = ctrl)

rfProfile_1718_100m

predictors(rfProfile_1718_100m)

# variable importance
varimp_1718_100m <- varImp(rfProfile_1718_100m$fit)
varimp_1718_100m$vars <- row.names(varimp_1718_100m)
varimp_1718_100m <- varimp_1718_100m[order(varimp_1718_100m$Overall, decreasing = TRUE),]

# prediction
predichos <- predict(rfProfile_1718_100m$fit, newdata = testTransformed_1718_100m)

# validation with test data
confusionMatrix(predichos,as.factor(testTransformed_1718_100m$Class))


###################
## 2018-2019 50m ##
###################

# create train and test partitions
set.seed(39)
inTraining <- createDataPartition(ovis_1819_50m_clus$series_18_19, 
                                  p = .7, list = FALSE) # row numbers for training

training <- ovis_1819_50m_clus[ inTraining, c(4:25,28)]
testing  <- ovis_1819_50m_clus[-inTraining, c(4:25,28)]

prop.table(table(training$series_18_19))
prop.table(table(testing$series_18_19))


# fix classes - numbers only not accepted
training$series_18_19 <- make.names(training$series_18_19)
testing$series_18_19 <- make.names(testing$series_18_19)


# balance train and test - upsampling
training <- upSample(training[,1:22],as.factor(training$series_18_19))

# rename testing class column
names(testing)[names(testing) == 'series_18_19'] <- "Class"

# pre-process
preProcValuestr_1819_50m <- preProcess(training, method = c("center", "scale"))
trainTransformed_1819_50m <- predict(preProcValuestr_1819_50m, training)
testTransformed_1819_50m <- predict(preProcValuestr_1819_50m, testing)

# feature selection using selection by filtering (sbf) - anovaScores
ctrl <- sbfControl(functions = rfSBF,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 10,
                   verbose = TRUE)

rfProfile_1819_50m <- sbf(Class ~ .,
                          data = trainTransformed_1819_50m,
                          sbfControl = ctrl)

rfProfile_1819_50m

predictors(rfProfile_1819_50m)

# variable importance
varimp_1819_50m <- varImp(rfProfile_1819_50m$fit)
varimp_1819_50m$vars <- row.names(varimp_1819_50m)
varimp_1819_50m <- varimp_1819_50m[order(varimp_1819_50m$Overall, decreasing = TRUE),]

# prediction
predichos <- predict(rfProfile_1819_50m$fit, newdata = testTransformed_1819_50m)

# validation with test data
confusionMatrix(predichos,as.factor(testTransformed_1819_50m$Class))


####################
## 2018-2019 100m ##
####################

# create train and test partitions
set.seed(39)
inTraining <- createDataPartition(ovis_1819_100m_clus$series_18_19, 
                                  p = .7, list = FALSE) # row numbers for training

training <- ovis_1819_100m_clus[ inTraining, c(4:25,28)]
testing  <- ovis_1819_100m_clus[-inTraining, c(4:25,28)]

prop.table(table(training$series_18_19))
prop.table(table(testing$series_18_19))


# fix classes - numbers only not accepted
training$series_18_19 <- make.names(training$series_18_19)
testing$series_18_19 <- make.names(testing$series_18_19)


# balance train and test - upsampling
training <- upSample(training[,1:22],as.factor(training$series_18_19))

# rename testing class column
names(testing)[names(testing) == 'series_18_19'] <- "Class"

# pre-process
preProcValuestr_1819_100m <- preProcess(training, method = c("center", "scale"))
trainTransformed_1819_100m <- predict(preProcValuestr_1819_100m, training)
testTransformed_1819_100m <- predict(preProcValuestr_1819_100m, testing)

# feature selection using selection by filtering (sbf) - anovaScores
ctrl <- sbfControl(functions = rfSBF,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 10,
                   verbose = TRUE)

rfProfile_1819_100m <- sbf(Class ~ ., 
                           data = trainTransformed_1819_100m,
                           sbfControl = ctrl)

rfProfile_1819_100m

predictors(rfProfile_1819_100m)

#variable importance
varimp_1819_100m <- varImp(rfProfile_1819_100m$fit)
varimp_1819_100m$vars <- row.names(varimp_1819_100m)
varimp_1819_100m <- varimp_1819_100m[order(varimp_1819_100m$Overall, decreasing = TRUE),]

# prediction
predichos <- predict(rfProfile_1819_100m$fit, newdata = testTransformed_1819_100m)

# validation with test data
confusionMatrix(predichos,as.factor(testTransformed_1819_100m$Class))


#
# Plots of variable importance
#


library(ggplot2)
library(forcats)
library(cowplot)

vi1 <- 
  varimp_1718_50m %>%
  mutate(vars = fct_reorder(vars, Overall)) %>%
  ggplot( aes(x=vars, y=Overall)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.9, width=.7) +
  coord_flip() +
  xlab("") + ylab("")

vi2 <- 
  varimp_1718_100m %>%
  mutate(vars = fct_reorder(vars, Overall)) %>%
  ggplot( aes(x=vars, y=Overall)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.9, width=.7) +
  coord_flip() +
  xlab("") + ylab("")

vi3 <- 
  varimp_1819_50m %>%
  mutate(vars = fct_reorder(vars, Overall)) %>%
  ggplot( aes(x=vars, y=Overall)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.9, width=.7) +
  coord_flip() +
  xlab("") + ylab("Importance")

vi4 <- 
  varimp_1819_100m %>%
  mutate(vars = fct_reorder(vars, Overall)) %>%
  ggplot( aes(x=vars, y=Overall)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.9, width=.7) +
  coord_flip() +
  xlab("") + ylab("Importance")

plot_grid(vi1, vi2, vi3, vi4, 
          align = "v", 
          ncol = 2, 
          labels = c("a","b","c","d"), 
          label_size = 18)

ggsave("var_imp.pdf", width = 180, height = 120, 
       units = "mm", dpi = "print")


#
# Prediccions over the map (see: http://smartdigiag.com/DSM_book/pdfs/P6_SSPF_cate_2017_MNLR.pdf)
#


library(raster)
library(rasterVis)
library(randomForest)

use_sp()

# 2017-2018 - 50m
to_import_50m <- c(
  "SEN2_2017_2018_pan_9_IDM",
  "class_s2_2017_2018_15c_diversity_shannon_size_9",
  "SEN2_2017_2018_ndwi_x_average_9",
  "class_s2_2017_2018_15c_diversity_simpson_size_9",
  "SEN2_2017_2018_lswi_average_9",
  "SEN2_2017_2018_ndwi_mf_average_9",
  "SEN2_2017_2018_evi_average_9",
  "class_s2_2017_2018_intersp_9",
  "SEN2_2017_2018_ndvi_average_9",
  "class_s2_2017_2018_rich_9",
  "class_s2_2017_2018_mode_9"
)

predictors_50m <- list()
for(i in to_import_50m){
  predictors_50m[i] <- raster(readRAST(i))
}

predictors_50m_r <- raster::stack(predictors_50m)
names(predictors_50m_r)
names(predictors_50m_r) <- c("IDM_9","Shannon_9","ndwi_x_av_9",
                             "Simpson_9","lswi_av_9","ndwi_mf_av_9",
                             "evi_av_9","intersp_9","ndvi_av_9",
                             "rich_9","mode_9")

# normalize variables according to training coefficients
mean50m <- preProcValuestr_1718_50m$mean[names(predictors_50m_r)]
std50m <- preProcValuestr_1718_50m$std[names(predictors_50m_r)]
transformed_50m <- (predictors_50m_r - mean50m) / std50m

# class prediction
map_pred_1718_50m <- predict(transformed_50m, 
                             rfProfile_1718_50m$fit, 
                             type = "class",
                             filename = "class_1718_50m.tif",
                             format = "GTiff", 
                             overwrite = T, 
                             datatype = "INT2S")

# plot
map_pred_1718_50m <- as.factor(map_pred_1718_50m)

# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1718_50m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1718_50m) <- rat

# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")

# plot
levelplot(map_pred_1718_50m, col.regions = area_colors, xlab = "", ylab = "")

# 2017-2018 - 100m
to_import_100m <- c(
  "SEN2_2017_2018_ndbi_sd_19",
  "SEN2_2017_2018_ndwi_mf_sd_19",
  "class_s2_2017_2018_15c_diversity_simpson_size_19",
  "SEN2_2017_2018_ndbi_average_19",
  "class_s2_2017_2018_15c_diversity_shannon_size_19",
  "SEN2_2017_2018_evi_sd_19",
  "SEN2_2017_2018_lswi_average_19",
  "SEN2_2017_2018_pan_19_IDM",
  "SEN2_2017_2018_pan_19_ASM",
  "SEN2_2017_2018_pan_19_Entr",
  "SEN2_2017_2018_evi_average_19",
  "SEN2_2017_2018_ndwi_x_average_19",
  "SEN2_2017_2018_ndvi_average_19",
  "SEN2_2017_2018_ndwi_mf_average_19",
  "class_s2_2017_2018_mode_19"
)

predictors_100m <- list()
for(i in to_import_100m){
  predictors_100m[i] <- raster(readRAST(i))
}

predictors_100m_r <- raster::stack(predictors_100m)
names(predictors_100m_r)
names(predictors_100m_r) <- c("ndbi_sd_19","ndwi_mf_sd_19","Simpson_19",
                              "ndbi_av_19", "Shannon_19", "evi_sd_19",
                              "lswi_av_19", "IDM_19", "ASM_19", "Entr_19",
                              "evi_av_19", "ndwi_x_av_19", "ndvi_av_19", 
                              "ndwi_mf_av_19", "mode_19")

# normalize variables according to training coefficients
mean100m <- preProcValuestr_1718_100m$mean[names(predictors_100m_r)]
std100m <- preProcValuestr_1718_100m$std[names(predictors_100m_r)]
transformed_100m <- (predictors_100m_r - mean100m) / std100m

# class prediction
map_pred_1718_100m <- predict(transformed_100m, 
                              rfProfile_1718_100m$fit, 
                              type = "class",
                              filename = "class_1718_100m.tif",
                              format = "GTiff", 
                              overwrite = T, 
                              datatype = "INT2S")

# plot
map_pred_1718_100m <- as.factor(map_pred_1718_100m)

# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1718_100m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1718_100m) <- rat

# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")

# plot
levelplot(map_pred_1718_100m, col.regions = area_colors, xlab = "", ylab = "")


# 2018-2019 - 50m
to_import_50m <- c(
  "SEN2_2018_2019_pan_9_Entr",
  "SEN2_2018_2019_pan_9_ASM",
  "SEN2_2018_2019_ndwi_x_sd_9",
  "SEN2_2018_2019_pan_9_IDM",
  "SEN2_2018_2019_ndvi_sd_9",
  "SEN2_2018_2019_pan_9_Contr",
  "SEN2_2018_2019_ndwi_mf_sd_9",
  "class_s2_2018_2019_intersp_9")

predictors_50m <- list()
for(i in to_import_50m){
  predictors_50m[i] <- raster(readRAST(i))
}

predictors_50m_r <- raster::stack(predictors_50m)
names(predictors_50m_r)
names(predictors_50m_r) <- c("Entr_9","ASM_9","ndwi_x_sd_9",
                             "IDM_9","ndvi_sd_9","Contr_9",
                             "ndwi_mf_sd_9","intersp_9")

# normalize variables according to training coefficients
mean50m <- preProcValuestr_1819_50m$mean[names(predictors_50m_r)]
std50m <- preProcValuestr_1819_50m$std[names(predictors_50m_r)]
transformed_50m <- (predictors_50m_r - mean50m) / std50m

# class prediction
map_pred_1819_50m <- predict(transformed_50m, 
                             rfProfile_1819_50m$fit, 
                             type = "class",
                             filename = "class_1819_50m.tif",
                             format = "GTiff", 
                             overwrite = T, 
                             datatype = "INT2S")

# plot
map_pred_1819_50m <- as.factor(map_pred_1819_50m)

# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1819_50m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1819_50m) <- rat

# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")

# plot
levelplot(map_pred_1819_50m, col.regions = area_colors, xlab = "", ylab = "")


# 2018-2019 - 100m
to_import_100m <- c(
  "class_s2_2018_2019_intersp_19",
  "class_s2_2018_2019_15c_diversity_simpson_size_19",
  "SEN2_2018_2019_pan_19_ASM",
  "SEN2_2018_2019_ndwi_x_sd_19",
  "SEN2_2018_2019_pan_19_Contr",
  "SEN2_2018_2019_pan_19_Entr",
  "SEN2_2018_2019_pan_19_IDM",
  "class_s2_2018_2019_rich_19")


predictors_100m <- list()
for(i in to_import_100m){
  predictors_100m[i] <- raster(readRAST(i))
}

predictors_100m_r <- raster::stack(predictors_100m)
names(predictors_100m_r)
names(predictors_100m_r) <- c("intersp_19","Simpson_19","ASM_19",
                              "ndwi_x_sd_19","Contr_19","Entr_19",
                              "IDM_19","rich_19")

# normalize variables according to training coefficients
mean100m <- preProcValuestr_1819_100m$mean[names(predictors_100m_r)]
std100m <- preProcValuestr_1819_100m$std[names(predictors_100m_r)]
transformed_100m <- (predictors_100m_r - mean100m) / std100m

# class prediction
map_pred_1819_100m <- predict(transformed_100m, 
                              rfProfile_1819_100m$fit, 
                              type = "class",
                              filename = "class_1819_100m.tif",
                              format = "GTiff", 
                              overwrite = T, 
                              datatype = "INT2S")

# plot
map_pred_1819_100m <- as.factor(map_pred_1819_100m)

# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1819_100m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1819_100m) <- rat

# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")

# plot
levelplot(map_pred_1819_100m, col.regions = area_colors, xlab = "", ylab = "")


#
# Predict for 2019-2020 - using 2018-2019 model and 2019-2020 raster layers
# 


# 2019-2020 - 50m
to_import_50m <- c(
  "SEN2_2019_2020_pan_9_Entr",
  "SEN2_2019_2020_pan_9_ASM",
  "SEN2_2019_2020_ndwi_x_sd_9",
  "SEN2_2019_2020_pan_9_IDM",
  "SEN2_2019_2020_ndvi_sd_9",
  "SEN2_2019_2020_pan_9_Contr",
  "SEN2_2019_2020_ndwi_mf_sd_9",
  "class_s2_2019_2020_intersp_9")

predictors_50m <- list()
for(i in to_import_50m){
  predictors_50m[i] <- raster(readRAST(i))
}

predictors_50m_r <- raster::stack(predictors_50m)
names(predictors_50m_r)
names(predictors_50m_r) <- c("Entr_9","ASM_9","ndwi_x_sd_9",
                         "IDM_9","ndvi_sd_9","Contr_9",
                         "ndwi_mf_sd_9","intersp_9")

# normalize variables according to training coefficients
mean50m <- preProcValuestr_1819_50m$mean[names(predictors_50m_r)]
std50m <- preProcValuestr_1819_50m$std[names(predictors_50m_r)]
transformed_50m <- (predictors_50m_r - mean50m) / std50m

# class prediction
map_pred_1920_50m <- predict(transformed_50m, 
                             rfProfile_1819_50m$fit, 
                             type = "class",
                             filename = "class_1920_50m.tif",
                             format = "GTiff", 
                             overwrite = T, 
                             datatype = "INT2S")
 
# plot
map_pred_1920_50m <- as.factor(map_pred_1920_50m)
 
# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1920_50m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1920_50m) <- rat
 
# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")
 
# plot
levelplot(map_pred_1920_50m, col.regions = area_colors, xlab = "", ylab = "")


# 2019-2020 - 100m
to_import_100m <- c(
  "class_s2_2019_2020_intersp_19",
  "class_s2_2019_2020_15c_diversity_simpson_size_19",
  "SEN2_2019_2020_pan_19_ASM",
  "SEN2_2019_2020_ndwi_x_sd_19",
  "SEN2_2019_2020_pan_19_Contr",
  "SEN2_2019_2020_pan_19_Entr",
  "SEN2_2019_2020_pan_19_IDM",
  "class_s2_2019_2020_rich_19")

predictors_100m <- list()
for(i in to_import_100m){
  predictors_100m[i] <- raster(readRAST(i))
}

predictors_100m_r <- raster::stack(predictors_100m)
names(predictors_100m_r)
names(predictors_100m_r) <- c("intersp_19","Simpson_19","ASM_19",
                         "ndwi_x_sd_19","Contr_19","Entr_19",
                         "IDM_19","rich_19")

# normalize variables according to training coefficients
mean100m <- preProcValuestr_1819_100m$mean[names(predictors_100m_r)]
std100m <- preProcValuestr_1819_100m$std[names(predictors_100m_r)]
transformed_100m <- (predictors_100m_r - mean100m) / std100m

# class prediction
map_pred_1920_100m <- predict(transformed_100m, 
                              rfProfile_1819_100m$fit, 
                              type = "class",
                              filename = "class_1920_100m.tif",
                              format = "GTiff", 
                              overwrite = T, 
                              datatype = "INT2S")

# plot
map_pred_1920_100m <- as.factor(map_pred_1920_100m)

# add a land class column to the Raster Attribute Table
rat <- levels(map_pred_1920_100m)[[1]]
rat[["terron"]] <- c("1", "2", "3")
levels(map_pred_1920_100m) <- rat

# HEX colors
area_colors <- c("#e5c541", "#547198", "#c53d3d")

# plot
levelplot(map_pred_1920_100m, col.regions = area_colors, xlab = "", ylab = "")


#
# Maps con tmap
#


library(tmap)
library(tmaptools)


pred_r <- raster::stack(c(map_pred_1718_50m,map_pred_1718_100m,
                          map_pred_1819_50m,map_pred_1819_100m))
names(pred_r)
masked <- mask(pred_r, st_buffer(st_zm(barrios), 500))
bb <- bb(st_buffer(st_zm(barrios), 500))
  
fig_pred <-
  tm_shape(masked, 
           bbox = bb) + 
  tm_raster(palette = "RdYlGn",
            style = "cat",
            title = "Group",
            legend.show = TRUE) +
  tm_shape(barrios) + 
  tm_borders(lwd = 0.5,
             alpha = 0.9) + 
  tm_layout(panel.label.bg.color = NA,
            panel.label.size = 0.9,
            panel.labels = c("2017-2018 - 50 m", "2017-2018 - 100 m",
                             "2018-2019 - 50 m", "2018-2019 - 100 m"), 
            legend.outside = TRUE,
            legend.outside.position = "right") +
  tm_facets(free.scales = FALSE,
            ncol = 2)

fig_pred

tmap_save(fig_pred, 
          filename = "predictions_1718_1819.png",
          width = 200,
          height = 160,
          units = "mm")


pred_r <- raster::stack(c(map_pred_1920_50m,map_pred_1920_100m))
masked <- mask(pred_r, st_buffer(st_zm(barrios), 500))

fig_pred <-
  tm_shape(masked,
           bbox = bb) + 
  tm_raster(palette = "RdYlGn",
            style = "cat",
            title = "Group",
            legend.show = TRUE) +
  tm_shape(barrios) + 
  tm_borders(lwd = 0.5,
             alpha = 0.9) + 
  tm_layout(panel.label.bg.color = NA,
            panel.label.size = 0.9,
            panel.labels = c("2019-2020 - 50 m", "2019-2020 - 100 m"), 
            legend.outside = TRUE,
            legend.outside.position = "right") +
  tm_facets(free.scales = FALSE,
            ncol = 2)

fig_pred

tmap_save(fig_pred, 
          filename = "pred_2019_2020.png",
          width = 280,
          height = 140,
          units = "mm")


#
# Boxplots groups vs 5 most important variables in 50m models
#


library(ggplot2)


v1_1718 <- 
  ggplot(ovis_1718_50m_clus,
       aes(x=as.factor(series_17_18),
           y=IDM_9,
           fill = as.factor(series_17_18))) +
  geom_boxplot() +
  labs(title="",x="", y="IDM") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v2_1718 <- 
  ggplot(ovis_1718_50m_clus,
         aes(x=as.factor(series_17_18),
             y=Shannon_9,
             fill = as.factor(series_17_18))) +
  geom_boxplot() +
  labs(title="",x="", y = "Shannon index") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v3_1718 <- 
  ggplot(ovis_1718_50m_clus,
         aes(x=as.factor(series_17_18),
             y=ndwi_x_av_9,
             fill = as.factor(series_17_18))) +
  geom_boxplot() +
  labs(title="",x="", y = "Average NDWI") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v4_1718 <- 
  ggplot(ovis_1718_50m_clus,
         aes(x=as.factor(series_17_18),
             y=Simpson_9,
             fill = as.factor(series_17_18))) +
  geom_boxplot() +
  labs(title="",x="", y = "Simpson index") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v5_1718 <- 
  ggplot(ovis_1718_50m_clus,
         aes(x=as.factor(series_17_18),
             y=lswi_av_9,
             fill = as.factor(series_17_18))) +
  geom_boxplot() +
  labs(title="",x="Group", y = "Average LSWI") +
  theme_gray() +
  theme(legend.position = "none")



v1_1819 <- 
  ggplot(ovis_1819_50m_clus,
         aes(x=as.factor(series_18_19),
             y=Entr_9,
             fill = as.factor(series_18_19))) +
  geom_boxplot() +
  labs(title="",x="", y = "Entropy") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v2_1819 <- 
  ggplot(ovis_1819_50m_clus,
         aes(x=as.factor(series_18_19),
             y=ASM_9,
             fill = as.factor(series_18_19))) +
  geom_boxplot() +
  labs(title="",x="", y = "ASM") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v3_1819 <- 
  ggplot(ovis_1819_50m_clus,
         aes(x=as.factor(series_18_19),
             y=ndwi_x_sd_9,
             fill = as.factor(series_18_19))) +
  geom_boxplot() +
  labs(title="",x="", y = "Std Dev NDWI") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v4_1819 <- 
  ggplot(ovis_1819_50m_clus,
         aes(x=as.factor(series_18_19),
             y=IDM_9,
             fill = as.factor(series_18_19))) +
  geom_boxplot() +
  labs(title="",x="", y = "IDM") +
  theme_gray() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

v5_1819 <- 
  ggplot(ovis_1819_50m_clus,
         aes(x=as.factor(series_18_19),
             y=ndvi_sd_9,
             fill = as.factor(series_18_19))) +
  geom_boxplot() +
  labs(title="",x="Group", y = "Std Dev NDVI") +
  theme_gray() +
  theme(legend.position = "none")

plot_grid(v1_1718, v1_1819, v2_1718, v2_1819, 
          v3_1718, v3_1819, v4_1718, v4_1819,
          v5_1718, v5_1819, 
          align = "v", 
          ncol = 2, 
          labels = c("a","b","c","d","e","f","g","h","i","j"), 
          label_size = 18)

ggsave("boxplot_5_most_imp_vars.pdf", width = 180, height = 300, 
       units = "mm", dpi = "print")

