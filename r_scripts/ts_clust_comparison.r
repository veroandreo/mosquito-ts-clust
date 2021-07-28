####################################################################
# Script to perform clustering comparisons
#
# Author: Veronica Andreo
# Last modified: November 2020
####################################################################


#
# Load libraries
#


library(caret) # confusion matrix
library(clue) # clustering comparisons


#
# Write file with labels
#


cluster_result <- 
  data.frame(vivienda = row.names(ovis_filled_clean),
             series_full = best_dtw_pam_pp@cluster,
             series_17_18 = best_dtw_pam_17_18_pp@cluster, 
             series_18_19 = best_sbd_shape_18_19@cluster)

View(cluster_result)

write.csv(cluster_result, 
          "clustering_results.csv", 
          row.names = FALSE)


#
# Confusion matrix
#


comparison <- 
  confusionMatrix(data = as.factor(cluster_result$series_17_18),
                  reference = as.factor(cluster_result$series_full),
                  dnn = c("series_17_18","series_full"))
comparison$table

comparison <- 
  confusionMatrix(data = as.factor(cluster_result$series_18_19),
                  reference = as.factor(cluster_result$series_full),
                  dnn = c("series_18_19","series_full"))

comparison$table

comparison <- 
  confusionMatrix(data = as.factor(cluster_result$series_18_19),
                  reference = as.factor(cluster_result$series_17_18),
                  dnn = c("series_18_19","series_17_18"))
comparison$table


#
# Clustering comparisons with clue
#


test <- cl_ensemble(list = c(best_dtw_pam_pp,
                             best_dtw_pam_17_18_pp, 
                             best_sbd_shape_18_19))
series <- c("full", "1718", "1819")
names(test) <- series

round(cl_dissimilarity(test, method = "euclidean"), 3)
round(cl_dissimilarity(test, method = "manhattan"), 3)
round(cl_dissimilarity(test, method = "comemberships"), 3)
round(cl_dissimilarity(test, method = "symdiff"), 3)
round(cl_dissimilarity(test, method = "Rand"), 3)
round(cl_dissimilarity(test, method = "GV1"), 3)
round(cl_dissimilarity(test, method = "BA/d"), 3)
round(cl_dissimilarity(test, method = "VI"), 3)
