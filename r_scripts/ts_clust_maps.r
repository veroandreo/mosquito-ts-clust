########################################################################
# Script to obtain different maps for the manuscript
#
# Author: Veronica Andreo
# Last modified: November 2020
########################################################################


#
# Load libraries
#


library(raster)
library(rgrass7)
library(sf)
library(tmap)
library(tmaptools)
library(dplyr)


#
# Set GRASS GIS variables for initialization
#


# path to GRASS binaries
myGRASS <- "/usr/lib64/grass78"
# path to GRASS database
myGISDbase <- "/home/veroandreo/grassdata/"
# path to location
myLocation <- "utm20s"
# path to mapset
myMapset <- "ts_ovis_cba"


# start GRASS GIS from R
initGRASS(gisBase = myGRASS,
          home = tempdir(),
          gisDbase = myGISDbase,
          location = myLocation,
          mapset = myMapset,
          SG="class_s2_2018_2019_15c",
          override = TRUE)

gmeta()


#
# Import relavant data
#


# Import ovitraps points from GRASS
use_sf()
ovis_positions <- readVECT("ovitrampas_cba")

# vector data with dengue cases
execGRASS("g.list", parameters = list(type = "vector", pattern = "dengue*"))
autoctonos <- readVECT("dengue_autoctonos")

# vector con barrios - no idecor
execGRASS("g.list", parameters = list(type = "vector", pattern = "Barrios*"))
barrios <- readVECT("Barrios_Cba")

# satellite mosaic previously obtained
cba <- stack("/home/veroandreo/Documents/conae/dengue_cordoba/scripts/cba.tif")
map_cba <- 
  tm_shape(cba) + 
  tm_rgb(alpha = 0.7)
map_cba


# region 
region <- st_bbox(cba) %>% st_as_sfc()


#
# Fig study area and ovitraps
#


# Create Argentina map
gadm_arg <- readRDS("gadm36_ARG_1_sf.rds")
arg_map <- tm_shape(gadm_arg) + tm_borders()

# Create Cordoba map
cordoba <- gadm_arg[gadm_arg$NAME_1 == "Córdoba",]


# main fig
fig1 <- 
  map_cba + 
  tm_shape(barrios) + 
  tm_borders() + 
  tm_scale_bar(breaks = c(0, 3, 6), 
               text.size = 0.5,
               position = "right") +
  tm_compass(type = "8star", 
             size = 2, 
             position = c("right", "top")) + 
  tm_graticules(alpha = 0,
                labels.size = 0.4,
                labels.rot = c(0,90)) + 
  tm_shape(ovis_positions) + 
  tm_dots(col = "black",
          shape = 21, 
          size = 0.12,
          border.col = "white")
fig1

# inset map
fig1_inset <- arg_map + tm_shape(cordoba) + tm_polygons() +
  tm_shape(region) + tm_borders(col = "red", lwd = 1.5)
fig1_inset

fig1_vp <- viewport(0.22, 0.22, width = 0.32, height = 0.32)

# save
tmap_save(fig1, 
          filename= "fig_1b.png", 
          insets_tm = fig1_inset, 
          insets_vp = fig1_vp,
          width = 120,
          height = 100,
          units = "mm")


#
# Fig dengue cases
#


fig_casos <- 
  map_cba + 
  tm_shape(barrios) + 
  tm_borders() + 
  tm_scale_bar(breaks = c(0, 3, 6), 
               text.size = 0.5,
               position = "right") +
  tm_compass(type = "8star", 
             size = 2, 
             position = c("right", "top")) + 
  tm_graticules(alpha = 0,
                labels.size = 0.4,
                labels.rot = c(0,90)) + 
  tm_shape(autoctonos) + 
  tm_dots(col = "black",
          shape = 21, 
          size = 0.03,
          border.col = NA,
          border.lwd = 0)
fig_casos

# save
tmap_save(fig_casos, filename= "fig_casos.png", 
          width = 120,
          height = 100,
          units = "mm")


#
# Fig classifications
#


# import raster maps
execGRASS("g.list", parameters = 
            list(type = "raster", 
                 pattern = "class_s2*15c"))

use_sp()
class_2017_2018 <- raster(readRAST("class_s2_2017_2018_15c"))
class_2018_2019 <- raster(readRAST("class_s2_2018_2019_15c"))
class_2019_2020 <- raster(readRAST("class_s2_2019_2020_15c"))

classif_r <- raster::stack(c(class_2017_2018,class_2018_2019,class_2019_2020))
names(classif_r)

class_colors <- get_brewer_pal("-RdYlGn", n = 15)

fig_classif <-
  tm_shape(classif_r) + 
  tm_raster(palette = class_colors,
            legend.show = FALSE) +
  tm_layout(panel.label.bg.color = NA,
            panel.label.size = 0.9,
            panel.labels = c("2017-2018","2018-2019", "2019-2020"), 
            legend.outside = TRUE,
            legend.outside.position = "bottom") +
  tm_facets(free.scales = FALSE,
            ncol = 3) +
  tm_add_legend(type = "fill",
                title = "Spectral classes",
                col = class_colors,
                labels = c(1:15),
                is.portrait = FALSE)
fig_classif

tmap_save(fig_classif, 
          filename = "classifications.png",
          width = 240,
          height = 100,
          units = "mm")


#
# Fig RGB maps
#


# read maps from GRASS
bands_2017_2018 <- execGRASS("g.list", parameters = 
                          list(type = "raster", 
                               pattern = "SEN2_2017_2018_median_band.[1-3]"))
bands_2017_2018 <- attributes(bands_2017_2018)$resOut

bands_2018_2019 <- execGRASS("g.list", parameters = 
                          list(type = "raster", 
                               pattern = "SEN2_2018_2019_median_band.[1-3]"))
bands_2018_2019 <- attributes(bands_2018_2019)$resOut

bands_2019_2020 <- execGRASS("g.list", parameters = 
                          list(type = "raster", 
                               pattern = "SEN2_2019_2020_median_band.[1-3]"))
bands_2019_2020 <- attributes(bands_2019_2020)$resOut

use_sp()

bands <- list()
for (i in bands_2017_2018){
  bands[i] <- raster(readRAST(i))
}
bands_2017_2018_r <- stack(bands)

bands <- list()
for (i in bands_2018_2019){
  bands[i] <- raster(readRAST(i))
}
bands_2018_2019_r <- stack(bands)

bands <- list()
for (i in bands_2019_2020){
  bands[i] <- raster(readRAST(i))
}
bands_2019_2020_r <- stack(bands)

# plot
png("RGB_maps.png", width = 1400, height = 500)
par(mfcol=c(1,3))
plotRGB(bands_2017_2018_r, r=3, g=2, b=1,
        stretch="lin", maxpixels = 800000)
text(371171, 6512308,"A", col = "white", font = 2, cex = 5)
plotRGB(bands_2018_2019_r, r=3, g=2, b=1, 
        stretch="lin", maxpixels = 800000)
text(371171, 6512308,"B", col = "white", font = 2, cex = 5)
plotRGB(bands_2019_2020_r, r=3, g=2, b=1, 
        stretch="lin", maxpixels = 800000)
text(371171, 6512308,"C", col = "white", font = 2, cex = 5)
dev.off()


#
# Maps with groups per best clustering per period
#


# get clean house number from `cluster_result` - see ts_clust_comparison.r
cluster_result <- 
  cluster_result %>%
  separate(vivienda, c("v","vivienda")) %>%
  select(-v)

cluster_result$vivienda <- as.integer(cluster_result$vivienda)

# join `ovitrap_points` with groups from best `cluster_results`
ovi_clus_res <- right_join(ovis_positions,
                           cluster_result,
                           by = "vivienda")

# convert to long format
ovi_clus_res_long <- 
  ovi_clus_res %>%
  pivot_longer(cols = starts_with("series"), 
               names_to = "Series", 
               values_to = "Group")

ovi_clus_res_long <- st_as_sf(ovi_clus_res_long)

# plot the 3 clustering results in maps
clusters_map <- 
  tm_shape(barrios) +
  tm_borders(col = "darkgray", 
             lwd = 0.9) +
  tm_shape(ovi_clus_res_long) +
  tm_dots(size = 0.8,
          col = "Group") +
  tm_facets(by = "Series",
            free.scales = FALSE,
            ncol = 3) +
  tm_layout(panel.label.bg.color = NA,
            panel.label.size = 1.2)
clusters_map

tmap_save(clusters_map, 
          "clusters_map.pdf", 
          width = 360, 
          height = 120,
          units = "mm")





