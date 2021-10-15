## Spatial distribution of *Aedes aegypti* oviposition temporal patterns and their relationship with environment and dengue incidence

This repo holds the scripts that we have developed to analyze weekly records 
of mosquito egg counts by means of time series clustering and its relation with 
remote sensing derived variables and dengue incidence in Cordoba (Argentina).

The manuscript was published on October 9th, 2021 as part of the 
[Special Issue](https://www.mdpi.com/journal/insects/special_issues/host_vector_pathogen) 
"Impacts of Ecological Perturbations at the Host-Vector-Pathogen Interface 
under Global Change" of Insects - MDPI. 

Find the open access publication at: https://www.mdpi.com/2075-4450/12/10/919

### Software 

After atmospheric correction with ARCSI, all image processing was done in
[GRASS GIS 7.8+](https://grass.osgeo.org/download/).
The scripts assume that the users have the software, all dependencies and
extensions installed. Moreover, it requires that the
[*GRASS DATABASE*](https://grass.osgeo.org/grass78/manuals/grass_database.html)
is properly set with locations and mapsets created accordingly. For extra
info, visit the [first time users](https://grass.osgeo.org/learn/newcomers/)
and [tutorials](https://grass.osgeo.org/learn/tutorials/) pages in the
GRASS GIS website.

Time series clustering and further classification and statistical
analysis were carried out using different R packages.

### Scripts in this repo

1. `grass_scripts/s2_process.sh`: commands used to download, correct, import and process Sentinel 2 data in GRASS GIS.
1. `r_scripts/ts_clust_ovis.r`: commands used to set time series clustering configurations and run the analysis.
1. `r_scripts/ts_clust_comparisons.r`: commands used to compare clustering results.
1. `r_scripts/ts_clust_tables.r`: commands used to obtain descriptive statistics for temporal clusters, e.g., onset, offset and duration of season as well as max, mean(max) and dates of max egg counts.  
1. `r_scripts/ts_clust_plots.r`: commands used to obtain time series and flow plots.
1. `r_scripts/ts_clust_maps.r`: commands used to obtain maps like study area, and distribution of ovitraps and dengue cases, RGB compositions, classification results and, distribution of temporal clusters. 
1. `r_scripts/ts_clust_modeling.r`: commands used to classify temporal clusters as a function of remote sensing variables by means of random forest. The file includes validation and prediction too.
1. `r_scripts/ts_clust_vs_dengue.r`: commands used to analyze Dengue incidence vs proportion of pixels of different temporal clusters in each neighbourhood.
1. `r_scripts/ts_chirps_lst_veg.r`: commands used to import, process and plot data from CHIRPS and MODIS in order to visually compare seasons. 

