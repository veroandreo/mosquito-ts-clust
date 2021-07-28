#!/usr/bin/env bash

########################################################################
# Script that searches, downloads and process S2 level 1C data to create
# temporal mosaics and extract environmental information to be related
# to mosquito activity temporal pattern
#
# Author: Veronica Andreo
# Last modified: June 2021
########################################################################

#
# NOTES:
#
# This script assumes the user has GRASS GIS > 7.8 with all dependencies
# and extensions installed. See: https://grass.osgeo.org/download/
#
# Extensions needed for this script are:
#       r.in.nasadem
#       i.sentinel
#       i.wi
#       r.texture.tiled
#       r.diversity
#
# It also asumes that the grass database is set with locations and 
# mapsets created accordingly. We use here a UTM 20S location.
#
#
# Other dependencies:
#
# ARCSI
# Docker container: https://hub.docker.com/r/mundialis/arcsi/
#
#~ docker pull mundialis/arcsi
#
# GDAL plugin for kea format (Fedora distro): 
# https://copr.fedorainfracloud.org/coprs/neteler/kealib/
#
#~ sudo dnf copr enable neteler/kealib
#~ sudo dnf update
#~ sudo dnf install kealib
#
# set the path to gdalplugins in .bashrc
#
#~ export GDAL_DRIVER_PATH=/usr/lib64/gdalplugins
#~ source ~/.bashrc
#


#
# Set relevant variables 
#


export GRASS_OVERWRITE=1

# needed for ARCSI
DEM=dem_5m_cba.tif
MY_S2_PATH=$HOME/tmp/cba/
MY_DEM_PATH=$HOME/tmp/cba/
XML=MTD_MSIL1C.xml
PRODUCTS="DOSAOTSGL STDSREF METADATA"

MY_S2_PROCESSED=$HOME/s2_data/

DATES=(20171114 20171214 20180115 20180214 20180321 20181114 20181214 20190120 20190214 20190324 20191129 20191209 20200128 20200212 20200320)
INDICES=(ndvi evi ndwi_x ndwi_mf lswi ndbi)
YEARS=(2017_2018 2018_2019 2019_2020)

# texture
TEXTURE_METHODS=(asm idm contrast entr corr)
WINDOW_SIZES=(5 9 19) # ~25, ~50 & ~100m
PROCESSES=6


#
# Search for S2 data
#


# set region
g.region -p 

# search for S1 level 1C data
i.sentinel.download -l settings=/home/veroandreo/sentinel \
  area_relation=Contains clouds=1 producttype=S2MSI1C \
  start="2017-11-01" end="2018-03-31"
i.sentinel.download -l settings=/home/veroandreo/sentinel \
  area_relation=Contains clouds=10 producttype=S2MSI1C \
  start="2018-11-01" end="2019-03-31"
i.sentinel.download -l settings=/home/veroandreo/sentinel \
  area_relation=Contains clouds=10 producttype=S2MSI1C \
  start="2019-11-01" end="2020-03-31"

# select and download scenes
# scenes older than 1 year are moved to LTA, user needs to run the
# following lines at least twice till the scene is made available
i.sentinel.download settings=/home/veroandreo/sentinel \
  uuid=63474736-31bf-4437-b2de-a00e4840ed9f,48a2c2d6-cc10-4005-b138-36f6b5834fd0 \
  output=s2_data/
i.sentinel.download settings=/home/veroandreo/sentinel \
  uuid=0b9ed4f4-3822-4d31-8ad2-fbebc06ba6a7,d19e3abd-5390-4285-a88e-0e3aa66c6d71 \
  output=s2_data/
i.sentinel.download settings=/home/veroandreo/sentinel \
  uuid=14b92b2a-5c5a-4e89-a4e5-72e5e7d8c2d3,69d5c54a-bd64-4e1c-843a-1ece9e9ad258 \
  output=s2_data/
i.sentinel.download settings=/home/veroandreo/sentinel \
  uuid=4c252aa4-2c98-46d2-90c0-e4421413940a,d0db2b9f-262f-417c-9cb8-fbcf5a4532b8 \
  output=s2_data/
i.sentinel.download settings=/home/veroandreo/sentinel \
  uuid=b7db3039-0dc9-4621-9be6-d70a7d82f414,aeb0fb36-745f-4d1d-b306-55b0d74ca16d \
  output=s2_data/


#
# Import NASADEM in UTM 20 S location and export tif
#


r.in.nasadem username=veroandreo password=Vero1981# \
  output=nasadem_cba resolution=30
r.out.gdal in=nasadem_cba out=nasadem_cba.tif format=GTiff


#
# Re project Cba DEM 5m res (Source IGN)
#


r.proj location=posgar07_f4 mapset=cba_obia input=dem_5m_cba \
  dbase=/home/veroandreo/grassdata output=dem_5m_cba method=bilinear


#
# Atmospheric correction of S2 level 1C data
#


# set up a folder with .SAFE folders and exported DEM 


# run ARCSI for all scenes in the folder
# we use volume mapping to make S2 and DEM visible inside the docker container
# see arcsi guide: Section 5.7.2 in https://www.arcsi.remotesensing.info/tutorials/ARCSI_Intro_Tutorial_compress.pdf
# get Standardized Surface Reflectance results, use DOSAOTSGL for AOT estimation
for S2IMG in `ls --hide=*tif $MY_S2_PATH` ; do

# define name of Sentinel-2 scene - note: omit .SAFE
S2IMG=`basename $S2IMG .SAFE`
echo "Processing scene:" $S2IMG

podman run -it --rm -v ${MY_S2_PATH}:/data -v ${MY_DEM_PATH}:/dem mundialis/arcsi \
       arcsi.py --sensor sen2 -i /data/${S2IMG}.SAFE/$XML -o /data/${S2IMG}.SAFE/output \
       --tmpath /tmp -f KEA --stats -p ${PRODUCTS} \
       --aeroimg /opt/conda/share/arcsi/WorldAerosolParams.kea \
       --atmosimg /opt/conda/share/arcsi/WorldAtmosphereParams.kea \
       --dem /dem/${DEM} --demnodata 0 --minaot 0.05 --maxaot 0.6

done


#
# Import output .kea files into GRASS
#


# create a mapset
g.mapset -c mapset=ts_ovis_cba
g.mapsets mapset=dengue_cba operation=add

# copy dem
g.copy raster=dem_5m_cba@dengue_cba,dem_5m_cba

# set region
g.region -p region=dengue_6m

# Note: 
# bands are: B, G, R, RE-5, RE-6, RE-7, NIR, NIR-8A, SWIR1, SWIR2 
# all resampled to 10m

for i in `find $MY_S2_PROCESSED -type f -name *stdsref.kea` ; do

out=`basename $i .kea | cut -d'_' -f1,2,11`

r.import input=$i \
         output=${out}_band \
         extent=region

done


#
# Set region to 10m res
#


g.region -p raster=SEN2_20171214_stdsref_band.1 save=ts_clust_10m


#
# Ceiling (see: https://spectraldifferences.wordpress.com/tag/arcsi/)
#


# many map's max value were above 1000
for map in `g.list rast pat=SEN2_* map=.` ; do

r.mapcalc expression="${map}_fix = if(${map} > 1000, 1000, ${map})"

done


#
# Estimate water and veg indices for all dates
#

for DATE in ${DATES[*]} ; do

RED=SEN2_${DATE}_stdsref_band.3_fix
GREEN=SEN2_${DATE}_stdsref_band.2_fix
BLUE=SEN2_${DATE}_stdsref_band.1_fix
NIR=SEN2_${DATE}_stdsref_band.7_fix
SWIR1=SEN2_${DATE}_stdsref_band.9_fix
SWIR2=SEN2_${DATE}_stdsref_band.10_fix

# ndvi
i.vi red=$RED nir=$NIR viname=ndvi output=SEN2_${DATE}_ndvi

# evi
i.vi red=$RED nir=$NIR blue=$BLUE viname=evi output=SEN2_${DATE}_evi

# ndwi_mcfeeters
i.wi green=$GREEN nir=$NIR winame=ndwi_mf output=SEN2_${DATE}_ndwi_mf

# ndwi_xu
i.wi green=$GREEN band5=$SWIR1 winame=ndwi_x output=SEN2_${DATE}_ndwi_x

# lswi
i.wi nir=$NIR band7=$SWIR2 winame=lswi output=SEN2_${DATE}_lswi

# built-up areas
r.mapcalc expression="SEN2_${DATE}_ndbi = float($SWIR1 - $NIR) / float($SWIR1 + $NIR)"

done


#
# Temporal average mosaics for indices
#


for INDEX in ${INDICES[*]} ; do

# 2017-2018
LISTA=`g.list -e rast pat="SEN2_(201[7|8][0|1][1-3][0-3][0-9])_${INDEX}" exclude='20181[1|2]' map=. sep=,`
echo "Temporal mosaics, period 2017-2018, index < $INDEX >..."

r.series input=$LISTA output=SEN2_2017_2018_average_${INDEX} method=average

# 2018-2019
LISTA=`g.list -e rast pat="SEN2_(201[8|9][0|1][1-3][0-3][0-9])_${INDEX}" exclude='20180[1-3]|20191[1|2]' map=. sep=,`
echo "Temporal mosaics, period 2018-2019, index < $INDEX >..."

r.series input=$LISTA output=SEN2_2018_2019_average_${INDEX} method=average

# 2019-2020
LISTA=`g.list -e rast pat="SEN2_(20[1|2][9|0][0|1][1-3][0-3][0-9])_${INDEX}" exclude='20190[1-3]' map=. sep=,`
echo "Temporal mosaics, period 2019-2020, index < $INDEX >..."

r.series input=$LISTA output=SEN2_2019_2020_average_${INDEX} method=average

done


#
# Spatial average and sd for indices
#


for INDEX in ${INDICES[*]} ; do
  for YEAR in ${YEARS[*]} ; do
    for SIZE in ${WINDOW_SIZES[*]} ; do

    r.neighbors \
      input=SEN2_${YEAR}_average_${INDEX} \
      method=average \
      size=${SIZE} \
      output=SEN2_${YEAR}_${INDEX}_average_${SIZE}
    
    r.neighbors \
      input=SEN2_${YEAR}_average_${INDEX} \
      method=stddev \
      size=${SIZE} \
      output=SEN2_${YEAR}_${INDEX}_sd_${SIZE}
    
    done
  done
done


#
# Temporal mosaics for all bands 2017-2018, 2018-2019 and 2019-2020
#


for BAND in `seq 1 10` ; do

# 2017-2018
LISTA=`g.list -e rast pat="SEN2_(201[7|8][0|1][1-3][0-3][0-9])_stdsref_band.${BAND}_fix$" exclude='20181[1|2]' map=. sep=,`
echo "Temporal mosaics, period 2017-2018, band < $BAND >..."

r.series input=$LISTA output=SEN2_2017_2018_median_band.${BAND} method=median
r.series input=$LISTA output=SEN2_2017_2018_average_band.${BAND} method=average
r.mapcalc expression="SEN2_2017_2018_average_band.${BAND} = round(SEN2_2017_2018_average_band.${BAND})"

# 2018-2019
LISTA=`g.list -e rast pat="SEN2_(201[8|9][0|1][1-3][0-3][0-9])_stdsref_band.${BAND}_fix$" exclude='20180[1-3]|20191[1|2]' map=. sep=,`
echo "Temporal mosaics, period 2018-2019, band < $BAND >..."

r.series input=$LISTA output=SEN2_2018_2019_median_band.${BAND} method=median
r.series input=$LISTA output=SEN2_2018_2019_average_band.${BAND} method=average
r.mapcalc expression="SEN2_2018_2019_average_band.${BAND} = round(SEN2_2018_2019_average_band.${BAND})"

# 2019-2020
LISTA=`g.list -e rast pat="SEN2_(20[1|2][9|0][0|1][1-3][0-3][0-9])_stdsref_band.${BAND}_fix$" exclude='20190[1-3]' map=. sep=,`
echo "Temporal mosaics, period 2019-2020, band < $BAND >..."

r.series input=$LISTA output=SEN2_2019_2020_median_band.${BAND} method=median
r.series input=$LISTA output=SEN2_2019_2020_average_band.${BAND} method=average
r.mapcalc expression="SEN2_2019_2020_average_band.${BAND} = round(SEN2_2019_2020_average_band.${BAND})"

done


#
# Enhance colors and assess results
#


for map in `g.list -e rast pat='median|average' map=.` ; do
r.colors $map color=grey 
done

i.colors.enhance red=SEN2_2017_2018_median_band.3 green=SEN2_2017_2018_median_band.2 blue=SEN2_2017_2018_median_band.1
i.colors.enhance red=SEN2_2017_2018_average_band.3 green=SEN2_2017_2018_average_band.2 blue=SEN2_2017_2018_average_band.1

i.colors.enhance red=SEN2_2018_2019_median_band.3 green=SEN2_2018_2019_median_band.2 blue=SEN2_2018_2019_median_band.1
i.colors.enhance red=SEN2_2018_2019_average_band.3 green=SEN2_2018_2019_average_band.2 blue=SEN2_2018_2019_average_band.1

i.colors.enhance red=SEN2_2019_2020_median_band.3 green=SEN2_2019_2020_median_band.2 blue=SEN2_2019_2020_median_band.1
i.colors.enhance red=SEN2_2019_2020_average_band.3 green=SEN2_2019_2020_average_band.2 blue=SEN2_2019_2020_average_band.1

# median seems to be the best representation, I use them for synthetic pan and texture indices


#
# Sinthetic PAN band (median mosaics)
#


for YEAR in ${YEARS[*]} ; do

RED=SEN2_${YEAR}_median_band.3
GREEN=SEN2_${YEAR}_median_band.2
BLUE=SEN2_${YEAR}_median_band.1

r.mapcalc expression="SEN2_${YEAR}_pan = (${RED} + ${GREEN} + ${BLUE}) / 3"

done


#
# Texture measures over the pan sinthetic band
#


for YEAR in ${YEARS[*]} ; do
  for SIZE in ${WINDOW_SIZES[*]} ; do
    for METHOD in ${TEXTURE_METHODS[*]} ; do
  
    texture_prefix=`echo 'SEN2_'$YEAR'_pan_'$SIZE`
    
    r.texture.tiled input=SEN2_${YEAR}_pan \
                    output=$texture_prefix \
                    method=$METHOD \
                    size=$SIZE \
                    processes=$PROCESSES
    done
  done
done


#
# Unsupervised Classification
#


CLASS_NUM=15

for YEAR in ${YEARS[*]} ; do

# Create group of bands
i.group group=s2_${YEAR} \
  subgroup=bands \
  input=`g.list -e rast pat="SEN2_${YEAR}" exclude="median_band|average_n|average_l|average_e|pan_5|pan_9" mapset=. sep=,`

# Signature files (stats for the classes)
i.cluster group=s2_${YEAR} \
  subgroup=bands \
  signaturefile=signat_s2_${YEAR} \
  classes=${CLASS_NUM} \
  reportfile=rep_s2_${YEAR}.txt \
  separation=0.6 \
  iterations=50 \
  sample=20,20 --o

# Unsupervised classification
i.maxlik \
  group=s2_${YEAR} \
  subgroup=bands \
  signaturefile=signat_s2_${YEAR} \
  output=class_s2_${YEAR}_${CLASS_NUM}c

done


#
# Class proportions
#


for YEAR in ${YEARS[*]} ; do
  for SIZE in ${WINDOW_SIZES[*]} ; do

r.neighbors \
  input=class_s2_${YEAR}_${CLASS_NUM}c \
  method=diversity \
  size=${SIZE} \
  output=class_s2_${YEAR}_rich_${SIZE}

r.neighbors \
  input=class_s2_${YEAR}_${CLASS_NUM}c \
  method=mode \
  size=${SIZE} \
  output=class_s2_${YEAR}_mode_${SIZE}

r.neighbors \
  input=class_s2_${YEAR}_${CLASS_NUM}c \
  method=interspersion \
  size=${SIZE} \
  output=class_s2_${YEAR}_intersp_${SIZE}

  done
done


#
# Diversity measures
#


# add-on r.diversity
g.extension extension=r.diversity

for YEAR in ${YEARS[*]} ; do
  for SIZE in ${WINDOW_SIZES[*]} ; do

r.diversity \
  input=class_s2_${YEAR}_${CLASS_NUM}c \
  prefix=class_s2_${YEAR}_${CLASS_NUM}c_diversity \
  size=${SIZE} \
  method=simpson

r.diversity \
  input=class_s2_${YEAR}_${CLASS_NUM}c \
  prefix=class_s2_${YEAR}_${CLASS_NUM}c_diversity \
  size=${SIZE} \
  method=shannon

  done
done


#
# Variable's list per year
#

g.copy vector=ovitrampas_cba@dengue_cba,ovitraps
g.copy vector=ovitraps,ovitraps_with_data_2017_2018_50m
g.copy vector=ovitraps,ovitraps_with_data_2017_2018_100m
g.copy vector=ovitraps,ovitraps_with_data_2018_2019_50m
g.copy vector=ovitraps,ovitraps_with_data_2018_2019_100m

# 2017-2018 vars
VARS20172018_50m=`echo "SEN2_2017_2018_evi_average_9,SEN2_2017_2018_evi_sd_9,SEN2_2017_2018_lswi_average_9,SEN2_2017_2018_lswi_sd_9,SEN2_2017_2018_ndbi_average_9,SEN2_2017_2018_ndbi_sd_9,SEN2_2017_2018_ndvi_average_9,SEN2_2017_2018_ndvi_sd_9,SEN2_2017_2018_ndwi_mf_average_9,SEN2_2017_2018_ndwi_mf_sd_9,SEN2_2017_2018_ndwi_x_average_9,SEN2_2017_2018_ndwi_x_sd_9,SEN2_2017_2018_pan_9_ASM,SEN2_2017_2018_pan_9_Contr,SEN2_2017_2018_pan_9_Corr,SEN2_2017_2018_pan_9_Entr,SEN2_2017_2018_pan_9_IDM,class_s2_2017_2018_15c_diversity_shannon_size_9,class_s2_2017_2018_15c_diversity_simpson_size_9,class_s2_2017_2018_intersp_9,class_s2_2017_2018_mode_9,class_s2_2017_2018_rich_9"`
VARS20172018_100m=`echo "SEN2_2017_2018_evi_average_19,SEN2_2017_2018_evi_sd_19,SEN2_2017_2018_lswi_average_19,SEN2_2017_2018_lswi_sd_19,SEN2_2017_2018_ndbi_average_19,SEN2_2017_2018_ndbi_sd_19,SEN2_2017_2018_ndvi_average_19,SEN2_2017_2018_ndvi_sd_19,SEN2_2017_2018_ndwi_mf_average_19,SEN2_2017_2018_ndwi_mf_sd_19,SEN2_2017_2018_ndwi_x_average_19,SEN2_2017_2018_ndwi_x_sd_19,SEN2_2017_2018_pan_19_ASM,SEN2_2017_2018_pan_19_Contr,SEN2_2017_2018_pan_19_Corr,SEN2_2017_2018_pan_19_Entr,SEN2_2017_2018_pan_19_IDM,class_s2_2017_2018_15c_diversity_shannon_size_19,class_s2_2017_2018_15c_diversity_simpson_size_19,class_s2_2017_2018_intersp_19,class_s2_2017_2018_mode_19,class_s2_2017_2018_rich_19"`

# 2018-2019 vars
VARS20182019_50m=`echo "SEN2_2018_2019_evi_average_9,SEN2_2018_2019_evi_sd_9,SEN2_2018_2019_lswi_average_9,SEN2_2018_2019_lswi_sd_9,SEN2_2018_2019_ndbi_average_9,SEN2_2018_2019_ndbi_sd_9,SEN2_2018_2019_ndvi_average_9,SEN2_2018_2019_ndvi_sd_9,SEN2_2018_2019_ndwi_mf_average_9,SEN2_2018_2019_ndwi_mf_sd_9,SEN2_2018_2019_ndwi_x_average_9,SEN2_2018_2019_ndwi_x_sd_9,SEN2_2018_2019_pan_9_ASM,SEN2_2018_2019_pan_9_Contr,SEN2_2018_2019_pan_9_Corr,SEN2_2018_2019_pan_9_Entr,SEN2_2018_2019_pan_9_IDM,class_s2_2018_2019_15c_diversity_shannon_size_9,class_s2_2018_2019_15c_diversity_simpson_size_9,class_s2_2018_2019_intersp_9,class_s2_2018_2019_mode_9,class_s2_2018_2019_rich_9"`
VARS20182019_100m=`echo "SEN2_2018_2019_evi_average_19,SEN2_2018_2019_evi_sd_19,SEN2_2018_2019_lswi_average_19,SEN2_2018_2019_lswi_sd_19,SEN2_2018_2019_ndbi_average_19,SEN2_2018_2019_ndbi_sd_19,SEN2_2018_2019_ndvi_average_19,SEN2_2018_2019_ndvi_sd_19,SEN2_2018_2019_ndwi_mf_average_19,SEN2_2018_2019_ndwi_mf_sd_19,SEN2_2018_2019_ndwi_x_average_19,SEN2_2018_2019_ndwi_x_sd_19,SEN2_2018_2019_pan_19_ASM,SEN2_2018_2019_pan_19_Contr,SEN2_2018_2019_pan_19_Corr,SEN2_2018_2019_pan_19_Entr,SEN2_2018_2019_pan_19_IDM,class_s2_2018_2019_15c_diversity_shannon_size_19,class_s2_2018_2019_15c_diversity_simpson_size_19,class_s2_2018_2019_intersp_19,class_s2_2018_2019_mode_19,class_s2_2018_2019_rich_19"`

# columns labels
COLUMNS_50m=`echo "evi_av_9,evi_sd_9,lswi_av_9,lswi_sd_9,ndbi_av_9,ndbi_sd_9,ndvi_av_9,ndvi_sd_9,ndwi_mf_av_9,ndwi_mf_sd_9,ndwi_x_av_9,ndwi_x_sd_9,ASM_9,Contr_9,Corr_9,Entr_9,IDM_9,Shannon_9,Simpson_9,intersp_9,mode_9,rich_9"`
COLUMNS_100m=`echo "evi_av_19,evi_sd_19,lswi_av_19,lswi_sd_19,ndbi_av_19,ndbi_sd_19,ndvi_av_19,ndvi_sd_19,ndwi_mf_av_19,ndwi_mf_sd_19,ndwi_x_av_19,ndwi_x_sd_19,ASM_19,Contr_19,Corr_19,Entr_19,IDM_19,Shannon_19,Simpson_19,intersp_19,mode_19,rich_19"`


#
# Get environmental data for ovitraps
#

g.extension v.what.rast.multi

v.what.rast.multi map=ovitraps_with_data_2017_2018_50m raster=$VARS20172018_50m columns=$COLUMNS_50m
v.what.rast.multi map=ovitraps_with_data_2017_2018_100m raster=$VARS20172018_100m columns=$COLUMNS_100m

v.what.rast.multi map=ovitraps_with_data_2018_2019_50m raster=$VARS20182019_50m columns=$COLUMNS_50m
v.what.rast.multi map=ovitraps_with_data_2018_2019_100m raster=$VARS20182019_100m columns=$COLUMNS_100m


#
# Import cluster predictions from R and get class proportion and number of dengue cases per neighbourhood
#

# predictions were imported manually with the GRASS GUI

g.extension r.zonal.classes
v.to.rast input=Barrios_Cba output=Barrios_Cba use=cat
r.zonal.classes -p zone_map=Barrios_Cba raster=pred_2019_2020_50m csvfile=class_prop.csv
v.vect.stats -p points=dengue_autoctonos areas=Barrios_Cba count_column=counts_dengue
v.to.db map=Barrios_Cba option=area columns=area units=hectares -p


#
# Get population data from worldpop and aggregate to neighbourhood level
#

r.import extent=region resolution=value resolution_value=100 input=arg_ppp_2020_UNadj_constrained.tif output=arg_ppp_2020_UNadj_constrained
v.rast.stats map=Barrios_Cba raster=arg_ppp_2020_UNadj_constrained column_prefix=ppp method=sum
v.db.select map=Barrios_Cba columns=cat,ppp_sum file=tabla_poblacion_por_barrio.csv

