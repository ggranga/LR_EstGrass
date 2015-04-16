
###############################################################################
# Estimation of nutritional properties of alpine grassland from MODIS NDVI data
# (L. Ranghetti, B. Bassano, G. Bogliani, A. Palmonari, A. Formigoni and A. von Hardenberg)
# 
# SUPPLEMENTARY MATERIAL
# ----------------------------------------------------------------------------
# 
# Code used to preprocess data and make statistical analyses used in the paper 
# "Estimation of nutritional properties of alpine grassland from MODIS NDVI data"
# (L. Ranghetti, B. Bassano, G. Bogliani, A. Palmonari, A. Formigoni and A. von Hardenberg).
# The code is self-running. The following sources files are required:
# - R source files (see the list at the end of this main script);
# - ESRI Shapefiles of field plots ("plot2012" and "plot2013" sets; see script 02 for their requirements);
# - field dataset ("field2012.csv", "field2012_heights.csv" and "field2013.csv"; see script 04 for their requirements).
#
# Author: Luigi Ranghetti, PhD
# email: ranghetti.l@irea.cnr.it
# 
# license: GPL(>2)
# 
###############################################################################


## Check if required packages are present, or install them; and charge
pkg_list <- c("sp","rgdal","raster", "stringr","tcltk","foreach","car","QuantPsyc")
for (pkg in pkg_list) {while (!require(pkg,character.only=TRUE)) {install.packages(pkg,dep=TRUE)}}

## Set preprocessing options
download_HDF     = FALSE  # TRUE if original MODIS HDF files have to be downloaded; FALSE if not
preprocess_MODIS = FALSE  # TRUE if preprocessing is required; FALSE if not

## Set global parameters
years <- c("2012","2013")  # Years of data used in the study
CRS_MODIS <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
# Coordinate Reference System used for analyses (the same of MODIS data, to avoid reprojection) 

## Define directory paths
codedir <- dirname(sys.frame(1)$ofile)                    # Directory where script files are
main_wd   <- dirname(codedir)                             # Root directory of the project
modisdir  <- file.path(main_wd,"files/MODIS")             # Directory where MODIS images will be downloaded and created
plotdir   <- file.path(main_wd,"files/plots")             # Directory where shapefiles of field plots should be found
fielddir  <- file.path(main_wd,"files/field")             # Directory where field dataset should be found
MRTlogdir <- file.path(main_wd,"R/accessories/MRT/logs")  # Directory where ouput logs of ModisTool will be stored
outdir    <- file.path(main_wd,"results")                 # Directory where results of analysis will be stored

lapply(list(modisdir,plotdir,fielddir,MRTlogdir,outdir), dir.create, recursive=TRUE, showWarnings=FALSE)

## Download the required datasets
download.file("http://files.figshare.com/2023421/field2012.csv",file.path(fielddir,"field2012.csv"))
download.file("http://files.figshare.com/2014825/field2013.csv",file.path(fielddir,"field2013.csv"))
download.file("http://files.figshare.com/2014830/field2012_heights.csv",file.path(fielddir,"field2012_heights.csv"))
download.file("http://files.figshare.com/2014831/plots2012.zip",file.path(plotdir,"plot2012.zip"))
download.file("http://files.figshare.com/2023420/plots2013.zip",file.path(plotdir,"plot2013.zip"))
unzip(file.path(plotdir,"plot2012.zip")); unlink(file.path(plotdir,"plot2012.zip"))
unzip(file.path(plotdir,"plot2013.zip")); unlink(file.path(plotdir,"plot2013.zip"))

## Run code
if (preprocess_MODIS)
    source(file.path(codedir,"LR_EstGrass_01_MODIS_preprocessing.R"))  # Preprocessing of MODIS remote data <-- run only one time!
source(file.path(codedir,"LR_EstGrass_02_MODIS_time_series.R"))        # Generation of NDVI time series on plots
source(file.path(codedir,"LR_EstGrass_03_Phenological_variables.R"))   # Extraction of phenological variables
source(file.path(codedir,"LR_EstGrass_04_Field_dataset.R"))            # Fusion of field dataset
source(file.path(codedir,"LR_EstGrass_05_Cal_val.R"))                  # Calibration and validation of predictive models

cat('All done!\nSee directory',outdir,'for analysis metrics, or obtain the desired objects from the produced workspace.\n')
