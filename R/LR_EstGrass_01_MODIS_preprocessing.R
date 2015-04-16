
###############################################################################
# Preprocessing of MODIS remote data
# ----------------------------------------------------------------------------
# 
# Download of the necessary MODIS tiles, generation of GeoTIFFs clipped on 
# the local study area, calculation of NDVI images
# 
# WARNING: this code makes use of external calls on a Linux OS.
# The following binaries are required (ensure also that PATH is set to find them):
# wget, cd, rm; gdal_translate (GDAL); resample (MRT). 
# 
# Author: Luigi Ranghetti
###############################################################################


## Set parameters
dataset = "MOD09Q1"       # MODIS dataset used to extract NDVI values
dataset_A = "MOD09A1"     # Ancillary MODIS dataset used to extract information about the DOY of aquisition
tile = "h18v04"           # MODIS tile of the study area
location = "PNGP"         # String which identifies the extent of the study area
extent = c(542539,5083930,596746,5045012)
                          # Bounding box of the study area (in CRS_MODIS: c(xmin,ymax,xmax,ymin) )

## Set subdirectories
hdffolders <- file.path(modisdir,"hdf",dataset,years)        # Folders in which original HDF MODIS images will be downloaded
tiffolders <- file.path(modisdir,"tif",dataset,years)        # Folders in which derived GeoTIFFs will be stored
hdffolders_A <- file.path(modisdir,"hdf",dataset_A,years)    # Idem for the ancillary dataset
tiffolders_A <- file.path(modisdir,"tif",dataset_A,years)    # Idem for the ancillary dataset
names(hdffolders) <- names(tiffolders) <- names(hdffolders_A) <- names(tiffolders_A) <- years


### Retrieve original MODIS data ###
lapply(as.list(c(hdffolders,tiffolders,hdffolders_A,tiffolders_A)), dir.create, recursive=TRUE, showWarnings=FALSE)

if (download_HDF) {
    
    ## Create subdirs (if not already present)
    dir.create(logdir,recursive=TRUE,showWarnings=FALSE)
    
    ## Download the required HDF files (take a cup of tea... or better, go lunch!)
    for (year in years) {
        system(paste0("wget -S --recursive --no-parent --no-directories -N -P ",hdffolders," --accept \"*",tile,"*\" ",file.path("ftp://ladsweb.nascom.nasa.gov/allData/5",dataset,year)))
        system(paste0("wget -S --recursive --no-parent --no-directories -N -P ",hdffolder_A," --accept \"*",tile,"*\" ",file.path("ftp://ladsweb.nascom.nasa.gov/allData/5",dataset_A,year)))
    }
}

## Retrieve file lists
hdflists <- lapply(hdffolders, list.files, "h18v04[0-9.]*\\.hdf$")      # Lists of available HDF images
hdflists_A <- lapply(hdffolders_A, list.files, "h18v04[0-9.]*\\.hdf$")  # Idem for the ancillary dataset
tiflists_base <- lapply(hdflists, function(x) gsub("h18v04([0-9.]*)\\.hdf", "PNGP\\1\\.tif", x))
# Prefix of available GeoTIFFs (without band name)
tiflists_A_base <- lapply(hdflists_A, function(x) gsub("h18v04([0-9.]*)\\.hdf", "PNGP\\1\\.tif", x))
# Idem for the ancillary dataset

### Create NDVI GeoTiffs ###
for (year in years) {

    ## Clip and convert to GeoTIFF (without reprojecting)
    if ( length(hdflists[[year]]) != length(hdflists_A[[year]]) ) stop ("MOD09Q1 and MOD09A1 files must be of the same number: check the downloaded hdf.")
    resample_log <- file(file.path(MRTlogdir,paste0("MRT_",strftime(Sys.time(),'%y%m%d_%H%M%S'),'.log')))
    sink(resample_log,append=logappend) # Output of ModisTool is redirected to the specified log file
    for ( i in 1:length(hdflists[[year]])) {
        cat(system(paste("resample -f -p", file.path(main_wd,"R/accessories/MRT/PNGP_MOD09Q1_b1b2_clip.prm"), 
                                "-i", file.path(hdffolders[year],hdflists[[year]][i]),
                                "-o", file.path(tiffolders[year],tiflists_base[[year]][i])), intern=TRUE),sep="\n")              
        cat(system(paste("resample -f -p", file.path(main_wd,"R/accessories/MRT/PNGP_MOD09A1_doy_clip.prm"),
                                "-i", file.path(hdffolders_A[year],hdflists_A[[year]][i]),
                                "-o", file.path(tiffolders_A[year],tiflists_A_base[[year]][i])),intern=TRUE),sep="\n")              
    }
    sink()
    
    ## Create DOY for MOD09Q1 from MOD09A1
    for ( tifname in gsub("\\.tif$","\\.sur_refl_day_of_year\\.tif", tiflists_A_base[[year]]) )
        system(paste("gdal_translate -outsize 200% 200% -of GTiff", file.path(tiffolders_A[year],tifname),
                        file.path(tiffolders[year],gsub("MOD09A1","MOD09Q1",tifname))))
    
    ## Compute NDVI on MOD09Q1
    for ( tifname in tiflists_base[[year]] ) {
        system(paste0("cd ",tiffolders[year],"; gdal_calc.py -A ",gsub("\\.tif$","\\.sur_refl_b01\\.tif",tifname), " -B ",gsub("\\.tif$","\\.sur_refl_b02\\.tif",tifname),
                        " --outfile=",gsub("\\.tif$","\\.NDVI\\.tif",tifname)," --type=\"Int16\" --calc=\"(B.astype(float32)-A)*10000/(B+A)\""))
        system(paste0("cd ",tiffolders[year],"; gdal_calc.py -A ",gsub("\\.tif$","\\.NDVI\\.tif",tifname),
                        " --outfile=",gsub("\\.tif$","\\.NDVImask\\.tif",tifname)," --type=\"Int16\" --calc=\"logical_and(A>-10000,A<10000)\" --NoDataValue=0"))
            # this is used to set NA values out of range
        system(paste0("cd ",tiffolders[year],"; gdal_calc.py -A ",gsub("\\.tif$","\\.NDVI\\.tif",tifname)," -B ",gsub("\\.tif$","\\.NDVImask\\.tif",tifname),
                        " --outfile=",gsub("\\.tif$","\\.NDVI\\.tif",tifname)," --type=\"Int16\" --NoDataValue=-32767 --calc=\"A*B\""))
        system(paste("rm",file.path(tiffolders[year],gsub("\\.tif$","\\.NDVImask\\.tif",tifname))))
    }

} # end of year cycle
