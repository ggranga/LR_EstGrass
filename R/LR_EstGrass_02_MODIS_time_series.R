
###############################################################################
# Generation of NDVI time series on plots
# ----------------------------------------------------------------------------
# 
# Estimation of daily NDVI values for the position of field plots
# from MODIS preprocessed data
# 
# Author: Luigi Ranghetti
###############################################################################

## Definition of list of objects (for each list, one element per year)
MOD09Q1.PNGP_filelist_ndvi <- MOD09Q1.PNGP_filelist_doy <- list()        # name lists of GeoTIFF
MOD09Q1.PNGP.ndvi <- MOD09Q1.PNGP.doy <- list()                          # raster of NDVI and DOY values
MOD09Q1.doy.composite <- list()  # vector with DOY windows of composites (first day, it should be the same for all the years)
plots <- list()                                                          # SpatialPointsDataFrame of the sampling plots
plots.doy <- plots.NDVI <- plots.NDVI_corrected <- list()                # SPDF of NDVI and relative doys 
plots.NDVI.daily <- plots.NDVI.daily.ravg <- plots.NDVI.Spline <- list() # matrices of daily NDVI values

## Functions used in the script

# Function to extract punctual values of NDVI from the pixels near the position of the plot (see text)
weight.pixel <- function( plots, raster.valori, raggio=250, sigma=1 ) {
    ### raster.distanze: raster che, per ogni recinto, dà la distanza dei pixel dell'area dal recinto
    raster.distanze <- SpatialPixelsDataFrame(raster.valori, data=as.data.frame(spDists(raster.valori,plots)))
    names(raster.distanze@data) <- plots$plot
    raster.distanze@data <- as.data.frame( 1/(2*pi*sigma^2) * exp(-(raster.distanze@data/raggio)^2/(2*sigma^2)) )
    raster.distanze@data[raster.distanze@data<0.05]<-0  # con sigma=1, eliminando i valori <0.05 rimangono 8-9 pixel per plot
    ## raster.pesi: il peso relativo di ogni pixel dell'area nella determinazione del valore di ogni recinto
    raster.pesi <- SpatialGridDataFrame( raster.valori, data= raster.distanze@data / apply(raster.distanze@data,2,sum) )
    ## raster.valoripesati: l'output
    raster.valoripesati <- SpatialPointsDataFrame(plots, cbind( "plot"=plots$plot, over(plots,raster.valori) ) )
    raster.valoripesati@data <- as.data.frame(apply( raster.valoripesati@data, 2, function(x) x <- rep(NA,length(x)) ))
    rownames(raster.valoripesati@data) <- rownames(raster.valoripesati@coords) <- plots$plot
    for ( i in rownames(plots@data) ) {
        for ( j in colnames(raster.valori@data) ) {
            raster.valoripesati@data[i,j] <- sum( raster.valori@data[j] * raster.pesi@data[i], na.rm=TRUE )
        }
    }
    raster.valoripesati$plot <- plots$plot
    raster.valoripesati
} # TODO translate local vars

# Function to correct anomalies in NDVI time series
correct.NDVIts <- function( df, doy )  {
    require(tcltk); require(foreach)
    # use NDVI=-0.3 as minimum value
    df[,-1][df[,-1]>1] <- 1; df[,-1][df[,-1]< -0.3] <- -0.3
    dfC <- df.mmean <- df
    pb <- tkProgressBar(title="correct.NDVIts", min=0, max=ncol(df), initial=0)
    # compute values as moving average of previous and following values (to use as possible substitution values)
    for ( i in 3:(ncol(df)-1) )  {
        df.mmean[,i] <- ifelse( doy[,(i-1)]!=doy[,(i+1)], 
                ( df[,(i-1)] * (doy[,(i+1)]-doy[,i]) + df[,(i+1)] * (doy[,i]-doy[,(i-1)]) ) / (doy[,(i+1)]-doy[,(i-1)]), 
                df[,(i-1)] + df[,(i+1)] / 2 )
        setTkProgressBar( pb, i, label="Operation 1 of 4" )
    }
    # 1) removing "pits"
    for ( i in 3:(ncol(df)-1) )  {
        dfC[ df[,(i-1)]-df[,i]>0.1 & df[,(i+1)]-df[,i]>0.1, i ] <- df.mmean[ df[,(i-1)]-df[,i]>0.1 & df[,(i+1)]-df[,i]>0.1, i ]
        setTkProgressBar( pb, i, label="Operation 2 of 4" )
    }
    # recomputing moving average values (since values near "pits" would be infuenced)
    df <- dfC
    for ( i in 3:(ncol(df)-1) )  {
        df.mmean[,i] <- ifelse( doy[,(i-1)]!=doy[,(i+1)], 
                ( df[,(i-1)] * (doy[,(i+1)]-doy[,i]) + df[,(i+1)] * (doy[,i]-doy[,(i-1)]) ) / (doy[,(i+1)]-doy[,(i-1)]), 
                df[,(i-1)] + df[,(i+1)] / 2 )
        setTkProgressBar( pb, i, label="Operation 3 of 4" )
    }
    # removing "peaks"
    for ( i in 3:(ncol(df)-1) )  {
        dfC[ df[,i]-df[,(i-1)]>0.1 & df[,i]-df[,(i+1)]>0.1, i ] <- df.mmean[ df[,i]-df[,(i-1)]>0.1 & df[,i]-df[,(i+1)]>0.1, i ]
        setTkProgressBar( pb, i, label="Operation 4 of 4" )
    }
    df <- dfC
    close(pb)
    df
}


### Main script (running one time per year) ###

for (year in years) {
    
    ### Import remote data ###
    
    ## Import MODIS data
        # (lists are regenerated in order to be available also without re-running the code of preprocessing of remote data)
    tiffolder <- file.path(modisdir,"tif/MOD09Q1",year)
    MOD09Q1.PNGP_filelist_ndvi[[year]] <- sort(list.files(tiffolder,"\\.NDVI.tif$"))
    MOD09Q1.PNGP_filelist_doy[[year]] <- sort(list.files(tiffolder,"\\.sur_refl_day_of_year.tif$"))
    
    MOD09Q1.PNGP.ndvi[[year]] <- readGDAL(file.path(tiffolder,MOD09Q1.PNGP_filelist_ndvi[[year]][1]))
    names(MOD09Q1.PNGP.ndvi[[year]]) <- substr(MOD09Q1.PNGP_filelist_ndvi[[year]][1],9,16)
    for ( tifname in MOD09Q1.PNGP_filelist_ndvi[[year]][-1] )
        MOD09Q1.PNGP.ndvi[[year]]@data[,substr(basename(tifname),9,16)] <- readGDAL(file.path(tiffolder,tifname))$band1
    MOD09Q1.PNGP.ndvi[[year]]@data <- MOD09Q1.PNGP.ndvi[[year]]@data / 10000  # convert in range -1 - 1 
    
    MOD09Q1.PNGP.doy[[year]] <- readGDAL(file.path(tiffolder,MOD09Q1.PNGP_filelist_doy[[year]][1]))
    names(MOD09Q1.PNGP.doy[[year]]) <- substr(MOD09Q1.PNGP_filelist_doy[[year]][1],9,16)
    for ( tifname in MOD09Q1.PNGP_filelist_doy[[year]][-1] )
        MOD09Q1.PNGP.doy[[year]]@data[,substr(basename(tifname),9,16)] <- readGDAL(file.path(tiffolder,tifname))$band1
    MOD09Q1.PNGP.doy[[year]]@data[MOD09Q1.PNGP.doy[[year]]@data[,ncol(MOD09Q1.PNGP.doy[[year]])]<361,ncol(MOD09Q1.PNGP.doy[[year]])] <- 366 
        # This to avoid that images taken in 1-3 January of year x+1 would be considered as taken in 1-3 January of year x
    
    MOD09Q1.doy.composite[[year]] <- as.integer(substr(names(MOD09Q1.PNGP.doy[[year]]),6,8))
    names(MOD09Q1.doy.composite[[year]]) <- names(MOD09Q1.PNGP.doy[[year]])
    
    if ( any(names(MOD09Q1.PNGP.ndvi[[year]]) != names(MOD09Q1.PNGP.doy[[year]])) ) stop ("NDVI and DOY files do not correspond: check the created tiffs.")
    
    
    ### Import plot information ###
    
    plots[[year]] <- readOGR(plotdir,paste0("plots",year))  # one shapefile per year must be present!
    #  str(plots[["2012"]]@data)
    #  'data.frame':	19 obs. of  5 variables:
    #  $ plot     : Factor w/ 19 levels  string code which identify single plots
    #  $ elevation: int                  elevation of the plot, estracted from the Tinitaly 10m DEM (see text) 
    #  $ aspect   : int                  aspect of the plot, derived from the elevation map
    #  $ Localita : Factor w/ 19 levels  descriptive location of plots (in Italian, only for 2012)
    plots[[year]] <- spTransform(plots[[year]], CRS_MODIS)
    plots[[year]]$plot <- as.character(plots[[year]]$plot)
    rownames(plots[[year]]@data) <- rownames(plots[[year]]@coords) <- plots[[year]]$plot
    plots[[year]]$aspectNS <- -cos(plots[[year]]$aspect*pi/180) # it varies between -1 (North) and 1 (South)
    
    ## estract DOY of MODIS values for the pixels in which plots are located
    plots.doy[[year]] <- SpatialPointsDataFrame(plots[[year]], cbind( "plot"=plots[[year]]$plot, over(plots[[year]],MOD09Q1.PNGP.doy[[year]])))
    rownames(plots.doy[[year]]@data) <- rownames(plots.doy[[year]]@coords) <- plots[[year]]$plot
    

    ### Processing of time series ###
    
    ## Extract NDVI values for the pixels in which plots are located, using a smoothing kernel filter (see text) 
    plots.NDVI[[year]] <- weight.pixel( plots[[year]], MOD09Q1.PNGP.ndvi[[year]] )
    
    ## Removing values that are not coherent with the time series (see text)
    plots.NDVI_corrected[[year]] <- plots.NDVI[[year]]
    plots.NDVI_corrected[[year]]@data <- correct.NDVIts( plots.NDVI[[year]]@data, plots.doy[[year]]@data )
    
    ## Daily smoothing (for phenology)
    plots.NDVI.daily[[year]] <- matrix( NA, nrow=nrow(plots.NDVI_corrected[[year]]@data), ncol=366,
            dimnames=list( rownames(plots.NDVI_corrected[[year]]@data), 1:366 ) )
    for ( plot in rownames(plots.NDVI.daily[[year]]) )
        plots.NDVI.daily[[year]][plot,as.integer(plots.doy[[year]]@data[plot,-1])] <- as.numeric(plots.NDVI_corrected[[year]]@data[plot,-1])
    # (These are the same values in a daily table)
    plots.NDVI.daily.ravg[[year]] <- plots.NDVI.daily[[year]]
    for ( j in 1:nrow(plots.NDVI.daily.ravg[[year]]) )
        plots.NDVI.daily.ravg[[year]][j,which(!is.na(plots.NDVI.daily.ravg[[year]][j,]))] <-
                predict( loess( plots.NDVI.daily[[year]][j,] ~ c(1:ncol(plots.NDVI.daily[[year]])), span=0.3 ) ) 
    # (The same values with a loess filter - see text)
    plots.NDVI.Spline[[year]] <- plots.NDVI.daily.ravg[[year]]
    for ( j in 1:nrow(plots.NDVI.daily[[year]]) )
        plots.NDVI.Spline[[year]][j,] <- spline( 1:ncol(plots.NDVI.daily[[year]]), plots.NDVI.daily.ravg[[year]][j,], 
                xmin=1, xmax=ncol(plots.NDVI.daily[[year]]), n=ncol(plots.NDVI.daily[[year]]) )[[2]]
    # (Daily interpolation with a spline - see text)
    
} # end of year cycle (all the code)
