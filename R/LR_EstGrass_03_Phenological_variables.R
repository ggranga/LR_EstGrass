
###############################################################################
# Extraction of phenological variables
# ----------------------------------------------------------------------------
# 
# Determination of phenological variables (beginning of growing season,
# maximum yearly NDVI and relative day of occurrence - see text) for each plot
#
# Author: Luigi Ranghetti
###############################################################################


### Determination of BGS threshold ###

# Function to determine the relative NDVI threshold to be used as proxy for 
# the date of beginning of growing season (as defined in the text)
BGS_threshold <- function( field, plots.NDVI.Spline, ... ) {
    # field dataframe must contain the following columns:
    #   $ doy    : int     day of the year of the harvest
    #   $ plot   : Factor  plot name
    #   $ h_tot  : num     mean height of total biomass (cm)
    # plots.NDVI.Spline could have been computed here, but we passed it since we already computed it in 02_MODIS_time_series.R
    # (it must contains j rows (one for each field plot) and 366 columns (one for doy))
    
    ## Daily smoothing of the heights
    h_daily <- matrix( NA, nrow=length(unique(field$plot)), ncol=366, dimnames=list( unique(field$plot), 1:366 ) )
    for ( j in 1:nrow(field) )
        h_daily[ as.character(field$plot[j]), field$doy[j] ] <- field$h_tot[j]
    # (These are the same values in a daily table)
    h_daily.loess <- h_daily
    for ( j in rownames(h_daily) )
        h_daily.loess[j,] <- predict( loess(h_tot~doy,data.frame("h_tot"=h_daily[j,!is.na(h_daily[j,])],"doy"=c(1:366)[!is.na(h_daily[j,])])), data.frame("doy"=1:366) )
    # (The same values with a loess filter)
    
    ## Computing BGS from field data
    bgs5cm <- first_data <- numeric()
    for ( j in rownames(h_daily) )  {
        bgs5cm[j] <- which(h_daily.loess[j,]>=5)[1]
        first_data[j] <- which(!is.na(h_daily[j,]))[1]
        if ( bgs5cm[j]-first_data[j] <=1 ) bgs5cm[j] <- NA # in the case the rist observation is already > 5cm, no BGS value is computable 
    }
    
    # Computing all the possible relative thresholds (1%-99%) for the considered plots
    BGS_thresholds <- matrix( NA, nrow=length(unique(field$plot)), ncol=99, dimnames=list( unique(field$plot), 1:99 ) )
    for ( thr in 1:99 )  {
        for ( plot in rownames(h_daily) )  {
            BGS_thresholds[plot,thr]  <- which( plots.NDVI.Spline[plot,][101:365] >= max(plots.NDVI.Spline[plot,],na.rm=TRUE)*thr/100 )[1] + 100
        }
    }
    ## Statistics, for each relative threshold, of the goodness of fit with computed BGS doys
    BGS_thresholds.est <- data.frame( "diff"=rep(NA,99), "adj.r2"=NA )
    for ( i in 1:99)  {
        BGS_thresholds.est$diff[i] <- mean( BGS_thresholds[,i] - bgs5cm, na.rm=TRUE )
        BGS_thresholds.est$adj.r2[i] <- summary( lm( bgs.ndvi~bgs.5cm, data.frame("bgs.ndvi"=BGS_thresholds[,i],"bgs.5cm"=bgs5cm) ) )$adj.r.squared
    }
    
    ## Choosing the best threshold as the value which minimises the difference with field values
    BGS_threshold_value <- which(BGS_thresholds.est$diff==min(abs(BGS_thresholds.est$diff)))/100  # return the threshold
    attr(BGS_threshold_value,"OD") <- BGS_thresholds.est$diff         # return all the OD values as attribute
    attr(BGS_threshold_value,"adj.r2") <- BGS_thresholds.est$adj.r2   # return all the R² values as attribute
    return(BGS_threshold_value)
    
}

## Computing the value of the threshold 
field.height <- list()  # dataframe with observations of plant heights
field.height[["2012"]] <- read.csv(file.path(fielddir,"field2012_heights.csv"),sep=",",header=TRUE)
    # str(field.height.2012)
    # 'data.frame':	147 obs. of  9 variables:
    #  $ int_id : int                   record ID (serial)
    #  $ text_id: Factor w/ 142 levels  record ID (string)
    #  $ group  : Factor w/ 18 levels   group of the harvest (number of repetition) 
    #  $ doy    : int                   day of the year of the harvest
    #  $ plot   : Factor w/ 19 levels   plot name (matching with plot.2012$plot)
    #  $ h_tot  : num                   Mean height of total biomass (cm)
    #  $ h_green: num                   Mean height of green biomass (cm)
BGS_thr <- BGS_threshold(field=field.height[["2012"]], plots.NDVI.Spline=plots.NDVI.Spline[["2012"]])

## Computing phenological metrics for field plots
for (year in years) {
    plots[[year]]$BGS <- plots[[year]]$Dmax <- plots[[year]]$NDVImax <- NA # one vector for each phenological variable
    for ( plot in rownames(plots[[year]]@data) )  {
        plots[[year]]@data[plot,"BGS"]  <- which( plots.NDVI.Spline[[year]][plot,] > max(plots.NDVI.Spline[[year]][plot,],na.rm=TRUE)*BGS_thr )[1]
        plots[[year]]@data[plot,"Dmax"] <- which( plots.NDVI.Spline[[year]][plot,] == max(plots.NDVI.Spline[[year]][plot,],na.rm=TRUE) )[1]
        plots[[year]]@data[plot,"NDVImax"] <- max(plots.NDVI.Spline[[year]][plot,],na.rm=TRUE)
    }
}
