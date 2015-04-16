
###############################################################################
# Fusion of field dataset
# ----------------------------------------------------------------------------
# 
# Integration of field data (provided as external csv files) with computed 
# measures of NDVI and phenological metrics
# 
# Author: Luigi Ranghetti
###############################################################################


### Import field data ###

field <- list()  # dataframes (one for year) with field and blromatological data (here to be integrated with other data)

for (year in years) {
    
    field[[year]] <- read.csv(file.path(fielddir,paste0("field",year,".csv")),sep=",",header=TRUE) # one csv per year must be present!
    # str(field[[year]])
    # 'data.frame':	142 obs. of  15 variables:
    #  $ int_id : int                   record ID (serial)
    #  $ text_id: Factor w/ 142 levels  record ID (string)
    #  $ group  : Factor w/ 18 levels   group of the harvest (number of repetition) 
    #  $ doy    : int                   day of the year of the harvest
    #  $ plot   : Factor w/ 19 levels   plot name (matching with plot.2012$plot)
    #  $ AB     : num                   Aboveground Biomass (g)
    #  $ H2O    : num                   Water content (ratio between AB and fresh weight)
    #  $ CP     : num                   Crude Protein (% of total weight)
    #  $ NDF    : num                   Neutral Detergent fiber (% of total weight)
    #  $ ADF    : num                   Acid Detergent fiber (% of total weight)
    #  $ ADL    : num                   Lignin (% of total weight)
    #  $ dNDF24 : num                   NDF digestibility after 24 hours (% of NDF)
    #  $ dNDF240: num                   NDF digestibility after 240 hours (% of NDF)
    #  $ h_tot  : num                   Mean height of total biomass (cm) (only 2012)
    #  $ h_green: num                   Mean height of green biomass (cm) (only 2012)
    field[[year]]$plot <- as.character(field[[year]]$plot)
    field[[year]]$aCP <- field[[year]]$AB * field[[year]]$CP  # Available crude protein (g)
    
    ## NDVI integration
    field[[year]]$doy.MOD09Q1 <- field[[year]]$NDVI <- NA
        # variables for respectively doy of MOD09Q1 acquisition and NDVI value
    for (j in 1:nrow(field[[year]])) {
        if ( !is.na(field[[year]][j,"doy"]) )  {
            tmp_doy.composite <- MOD09Q1.doy.composite[[year]][which(MOD09Q1.doy.composite[[year]] > field[[year]][j,"doy"])[1]-1]
            field[[year]][j,"doy.MOD09Q1"] <- plots.doy[[year]]@data[field[[year]][j,"plot"],names(tmp_doy.composite)]
            field[[year]][j,"NDVI"] <- plots.NDVI_corrected[[year]]@data[field[[year]][j,"plot"],names(tmp_doy.composite)]
        }
    }
        
    ## Phenology and morphometry integration
    field[[year]]$elevation <- field[[year]]$aspectNS <- field[[year]]$BGS <- field[[year]]$Dmax <- field[[year]]$NDVImax <- NA
    for (plot in rownames(plots[[year]]@data)) {
        field[[year]]$elevation[which(field[[year]]$plot==plot)] <- plots[[year]]@data[plot,"elevation"]
        field[[year]]$aspectNS[which(field[[year]]$plot==plot)]  <- plots[[year]]@data[plot,"aspectNS"]
        field[[year]]$BGS[which(field[[year]]$plot==plot)]       <- plots[[year]]@data[plot,"BGS"]
        field[[year]]$Dmax[which(field[[year]]$plot==plot)]      <- plots[[year]]@data[plot,"Dmax"]
        field[[year]]$NDVImax[which(field[[year]]$plot==plot)]   <- plots[[year]]@data[plot,"NDVImax"]
    }
    field[[year]]$dos <- field[[year]]$doy - field[[year]]$BGS  # Day Of Season (see text)
    
}