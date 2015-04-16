
###############################################################################
# Calibration and validation of predictive models
# ----------------------------------------------------------------------------
# 
# Calibration of the predictive models about biomass and nutritional variables 
# (2012 dataset) and validation (2012 CV and 2013 datasets)
# 
# Author: Luigi Ranghetti
###############################################################################


## Checking collinearity and choose X_names
X_names <- c('BGS','Dmax','NDVImax','elevation','aspectNS','NDVI','doy') # names of predictors
lm_all <- list() # list of all the possible calibration models
collinearity_matrix <- as.dist(cor(field[["2012"]][,X_names],method='spearman'))
if (any(collinearity_matrix > 0.7)) {
    cat("Some predictors between",paste(X_names,collapse=", "),"are affected by collinearity: check \"collinearity_matrix\" manually and remove them.\n") 
} else cat("No collinearity found between predictors",paste(X_names,collapse=", "),".\n")
# Remove here manually variables with collinearity.
X_names_2 <- c('BGS','NDVImax','aspectNS','NDVI','doy')
collinearity_matrix_2 <- as.dist(cor(field[["2012"]][,X_names_2],method='spearman'))
if (any(collinearity_matrix_2 > 0.7)) {
    cat("Some predictors between",paste(X_names_2,collapse=", "),"are affected by collinearity: check \"collinearity_matrix\" manually and remove them.\n") 
} else cat("No collinearity found between predictors",paste(X_names_2,collapse=", "),".\n")

## Model selection
Y_names <- c('AB','CP','NDF','ADF','ADL','dNDF24','dNDF240','aCP') # names of dependent variables
lm_NDVI<- list() # list of the univariate linear regressions between Y and NDVI
lm_all <- list() # list of all the possible calibration models
lm_aic <- list() # list of AIC tables
lm_cal <- list() # list of chosen calibrarion models
lm_cal_which <- c()

for (Y in Y_names) {
    lm_all[[Y]] <- lm_NDVI[[Y]] <- list()
    # Compute the univariate regression
    lm_NDVI[[Y]] <- lm( get(Y) ~ NDVI, field[["2012"]] )
    # For each Y, the model is chosen between:
    lm_all[[Y]][[1]] <- lm( get(Y) ~ BGS+NDVImax+aspectNS+NDVI*doy, field[["2012"]] )
    lm_all[[Y]][[2]] <- lm( get(Y) ~ BGS+NDVImax+aspectNS+NDVI*(doy+I(doy^2)), field[["2012"]] )
    lm_all[[Y]][[3]] <- lm( get(Y) ~ BGS+NDVImax+aspectNS+NDVI*(dos+I(dos^2)), field[["2012"]] )
    # (doy.gs at first grade is not used, since its AIC is the same of dos
    lm_aic[[Y]] <- AIC( lm_all[[Y]][[1]], lm_all[[Y]][[2]], lm_all[[Y]][[3]] )
    lm_cal_which[Y] <- which(lm_aic[[Y]]$AIC==min(lm_aic[[Y]]$AIC))
    lm_cal[[Y]] <- lm_all[[Y]][[lm_cal_which[Y]]]
}

## Making predictions
# Predicted values (<VarX>.pred) and 95% confidence intevals (<VarX>.pred.lwr and <VarX>.pred.upr) are added to fields[[year]] datasets 
for (Y in Y_names) {
    # 2012 dataset (leave-one-out-CV)
    field[["2012"]][,paste0(Y,c('.pred','.pred.upr','.pred.lwr'))] <- NA
    for ( i in 1:nrow(field[["2012"]]) )  {
        lm_loucv <- lm(formula(lm_cal[[Y]]),field[["2012"]][-i,])
        field[["2012"]][i,paste0(Y,c('.pred','.pred.upr','.pred.lwr'))] <- predict(lm_loucv, field[["2012"]][i,], interval="confidence")
    }
    # 2013 dataset
    field[["2013"]][,paste0(Y,c('.pred','.pred.upr','.pred.lwr'))] <- predict(lm_cal[[Y]], field[["2013"]], interval="confidence")
}


### Create variables with calibration and validation metrics ###

## Descriptive metrics
desc_metrics <- data.frame('mean' = sapply(Y_names, function(Y) { mean(field[["2012"]][[Y]],na.rm=TRUE) }),   # mean Y values
        'sd' = sapply(Y_names, function(Y) { sd(field[["2012"]][[Y]],na.rm=TRUE) }),                          # standard deviation
        'minmax' = sapply(Y_names, function(Y) { diff(range(field[["2012"]][[Y]],na.rm=TRUE)) }) )            # range extent

## Univariate regression metrics
NDVI_metrics <- data.frame( 't' = sapply(lm_NDVI,function(lm) {summary(lm)$coefficients["NDVI","t value"]}),  # t
        'p' = sapply(lm_NDVI,function(lm) {summary(lm)$coefficients["NDVI","Pr(>|t|)"]}),                     # p value
        'R.squared' = sapply(lm_NDVI,function(lm) {summary(lm)$r.squared}),                                   # R²
        'adj.R.squared' = sapply(lm_NDVI,function(lm) {summary(lm)$adj.r.squared}) )                          # Adjusted R²

## Standardised beta coefficients
beta_metrics <-t(sapply(Y_names, function(Y) {lm.beta(lm_cal[[Y]])[if (lm_cal_which[Y]==1) c(1:5,NA,6,NA) else 1:8]} ))

## Calibration metrics
cal_metrics <- data.frame( 'R.squared' = sapply(lm_cal,function(lm) {summary(lm)$r.squared}),                 # R²
        'adj.R.squared' = sapply(lm_cal,function(lm) {summary(lm)$adj.r.squared}),                            # Adjusted R²
        'MSE' = sapply(lm_cal,function(lm) {summary(lm)$sigma}) )                                             # Mean Square Error
cal_metrics[,'NMSE'] = cal_metrics[,'MSE'] / desc_metrics[,'minmax']                                          # Normalised MSE

## Validation metrics
val_metrics <- data.frame( 'RMSE.2012' = sapply(Y_names, function(Y) { sqrt(mean((field[["2012"]][[Y]]-field[["2012"]][[paste0(Y,'.pred')]])^2,na.rm=TRUE)) }),
        'RMSE.2013' = sapply(Y_names, function(Y) { sqrt(mean((field[["2013"]][[Y]]-field[["2013"]][[paste0(Y,'.pred')]])^2,na.rm=TRUE)) }),
        'MAE.2012' = sapply(Y_names, function(Y) { mean(abs(field[["2012"]][[Y]]-field[["2012"]][[paste0(Y,'.pred')]]),na.rm=TRUE) }),
        'MAE.2013' = sapply(Y_names, function(Y) { mean(abs(field[["2013"]][[Y]]-field[["2013"]][[paste0(Y,'.pred')]]),na.rm=TRUE) }),
        'D.2012.mean' = sapply(Y_names, function(Y) { mean(field[["2012"]][[Y]]-field[["2012"]][[paste0(Y,'.pred')]],na.rm=TRUE) }),
        'D.2012.sd' = sapply(Y_names, function(Y) { sd(field[["2012"]][[Y]]-field[["2012"]][[paste0(Y,'.pred')]],na.rm=TRUE) }),
        'D.2013.mean' = sapply(Y_names, function(Y) { mean(field[["2013"]][[Y]]-field[["2013"]][[paste0(Y,'.pred')]],na.rm=TRUE) }),
        'D.2013.sd' = sapply(Y_names, function(Y) { sd(field[["2013"]][[Y]]-field[["2013"]][[paste0(Y,'.pred')]],na.rm=TRUE) }) )
val_metrics[,c('NRMSE.2012','NRMSE.2013','NMAE.2012','NMAE.2013')] <- sweep( val_metrics[,c('RMSE.2012','RMSE.2013','MAE.2012','MAE.2013')], 1, desc_metrics[,'minmax'], '/' )
#  str(val_metrics)
#  'data.frame':	8 obs. of  12 variables:
#  $ RMSE.2012  : Root Mean Squared Error (on 2012 cross validation)
#  $ RMSE.2013  : Root Mean Squared Error (on 2013 validation)
#  $ MAE.2012   : Mean Averaged Error (on 2012 cross validation) 
#  $ MAE.2013   : Mean Averaged Error (on 2013 validation)
#  $ D.2012.mean: Mean difference between true and predicted values (on 2012 cross validation)
#  $ D.2012.sd  : Standard deviation on difference between true and predicted values (on 2013 validation)
#  $ D.2013.mean: Mean difference between true and predicted values (on 2012 cross validation)
#  $ D.2013.sd  : Standard deviation on difference between true and predicted values (on 2013 validation)
#  $ NRMSE.2012 : Normalised RMSE (on 2012 cross validation)
#  $ NRMSE.2013 : Normalised RMSE (on 2013 validation)
#  $ NMAE.2012  : Normalised MAE (on 2012 cross validation)
#  $ NMAE.2013  : Normalised MAE (on 2013 validation)

## Export metrics as csv
write.csv(desc_metrics,file.path(outdir,'Descriptive_metrics.csv'))
write.csv(NDVI_metrics,file.path(outdir,'Univariate_NDVI_metrics.csv'))
write.csv(beta_metrics,file.path(outdir,'Beta_values.csv'))
write.csv(cal_metrics,file.path(outdir,'Calibration_metrics.csv'))
write.csv(val_metrics,file.path(outdir,'Validation_metrics.csv'))

## Save workspace
save.image(file.path(outdir,'LR_EstGrass.RData'))
