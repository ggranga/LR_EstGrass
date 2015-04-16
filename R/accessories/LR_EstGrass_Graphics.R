
###############################################################################
# Graphics and Sweave compiling
# ----------------------------------------------------------------------------
# 
# Script used to compile the Rtex of the paper and to make graphics.
# 
# Author: Luigi Ranghetti
###############################################################################

library(lattice)
library(tikzDevice)
library(QuantPsyc)
library(xtable)

main_wd  <- path.expand("~/phd/analisi/paper1_new_scripts") # Set here the path of the main directory
graphdir <- file.path(main_wd,"latex/graphics")             # Directory where graphics have to be put
source(file.path(main_wd,"R","LR_EstGrass_Main.R"))         # Charge the required variables


## graphic: fig:bgs
tikz(file.path(graphdir,"bgs.tex"),5,3.5)
    par(mfrow=c(2,1),mar=c(2.25,3.25,0,0)+0.1,mgp=c(2.25,1,0))
    plot(attr(BGS_thr,"OD"),pch=3,ylab="$\\textup{OD}_t$",xlab="$t$"); abline( h=0, v=BGS_thr*100, lty=2 )
    plot(attr(BGS_thr,"adj.r2"),pch=3,ylab="$\\textup{adj-R}^2_t$",xlab="$t$"); abline( v=BGS_thr*100, lty=2 )
dev.off()


## graphics: fig:vartrend1-8

l<-r<-t<-b<-0 # lattice settings
options( tikzLatexPackages = c(
                "\\usepackage{tikz}",
                "\\usepackage[active,tightpage]{preview}",
                "\\PreviewEnvironment{pgfpicture}",
                "\\setlength\\PreviewBorder{0pt}" ))
Sys.setlocale("LC_TIME", "en_GB.UTF-8") # ensure the output to be in English

Y_labels = c('Aboveground biomass (AB)','Crude protein (CP)','Neutral detergent fiber (NDF)','Acid detergent fiber (ADF)',
        'Lignin (ADL)','NDF dig.\\ at 24\\,h (dNDF24)','NDF dig.\\ at 240\\,h (dNDF240)','Available crude protein (aCP)')
names(Y_labels) <- Y_names
#for (Y in Y_names) {
#    tikz(file.path(graphdir,paste0("trend2012-",Y,".tex")),8,5.5)
#        trellis.par.set( layout.widths = list( left.padding=l, right.padding=r ),
#                layout.heights = list( bottom.padding=b, top.padding=t+1 ),
#                strip.background=list(col="white") )
#        xyplot( get(Y) ~ doy | plot, field[["2012"]], type="b", cex=0.2,
##                main=Y_labels[Y],
#                xlab="", ylab="",
#                panel=function(x,y,subscripts) {
#                    panel.xyplot(x,y, type="b", col="black", cex=0.2)
#                    panel.xyplot(field[["2012"]][['doy']][subscripts],field[["2012"]][[paste0(Y,".pred")]][subscripts], type="b", col="black", lty=2, cex=0.2)
#                    panel.xyplot(field[["2012"]][['doy']][subscripts],field[["2012"]][[paste0(Y,".pred.upr")]][subscripts], type="l", col="black", lty=3 )
#                    panel.xyplot(field[["2012"]][['doy']][subscripts],field[["2012"]][[paste0(Y,".pred.lwr")]][subscripts], type="l", col="black", lty=3 )
#                }, as.table=T )
#    dev.off()
#}


### graphic: fig:modelval
#tikz(file.path(graphdir,"validation.tex"),4.5,8)
#    par(mfrow=c(4,2),mar=c(3.25,3.25,2.25,1)+0.1,mgp=c(2.25,1,0))
#    for (Y in Y_names) {
#        plot(get(paste0(Y,".pred"))~get(Y), field[["2012"]], pch=3, xlab="Measured values", ylab="Predicted values", main=Y_labels[Y])
#        points(get(paste0(Y,".pred"))~get(Y), field[["2013"]], pch=4, cex=0.75, col="blue")
#        abline(0,1,lty=3)
#    }
#dev.off()


# format values of tab:models0
NDVI_metrics_formatted <- data.frame( "t" = format(NDVI_metrics[,"t"],dig=2),
        "p" = ifelse(NDVI_metrics[,"p"]<0.001, "< 0.001", format(NDVI_metrics[,"p"],dig=0,nsmall=3,scientific=FALSE)),
        "adj.R.squared" = format(NDVI_metrics[,"adj.R.squared"],dig=0,nsmall=2) )
rownames(NDVI_metrics_formatted) <- rownames(NDVI_metrics)

# format values of tab:modelcal
cal_metrics_notformatted <- cbind(lm_cal_which,beta_metrics,cal_metrics[,c("MSE","NMSE","adj.R.squared")])
  # in this case, since all the columns have the same formattation (1.2) this is directly done with xtable;
  # in this way there is no need to replace "NA".

# format values of tab:modelval
val_metrics_formatted <- cbind( format(val_metrics[,c("RMSE.2012","RMSE.2013","MAE.2012","MAE.2013","NRMSE.2012","NRMSE.2013","NMAE.2012","NMAE.2013")],dig=0,nsmall=2),
        "D.2012"=apply(format(val_metrics[,c("D.2012.mean","D.2012.sd")],dig=0,nsmall=4),1,paste,collapse=" \\pm "),
        "D.2013"=apply(format(val_metrics[,c("D.2013.mean","D.2013.sd")],dig=0,nsmall=4),1,paste,collapse=" \\pm ") )


# compile Sweave chunks
#setwd("~/phd/tesi/1_correlazioneNDVIpraterie")
#Sweave("1_titlepage.Rtex", encoding="utf8") # FIXME replace with definitive latex file
#Sweave("1_document.Rtex", encoding="utf8")  # FIXME idem
setwd("~/phd/tesi/1_correlazioneNDVIpraterie/remotesensing_submission")
Sweave("Estimation_of_nutritional_properties_of_alpine_grassland_from_MODIS_NDVI_data.Rtex", encoding="utf8")
setwd(main_wd)
