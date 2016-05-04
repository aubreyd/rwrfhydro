
#' Plot time series comparing modeled and observed fluxes
#' 
#' \code{PlotFluxCompare} plots a time series of an observed flux (e.g.,
#' streamflow, ET) and up to 2 modelled fluxes.
#' 
#' \code{PlotFluxCompare} reads modelled and observed dataframes (e.g., as
#' generated from \code{\link{ReadFrxstPts}} and \code{\link{ReadUsgsGage}}) and
#' plot the time series and summary statistics. The tool will subset data to
#' matching time periods (e.g., if the observed data is at 5-min increments and
#' modelled data is at 1-hr increments, the tool will subset the observed data
#' to select only observations on the matching hour break).
#' 
#' @param strDf.obs The OBSERVED flux time series dataframe (e.g., output from
#'   \code{\link{ReadUsgsGage}}). The dataframe must contain a column of flux
#'   values and a POSIXct or Date column.
#' @param strCol.obs The name of the column containing the flux values for the
#'   OBSERVED dataframe (DEFAULT="q_cms").
#' @param strDf.mod1 The FIRST MODEL flux time series dataframe (e.g., output
#'   from \code{\link{ReadFrxstPts}}). The dataframe must contain a column of
#'   flux values and a POSIXct or Date column.
#' @param strCol.mod1 The name of the column containing the FIRST MODEL flux
#'   values (DEFAULT="q_cms").
#' @param strDf.mod2 The SECOND MODEL flux time series dataframe (e.g., output
#'   from \code{\link{ReadFrxstPts}}). The dataframe must contain a column of
#'   flux values and a POSIXct or Date column.
#' @param strCol.mod2 The name of the column containing the SECOND MODEL flux
#'   values (DEFAULT="q_cms").
#' @param stdate Start date for plot/statistics (DEFAULT=NULL, all records will
#'   be used). Date MUST be specified in POSIXct format with appropriate
#'   timezone (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d
#'   \%H:\%M:\%S", tz="UTC")) or Date (e.g., as.Date("2013-05-01", 
#'   format="\%Y-\%m-\%d")) where Date is assumed to match a UTC date.
#' @param enddate End date for plot/statistics (DEFAULT=NULL, all records will
#'   be used). Date MUST be specified in POSIXct format with appropriate
#'   timezone (e.g., as.POSIXct("2013-05-01 00:00:00", format="\%Y-\%m-\%d
#'   \%H:\%M:\%S", tz="UTC")) or Date (e.g., as.Date("2013-05-01", 
#'   format="\%Y-\%m-\%d")) where Date is assumed to match a UTC date.
#' @param logy (TRUE or FALSE) Optional flag to set the y-axis to log-scale
#'   (DEFAULT=FALSE).
#' @param labelObs Optional label for the observed streamflow
#'   (DEFAULT="Observed")
#' @param labelMod1 Optional label for the FIRST MODEL (DEFAULT="Model 1")
#' @param labelMod2 Optional label for the SECOND MODEL (DEFAULT="Model 2")
#' @param title Optional for the plot (DEFAULT="Observed and Modelled Fluxes")
#' @param colorObs Optional color for the observed line (DEFAULT="black")
#' @param colorMod1 Optional color for the FIRST MODEL line (DEFAULT="green2")
#' @param colorMod2 Optional color for the FIRST MODEL line (DEFAULT="blue")
#' @return A plot of the hydrographs.
#'   
#' @examples
#' ## Take a time series of observed 5-minute streamflow values for Fourmile
#' ## Creek (obsStr5min.fc) and two model runs (mod1Str1h.fc, mod2Str1h.fc), 
#' ## all with streamflow columns named "q_cms", and plot the the hydrographs 
#' ## for all three over the May-June snowmelt period.
#' 
#' \dontrun{
#' PlotFluxCompare(obsStr5min.fc, "q_cms", modStrh.chrt.fc, "q_cms", 
#'                 strDf.mod2=modStrh.allrt.fc, strCol.mod2="q_cms",
#'                 labelObs="Observed Fourmile Creek at Orodell",
#'                 labelMod1="Channel Routing Only", labelMod2="All Routing",
#'                 title="Streamflow: Fourmile Creek",
#'                 stdate=as.POSIXct("2013-05-01 00:00:00", 
#'                                   format="%Y-%m-%d %H:%M:%S", tz="UTC"),
#'                 enddate=as.POSIXct("2013-06-30 00:00:00",
#'                                    format="%Y-%m-%d %H:%M:%S", tz="UTC"))
#' }
#' @export

PlotFluxCompare <- function(strDf.obs, strCol.obs="q_cms",
                            strDf.mod1, strCol.mod1="q_cms",
                            strDf.mod2=NULL, strCol.mod2="q_cms",
                            stdate=NULL, enddate=NULL, logy=FALSE,
                            labelObs="Observed", labelMod1="Model 1", labelMod2="Model 2",
                            title="Observed and Modelled Fluxes",
                            colorObs="black", colorMod1="green2", colorMod2="blue") {
    # PREP DATA
    if ("POSIXct" %in% names(strDf.obs) & "POSIXct" %in% names(strDf.mod1)) {
      dateCol <- "POSIXct"
    } else if ("Date" %in% names(strDf.obs) & "Date" %in% names(strDf.mod1)) {
      dateCol <- "Date"
    } else {
      stop("No time/date column (checked: POSIXct or Date). Exiting.")
    }
    if (!is.null(stdate) && !is.null(enddate)) {
        strDf.obs <- subset(strDf.obs, strDf.obs[,dateCol]>=stdate & strDf.obs[,dateCol]<=enddate)
        strDf.mod1 <- subset(strDf.mod1, strDf.mod1[,dateCol]>=stdate & strDf.mod1[,dateCol]<=enddate)
        if (!is.null(strDf.mod2)) {
            strDf.mod2 <- subset(strDf.mod2, strDf.mod2[,dateCol]>=stdate & strDf.mod2[,dateCol]<=enddate)
            }
        ttext <- paste0(title, " (", stdate, " to ", enddate, ")")
        }
    else {
        ttext <- title
        }
    strDf.obs$qcomp.obs <- strDf.obs[,strCol.obs]
    strDf.mod1$qcomp.mod1 <- strDf.mod1[,strCol.mod1]
    if (!is.null(strDf.mod2)) {
        strDf.mod2$qcomp.mod2 <- strDf.mod2[,strCol.mod2]
    }
    strDf <- merge(strDf.obs[c(dateCol,"qcomp.obs")], strDf.mod1[c(dateCol,"qcomp.mod1")], by=c(dateCol))
    if (!is.null(strDf.mod2)) {
        strDf <- merge(strDf, strDf.mod2[c(dateCol,"qcomp.mod2")], by<-c(dateCol))
        }
    # STATS
    nseflow1 <- round(Nse(strDf$qcomp.mod1, strDf$qcomp.obs), 2)
    biasflow1 <- round(sum(strDf$qcomp.mod1-strDf$qcomp.obs, na.rm=TRUE)/sum(strDf$qcomp.obs, na.rm=TRUE) * 100, 1)
    maxflow <- max(max(strDf$qcomp.obs, na.rm=TRUE), max(strDf$qcomp.mod1, na.rm=TRUE))
    minflow <- min(min(strDf$qcomp.obs, na.rm=TRUE), min(strDf$qcomp.mod1, na.rm=TRUE))
    if (!is.null(strDf.mod2)) {
        maxflow <- max(maxflow, max(strDf$qcomp.mod2, na.rm=TRUE))
        minflow <- min(minflow, min(strDf$qcomp.mod2, na.rm=TRUE))
        nseflow2 <- round(Nse(strDf$qcomp.mod2, strDf$qcomp.obs), 2)
        biasflow2 <- round(sum(strDf$qcomp.mod2-strDf$qcomp.obs, na.rm=TRUE)/sum(strDf$qcomp.obs, na.rm=TRUE) * 100, 1)
        }
    # PLOT
    if (logy) {
        plot(strDf[,dateCol], log10(strDf$qcomp.mod1), typ='l', log='y', col=colorMod1, ylab=paste0(strCol.mod1),
                                xlab=dateCol, main=ttext, ylim=c(minflow,maxflow))
        }
    else {
        plot(strDf[,dateCol], strDf$qcomp.mod1, typ='l', col=colorMod1, ylab=paste0(strCol.mod1),
                                xlab=dateCol, main=ttext, ylim=c(minflow,maxflow))
        }
    if (!is.null(strDf.mod2)) { lines(strDf[,dateCol], strDf$qcomp.mod2, col=colorMod2) }
    lines(strDf[,dateCol], strDf$qcomp.obs, col=colorObs)
    if (!is.null(strDf.mod2)) {
        legend('topright', c(labelMod1, labelMod2, labelObs), col=c(colorMod1,colorMod2,colorObs), lty=c(1,1,1), bg="white")
        mtext(c(paste0("MODEL1: NSE=", nseflow1, " Bias=", biasflow1, "%  MODEL2: NSE=", nseflow2, " Bias=", biasflow2, "%")), side=3, line=0.0, cex=0.9)
        }
    else {
        legend('topright', c(labelMod1, labelObs), col=c(colorMod1,colorObs), lty=c(1,1), bg="white")
        mtext(c(paste0("MODEL: NSE=", nseflow1, " Bias=", biasflow1, "%")), side=3, line=0.0, cex=0.9)
        }
}



#' Plot water balance from WRF-Hydro (w/NoahMP) output
#' 
#' \code{PlotWatBudg} plot water budget components from WRF-Hydro (w/NoahMP)
#' model output.
#' 
#' Read water budget dataframe (as generated from
#' \code{\link{CalcNoahmpWatBudg}}) and plot water budget components as a
#' piechart or barchart. NOTE: Currently only works for runs using NoahMP as the
#' LSM.
#' 
#' @param wbDf The water budget dataframe (required)
#' @param plottyp The plot type (pie or bar) (default=pie)
#' @return A plot of the water budget components in mm.
#'   
#' @examples
#' ## Plot the water budget components from a water budget dataframe generated 
#' ## using CalcNoahmpWatBudg. Plot as a piechart.
#' 
#' \dontrun{
#' PlotWatBudg(wb.allrt.fc)
#' 
#' ## Plot the same as a barchart.
#' 
#' PlotWatBudg(wb.allrt.fc, "bar")
#' }
#' @export

PlotWatBudg <- function(wbDf, plottyp="pie") {

    lbls <- c("Canopy Evap", "Transpiration", "Surface Evap", "Surface Runoff",
             "Groundwater Outflow")
    pcts <- with(wbDf,c(LSM_ECAN/LSM_PRCP*100, LSM_ETRAN/LSM_PRCP*100, LSM_EDIR/LSM_PRCP*100,
                  (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)) / LSM_PRCP * 100,
                  WB_GWOUT/LSM_PRCP*100))
    lbls_pcts=c()
    for (i in 1:length(lbls)) { lbls_pcts[i] <- paste0(lbls[i], "\n", round(pcts[i],1), "%") }
    if (plottyp == "pie") {
        if (wbDf$STOR_FRAC > 0) {
            lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in\nStorage", "\n",
                                                round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                                ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                                ifelse(is.na(WB_DELGWSTOR), 0.0, WB_DELGWSTOR)) / LSM_PRCP * 100), 1), "%")
            pie(as.matrix(with(wbDf, c(LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT, LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(WB_DELGWSTOR), 0.0, WB_DELGWSTOR)))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue","grey30"),
                main=c("Water Budget"), labels=lbls_pcts)
            }
        else {
            pie(as.matrix(with(wbDf, c(LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT))),
                col=c("chartreuse3","darkgreen","darkgoldenrod2","cornflowerblue","darkblue"),
                main=c("Water Budget"), labels=lbls_pcts)
            text(0,-1, paste0("*Storage Loss: ",
                        round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(WB_DELGWSTOR), 0.0, WB_DELGWSTOR)) /
                                        LSM_PRCP * 100), 1),"%"))
            } # end storage fraction split
        } # end pie
    else if (plottyp =="bar") {
        lbls_pcts[length(lbls_pcts)+1] <- paste0("Change in Storage", "\n",
                                round( with( wbDf, (LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(WB_DELGWSTOR), 0.0, WB_DELGWSTOR)) /
                                        LSM_PRCP * 100), 1), "%")
        plotDf <- with(wbDf,c(LSM_DELSOILM + LSM_DELSWE + LSM_DELCANWAT +
                                        ifelse(is.na(HYD_DELSFCHEAD), 0.0, HYD_DELSFCHEAD) +
                                        ifelse(is.na(WB_DELGWSTOR), 0.0, WB_DELGWSTOR),
                                        LSM_ECAN, LSM_ETRAN, LSM_EDIR,
                                        (WB_SFCRNOFF + ifelse(is.na(HYD_QBDRY), 0.0, HYD_QBDRY)),
                                        WB_GWOUT))
        plotDf1 <- abs(plotDf)
        ylabs <- round(c(0,cumsum(plotDf1))-((plotDf1[1]-plotDf[1])/2),0)
        par(mar = c(5.1, 4.1, 5.1, 12.1), xpd = TRUE)
        barplot(as.matrix(plotDf1), axes=FALSE,
            col=c("grey70", "chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue"),
            main=c("Water Budget"), xlim=c(0,1), width=0.6, space=0.2, ylab=c("Total Water (mm)"))
        axis(2,c(0,cumsum(plotDf1)),labels=ylabs)
        if (plotDf[1]>=0) { segments(0.0, 0.0, 1.0, 0.0, lty=2) } else { segments(0.0, cumsum(plotDf1)[1], 1.0, cumsum(plotDf1)[1], lty=2) }
        legend("topright", legend=lbls_pcts,fill=c("chartreuse", "darkgreen", "orange", "cornflowerblue", "darkblue","grey70"),
            inset=c(-0.5, 0), bg=c("white"), yjust=0.5, y.intersp=2)
        } # end bar
}

#' Plot point NetCDF station time series observations
#'
#' \code{PlotStationData} plots station observations from list containing data frame of time series
#'
#' \code{PlotStationData} plots station observation time series for all recorded variables
#' 
#'
#' @param stnObs.list Full list of data output from ReadNearStationData, including time series data, list of units and filename
#' @param stack (default=FALSE) if TRUE, instead of individual plots to click through, the output will be one page of stacked plots
#'
#' @return Plot comparison of model and station data
#'
#' @examples
#' ## Plot Station Observations at a given station
#' ## (Upper Canejos River, URG1)
#' PlotStationData(URG1)
#'
#' @keywords IO
#' @concept dataGet
#' @family obsDataReads
#' @export
PlotStationData <- function(stnObs.list,stack=FALSE){
  
  stnObs.df <- stnObs.list[[1]]
  
  stnObs.df[stnObs.df < -9999] <- NA ##remove -9999+ values
  stnObs.df <- stnObs.df[ ,colSums(is.na(stnObs.df))<nrow(stnObs.df)] ##remove columns w/ all NA values
  numplots <- length(stnObs.df) - 6
  
  ynames <- colnames(stnObs.df)
  
  yind <- match(ynames,stnObs.list[[2]]$var)
  
  if(!stack){
    par(ask=TRUE)
    for(i in 1:numplots){
      ylab <- paste(ynames[i]," ","(",stnObs.list[[2]]$unit[yind[i]],")",sep="")
      plot(stnObs.df$POSIXct,stnObs.df[,i],type='l',xlab="Date",ylab=ylab,main=stnObs.list[[3]],lwd=2,col="blue")}
  }
  
  if(stack){
    par(ask=FALSE)
    par(oma=c(2,2,5,2))
    par(mar=c(0,6,0,1))
    ifelse(numplots %% 2 == 0,layout(matrix(1:(numplots+2),ncol=2)),layout(matrix(1:(numplots+1),ncol=2)))
    ylab <- paste(ynames[1]," ","(",stnObs.list[[2]]$unit[yind[1]],")",sep="")
    plot(stnObs.df$POSIXct,stnObs.df[,1],type='l',xlab='POSIXct',ylab=ylab,col="blue")  ##xact='n' if want to remove
    mtext(stnObs.list[[3]],side=3,line=3)
    par(mar=c(0,6,0,1))
    for(i in 2:(numplots-1)){
      ylab <- paste(ynames[i]," ","(",stnObs.list[[2]]$unit[yind[i]],")",sep="")
      plot(stnObs.df$POSIXct,stnObs.df[,i],type='l',xlab="POSIXct",ylab=ylab,col="blue")
    }
    ylab <- paste(ynames[i+1]," ","(",stnObs.list[[2]]$unit[yind[i+1]],")",sep="")
    plot(stnObs.df$POSIXct,stnObs.df[,i+1],type='l',xlab="POSIXct",ylab=ylab,col="blue")
  }
  
}


#'Plot modeled and observed temperature, SW radiation, snow depth and precipitation (if available)
#'
#'\code{PlotCustomModelStations} Plots both met. station data and model output for comparsion
#'
#'@param MetStn observed data from meteorological station
#'@param CustomModel Custom WRF-Hydro model output with 1-6 Upper Rio Grande stations
#'@param stn_no Number 1-6 in reference to the specific URG station
#'@param stack FALSE is default, TRUE for all plots on one page
#'
#'@return nothing
#'
#'@example PlotCusstomModelStations(URG3,LDASout,3) ##plot of Platoro Cabin temperature, solar radiation, snow depth and precipitation
#'@example PlotCustomModelStations(URG3,LDASout,3,TRUE) ##all plots on one page

PlotCustomModelStations <- function(MetStn,CustomModel,stn_no,stack=FALSE){
  
  stn_names <- c("Upper Conejos above Platoro","Red Mountain","Platoro Cabin","Nathan's Cabin","Forest King", "Rocky Mountain Estates")
  file_names <- c("Upper_Conejos_abv_Platoro","Red_Mountain","Platoro_cabin","Nathans_Cabin","Forest_King","Rcky_Mtn_Estates_estim")
  stn <- stn_names[stn_no]

  ##convert 15min station to hourly
  MetStn[[1]]$Hour <- as.character(trunc(as.POSIXct(format(MetStn[[1]]$POSIXct, tz="UTC"), tz="UTC"), "hours"))
  MetStn[[1]]$POSIXct <- NULL ##get rid of POXIXct because R won't handle it
  MetStn_hr <- plyr::ddply(MetStn[[1]],plyr::.(Hour),
                           plyr::summarise,
                           RH_mean=mean(relative_humidity, na.rm=TRUE),
                           T_mean=mean(temperature, na.rm=TRUE),
                           Wind_mean=mean(wind, na.rm=TRUE),
                           WindDir_mean=mean(wdirection, na.rm=TRUE),
                           SoilTemp1_mean=mean(Soil_Temp1, na.rm=TRUE),
                           SM1_mean=mean(Soil_moist1, na.rm=TRUE),
                           SoilTemp2_mean=mean(Soil_Temp2, na.rm=TRUE),
                           SM2_mean=mean(Soil_moist2, na.rm=TRUE),
                           SnoDep_mean=mean(snow_depth, na.rm=TRUE),
                           SWRad_mean=mean(shortwave_radiation, na.rm=TRUE),
                           LeafWet_mean=mean(Leaf_Wetness, na.rm=TRUE),
                           ##PrecAcc_max=max(Total_Accumulated_Precipitation, na.rm=TRUE),
                           ##PrecTot=sum(Prec_mm, na.rm=TRUE),
                           ##PrecInt_mean=mean(Precipitation_Intensity, na.rm=TRUE),
                           ##SurfPress_mean=mean(Surface_Pressure, na.rm=TRUE),
                           .parallel=FALSE)
  if("Total_Accumulated_Precipitation" %in% colnames(MetStn[[1]])){
    hrlyprcp <- plyr::ddply(MetStn[[1]],plyr::.(Hour),
                            plyr::summarise,
                            PrecAcc_max=max(Total_Accumulated_Precipitation, na.rm=TRUE),
                            .parallel=FALSE)
    
      MetStn_hr <- cbind(MetStn_hr,hrlyprcp)
    }
  
  MetStn_hr$Hour <- as.POSIXct(MetStn_hr$Hour,tz="UTC",format="%F %R") ##make back to POSIX to actually plot
  
  if(!stack){
    ##temperature
    par(ask=TRUE)
    CustomModel[[1]]$mod_K <- CustomModel[[1]]$T2MV - 273.15
    plot(MetStn_hr$Hour,MetStn_hr$T_mean,
         lwd=3,type="l",col="blue",xlab="Date",ylab="Temperature (deg C)",main=stn,ylim=c(-30,30),sub="Water Year 2015")
    lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$mod_K[which(CustomModel[[1]]$stn == stn_no)],
          type="l",col="red",lwd=2)
    legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
    
    ##solar radaiation
    MetStn_hr$SWRad_mean[MetStn_hr$SWRad_mean < -9999] <- NA  ###gets rid of large negatives in place of missing values
    plot(MetStn_hr$Hour,MetStn_hr$SWRad_mean,
         lwd=3,type="l",col="blue",xlab="Date",ylab="Solar Radiation (W m-2)",main=stn,ylim=c(0,1300),sub="Water Year 2015")
    lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$FSA[which(CustomModel[[1]]$stn == stn_no)],
          type="l",col="red",lwd=2)
    legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))

    ##snow depth
    CustomModel[[1]]$Dsnow_1000 <- CustomModel[[1]]$SNOWH * 1000
    plot(MetStn_hr$Hour,MetStn_hr$SnoDep_mean,
         lwd=3,type="l",col="blue",xlab="Date",ylab="Snow Depth (mm)",main=stn,ylim=c(0,1400),sub="Water Year 2015")
    lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$Dsnow_1000[which(CustomModel[[1]]$stn == stn_no)],
          type="l",col="red",lwd=2)
    legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))

    ##precipitation
    if(stn_no == 3 | stn_no == 6){
      start_t <- MetStn_hr$Hour[1]
      cm_subp <- cumsum(CustomModel[[1]]$RAINRATE[which(CustomModel[[1]]$stn == stn_no & CustomModel[[1]]$POSIXct >= start_t)])
      cm_subt <- CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no & CustomModel[[1]]$POSIXct >= start_t)]
      plot(MetStn_hr$Hour,MetStn_hr$PrecAcc_max,
           lwd=3,type="l",col="blue",xlab="Date",ylab="Accumulated Precipitation (mm)",main=stn,ylim=c(0,625),sub="Water Year 2015")
      lines(cm_subt,cm_subp,type="l",col="red",lwd=2)
      legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
      }
  }
  
  if(stack){
    par(ask=FALSE,oma=c(2,2,5,2))
        par(mar=c(0,6,0,1))
        layout(matrix(1:4,ncol=2))
               CustomModel[[1]]$mod_K <- CustomModel[[1]]$T2MV - 273.15
               plot(MetStn_hr$Hour,MetStn_hr$T_mean,
                    lwd=3,type="l",col="blue",xlab="Date",ylab="Temperature (deg C)",ylim=c(-30,30),sub="Water Year 2015")
               lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$mod_K[which(CustomModel[[1]]$stn == stn_no)],
                     type="l",col="red",lwd=2)
               legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
               mtext(stn,side=3,line=3)
               
        
               ##solar radaiation
               MetStn_hr$SWRad_mean[MetStn_hr$SWRad_mean < -9999] <- NA  ###gets rid of large negatives in place of missing values
               plot(MetStn_hr$Hour,MetStn_hr$SWRad_mean,
                    lwd=3,type="l",col="blue",xlab="Date",ylab="Solar Radiation (W m-2)",ylim=c(0,1300),sub="Water Year 2015")
               lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$FSA[which(CustomModel[[1]]$stn == stn_no)],
                     type="l",col="red",lwd=2)
               legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
               
               ##snow depth
               CustomModel[[1]]$Dsnow_1000 <- CustomModel[[1]]$SNOWH * 1000
               plot(MetStn_hr$Hour,MetStn_hr$SnoDep_mean,
                    lwd=3,type="l",col="blue",xlab="Date",ylab="Snow Depth (mm)",ylim=c(0,1400),sub="Water Year 2015")
               lines(CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no)],CustomModel[[1]]$Dsnow_1000[which(CustomModel[[1]]$stn == stn_no)],
                     type="l",col="red",lwd=2)
               legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
               
               ##precipitation
               if(stn_no == 3 | stn_no == 6){
                 start_t <- MetStn_hr$Hour[1]
                 cm_subp <- cumsum(CustomModel[[1]]$RAINRATE[which(CustomModel[[1]]$stn == stn_no & CustomModel[[1]]$POSIXct >= start_t)])
                 cm_subt <- CustomModel[[1]]$POSIXct[which(CustomModel[[1]]$stn == stn_no & CustomModel[[1]]$POSIXct >= start_t)]
                 plot(MetStn_hr$Hour,MetStn_hr$PrecAcc_max,
                      lwd=3,type="l",col="blue",xlab="Date",ylab="Accumulated Precipitation (mm)",ylim=c(0,625),sub="Water Year 2015")
                 lines(cm_subt,cm_subp,type="l",col="red",lwd=2)
                 legend("topleft",legend=c("Station","Model"),col=c("blue","red"),lty=c(1,1))
               }
  
    }
}

