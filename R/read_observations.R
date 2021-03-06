#' Read standard-format text data table downloaded from USGS Water Data
#'
#' \code{ReadUsgsGage} reads USGS data table (streamflow and/or stage) and puts into a dataframe.
#'
#' \code{ReadUsgsGage} reads a streamflow or stage time series data table (standard USGS Water
#' Data format) and outputs a dataframe with consistent date and data columns for use with other
#' rwrfhydro tools.
#' 
#' @param pathGageData The full pathname to the streamflow/stage time series text file as
#' downloaded from USGS Water Data. The file should include the standard USGS header info and
#' the data time series with standard USGS columns.
#' @param returnEnglish Logical: Return variables in english units (cfs and ft)?
#' @param returnMetric  Logical: Return variables in these units (cms and m)?
#' @param timeZone  (OPTIONAL) The time zone for the USGS gage data. Only necessary if tz code
#' is NOT provided in the input file (e.g., daily streamflow gage data files).
#' Time zone name must be R-friendly for your current OS. See: \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}
#' @return A dataframe containing the USGS flow and/or stage data.
#'
#' @examples
#' ## Take a text file downloaded from the USGS Water Data website for 5-minute flow at Fourmile Creek
#' ## and create a dataframe called "obsStr5min.fc".
#'
#' obsStr5min.fc <- ReadUsgsGage("../OBS/STRFLOW/5min_str_06727500_110401_140810.txt")
#' obsStr5min.fc <- ReadUsgsGage("../OBS/STRFLOW/5min_str_06727500_110401_140810.txt, returnEnglish=FALSE")
#' @keywords IO
#' @concept aconcept
#' @export
ReadUsgsGage <- function(pathGageData, returnMetric=TRUE, returnEnglish=TRUE, timeZone=NULL) {

  outDf <- read.table(pathGageData, sep="\t", na.strings=c("","Rat","Mnt"),
                      stringsAsFactors=F, comment.char="#")
  outDf.head <- outDf[1,]
  outDf <- outDf[3:nrow(outDf),]

  cdlist <- list("00060"="q_cfs",
                 "00061"="qinst_cfs",
                 "00062"="reslev_ft",
                 "00064"="strlev_ft",
                 "00065"="ht_ft",
                 "00060_00003"="qmean_cfs",
                 "00060_00001"="qmax_cfs",
                 "00060_00002"="qmin_cfs",
                 "00065_00003"="htmean_ft",
                 "00065_00001"="htmax_ft",
                 "00065_00002"="htmin_ft")

  for (i in names(cdlist)) { # if grep dosent match, the zero index is set.
    indx <- grep(paste(i,"$",sep=""),outDf.head)
    outDf.head[indx] <- cdlist[[i]]
    indx <- grep(paste(i,"_cd",sep=""),outDf.head)
    outDf.head[indx] <- paste(cdlist[[i]],"_cd",sep="")
  }
  colnames(outDf) <- as.matrix(outDf.head)
  for (i in names(cdlist)) {
    if (cdlist[[i]] %in% colnames(outDf)) {
      outDf[,cdlist[[i]]] <- as.numeric(outDf[,cdlist[[i]]])
    }
  }
  
  if (is.null(timeZone)) {
    timeZone <- TransTz(outDf$tz_cd)
    # Since a gauge shouldnt move, there should only be one time zone
    if(!all(timeZone == timeZone[1]))
      warning(paste0("There should only be one timezone for a given gauge, ",
                   "please check the file: ",pathGageData))
    timeZone <- timeZone[1]
  }
  # instantaneous vs daily average values
  usgsTimeFmt <- if (grepl(" ",outDf$datetime[1])) {
     "%Y-%m-%d %H:%M" # instantaneous
  } else { "%Y-%m-%d" }  # daily avg

  outDf$POSIXct <- as.POSIXct(as.character(outDf$datetime), format=usgsTimeFmt, tz=timeZone)

  outDf$wy <- CalcWaterYear(outDf$POSIXct)
  #outDf$wy <- ifelse(as.numeric(format(outDf$POSIXct,"%m"))>=10,
  #                   as.numeric(format(outDf$POSIXct,"%Y"))+1,
  #                   as.numeric(format(outDf$POSIXct,"%Y")))

  if (returnMetric) {
    ftToM <- 0.3048
    if ("q_cfs"     %in% colnames(outDf))      outDf$q_cms  <- outDf$q_cfs*(ftToM^3)
    if ("qinst_cfs" %in% colnames(outDf))  outDf$qinst_cms  <- outDf$qinst_cfs*(ftToM^3)
    if ("qmean_cfs" %in% colnames(outDf))  outDf$qmean_cms  <- outDf$qmean_cfs*(ftToM^3)
    if ("qmin_cfs"  %in% colnames(outDf))   outDf$qmin_cms  <- outDf$qmin_cfs*(ftToM^3)
    if ("qmax_cfs"  %in% colnames(outDf))   outDf$qmax_cms  <- outDf$qmax_cfs*(ftToM^3)
    if ("reslev_ft" %in% colnames(outDf))   outDf$reslev_m  <- outDf$reslev_ft*(ftToM)
    if ("strlev_ft" %in% colnames(outDf))   outDf$strlev_m  <- outDf$strlev_ft*(ftToM)
    if ("ht_ft"     %in% colnames(outDf))       outDf$ht_m  <- outDf$ht_ft*(ftToM)
    if ("htmean_ft" %in% colnames(outDf))   outDf$htmean_m  <- outDf$htmean_ft*(ftToM)
    if ("htmin_ft"  %in% colnames(outDf))    outDf$htmin_m  <- outDf$htmin_ft*(ftToM)
    if ("htmax_ft"  %in% colnames(outDf))    outDf$htmax_m  <- outDf$htmax_ft*(ftToM)
  }

  ## dont return nothing!
  if(!returnEnglish & returnMetric) {
    englishCols <- which(grepl('_cfs$|cfs_cd$|_ft$|_ft_cd$',colnames(outDf)))
    outDf <- outDf[,-1*englishCols]
    ## problem is that this kills the codes, should translated the codes. 
  }
  
  outDf
}


#' Read standard-format NetCDF data downloaded from Ameriflux
#'
#' \code{ReadAmerifluxNC} reads Ameriflux data table (Level 2 standardized NetCDF file) and creates
#' a dataframe.
#'
#' \code{ReadAmerifluxNC} reads an Ameriflux Level 2 standardized NetCDF file and outputs a dataframe
#' with consistent date and data columns for use with other rwrfhydro tools.
#' 
#' @param pathFluxData The full pathname to the flux time series NetCDF file as downloaded from an
#' Ameriflux data server.
#' @param timeZone The time zone for the flux data. Time zone name must be R-friendly for your
#' current OS. See: \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}
#' @return A dataframe containing the Ameriflux data.
#'
#' @examples
#' ## Takes a NetCDF file downloaded from the ORNL Amerifux website for US-NC2 (North Carolina Loblolly Pine)
#' ## and returns a dataframe.
#'
#' obsFlux30min.usnc2 <- ReadAmerifluxNC("../OBS/FLUX/AMF_USNC2_2005_L2_WG_V003.nc", "America/New_York")
#' @keywords IO
#' @concept aconcept
#' @export
ReadAmerifluxNC <- function(pathFluxData, timeZone) {
    ncFile <- nc_open(pathFluxData)
    nc <- ncFile$nvars
    nr <- ncFile$var[[1]]$varsize
    outDf <- as.data.frame(matrix(nrow=nr, ncol=nc))
    ncVarList <- list()
    for (i in 1:nc ) {
        ncVar <- ncFile$var[[i]]
        ncVarList[i] <- ncVar$name
        outDf[,i] <- ncvar_get( ncFile, ncVar )
    }
    colnames(outDf) <- ncVarList
    nc_close(ncFile)
    outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                        as.character(ifelse(substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2)=='', "00", substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2))),
                        as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,nchar(outDf$HRMIN))), sep="-"),
                        format="%Y-%j-%H-%M", tz=timeZone )
    outDf$wy <- ifelse(as.numeric(format(outDf$POSIXct, "%m"))>=10,
                        as.numeric(format(outDf$POSIXct,"%Y"))+1,
                        as.numeric(format(outDf$POSIXct,"%Y")))
    outDf
}



#' Read standard-format CSV data downloaded from Ameriflux
#'
#' \code{ReadAmerifluxCSV} reads Ameriflux data table (Level 2 standardized CSV file) and creates
#' a dataframe.
#'
#' \code{ReadAmerifluxCSV} reads an Ameriflux Level 2 standardized CSV file and outputs a dataframe
#' with consistent date and data columns for use with other rwrfhydro tools.
#' 
#' @param pathFluxData The full pathname to the flux time series CSV file as downloaded from an
#' Ameriflux data server.
#' @param timeZone The time zone for the flux data. Time zone name must be R-friendly for your
#' current OS. See: \url{http://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html}
#' @return A dataframe containing the Ameriflux data.
#'
#' @examples
#' ## Takes a CSV file downloaded from the ORNL Amerifux website for US-NR1 (Niwot Ridge)
#' ## and returns a dataframe.
#'
#' obsFlux30min.usnr1 <- ReadAmerifluxCSV("../OBS/FLUX/AMF_USNR1_2013_L2_GF_V008.csv", "America/Denver")
#' @keywords IO
#' @concept aconcept
#' @export
ReadAmerifluxCSV <- function(pathFluxData, timeZone) {
    outDf <- read.table(pathFluxData, sep=",", skip=20, na.strings=c(-6999,-9999), strip.white=T)
    outDf.head <- read.table(pathFluxData, sep=",", skip=17, nrows=1, strip.white=T)
    colnames(outDf) <- as.matrix(outDf.head)[1,]
    outDf$POSIXct <- as.POSIXct( paste(as.character(outDf$YEAR), as.character(outDf$DOY),
                        as.character(ifelse(substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2)=='', "00", substr(outDf$HRMIN,1,nchar(outDf$HRMIN)-2))),
                        as.character(substr(outDf$HRMIN,nchar(outDf$HRMIN)-1,nchar(outDf$HRMIN))), sep="-"),
                        format="%Y-%j-%H-%M", tz=timeZone )
    outDf$wy <- ifelse(as.numeric(format(outDf$POSIXct, "%m"))>=10,
                        as.numeric(format(outDf$POSIXct,"%Y"))+1,
                        as.numeric(format(outDf$POSIXct,"%Y")))
    outDf
}
