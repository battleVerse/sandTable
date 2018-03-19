#' @title Format scenarioMaker data into SIMDIS position data
#'
#' @description Format scenarioMaker data into SIMDIS position data
#'
#' @param platformData position data on all the platforms. This is the scenarioMaker truth format. Requires the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) sensor system heading in degrees azimuth}
#'  }
#' @param timeOverride if non-zero, will overwrite the time in platformData (default=0) (used for the persistPosition option in create_ASI_platforms)
#'
#'
#' @return platform data for SIMDIS
#'
#'


# a few functions to help with plotting

#user facing

format_ASI_position  = function(platformData, timeOverride=0){
    if (timeOverride == 0) {
        time=platformData$time
    } else {
        time=timeOverride
    }
    lat=platformData$lat
    lon=platformData$lon
    alt=platformData$alt
    platformID=as.numeric(platformData$truthID)

    t=sprintf("PlatformData %d '%f' %f %f %f\n", platformID, time, lat, lon, alt)
    return(t)
}
