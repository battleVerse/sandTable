#' @title Plot SUW combat money chart
#'
#' @description This figure plots target range as a function of time
#' @param scenario must contain the following items:
#' @param useDefaultColors (default=FALSE) if you DO have platformInfo filled in, but want to ignore the colors and use defaults, set this to true

#' targetTruth - (scenarioMaker format)
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) target heading in degrees azimuth}
#'  }
#'  ownShipTruth - (scenarioMaker format)
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) target heading in degrees azimuth}
#'  }
#' engagementData - who shot whom, with what, when, what color, and did it kill them?
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{source: (factor) the truthID of the shooter}
#'  \item{target: (factor) the truthID of the thing being shot}
#'  \item{weapon: (factor) name of the weapon}
#'  \item{color: (string) color of the weapon (this color will be used in ggplot, plotly, and exported to SIMDIS) (this function doesn't yet respect color)}
#'  \item{kill: (double) if the engagement killed the target, kill = 1, otherwise kill = 0}
#'}
#'
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#' plot_add_engagements(scenarioMaker::example1_scenario)


# a few functions to help with plotting

#user facing

plot_add_engagements = function(scenario,useDefaultColors=TRUE){

    if (!is.data.frame(scenario$targetTruth) || !is.data.frame(scenario$ownShipTruth)){
        stop("To use this plotting function, the scenario must include both of the following: targetTruth, ownShipTruth")
    }

    truthData=scenario$targetTruth
    ownShipData=scenario$ownShipTruth
    engagementData=scenario$engagementData

    basePlot=scenarioMaker::plot_truth_data_plotly(scenario=scenario, useDefaultColors = useDefaultColors)

    if (!is.data.frame(engagementData)) { #there are no engagements at all - return the basePlot
        return(basePlot)
    } else {
        updatedEngagements = filter(engagementData,source %in% ownShipData$truthID) #only use engagements originating from ownShip
    }

    if (!identical(engagementData,updatedEngagements)){
        ownShipName=unique(ownShipData$truthID)
        warning(sprintf("This function will only plot engagements originating from your ownShip: '%s'.",ownShipName))

        if (nrow(updatedEngagements)==0){
            s=sprintf("It looks like there are no engagements originating from your ownShip: '%s' - did you maybe mispell the name of the target in one of the dataframes?",ownShipName)
            warning(s)
            return(basePlot)

        }
    }

    numNARows=sum(is.na(updatedEngagements$slantRange))

    if (numNARows != 0){
        updatedEngagements=updatedEngagements %>% filter(!is.na(slantRange))

        s=sprintf("%d engagements in this scenario have NA values for their slantRange, and will not be plotted",numNARows)
        warning(s)
    }

    ### pull out info on where the target was ###
    targetInfo = updatedEngagements %>% select(time, source, target, weapon, color, kill, targetLon, targetLat, slantRange) %>%
        rename(lon=targetLon, lat=targetLat) %>%
        mutate(engNum=1:n(), iAm="target")


    ### pull out info on where the source was ###
    sourceInfo = updatedEngagements %>% select(time, source, target, weapon, color, kill, sourceLon, sourceLat,slantRange) %>%
        rename(lon=sourceLon, lat=sourceLat) %>%
        mutate(engNum=1:n(), iAm="source")

    ### combine source/target, then group by engNum - this will force the rows to be drawn as pairs (source and target) ###
    weaponInfo=bind_rows(targetInfo, sourceInfo) %>%
        mutate(engNum=as.factor(engNum)) %>%
        group_by(engNum)



    newPlot=basePlot %>%
    plotly::add_trace(data=weaponInfo, ### Assignment Lines ###
              x=~lon,
              y=~lat,
              type='scatter',
              mode='lines+markers',
              split =~ weapon,
              line=list(dash='dash'),
              marker=list(symbol='circle-open'),
              hoverinfo='text',
              text=~paste('Weapon Engagement',
                          '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                          '<br>Weapon Type: ',weapon,
                          '<br>Target Name: ', target,
                          '<br>Target Range to OwnShip (m):', round(slantRange,1),
                          '<br>Result: ',kill)
             )

    return(newPlot)
}


#myPlot=plot_add_engagements(localTruth,localOwnShip,localEngagementData)
#style(myPlot,frame=~time,traces=c(3))

# localOwnShip=filter(combinedOwnship, eventName == "Feb2, 29-7")
# localTruth=filter(combinedTruth, eventName == "Feb2, 29-7")
# localEngagementData=filter(gunDF, eventName == "Feb2, 29-7")
