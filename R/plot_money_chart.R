#' @title Plot SUW combat money chart
#'
#' @description This figure plots target range from ownship and weapon engagements from ownship against the targets
#'
#' @description This figure plots target range as a function of time
#' @param scenario must contain the following items:
#'
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
#'  \item{color: (string) color of the weapon (this color will be used in ggplot, plotly, and exported to SIMDIS)}
#'  \item{kill: (double) if the engagement killed the target, kill = 1, otherwise kill = 0}
#'}
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_money_chart(scenarioMaker::example1_scenario)


# a few functions to help with plotting

#user facing

plot_money_chart = function(scenario){

    if (!is.data.frame(scenario$targetTruth) || !is.data.frame(scenario$ownShipTruth)|| !is.data.frame(scenario$engagementData)){
        stop("To use this plotting function, the scenario must include all of the following: targetTruth, ownShipTruth, and engagementData")
    }

    if (!is.data.frame(scenario$targetOwnShipDistance)){
        stop("This scenario is does not have targetOwnShipDistance pre-calculated, but targetTruth and ownShipTruth are both present. This shouldn't be possible.")
    }

    truthData=scenario$targetTruth
    ownShipData=scenario$ownShipTruth
    engagementData=scenario$engagementData

    ### pull out only source = ownShip
    updatedEngagements = filter(engagementData,source %in% ownShipData$truthID) #only use engagements originating from ownShip

    ### warn the user if there's a problem about sourceIDs
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


    ### figure out how close the bad guys got
    cpa=scenario$targetOwnShipDistance %>% group_by(targetTruthID) %>% summarize(minR=min(slantRange),maxR=max(slantRange))

    ### figure out how big we should make the y axis
    maxRange=round(max(updatedEngagements$slantRange) + 500, digits=-3) #add 1000 and round to the nearest 1000

    #create a match between weapon name and color
    colorNameMatch=stats::setNames(unique(as.character(updatedEngagements$color)),unique(as.character(updatedEngagements$weapon)))


    myPlot=ggplot(data=updatedEngagements) +
        geom_jitter(aes(x=target,y=slantRange,color=weapon,shape=kill),size=3,stroke=2,width=.1,height=0)+
        geom_segment(data=cpa, aes(y=minR,yend=maxR,x=targetTruthID,xend=targetTruthID),alpha=.2,size=2)+
        scale_shape_manual(values=c(1,4),name="Result")+
        scale_color_manual(values=colorNameMatch,name="Weapon")+
        theme_bw()+
        xlab("Target Name")+
        ylab("Range to Ship (m)")+
        coord_cartesian(ylim=c(0,maxRange))

    return(myPlot)
}
