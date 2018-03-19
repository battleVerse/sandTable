#' @title Writes your scenario as an ASI file
#'
#' @description
#'
#' @param scenario must have targetTruth and/or ownShipTruth as well as platformInfo
#' @param outFileName output file name (including path if desired) for the ASI. Optional to include '.asi' in the output
#' @param drawEngagements (default=TRUE) if the scenario includes engagementData, the engagements will be drawn in the asi file (it's okay if drawEngagements=TRUE but no engagementData exists in the scenario)
#' @param drawSensorTracks (default=FALSE) if the scenario includes sensorData, the tracks will be drawn in the asi file (it's okay if drawSensorTracks=TRUE but no sensorData exists in the scenario)
#' @param persistPosition (boolean) if true, will persist all platforms until the end of the file (and gray out the title at their last position) (default=false)
#' @param prefFile (default=NA) specify name of preference file (must be in same folder as the ASI output when you open it in simdis)
#'
#'
#' @export
#'

export_scenario_to_ASI = function(scenario, outFileName, drawEngagements=TRUE, drawSensorTracks=FALSE, persistPosition=FALSE,prefFile=NA){

    ### Make sure we have the right elements in the scenario ###
    if (!is.data.frame(scenario$targetTruth) && !is.data.frame(scenario$ownShipTruth)){
        stop("To use this function, the scenario must include targetTruth and/or ownShipTruth")
    }


    if (!is.data.frame(scenario$platformInfo)){
        warning("The scenario has no platformInfo. We can still export to an ASI, but if you want to specify platform colors and platform models, you need to supply platformInfo when you create the scenario.")
    }


    fileExtension=stringr::str_sub(outFileName,-4,-1)
    if (fileExtension != ".asi"){
        outFileName=sprintf("%s.asi",outFileName)
    }

    ### make the header and add all the platforms
    asiFile=create_ASI_header(prefFile) %>%
        create_ASI_platforms(scenario, persistPosition=persistPosition,drawSensorTracks=drawSensorTracks)

    ### if we have engagementData AND the user wants to plot it
    if (is.data.frame(scenario$engagementData) && drawEngagements==TRUE){
        asiFile=add_ASI_engagements(asiFile,scenario)
    }

    write(asiFile,file=outFileName)

}
