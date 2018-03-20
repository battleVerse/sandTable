#' @title Create an ASI platform
#'
#' @description Gives text for platform instantiation
#'
#' @param header output from create_ASI_header (can be piped into this function)
#'
#' @param scenario must have targetTruth and/or ownShipTruth as well as platformInfo:
#'
#' platformData (targetTruth and/or ownShipTruth) position data on all the platforms. This is the Nautilus truth format. Requires the following columns:
#' \itemize{
#'  \item{time: (double) time of measurement. We currently recommend POSIX}
#'  \item{lon: (double) longitude of target at time of measurement}
#'  \item{lat: (double) latitude of target at time of measurement}
#'  \item{alt: (double) altitude of target at time of measurement}
#'  \item{truthID: (factor) name or identifier for target. We recommend letters or names}
#'  \item{heading: (double) sensor system heading in degrees azimuth}
#'  }
#'
#' platformInfo display info for all platforms. Requires the following columns:
#' \itemize{
#'  \item{truthID: (factor) name or identifier for target}
#'  \item{platformIcon: (string) name of platform icon (must choose from valid SIMDIS options)}
#'  \item{platformType: (string) type of platform (e.g., ship) (must choose from valid SIMDIS options)}
#'  \item{trackColor: (string) color of the track (must be a valid SIMDIS choice)}
#'  }
#'
#' @param persistPosition (boolean) if true, will persist all platforms until this time (and gray out the title at their last position) (default=false)
#' @param drawSensorTracks (default=FALSE) if the scenario includes sensorData, the tracks will be drawn in the asi file (it's okay if drawSensorTracks=TRUE but no sensorData exists in the scenario)
#' @return text
#'
#'

create_ASI_platforms = function(header, scenario, persistPosition=FALSE,drawSensorTracks=FALSE){



    #get the combined truth data
    combinedTruth=scenarioMaker:::combine_target_ownship_truth(scenario)


    ############################################################################################
    ### if drawSensorTracks==TRUE, then we'll need to reshape it to add it to the truth data ###
    ############################################################################################
    # if (is.data.frame(scenario$sensorData)) { #if you only have sensor data, plot the simpler thing
    #     sensorData=scenario$sensorData %>%
    #         rename(uniqueID=trackNum) %>% #rename for joining later
    #         mutate(color="red", uniqueID=as.character(uniqueID)) %>% #color will be over-written later if we're NOT using default colors
    #         select(time, lon, lat,uniqueID, hoverText, color)
    # }

    platformList = unique(combinedTruth$truthID)

    asiFile=header

    for (platformName in platformList){
        #Pull out this platform's data
        thisPlatData = combinedTruth %>% filter(truthID == platformName)

        #Pull out this platform's info
        thisPlatInfo = scenario$platformInfo %>% filter(truthID == platformName)

        if (nrow(thisPlatInfo)==1){ #if the platform is included in platformInfo, then pull out the appropriate info
            platformID=as.numeric(thisPlatData$truthID)[[1]] #create the platform ID by getting the factor level of this truthID
            platformIcon=thisPlatInfo$platformIcon
            platformType=thisPlatInfo$platformType
            trackColor=thisPlatInfo$trackColor
        } else { #if there is no platformInfo for this platform, then set defaults
            platformID=as.numeric(thisPlatData$truthID)[[1]] #create the platform ID by getting the factor level of this truthID
            platformIcon="NTDSUnknownUnknown2D"
            platformType="0" #0 means unknown
            trackColor="red"
        }

        ### create the instantiation header for this platform
        createPlatform=sprintf("PlatformID %d\nPlatformName %d '%s'\nPlatformIcon %d '%s'\nPlatformType %d '%s'\nGenericData %d 'SIMDIS_TrackColor' '%s' '0'\n\n",
                               platformID,platformID,platformName,platformID,platformIcon,platformID,platformType,platformID,trackColor)

        ### Create Position Data ###
        positionData = thisPlatData %>%
            purrrlyr::by_row(format_ASI_position) %>%
            select(.out) %>% unlist() %>%
            paste(.,collapse='')



        ### Handle Persist Option - keeps 'dead' objects drawn, but grays out title ###
        if (persistPosition == TRUE) {
            maxTime=max(combinedTruth$time)
            thisPlatformFinalTime=max(thisPlatData$time)

            ### Grab the last row for this platform
            lastRow=thisPlatData %>% slice(n())

            ### Make a copy of it, with a new timestamp
            lastPosition = format_ASI_position(lastRow,timeOverride=maxTime)

            ### Make sure the thing starts white
            initialColor=sprintf("GenericData %d 'SIMDIS_FontColor' '0xFFFFFFFF' '0'\n",platformID)

            ### Make it grey at the appropriate time
            changeColor=sprintf("GenericData %d 'SIMDIS_FontColor' '0x70FFFFFF' '%f'\n\n\n\n",platformID,thisPlatformFinalTime)

            endCap=paste(lastPosition,initialColor,changeColor)

        } else {
            endCap='\n\n\n\n'
        }
        asiFile=paste(asiFile,createPlatform,positionData,endCap)
    }

    return(asiFile)

}
