#' @title Add engagements to ASI file
#'
#' @description Add engagements to ASI file
#'
#' @param asiText output from create_ASI_platforms (can be piped into this function)
#' @param scenario must have targetTruth and/or ownShipTruth as well as engagementData:
#' @param drawSensorTracks (default=FALSE) needed for proper PlatformID numbering
#'
#' @return text
#'
#'
#'

add_ASI_engagements = function(asiText, scenario,drawSensorTracks=FALSE){
    #drawSensorTracks is required so that add_ASI_engagements can start counting from the right number


    #combine the two (this is okay even if one is NA!)
    combinedTruth=scenarioMaker:::combine_target_ownship_truth(scenario)

    engagementData=scenario$engagementData

    ### The first laser ID will be 1 above the highest existing ID
    laserID = max(as.numeric(combinedTruth$truthID))+1

    ### Go through each shooter ###
    sourceList = unique(engagementData$source)
    for (sourceName in sourceList){

        #Pull out this platform's engagement data
        thisSourceEngagement = engagementData %>% filter(source == sourceName)

        #Pull out this platform's position data and platform ID
        thisSourcePosition = combinedTruth %>% filter(truthID == sourceName)

        #look up the platform ID by getting the factor level of this truthID
        thisSourcePlatformID = as.numeric(thisSourcePosition$truthID)[[1]]



        ### Go through each weapon on the shooter ###
        weaponList = unique(thisSourceEngagement$weapon)
        for (weaponName in weaponList){
            thisWeaponEngagements=thisSourceEngagement %>% filter(weapon == weaponName)


            createLaser=sprintf("LOBID %d %d\nLOBType %d 'Absolute'\nLOBDesc %d '%s'\n",thisSourcePlatformID,laserID,laserID,laserID,weaponName)

            laserOut=""
            for (engagementNum in 1:nrow(thisWeaponEngagements)){
                thisEngagement=thisWeaponEngagements[engagementNum,]

                if (thisEngagement$kill==1){
                    setColor=sprintf("LOBDrawStyle %d '%f' %s 0xFF000000 0xFF00 0x00FF 3\n",laserID,thisEngagement$time,thisEngagement$color)
                } else {
                    setColor=sprintf("LOBDrawStyle %d '%f' %s 0x00000000 0xFF00 0x00FF 3\n",laserID,thisEngagement$time,thisEngagement$color)
                }

                laserLine=sprintf("LOBData %d '%f' %f 0 %f\n",laserID,thisEngagement$time,thisEngagement$trueBearingToTarget,thisEngagement$groundRange)

                laserOut=paste(laserOut,"\n",setColor,laserLine,sep="")


            }


            asiText=paste(asiText,createLaser,laserOut,'\n\n\n',sep="")

            laserID = laserID + 1 #increment laser ID before moving to the next laser

        }

    }
    return(asiText)

}
