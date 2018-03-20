#' @title Read platform data (truth data) from an ASI file
#'
#' @description  Read platform data (truth data) from an ASI file. Only reads in lat, lon, alt, and heading (no speed, velocity, etc.).
#' Only works on very SIMDIS files written in an LLA coordinate system. If you export your SIMDIS file (from the File menu), it will save it in a way that this can read
#'
#' @param ASIFilePath path to ASI file
#' @param timeFormat (default="\%j \%Y \%H:\%M:\%OS") the format the time information is written in - if it's just seconds, use "\%OS\%"
#' @param timeZone (default=NULL) specify string of time zone. If no argument supplied, will actually default to UTC, but will warn the user
#'
#' @return truth data dataframe
#'
#'
#' @examples
#' \dontrun{read_ASI("C:/myFile.asi",timeZone="US/Pacific")}
#'
#' @export
#'


read_ASI = function(ASIFilePath, timeFormat = "%j %Y %H:%M:%OS",timeZone=NULL) {


    if (is.null(timeZone)){
        s="You did not specify a time zone, so read_ASI will default to assuming the ASI file is in 'UTC'. Too specify the time zone, use the argument: \"timeZone='Time Zone Name'\""
        warning(s)
        timeZone="UTC"
    }

    cat("Reading ASI File\n")
    conn <- file(ASIFilePath,open="r")
    lines <- readLines(conn)
    close(conn)
    cat("Finished Reading File\n")


    ######################################################
    ### Let's figure out if it's in degrees or radians ###
    ######################################################
    angleUnits=lines[grepl(pattern="(DegreeAngles)", lines)] %>% #pull out ONLY lines that match the pattern
        paste0(collapse='\n') %>% #force this text into a file-like structure
        textConnection() %>% #send it through a text connection
        utils::read.csv(sep="",header=FALSE) %>% #pretend it's a csv
        select(V2) #pull out the unit setting


    if (angleUnits$V2 == 1) {
        angleConversion=1
        cat("Input file is in units of degrees\n")
    } else {
        angleConversion=57.2958
        cat("Input file is in units of radians\n")
    }

    ####################################
    ### Let's get the reference year ###
    ####################################
    refYear=lines[grepl(pattern="(ReferenceYear)", lines)]

    if (length(refYear)>0){ #if we found anything

        refYear = refYear %>% #pull out ONLY lines that match the pattern
            paste0(collapse='\n') %>% #force this text into a file-like structure
            textConnection() %>% #send it through a text connection
            utils::read.csv(sep="",header=FALSE) %>% #pretend it's a csv
            select(V2) #pull out the unit setting

        cat(sprintf("Reference Year: %s\n",refYear))

        refYear=as.POSIXct(ISOdate(refYear$V2,1,1,0,0),tz="UTC")

    } else { # if we found nothing, just set it to 0
        refYear=as.POSIXct("1970-01-01 00:00:00", tz="UTC")
    }



    ####################################################
    ### Let's read in all platforms with their names ###
    ####################################################
    platNames=lines[grepl(pattern="(PlatformName)", lines)] %>% #pull out ONLY lines that match the pattern
        paste0(collapse='\n') %>% #force this text into a file-like structure
        textConnection() %>% #send it through a text connection
        utils::read.csv(sep="",header=FALSE) %>% #pretend it's a csv
        select(2,3) %>% #drop the first column
        rename(simdisPlatformNum=V2, platformName=V3)


    ###########################################################
    ### Now let's pull in the platform position information ###
    ###########################################################
    platformPositionPattern = "(PlatformData)"


    N=10000 #read in 10,000 lines at a time
    currentLine = 1
    platPositionData=NULL

    platPositionLines=lines[grepl(pattern="(PlatformData)", lines)]
    while (currentLine <= length(platPositionLines)) { #go through in chunks until we reach the end
        startIndex=currentLine
        stopIndex=min(startIndex+N, length(platPositionLines)) #go until either the end of next chunk, or end of file

        cat(sprintf("Reading Platform Positions %i through %i (of total %i)\n", startIndex, stopIndex, length(platPositionLines)))

        tmpPlatData=platPositionLines[startIndex:stopIndex] %>% #pull out ONLY lines that match the pattern
            paste0(collapse='\n') %>% #force this text into a file-like structure
            textConnection() %>% #send it through a text connection
            utils::read.csv(sep="",header=FALSE)

        platPositionData=bind_rows(platPositionData, tmpPlatData)


        currentLine=stopIndex+1
    }

    ### clean up the data we just read in ###
    if (ncol(platPositionData)==6) { #this means there is no heading data

        platformData=platPositionData %>%
            rename(simdisPlatformNum = V2,
                   time=V3,
                   lat=V4,
                   lon=V5,
                   alt=V6) %>%
            select(simdisPlatformNum, time, lat, lon, alt) %>%
            mutate(lat=lat*angleConversion,
                   lon=lon*angleConversion,
                   time=as.numeric(as.POSIXct(time, format=timeFormat, tz=timeZone, origin=refYear))) %>%
            stats::na.omit()

    } else {
        platformData=platPositionData %>%
            rename(simdisPlatformNum = V2,
                   time=V3,
                   lat=V4,
                   lon=V5,
                   alt=V6,
                   heading=V7) %>%
            select(simdisPlatformNum, time, lat, lon, alt, heading) %>%
            mutate(lat=lat*angleConversion,
                   lon=lon*angleConversion,
                   heading=heading*angleConversion,
                   time=as.numeric(as.POSIXct(time, format=timeFormat, tz=timeZone, origin=refYear))) %>%
            stats::na.omit()
    }


    platformData = left_join(platformData, platNames, by="simdisPlatformNum") %>%
        rename(truthID=platformName)

    return(platformData)

}

