#' @title Create the header for an ASI file
#'
#' @description  Create the header for an ASI file
#'

#' @param prefFile optional. Adds a preference file title to the header  (default=NA)
#'
#' @return header text
#'
#'
#'

create_ASI_header = function(prefFile = NA) {

    t=sprintf(
        "# SIMDIS ASCII Scenario Input (ASI) File Format
        # Scenario Initialization Keywords
        Version 21
        RefLLA 0. 0. 0.
        DegreeAngles 1
        CoordSystem 'LLA'
        "
    )

    if (is.na(prefFile)){
        header=paste(t,"\n\n\n")
    } else {
        pref=sprintf("RuleFile %s \n\n\n",prefFile)
        header=paste(t,pref)
    }

    return(header)
}
