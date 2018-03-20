#' sandTable: A package for analyzing weapon engagements.  A part of the battleVerse
#'
#' Hello and welcome to sandTable, a part the battleVerse!  sandTable is a package designed to help you analyze weapon engagements and to import/export SIMDIS format data.
#'
#'
#'
#' @section Import/Export SIMDIS data:
#'
#' There are several 'helper' functions designed to help you convert your data to and from SIMDIS format:
#'
#' \itemize{
#'  \item{\code{\link{add_ASI_engagements}}}
#'  \item{\code{\link{create_ASI_header}}}
#'  \item{\code{\link{create_ASI_platforms}}}
#'  \item{\code{\link{export_scenario_to_ASI}}}
#'  \item{\code{\link{format_ASI_position}}}
#'  \item{\code{\link{read_ASI}}}
#'  }
#'
#'
#' @section Plotting Functions:
#'
#' sandTable has several plotting functions for weapon engagements
#'
#' \itemize{
#' \item{ \code{\link{plot_add_engagements}} }
#' \item{ \code{\link{plot_money_chart}} }
#' }
#'
#'
#'
#'
#' @docType package
#' @name sandTable
#'

NULL


if(getRversion() >= "2.15.1")  utils::globalVariables(c("time",
                                                        "lat",
                                                        "lon",
                                                        "alt",
                                                        "truthID",
                                                        "kill",
                                                        "maxR",
                                                        "minR",
                                                        "target",
                                                        "targetLat",
                                                        "targetLon",
                                                        "simdisPlatformNum",
                                                        "slantRange",
                                                        "sourceLat",
                                                        "sourceLon",
                                                        "weapon",
                                                        ".out",
                                                        ".",
                                                        "engNum",
                                                        "color",
                                                        "V2",
                                                        "V3",
                                                        "V4",
                                                        "V5",
                                                        "V6",
                                                        "V7",
                                                        "basePlot",
                                                        "heading",
                                                        "targetTruthID",
                                                        "platformName"
))
