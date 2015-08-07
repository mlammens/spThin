#' Occurrence record locations for \emph{Heteromys anomalus}
#' 
#' 
#' A dataset containing compiled occurrence record locations for 
#' \emph{Heteromys anomalus} in northern coastal South America. These records
#' have been examined to check for accurate species identification.
#' 
#' \itemize{
#'   \item \code{SPEC}: \code{factor} species name assigned to occurrence record.
#'   \item \code{LAT}: \code{numeric} latitude of record (decimal degree).
#'   \item \code{LONG}: \code{numeric} longitude of record (decimal degree).
#'   \item \code{X}: \code{numeric} x-coordinate of record (PSAD56 / ICN Regional (EPSG:2317) coordinate system).
#'   \item \code{Y}: \code{numeric} y-coordinate of record (PSAD56 / ICN Regional (EPSG:2317) coordinate system).
#'   \item \code{REGION} \code{factor} name of region record was observed in.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data.frame with 201 rows and 4 variables
#' @name Heteromys_anomalus_South_America
#' @examples
#' data(Heteromys_anomalus_South_America)
#' head(Heteromys_anomalus_South_America)
NULL