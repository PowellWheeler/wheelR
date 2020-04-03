#' @title Convert a GPS coordinate in degrees and minutes (ddmm.mmm) to decimal degrees (dd.ddd).
#'
#' @description Convert historically common degrees and minutes (ddmm.mmm) to more contempoary decimal degrees (dd.ddd).
#'
#' @param ddmm A numeric vector of a latitude or longitude in the format: ddmm.mmmm.
#'
#' @return A numeric vector of a latitude or longitude in the format: dd.dddddd
#'
#'
#' @section testing: I haven't found a good example to test this against.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @examples
#' ## Convert ddmm.mmm to dd.dddd
#' coord <- c(-8355.99)
#' ddmm2dd(coord)
#'
#' @export

ddmm2dd <- function(ddmm) {
	sgn <- sign(ddmm)
	mm  <- abs(ddmm) %% 100
	dd  <- abs(ddmm) - mm
	decimalDegrees<- (dd + mm / 60) * sgn
	return(decimalDegrees)
}
