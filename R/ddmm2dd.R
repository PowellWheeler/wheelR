#' @title Convert a GPS coordinate in degrees and minutes (ddmm.mmm) to decimal degrees (dd.ddd).
#'
#' @description Convert historically common degrees and minutes (ddmm.mmm) to more contempoary decimal degrees (dd.ddd).
#'
#' @param ddmm A numeric vector of a latitude or longitude in the format: ddmm.mmmm.
#'
#' @return A numeric vector of a latitude or longitude in the format: dd.dddddd
#'
#' @section testing: Tested against some values I found on #check known values from http://www.earthpoint.us/Convert.aspx
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @examples
#' ## Convert ddmm.mmm to dd.dddd
#' coord <- c(-8355.99)
#' ddmm2dd(coord)
#'
#' @export

ddmm2dd <- function(ddmm){
	if((is.vector(ddmm)) && (length(ddmm) == 1) && (is.numeric(ddmm))){

  sgn <- sign(ddmm)
	mm  <- abs(ddmm) %% 100
	dd  <- (abs(ddmm) - mm) / 100
	}else{
 		return(NA)
			}

  if((dd <= 180) && (mm <= 60)){
	decimalDegrees <- (dd + mm / 60) * sgn
	return(decimalDegrees)
  }else{
	  return(NA)
		}
}
