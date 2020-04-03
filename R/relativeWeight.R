#' @title Calculate relative weight of fish.
#'
#' @description Function that calculates the relative weight of a fish from it's species, total length (mm), and weight (g). Returns NA if the fish is < the mimimal total length for the standard weight equation.
#'
#' @param species A character or factor vector of length = 1. This function uses standard North Carolina Wildlife Resources Commission abbrevaition for the species. See data(ncFishes).
#' @param tl_mm A numeric vector of length = 1. Fish total length in millimeters.
#' @param wt_g A numeric vector of length = 1. Fish weight in grams.
#'
#' @return The relative weight of a fish.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @author Andrew Harris
#'
#' @references Blackwell et al. 2000. Reviews in Fisheres Science. 8:1-44.
#'
#' @note This function is not vectorized. If you want to call it on multiple rows in a data.frame use mapply().  See examples.
#'
#' @examples
#' wr('LMB',200, 300)
#'
#' df$wr <- mapply(wr, df$species, df$tl.mm, df$wt.g)
#'
#' @export

wr <- function(species, tl_mm, wt_g) {

species <- as.character(species)

if(!is.null(wheelR:::wsLookup[[species]]) && (sum(is.na(c(species, tl_mm, wt_g))) == 0)){
  a = wheelR:::wsLookup[[species]][['a']]
  b = wheelR:::wsLookup[[species]][['b']]
  min_tl = wheelR:::wsLookup[[species]][['min_tl']]

    if (tl_mm >= min_tl){
	ws = 10^(a + (b * (log10(tl_mm))))
	wr = (wt_g / ws) * 100
	return(wr)
    } else {
        return(NA)
    }
} else {
    return(NA)
}
}
