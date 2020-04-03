#' @title Find GPS coordinates of anything
#'
#' @description Find GPS coordinates of anything.
#'
#' @param x A string describing the location of interest.
#'
#' @return A data.frame containing the longitude and latitude.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @examples
#' findCoords ("The White House")
#'
#' @export
findCoords <- function (place = "135 Caring Place Loop, Clyde, NC 28721") {
 if(!requireNamespace("ggmap", quitely = TRUE)) {
   stop("Package 'ggmap' needed for this function to work. Please install it.",
    call. = FALSE)
  }
#https://cloud.google.com/maps-platform/#get-started
register_google(key = Sys.getenv("google.api.key"))
options(digits = 8)
googleOutput <- as.data.frame(geocode(place))
return(googleOutput)
}
