#' @title Extreme Coordinates for the 48 States
#'
#' @description Somewhere I found polygons for the 48 states and extracted the minimum and maximum latitudes and longitudes for each state.  These coordinates help when you are figuring out where to crop maps.
#'
#' @name coordRange48States
#'
#' @docType data
#'
#' @usage data(coordRange48States)
#'
#' @format A data frame with 49 rows (48 states + D.C.) and the following 4 variables.
#'  \describe{
#'    \item{first}{a numeric vector of extreme eastern longitude values}
#'    \item{second}{a numeric vector of extreme western longitude values}
#'    \item{third}{a numeric vector of extreme northern latitude values}
#'    \item{fourth}{a numeric vector of extreme southern latitude values}
#'  }
#'
#' @source unknown
#'
#' @keywords datasets
#'
#' @examples
#' str(coordRange48States)
#' head(coordRange48States)
#'
"coordRange48States"
