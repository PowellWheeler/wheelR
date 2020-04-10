#' @title Fishes of North Carolina
#'
#' @description A data.frame of official NCWRC abbreviations, common names and latin names for fishes in North Carolina.
#'
#' @name NCFishes
#'
#' @docType data
#'
#' @usage data(NCFishes)
#'
#' @format A data frame with 130 rows of fish species and the following 4 variables:
#'  \describe{
#'    \item{first}{a character vector of NCWRC abbreviations}
#'    \item{second}{a character vector of genus and species}
#'    \item{third}{a character vector of common names}
#'    \item{fourth}{a character vector of wheather of not the species is a sportfish that is present in District-9}
#'  }
#'
#' @source unknown
#'
#' @keywords datasets
#'
#' @examples
#' str(NCFishes)
#' head(NCFishes)
#'
"NCFishes"

# d9Sportfish <- NCFishes[NCFishes$d9.status == 'sportfish',]
# d9Sportfish <- d9Sportfish[!(is.na(d9Sportfish$d9.status)),]
# rownames(d9Sportfish) <- NULL
# save(d9Sportfish, file='~/git/wheelR/data/d9Sportfish.rda')

#' @title District 9 Sportfish
#'
#' @description A data.frame of sportfishes in NCWRC District 9.
#'
#' @name d9Sportfish
#'
#' @docType data
#'
#' @usage data(d9Sportfish)
#'
#' @format A data frame with 36 rows of sportfish species and the following 4 variables:
#'  \describe{
#'    \item{first}{a character vector of NCWRC abbreviations}
#'    \item{second}{a character vector of genus and species}
#'    \item{third}{a character vector of common names}
#'    \item{fourth}{a character vector of wheather of not the species is a sportfish that is present in District-9}
#'  }
#'
#' @source unknown
#'
#' @keywords datasets
#'
#' @examples
#' str(d9Sportfish)
#' head(d9Sportfish)
#'
"d9Sportfish"
#
# d9SportfishVector <-as.vector(d9Sportfish$code)
# names(d9SportfishVector) <- d9Sportfish$common.name
# save(d9SportfishVector,file='~/git/wheelR/data/d9SportfishVector.rda')
#' @title District 9 Sportfish Vector
#'
#' @description A vector of sportfishes in NCWRC District 9.
#'
#' @name d9SportfishVector
#'
#' @docType data
#'
#' @usage data(d9SportfishVector)
#'
#' @format A vector of District-9 sportfish abreviations labeled with their common names.
#'
#' @source unknown
#'
#' @keywords datasets
#'
#' @examples
#' str(d9SportfishVector)
#' head(d9SportfishVector)
#'
"d9SportfishVector"
