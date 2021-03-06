% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_fishNames.R
\docType{data}
\name{NCFishes}
\alias{NCFishes}
\title{Fishes of North Carolina}
\format{
A data frame with 130 rows of fish species and the following 4 variables:
 \describe{
   \item{first}{a character vector of NCWRC abbreviations}
   \item{second}{a character vector of genus and species}
   \item{third}{a character vector of common names}
   \item{fourth}{a character vector of wheather of not the species is a sportfish that is present in District-9}
 }
}
\source{
unknown
}
\usage{
data(NCFishes)
}
\description{
A data.frame of official NCWRC abbreviations, common names and latin names for fishes in North Carolina.
}
\examples{
str(NCFishes)
head(NCFishes)

}
\keyword{datasets}
