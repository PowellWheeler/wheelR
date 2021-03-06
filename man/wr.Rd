% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relativeWeight.R
\name{wr}
\alias{wr}
\title{Calculate relative weight of fish.}
\usage{
wr(species, tl_mm, wt_g)
}
\arguments{
\item{species}{A character or factor vector of length = 1. This function uses standard North Carolina Wildlife Resources Commission abbrevaition for the species. See data(ncFishes).}

\item{tl_mm}{A numeric vector of length = 1. Fish total length in millimeters.}

\item{wt_g}{A numeric vector of length = 1. Fish weight in grams.}
}
\value{
The relative weight of a fish.
}
\description{
Function that calculates the relative weight of a fish from it's species, total length (mm), and weight (g). Returns NA if the fish is < the mimimal total length for the standard weight equation.
}
\note{
This function is not vectorized. If you want to call it on multiple rows in a data.frame use mapply().  See examples.
}
\examples{
wr('LMB',200, 300)

df$wr <- mapply(wr, df$species, df$tl.mm, df$wt.g)

}
\references{
Blackwell et al. 2000. Reviews in Fisheres Science. 8:1-44.
}
\author{
A. Powell Wheeler, \email{powell.wheeler@gmail.com}

Andrew Harris
}
