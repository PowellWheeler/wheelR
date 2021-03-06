% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descStats.R
\name{N_Obs}
\alias{N_Obs}
\alias{count}
\title{Count of observations while ignoring missing values}
\usage{
N_Obs(x)
}
\arguments{
\item{x}{A vector, data.frame, matrix, list, or whatever data storage object.}
}
\value{
A numeric vector containing a single value of the count of
  observations ignoring the missing values.
}
\description{
Count the observations while ignoring missing values.
}
\examples{
observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
N_Obs(observations)

}
\seealso{
Other descriptive functions: 
\code{\link{CI95}()},
\code{\link{CV}()},
\code{\link{L95CI}()},
\code{\link{Max}()},
\code{\link{Mean}()},
\code{\link{Median}()},
\code{\link{Min}()},
\code{\link{N_NA}()},
\code{\link{N_Zero}()},
\code{\link{SD}()},
\code{\link{SE}()},
\code{\link{Sum}()},
\code{\link{U95CI}()},
\code{\link{Var}()},
\code{\link{sumFunShort}()},
\code{\link{sumFun}()}
}
\author{
A. Powell Wheeler, \email{powell.wheeler@gmail.com}
}
\concept{descriptive functions}
