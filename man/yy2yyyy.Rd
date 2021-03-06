% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateTools.R
\name{yy2yyyy}
\alias{yy2yyyy}
\title{Converts dates stored as strings with two-digit years into dates stored as strings with four digit years.}
\usage{
yy2yyyy(X)
}
\arguments{
\item{X}{A character vector with length = 1. Date stored in a string in the following format mm/dd/yy.}
}
\value{
A vector of lenght = 1 with the date string X stored as a string with a four-digit year and the day, month, and year seperated by foward slashes (/). he You will probably want to convert these to R-format date
}
\description{
This is helpful if you have data where the dates are miXed between the two formats (yy and yyyy). The function will skip over NAs and dates with four-digit years and corect the two-digit year dates. The function assumes that all two-digit dates are from the last 100 years.
}
\note{
After calling yy2yyyy, your next step will likely be to convert the output to an R-format date with as.Date().
   yy2yyyy will convert '01/01/19' to '01/01/2019' but '1/1/19' to '1/1/2019'. Although there are two results for the same data, as.Date (X, format = '%m/%d/%Y'), will work on both.
   This function is not vectorized.  Use sapply() to call it on a vector or a data.frame column with length > 1. See eXamples.
}
\examples{

yy2yyy('01/01/19')

yy2yyy('1/1/19')

testData <- c('1/1/19', '01/01/19', '01/01/2019')

sapply(testData, yy2yyy)

}
\seealso{
Other date tools: 
\code{\link{ordinalDate}()},
\code{\link{today}()}
}
\author{
A. Powell Wheeler, \email{powell.wheeler@gmail.com}
}
\concept{date tools}
