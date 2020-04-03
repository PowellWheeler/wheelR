#' @title Assign ordinal dates.
#'
#' @description Function that converts R dates into ordinal dates. For example "2008-07-14" becomes 195. Ordinal dates can be useful for graphing. For example if you monitor some variable continually for multiple years and you want to graph them on a common January to December X-axis.
#'
#' @param X A date in R's native format.
#'
#' @return A vector or ordinal dates.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family date tools
#'
#' @note This function returns NA for leap year days (Februay 29) and is not vectorized.  If you want to call it on multiple observations in a vector or a data.frame column use sapply().  See examples.
#'
#' @examples
#' example <- as.Date("07/01/1999", format = '%m/%d/%Y')
#' ordinalDate(example)
#'
#' example2 <- as.Date(c("1/1/2000", "07/01/1999", "12/31/1970"), format = '%m/%d/%Y')
#' sapply(example2, ordinalDate)
#'
#' @rdname daysFromToday
#'
#' @export
ordinalDate <- function(X) wheelR:::ordinalLookup[which(wheelR:::ordinalLookup$monthDay == format(X, format="%m-%d")),2]

#' @title Find what day is X days from today
#'
#' @description Find what day is X days from today.
#'
#' @param x Numeric. The number of days in the future you are interested in.
#'
#' @return A vector of class date with the future date.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family date tools
#'
#' @note This is easier than manually counting days on a calendar. For example, I have to service water quality loggers every 90 days before their batteries expire. This function makes it easy to find 90 days from now.
#'
#' @examples
#'
#' daysFromToday(90)
#'
#' @rdname daysFromToday
#'
#' @export
daysFromToday <- function(x = 0) Sys.Date() + x

#' @title Find what day is today.
#'
#' @return The date in R date format.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family date tools
#'
#' @examples
#'
#' today()
#'
#' @rdname today
#'
#' @export
today <- function() Sys.Date()

#' @title Converts dates stored as strings with two-digit years into dates stored as strings with four digit years.
#'
#' @description This is helpful if you have data where the dates are miXed between the two formats (yy and yyyy). The function will skip over NAs and dates with four-digit years and corect the two-digit year dates. The function assumes that all two-digit dates are from the last 100 years.
#'
#' @param X A character vector with length = 1. Date stored in a string in the following format mm/dd/yy.
#'
#' @return A vector of lenght = 1 with the date string X stored as a string with a four-digit year and the day, month, and year seperated by foward slashes (/). he You will probably want to convert these to R-format date
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family date tools
#'
#' @note After calling yy2yyyy, your neXt step will likely be to convert the output to an R-format date with as.Date().
#'    yy2yyyy will convert '01/01/19' to '01/01/2019' but '1/1/19' to '1/1/2019'. Although there are two results for the same data, as.Date (X, format = '%m/%d/%Y'), will work on both.
#'    This function is not vectorized.  Use sapply() to call it on a vector or a data.frame column with length > 1. See eXamples.
#'
#' @examples
#'
#' yy2yyy('01/01/19')
#'
#' yy2yyy('1/1/19')
#'
#' testData <- c('1/1/19', '01/01/19', '01/01/2019')
#'
#' sapply(testData, yy2yyy)
#'
#' @rdname yy2yyyy
#'
#' @export

yy2yyyy <- function(X) {
current_2digit_year <- as.numeric(format(Sys.Date(), '%y'))

if (is.na(X)){
  output <- NA
}else{
  if  (suppressWarnings(is.na(as.numeric(substring(X, nchar(X) - 3, nchar(X)))))) { #tests if the last four digits can't be eXpressed as a number, it will kick out warnings that have to be suppressed
    year_2digit <- substring(X, nchar(X) - 1, nchar(X))
    month.day <- substring(X, 1, nchar(X) - 3)
      if(year_2digit <= current_2digit_year){
        prefix <- current_2digit_year
        }else{
        prefix <- current_2digit_year - 1
        }
    year_4digit <- paste0(prefix, year_2digit)
    output <-paste0(month.day, '/', year_4digit)
  }else{
    output <- X
}
return(output)
}
}
