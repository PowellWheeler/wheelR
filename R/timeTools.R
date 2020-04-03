#' @title Convert a time measurement in MM:SS format to seconds.
#'
#' @description Converts a time measurement in MM:SS format to seconds.
#'
#' @param minsec A character vector with the time measurement as MM:SS. For example 12 mins and 30 seconds would be "12:30".
#'
#' @return A vector of the time measurement in seconds.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family time tools
#'
#' @note This function is not vectorized. Please use sapply() to call it on multiple values.  sapply() will return a named vector, if you prefer an un-named vector use unname() in the call. See examples.
#'
#' @examples
#'
#' minsec2sec('12:30')
#'
#' testData <- c("01:30", "1:30", "100:00")
#'
#' sapply(testData, min.sec2sec) #Returns a named vector
#'
#' unname(testData, min.sec2sec)) #Returns an unnamed vector
#'
#' @rdname minsec2sec
#'
#' @export
minsec2sec <- function(minsec){
min <- as.numeric(substring(minsec,first = 1, last = regexpr(":", minsec)[1] - 1)) #this grabs the mins, by starting at the first character of the string and then ending at the the : minus one digit.
sec <- as.numeric(substring(minsec,first = regexpr(":", minsec)[1] + 1))
output <- (min * 60) + sec
return(output)
}

#' @name now
#'
#' @title Find the current date and time.
#'
#' @return The date and time in POSIXct format.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family time tools
#'
#' @examples
#'
#' now()
#'
#' @rdname now
#'
#' @export
now <- function() Sys.time()
