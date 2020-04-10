#' @title Find the numerical address for columns in a data.frame.
#'
#' @description Find the numerical address for columns in a data.frame.
#'
#' @param df A data.frame.
#'
#' @return A vector of column names, 'named' by their numerical address.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family terminal tools
#'
#' @examples
#'
#' colIdx(friutSalad())
#'
#' @rdname colIdx
#'
#' @export
colIdx   <- function (df) {
  if(is.data.frame(df)){
   vec <- names(df)
   names(vec) <- 1:length(vec)
   return(vec)
 }else{
   return(NA)
 }
}

#'
#' @title View a subset of the rows in a large data.frame.
#'
#' @description View the first, last, and evenly spaced rows throughout a data.frame.
#'
#' @param df Data.frame.
#'
#' @param N Number. The number of rows you want to display. Must be >= 3.
#'
#' @return A data.frame of the sytematically subsampled rows.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family terminal tools
#'
#' @references A more refined version was published in the FSA package as peek().
#'
#' @note This is helpful for getting a feel for what's going on in a very large data.frame if it is too large to easially display.
#'
#' @examples
#'
#' dfScan(friutSalad(1000))
#'
#' @rdname dfScan
#'
#' @export
dfScan   <- function (df, N = 50){
if(is.data.frame(df) && N >= 3){
  ifelse(nrow(df) <= N, return(df), return(df[c(1, 1:(N - 2) * (nrow(df)/(N - 1)),nrow(df)), ])) #displays the first, last and evenly spaced rows throughout the df
}else{
  return(NA)
 }
}
#' @title Basic computing system information.
#'
#' @description Displays basic information on R, hardware, operating system, and date.
#'
#' @return Does not return an object only prints information to the console.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family terminal tools
#'
#' @note This is helpful for adding system information to script outputs.
#'
#' @examples
#' ## Useage:
#' basicSysInfo()
#'
#' @rdname dfScan
#'
#' @export
basicSysInfo <- function(...) {

  infoOut <- sessionInfo()
  infoAboutR  <- paste(infoOut$R.version$version.string, infoOut$R.version$nickname, sep="     ")
  infoAboutPlatform <- infoOut$platform
  infoAboutOs  <- infoOut$running

  cat("\n")
  cat(paste("**********************************************\n"))
  cat(paste(infoAboutR ,"\n\n"))
  cat(paste(infoAboutPlatform ,"\n\n"))
  cat(paste(infoAboutOs ,"\n\n"))
  cat(paste("Now is: ",date(),"\n"))
  cat(paste("**********************************************\n"))
}
