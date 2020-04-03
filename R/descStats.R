#' @title Count of observations while ignoring missing values
#'
#' @description Count the observations while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the count of
#'   observations ignoring the missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases count
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' N_Obs(observations)
#'
#' @rdname N_Obs
#'
#' @export
N_Obs <- function(x) length(x[!is.na(x)])

#'
#' @title Count zero observations while ignoring missing values
#'
#' @description Count zero observations while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the count of
#'   observations of zero.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases count
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' N_Zero(observations)
#'
#' @rdname N_Zero
#'
#' @export
N_Zero <- function(x) length(x[(! is.na(x) & x == 0)])

#'
#' @title Count of missing (NA and NaN) values
#'
#' @description Count the missing values (NA and NaN).
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the count of
#'   observations that are NA or NaN.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases count
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(4, 4.4, 0/0, 1/0, -1/0, NA)
#' N_NA(observations)
#'
#' @rdname N_NA
#'
#' @export
N_NA <- function(x) length(x[is.na(x)])

#'
#' @title Sum of values while ignoring missing values
#'
#' @description Sum of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the sum of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases sum
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Sum(observations)
#'
#' @rdname Sum
#'
#' @export
Sum <- function(x) sum(x, na.rm = TRUE)

#'
#' @title Mean of values while ignoring missing values
#'
#' @description Mean of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the mean of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases mean average
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Mean(observations)
#'
#' @rdname Mean
#'
#' @export
Mean <- function(x) mean(x, na.rm = TRUE)

#'
#' @title Median of values while ignoring missing values
#'
#' @description Median of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the median of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases median
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Median(observations)
#'
#' @rdname Median
#'
#' @export
Median <- function(x) median(x, na.rm = TRUE)

#'
#' @title Minimum of values while ignoring missing values
#'
#' @description Minimum of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the minimum of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases min minimum
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Min(observations)
#'
#' @rdname Min
#'
#' @export
Min <- function(x) min(x, na.rm = TRUE)

#'
#' @title Maximum of values while ignoring missing values
#'
#' @description Maximum of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the maximum of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases maximum max
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Maximum(observations)
#'
#' @rdname Max
#'
#' @export
Max <- function(x) max(x, na.rm = TRUE)

#'
#' @title Variance of values while ignoring missing values
#'
#' @description Variance of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the variance of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases variance
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' Var(observations)
#'
#' @rdname Var
#'
#' @export
Var <- function(x) var(x, na.rm = TRUE)

#'
#' @title Standard deviation of values while ignoring missing values
#'
#' @description Standard deviation of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the standard deviation
#'   of observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases standard deviation
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' SD(observations)
#'
#' @rdname SD
#'
#' @export
SD <- function(x) sd(x, na.rm = TRUE)

#'
#' @title Coefficient of variation of values while ignoring missing values
#'
#' @description Coefficient of variation of values while ignoring missing
#'  values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the coefficient of
#'   variation of observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases cv
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' CV(observations)
#'
#' @rdname CV
#'
#' @export
CV <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100

#'
#' @title Standard error of values while ignoring missing values
#'
#' @description Standard error of values while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the standard error of
#'   observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @aliases se
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' SE(observations)
#'
#' @rdname SE
#'
#' @export
SE <- function(x) sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)]))

#'
#' @title Lower 95\% confidence interval while ignoring missing values
#'
#' @description Lower 95\% confidence interval while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the lower 95\%
#'   confidence interval of observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' L95CI(observations)
#'
#' @rdname L95CI
#'
#' @export
L95CI <- function(x) mean(x, na.rm = TRUE) - qt(0.975,
  df = (length(x[!is.na(x)]) - 1)) * (sd(x, na.rm = TRUE) /
  sqrt(length(x[!is.na(x)])))

#'
#' @title Upper 95\% confidence interval of values while ignoring missing values
#'
#' @description Upper 95\% confidence interval while ignoring missing values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a single value of the upper 95\%
#'   confidence interval of observations while ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' U95CI(observations)
#'
#' @rdname U95CI
#'
#' @export
U95CI <- function(x) mean(x, na.rm = TRUE) + qt(0.975,
  df = (length(x[!is.na(x)]) - 1)) * (sd(x, na.rm = TRUE) /
  sqrt(length(x[!is.na(x)])))

#'
#' @title 95\% confidence interval
#'
#' @description 95\% confidence interval (ignores missing values).
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing a two values: the lower and upper bounds
#'  of the 95\% confidence interval.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' CI95(observations)
#'
#' @rdname CI95
#'
#' @export
CI95 <- function(x) mean(x, na.rm = TRUE) + c(-1, 1) *
  qt(0.975, df = (length(x[!is.na(x)]) - 1)) * (sd(x, na.rm = TRUE)
  / sqrt(length(x[!is.na(x)])))

#'
#' @title Collection of descriptive statistics that ignoring missing values
#'
#' @description Collection of descriptive statistics that ignoring missing
#'   values.
#'
#' @param x A numeric vector.
#'
#' @return A named numeric vector containing a variety of descriptive statistics that
#'   ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' sumFun(observations)
#'
#' @rdname sumFun
#'
#' @export
sumFun <- function(x) {
  vector <- c(N_Obs(x), N_Zero(x), N_NA(x), Sum(x), Mean(x), SD(x),
  SE(x), L95CI(x), U95CI(x), Median(x), Var(x), CV(x), Min(x), Max(x))

  vector.names <- c('N_Obs', 'N_Zero', 'N_NA', 'Sum', 'Mean', 'SD', 'SE', 'L95CI',
    'U95CI', 'Median', 'Var', 'CV', 'Min', 'Max')

  names(vector) <- vector.names

  return(vector)
  }

#'
#' @title Collection of descriptive statistics that ignoring missing values
#'
#' @description Collection of descriptive statistics that ignoring missing
#'   values.
#'
#' @param x A numeric vector.
#'
#' @return A named numeric vector containing a a variety of descriptive statistics
#'   that ignoring missing values.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @family descriptive functions
#'
#' @examples
#' observations <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#' sumFunShort(observations)
#'
#' @rdname sumFunShort
#'
#' @export
sumFunShort <- function(x) {
  vector <- c(N_Obs(x), N_NA(x), Mean(x), SD(x), SE(x))

  vector.names <- c('N_Obs', 'N_NA', 'Mean', 'SD', 'SE')

  names(vector) <- vector.names

  return(vector)
  }
