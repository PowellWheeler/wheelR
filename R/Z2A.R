#' @title Convert instantaneous mortality (Z) to the annual mortality (A) rate.
#'
#' @description Quickly convert a difficult to interprete instantaneous mortality to a more understandable annual mortality rate. Also, converts the SE(Z) to SE(A) and bootstraps confidence intervals with the gamma distribution.
#'
#' @param Z Vector of length = 1 of an instantaneous mortality rate. This can be entered as a positive or negative number.
#' @param SE_Z Vector of length = 1 of Standard Error of the instantaneous mortality rate. Defaults to NA.
#' @param CI Vector of length = 1 for the width of the confidence interval. Defaults to 0.95.
#'
#' @return A list with with the estimates for A and Z, their standard errors, and their confidence intervals.  The confidence interval is derrived from a parameteric bootstrap from the theoretical distribution (a.k.a, Monte Carlo; Bolker 2008).
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @author Kyle Rachles added the solution for confidence intervals.
#'
#' @references Bolker, B. M. 2008. Ecological Models and Data in R. Princeton University Press, New Jersey.
#'
#' @section testing:  Tested against the commonly known relationship that a Z of 0.693 equals 50% A.  For example page 204 in Ogle, D.H. 2016. Introductory Fisheries Analyses with R. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @examples
#'
#' Z2A(0.69)
#'
#' Z2A(0.69, 0.1)
#'
#' Z2A(0.69, 0.1, 0.99)
#'
#' @export
Z2A <- function(Z, SE_Z = NA, CI = 0.95){
original_digits <- options()$digits
options(digits = 3)

Z <- abs(Z) #Z could be entered as positive or negative, this makes sure it is positive from this point out

if(!is.na(SE_Z)){
  Z_Boot <- vector()  #Empty Shell for bootstrapped Z estimates
  for(i in 1:10000){
    Shape <- Z^2 / SE_Z^2  # Shape parameter for Gamma distribution (Bolker 2008)
    Rate <- Z / SE_Z^2    # Rate parameter for Gamma distribution (Bolker 2008; from J. Hightower)
    Z_Boot[i] <- rgamma(1, Shape, Rate) # Create 10,000 bootstrapped Z estimates assuming Gamma distributed data
    }
    UCI <-   quantile(Z_Boot, probs=CI + ((1 - CI)/2))
    LCI <-   quantile(Z_Boot, probs=(1 - CI)/2)
  }else{
     UCI <-   NA
     LCI <-   NA
}

  A <- 1 - exp(-Z)
  SE_A <- (1 - A) * SE_Z

  Z_Stats <- c(Z, SE_Z, LCI, UCI)
  names(Z_Stats) <- c('Z', 'SE(Z)', paste0('Low', CI * 100, 'CI'), paste0('Up', CI * 100,'CI'))

  A_Stats <- c(A, SE_A, 1 - exp(-LCI), 1 - exp(-UCI))
  A_Stats <- round(100 * A_Stats, 1)
  names(A_Stats) <- c('A_pct', 'SE(A)', paste0('Low', CI * 100,'CI'), paste0('Up',CI * 100,'CI'))

  output <- list(Instantaneous = Z_Stats, Annual = A_Stats)
  return(output)
  options(digits = original_digits)
  }
