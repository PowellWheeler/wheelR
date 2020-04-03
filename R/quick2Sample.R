#' @name Quick Two Sample.
#'
#' @title Fast and easy two-sample inferrential tests for summarized data.
#'
#' @description Function runs a simple two-tailed two-sample t-test and z-test on summary data (estimate, standard error, and sample size). This is useful for analyzing summarized data when the raw data is not longer available.  For example, angler creel survey reports typically just include a parameter estimate such as an average catch rate and a standard error.
#'
#' @param est_A Numeric vector with length equal 1. The parameter estimate from the first population of interest. This could be a mean or another parameter estimate such as the estimated number of walleye harvested in Lake X.
#' @param est_B Numeric vector with length equal 1. The parameter estimate from second population of interest. This could be a mean or another parameter estimate such as the estimated number of walleye harvested in Lake Y.
#' @param SE_A  Numeric vector with length equal 1. The standard error of the parameter estimate from the first population.
#' @param SE_B  Numeric vector with length equal 1. The standard error of the parameter estimate from the second population.
#' @param N_A   Numeric vector with length equal 1. The sample size from the first population. This is optional and only needed to preform the t-test. The t-test is slightly more powerful than the z-test, especially at low sample sizes.
#' @param N_B   Numeric vector with length equal 1. The sample size from the second population. This is optional and only needed to preform the t-test. The t-test is slightly more powerful than the z-test, especially at low sample sizes.
#' @param CI    Numeric vector of length = 1 for the width of the confidence interval. Defaults to 0.95.
#'
#' @return A list with the results of the z-test and t-test.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @note This is a quick and dirty procedure as a last resort for testing differences when raw data is no longer available but summarized data is. Both the z- and t-test assume that there is a normal distribution of residuals and this is very important for claculating the p-values but cannot be verified when working with summarized data.
#'   However, the tests presented here are somewhat conservative and that will help prevent Type I errors. The z-test is more conservative than the t-test and this function only preforms two-sided tests which are more conservative (by a factor of 2) then single sided-tests.
#'
#' @aliases q2s quick2sample
#'
#' @examples
#' q2s (est_A = 26238, est_B = 77113, SE_A = 3555, SE_B = 13401, N_A = NA, N_B = NA, CI = 0.95)
#' q2s (26238, 77113, 3555, 13401)
#'
#' @export

#for checking: https://epitools.ausvet.com.au/twosamplettest
q2s <- function(est_A, est_B, SE_A, SE_B, N_A = NA, N_B = NA, CI = 0.95){
pre_digits <- options()$digits
options(digits = 4)

#Initial Calculations
pooled_SE <- sqrt(SE_A^2 + SE_B^2)
if(is.na(N_A) & is.na(N_B)) { #if N is unknown use z else use t for conf ints
    CI_Width <- qnorm(1 - (1 - CI) / 2)
  }else{
    CI_Width <- qt((1 - (1 - CI) / 2), N_A + N_B - 2)
    }

#Sample Summaries
SD_A <- SE_A * sqrt(N_A)
SD_B <- SE_B * sqrt(N_B)
VAR_A <- SD_A^2
VAR_B <- SD_B^2

output_names_samples <-c('value', 'VAR', 'SD', 'SE', 'N', paste0('Low', CI * 100, 'CI'), paste0('Up', CI * 100, 'CI'))
A_out <- c(est_A, VAR_A, SD_A, SE_A, N_A, est_A - (CI_Width * SE_A), est_A + (CI_Width * SE_A))
names(A_out) <- output_names_samples
B_out <- c(est_B, VAR_B, SD_B, SE_B, N_B, est_B - (CI_Width * SE_B), est_B + (CI_Width * SE_B))
names(B_out) <- output_names_samples

## Absolute Effect Sizes
effect_size     <- est_A - est_B
sgn_effect_size <- sign(effect_size)
abs_effect_size <- abs(effect_size)

effect_CI_up  <- effect_size + CI_Width * pooled_SE
effect_CI_low <- effect_size - CI_Width * pooled_SE

output_names_effect <-c('absolute', 'A-B', paste0('Low', CI * 100, 'CI'), paste0('Up', CI * 100, 'CI'))
effect_out <- c(abs_effect_size, effect_size, effect_CI_low, effect_CI_up)
names(effect_out) <- output_names_effect

## Percent Effect Sizes
effect_size_pct   <- 100 * ((max(est_A, est_B) - min(est_A, est_B)) / min(est_A, est_B))
effect_pct_CI_up  <- 100 * (effect_CI_up / min(est_A, est_B))
effect_pct_CI_low <- 100 * (effect_CI_low / min(est_A, est_B))

output_names_effect_pct <-c('percent', paste0('Low', CI * 100, 'CI'), paste0('Up', CI * 100, 'CI'))
effect_pct_out <- c(effect_size_pct, effect_pct_CI_low, effect_pct_CI_up)
names(effect_pct_out) <- output_names_effect_pct

## F-ratio Test for Variances
F_stat <- VAR_A / VAR_B
F_df_A <- N_A - 1
F_df_B <- N_B - 1
F_p_value  <- 2 * min (pf(F_stat, F_df_A, F_df_B), 1 - pf(F_stat, F_df_A, F_df_B)) # according to http://www.stat.umn.edu/geyer/old03/5102/examp/rp.html, p-value for the 2-tailed test is twice which ever is smaller of the left or right tail tests.
F_test_out <- c('F' = F_stat, 'df_A' = F_df_A, 'df_B' = F_df_B, 'p' = F_p_value)

## Z-test
test_stat <- (est_A - est_B) / pooled_SE #t- and z-tests share the same test statistic

Z_p_value <- 2 * pnorm(test_stat, 0, 1, lower.tail = FALSE) #function defaults to one-tailed test, multiplying by 2 makes it a two-tailed test.
Z_test_out <- c('Z' = test_stat, 'p' = Z_p_value)

## t-test
t_df <- N_A + N_B - 2 #degrees of freedom for the t-statistic
t_p_value <- 2 * pt(abs(test_stat), t_df, lower.tail = FALSE) #function defaults to one-tailed test, multiplying by 2 makes it a two-tailed test.
t_test_out <- c('t' = test_stat, 'df' = t_df, 'p' = t_p_value)

output <- list('Estimate_A' = A_out, 'Estimate_B' = B_out, 'Effect_Size_Absolute' = effect_out, 'Effect_Size_Percent' = effect_pct_out, 'F_Ratio_Test_For_Equal_Variances' = F_test_out, 'Z_Test' = Z_test_out, 't_Test' = t_test_out)
options(digits = pre_digits)

return(output)
}
