test_that ("quick2sample:", {

A <- rnorm (n = 20, mean = 100, sd = 20)
  A_Mean <- mean(A)
  A_N    <- length(A)
  A_SE   <- sd(A)/sqrt(A_N)
B <- rnorm (n = 20, mean = 100, sd = 20)
  B_Mean <- mean(B)
  B_N    <- length(B)
  B_SE   <- sd(B)/sqrt(B_N)
C <- rnorm (n = 20, mean = 130, sd = 20)
  C_Mean <- mean(C)
  C_N    <- length(C)
  C_SE   <- sd(C)/sqrt(C_N)

q2s_out <- q2s(A_Mean, B_Mean, A_SE, B_SE, A_N, B_N, CI = 0.95)
t.test_out <- t.test (A, B, var.equal = TRUE, conf.level = 0.95)
var.test_out <- var.test (A, B)

#Compare q2s to a proper t.test
# I had problems with t.test() rounding results to a seemlingly erratic number of digits which caused the tests to fail.  Exact to 6 digits is fine.
expect_equal(round(q2s_out$t_Test[['t']], 6), round(t.test_out$statistic[['t']], 6))
expect_equal(q2s_out$t_Test[['df']], t.test_out$parameter[['df']])
expect_equal(round(q2s_out$t_Test[['p']], 6), round(t.test_out$p.value, 6))
expect_equal(round(q2s_out$Effect_Size_Absolute[['Low95CI']], 6), round(t.test_out$conf.int[[1]], 6))
expect_equal(round(q2s_out$Effect_Size_Absolute[['Up95CI']],  6), round(t.test_out$conf.int[[2]], 6))

#Compare q2s to a proper variance test - I basically did a variance test by hand for no reason
expect_equal(round(q2s_out$F_Ratio_Test_For_Equal_Variances[['F']], 2), round(var.test_out$statistic[['F']], 2))
expect_equal(round(q2s_out$F_Ratio_Test_For_Equal_Variances[['p']], 6), round(var.test_out$p.value, 6))

#Repeat with a data likely to give a significant difference and a different confidence interval.
q2s_out <- q2s(A_Mean, C_Mean, A_SE, C_SE, A_N, C_N, CI = 0.90)
t.test_out <- t.test (A, C, var.equal = TRUE, conf.level = 0.90)
var.test_out <- var.test (A, C)

expect_equal(round(q2s_out$t_Test[['t']], 6), round(t.test_out$statistic[['t']], 6))
expect_equal(q2s_out$t_Test[['df']], t.test_out$parameter[['df']])
expect_equal(round(q2s_out$t_Test[['p']], 6), round(t.test_out$p.value, 6))
expect_equal(round(q2s_out$Effect_Size_Absolute[['Low90CI']], 6), round(t.test_out$conf.int[[1]], 6))
expect_equal(round(q2s_out$Effect_Size_Absolute[['Up90CI']],  6), round(t.test_out$conf.int[[2]], 6))

expect_equal(round(q2s_out$F_Ratio_Test_For_Equal_Variances[['F']], 2), round(var.test_out$statistic[['F']], 2))
expect_equal(round(q2s_out$F_Ratio_Test_For_Equal_Variances[['p']], 6), round(var.test_out$p.value, 6))

})
