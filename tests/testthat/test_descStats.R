#Im going to test if a vector using my functions with missing values gets the same result as base R functions without missing values.
# I'm simulaneously testing the function and it's ability to discard missing values.

test.vector <- c(0:10, NA, NaN, NA, NaN)
expected.vector <- c(0:10)

test_that ("descStats messages:", {
  expect_equal(N_Obs(test.vector), length(expected.vector))
  expect_equal(N_Zero(test.vector), length(expected.vector[expected.vector == 0]))
  expect_equal(N_NA(test.vector), length(test.vector) - length(expected.vector))
  expect_equal(Sum(test.vector), sum(expected.vector))
  expect_equal(Mean(test.vector), mean(expected.vector))
  expect_equal(Median(test.vector), median(expected.vector))
  expect_equal(Min(test.vector), min(expected.vector))
  expect_equal(Max(test.vector), max(expected.vector))
  expect_equal(Var(test.vector), var(expected.vector))
  expect_equal(SD(test.vector), sd(expected.vector))
  expect_equal(CV(test.vector), sd(expected.vector) / mean(expected.vector) * 100)
  expect_equal(SE(test.vector), sd(expected.vector) / sqrt(length(expected.vector)))
  #I calculate confidence intervals with a formula, but I'm going to compare to t.test()
  expect_equal(L95CI(test.vector), (stats::t.test(expected.vector))$conf.int[1])
  expect_equal(U95CI(test.vector), (stats::t.test(expected.vector))$conf.int[2])
  expect_equal(CI95(test.vector),  (stats::t.test(expected.vector))$conf.int[c(1,2)])
  # I didn't test sumFun and sumFunShort because they are collections of previously tested functions
})
