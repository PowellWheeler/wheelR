#Im going to test if a vector using my functions with missing values gets the same result as base R functions without missing values.
# I'm simulaneously testing the function and it's ability to discard missing values.

test.vector.num <- c(0:10, NA, NaN, NA, NaN)
expected.vector.num <- c(0:10)

test.vector.log  <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA)
expected.vector.log <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)

test.vector.char <- c(LETTERS, NA)
expected.vector.char <- c(LETTERS)

test.df <- data.frame(A = 0:25, B = LETTERS, C = rep(c(NA, NaN), 13))
expected.df <- data.frame(A = 0:25, B = LETTERS)

#test with numeric vector
test_that ("descStats messages:", {
  expect_equal(N_Obs(test.vector.num), length(expected.vector.num))
  expect_equal(N_Zero(test.vector.num), sum(expected.vector.num == 0))
  expect_equal(N_NA(test.vector.num), length(test.vector.num) - length(expected.vector.num))
  expect_equal(Sum(test.vector.num), sum(expected.vector.num))
  expect_equal(Mean(test.vector.num), mean(expected.vector.num))
  expect_equal(Median(test.vector.num), median(expected.vector.num))
  expect_equal(Min(test.vector.num), min(expected.vector.num))
  expect_equal(Max(test.vector.num), max(expected.vector.num))
  expect_equal(Var(test.vector.num), var(expected.vector.num))
  expect_equal(SD(test.vector.num), sd(expected.vector.num))
  expect_equal(CV(test.vector.num), sd(expected.vector.num) / mean(expected.vector.num) * 100)
  expect_equal(SE(test.vector.num), sd(expected.vector.num) / sqrt(length(expected.vector.num)))
  expect_equal(L95CI(test.vector.num), (stats::t.test(expected.vector.num))$conf.int[1])
  expect_equal(U95CI(test.vector.num), (stats::t.test(expected.vector.num))$conf.int[2])
  expect_equal(CI95(test.vector.num),  (stats::t.test(expected.vector.num))$conf.int[c(1,2)])
  # I didn't test sumFun and sumFunShort because they are collections of previously tested functions

  #test with logic vector
  expect_equal(N_Obs(test.vector.log), length(expected.vector.log))
  expect_equal(N_Zero(test.vector.log), sum(expected.vector.log == 0)) #counts FALSEs
  expect_equal(N_NA(test.vector.log), length(test.vector.log) - length(expected.vector.log))
  expect_equal(Sum(test.vector.log), sum(expected.vector.log)) #counts TRUEs
  expect_equal(Mean(test.vector.log), mean(expected.vector.log))

  expect_true(is.na(Median(test.vector.log)))
  expect_true(is.na(Min(test.vector.log)))
  expect_true(is.na(Max(test.vector.log)))
  expect_true(is.na(Var(test.vector.log)))
  expect_true(is.na(SD(test.vector.log)))
  expect_true(is.na(CV(test.vector.log)))
  expect_true(is.na(SE(test.vector.log)))
  expect_true(is.na(L95CI(test.vector.log)))
  expect_true(is.na(U95CI(test.vector.log)))
  expect_true(is.na(CI95(test.vector.log)))

 #test with data.frame
 # these happen to work with a data.frame, so I'm allowing it.
 expect_equal(N_Obs(test.df), length(expected.df[!is.na(expected.df)]))
 # the NA's are the only dimension difference in the two data.frames
 expect_equal(N_NA(test.df), prod(dim(test.df)) - prod(dim(expected.df)))

 # the rest should return NA for a data.frame
 expect_true(is.na(N_Zero(test.df)))
 expect_true(is.na(Sum(test.df)))
 expect_true(is.na(Mean(test.df)))
 expect_true(is.na(Median(test.df)))
 expect_true(is.na(Min(test.df)))
 expect_true(is.na(Max(test.df)))
 expect_true(is.na(Var(test.df)))
 expect_true(is.na(SD(test.df)))
 expect_true(is.na(CV(test.df)))
 expect_true(is.na(SE(test.df)))
 expect_true(is.na(L95CI(test.df)))
 expect_true(is.na(U95CI(test.df)))
 expect_true(is.na(CI95(test.df)))
 expect_true(is.na(sumFun(test.df)))
 expect_true(is.na(sumFunShort(test.df)))
})
