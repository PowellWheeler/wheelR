test_that ("sysdata:", {
  expect_true(file.exists('~/git/wheelR/R/sysdata.rda'))

  expect_true(is.data.frame(ordinalLookup))
  expect_equal(dim(ordinalLookup), c(366,2))


  expect_true(is.list(wsLookup))

  expect_true(is.vector(sumFunLabels))
  expect_true(is.vector(sumFunShortLabels))
})
