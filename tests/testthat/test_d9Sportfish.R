test_that ("d9SportfishVector:", {
  expect_true(file.exists('~/git/wheelR/data/d9SportfishVector.rda'))
  expect_true(is.vector(d9SportfishVector))
})
