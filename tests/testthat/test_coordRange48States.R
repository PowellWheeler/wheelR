test_that ("coordRange48States", {
  expect_true(file.exists('~/git/wheelR/data/coordRange48States.rda'))
  expect_true(is.data.frame(coordRange48States))
  expect_equal(dim(coordRange48States), c(49,4))
})
