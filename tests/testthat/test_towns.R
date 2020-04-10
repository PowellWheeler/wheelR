test_that ("towns:", {
  expect_true(file.exists('~/git/wheelR/data/towns.rda'))
  expect_true(is.data.frame(towns))
})
