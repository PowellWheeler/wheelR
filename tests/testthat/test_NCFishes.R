test_that ("NCFishes:", {
  expect_true(file.exists('~/git/wheelR/data/NCFishes.rda'))
  expect_true(is.data.frame(NCFishes))
})
