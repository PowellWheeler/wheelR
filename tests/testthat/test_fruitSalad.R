test_that ("fruitSalad:", {
  expect_true(is.data.frame(fruitSalad()))
  expect_equal(dim(fruitSalad()), c(20,14))
})
