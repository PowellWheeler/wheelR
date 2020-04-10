test_that ("fruitSalad:", {
  #Check bad Input
  expect_true(is.na(fruitSalad('dog')))
  expect_true(is.na(fruitSalad(1:10)))

  expect_true(is.data.frame(fruitSalad()))
  expect_equal(dim(fruitSalad()), c(20,14))
  expect_equal(dim(fruitSalad(100)), c(100,14))
})
