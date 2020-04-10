test_that ("westWing:", {
  #Check bad Input
  expect_true(is.na(westWing(NA)))
  expect_true(is.na(westWing('dog')))
  expect_true(is.na(westWing(1:10)))

  expect_true(is.vector(westWing()))
  expect_equal(length(westWing()), 1)
  expect_equal(length(westWing(100)), 100)
})
