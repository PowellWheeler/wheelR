test_that ("Z2A:", {
  expect_equal(Z2A(0.693)$Annual[[1]], 50)
  expect_equal(Z2A(-0.693)$Annual[[1]], 50)
})
