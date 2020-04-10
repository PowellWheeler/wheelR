df <- data.frame(A = c(1:2), B = c(3:4))

test_that ("Z2A:", {
  #make sure that 0.693 returns 50
  expect_equal(Z2A(0.693)$Annual[[1]], 50)
  expect_equal(Z2A(-0.693)$Annual[[1]], 50)

  #make sure that only vectors with length = 1 work
  expect_true(is.na(Z2A(df)))
  expect_true(is.na(Z2A(df$A)))
  expect_true(is.na(Z2A(0.693, df$B)))
  expect_true(is.na(Z2A(df$A, df$B)))

  #make sure valid uses result in a list
  expect_true(is.list(Z2A(0.693)))
  expect_true(is.list(Z2A(-0.693)))
  expect_true(is.list(Z2A(0.693, 0.1)))
  expect_true(is.list(Z2A(0.693, 0.1, 0.99)))

  #check if NA when not given three numbers
  expect_true(is.na(Z2A('dog')))
  expect_true(is.na(Z2A(0.69, 'dog', 0.99)))
  expect_true(is.na(Z2A(0.69, 0.1, 'dog')))

  #check if NA when CI <= 0 and >= 1
  expect_true(is.na(Z2A(0.69, 0.1, 1.0)))
  expect_true(is.na(Z2A(0.69, 0.1, 0.0)))

})
