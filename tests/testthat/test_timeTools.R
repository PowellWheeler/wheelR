test_that ("timeTools:", {
  expect_equal(minsec2sec("00:30"), 30)
  expect_equal(minsec2sec("0:30"),  30)
  expect_equal(minsec2sec("1:00"),  60)
  expect_equal(minsec2sec("1:30"),  90)

  expect_equal(now(), Sys.time())

})
