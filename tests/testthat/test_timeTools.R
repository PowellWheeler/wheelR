test_that ("timeTools:", {
  expect_true(is.na(minsec2sec("9999")))
  expect_true(is.na(minsec2sec(9999)))
  expect_true(is.na(minsec2sec("ABCD")))

  expect_equal(minsec2sec("00:30"), 30)
  expect_equal(minsec2sec("0:30"),  30)
  expect_equal(minsec2sec("1:00"),  60)
  expect_equal(minsec2sec("1:30"),  90)

  expect_equal(now(), Sys.time())
  expect_equal(now(4), Sys.time()) #should disreguard arguements
  expect_equal(now('cat'), Sys.time())#should disreguard arguements
  expect_equal(now(cat), Sys.time())#should disreguard arguements

})
