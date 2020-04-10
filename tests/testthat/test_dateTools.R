test_that ("dateTools:", {
 #ordinalDate tests
  expect_equal(ordinalDate(as.Date("12/31/2000", format = '%m/%d/%Y')), 365)
  expect_equal(ordinalDate(as.Date("1/1/00", format = '%m/%d/%y')), 1)
  expect_true(is.na(ordinalDate(as.Date("2/29/2000", format = '%m/%d/%Y'))))
  expect_equal(ordinalDate(as.Date("07/14/1976", format = '%m/%d/%Y')), 195)
  expect_equal(ordinalDate(as.Date("07-14-1976", format = '%m-%d-%Y')), 195)
  expect_equal(ordinalDate(as.Date("14/7/1976", format = '%d/%m/%Y')), 195)

  #daysFromToday tests
  expect_equal(daysFromToday(90), Sys.Date() + 90)

  #today tests
  expect_equal(today(), Sys.Date())
  expect_equal(today('call jenny 8675309'), Sys.Date()) #should ignore arguments without generating errors
  expect_equal(today(8675309), Sys.Date()) #should ignore arguments without generating errors
  expect_equal(today(NA), Sys.Date()) #should ignore arguments without generating errors

  #yy2yyyy tests
  expect_equal(yy2yyyy("1/1/1990"),   "1/1/1990")
  expect_equal(yy2yyyy("01/01/1990"), "01/01/1990")
  expect_equal(yy2yyyy("1/1/90"),     "1/1/1990")
  expect_equal(yy2yyyy("1/01/90"),    "1/01/1990")

})
