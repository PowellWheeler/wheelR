test_that ("dateTools:", {
  expect_equal(ordinalDate(as.Date("12/31/2000", format = '%m/%d/%Y')), 365)
  expect_equal(ordinalDate(as.Date("1/1/00", format = '%m/%d/%y')), 1)
  identical(ordinalDate(as.Date("2/29/2000", format = '%d/%m/%Y')), as.integer(NA))
  expect_equal(ordinalDate(as.Date("07/14/1976", format = '%m/%d/%Y')), 195)
  expect_equal(ordinalDate(as.Date("07-14-1976", format = '%m-%d-%Y')), 195)
  expect_equal(ordinalDate(as.Date("14/7/1976", format = '%d/%m/%Y')), 195)

  expect_equal(daysFromToday(90), Sys.Date() + 90)

  expect_equal(today(), Sys.Date())

  expect_equal(yy2yyyy("1/1/1990"),   "1/1/1990")
  expect_equal(yy2yyyy("01/01/1990"), "01/01/1990")
  expect_equal(yy2yyyy("1/1/90"),     "1/1/1990")
  expect_equal(yy2yyyy("1/01/90"),    "1/01/1990")
  # expect_equal(yy2yyyy("12/25/1990"), "12/25/1990")
  # expect_equal(yy2yyyy("12/25/90"),   "12/25/1990")
  # expect_equal(yy2yyyy("12/25/2010"), "12/25/2010")
  # expect_equal(yy2yyyy("12/25/10"),   "12/25/2010")



})
