testVector <- 1:100
testDF <- data.frame(A = LETTERS, B = 1:26)


test_that ("terminalTools:", {

expect_true(is.na(colIdx(testVector)))
expect_true(is.vector(colIdx(testDF)))

expect_true(is.na(dfScan(testVector)))
expect_true(is.na(dfScan(testDF, 2)))
expect_true(is.data.frame(dfScan(testDF)))
expect_true(is.data.frame(dfScan(testDF,3)))

expect_true(is.null(basicSysInfo()))
expect_true(is.null(basicSysInfo(4))) #should ignore arguements

})
