test_that ("ddmm2dd:", {

#test values that should work and produce an output object
expect_true (is.vector(ddmm2dd(3857.5634)))
expect_true (is.vector(ddmm2dd(17959.9999)))
expect_true (is.vector(ddmm2dd(0000.0000)))
expect_true (is.vector(ddmm2dd(1.5634)))

#check known values from http://www.earthpoint.us/Convert.aspx
expect_equal(ddmm2dd(3857.5634),38.959390)
expect_equal(ddmm2dd(09515.92890),95.265483)

#check that handels negative values correctly
expect_equal(ddmm2dd(-3857.5634), -38.959390)
expect_equal(ddmm2dd(-09515.92890), -95.265483)

#Check that handels NA values
expect_true (is.na(ddmm2dd(NA)))

#Bad input values
expect_true (is.na(ddmm2dd(18100.0000))) #out of range value for degrees
expect_true (is.na(ddmm2dd(17961.0000))) #out of range value for mins
})
