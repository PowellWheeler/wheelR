sp_c <- c("LMB", "SMB", "SPB")
tl <- c(200, 200, 200)
wt <- c(400L, 400L, 400L)
df <- data.frame(sp_c = sp_c, tl = tl , wt = wt, stringsAsFactors = FALSE)
df$sp_fact <- as.factor(df$sp_c)
test_that ("wr:", {

#check and make sure returns NA for no species match
expect_equal(is.na(wr("LMBX", 129, 10)), TRUE)
expect_equal(is.na(wr("Wendell", 129, 10)), TRUE)

#check and make sure returns NA when missing info
expect_equal(is.na(wr(NA, 450, 963)), TRUE)
expect_equal(is.na(wr("WY", NA, 963)), TRUE)
expect_equal(is.na(wr("WY", 450, NA)), TRUE)
expect_equal(is.na(wr("WY", NA, NA)), TRUE)
expect_equal(is.na(wr("NA", NA, NA)), TRUE)

# does species work as a factor and length and weight work as integers?
expect_equal(round(wr(as.factor("WY"), 450L, 800L), 0), 83)

#make sure that only vectors with length = 1 work
expect_true(is.na(wr(df, df, df)))
expect_true(is.na(wr(df$sp_c, df$tl, df$wt)))
expect_true(is.na(wr("LMB", 200, df$wt)))

#check and make sure returns NA for short fishes
expect_equal(is.na(wr("LMB", 149, 10)), TRUE)
expect_equal(is.na(wr("SMB", 149, 10)), TRUE)
expect_equal(is.na(wr("SPB", 99, 10)), TRUE)
expect_equal(is.na(wr("WY", 149, 10)), TRUE)
expect_equal(is.na(wr("WB", 114, 10)), TRUE)
expect_equal(is.na(wr("SB", 149, 10)), TRUE)
expect_equal(is.na(wr("WBH", 114, 10)), TRUE)
expect_equal(is.na(wr("YP", 99, 10)), TRUE)
expect_equal(is.na(wr("RBT", 119, 10)), TRUE)
expect_equal(is.na(wr("BNT", 139, 10)), TRUE)
expect_equal(is.na(wr("BKT", 119, 10)), TRUE)
expect_equal(is.na(wr("FC", 129, 10)), TRUE)
expect_equal(is.na(wr("CC", 69, 10)), TRUE)
expect_equal(is.na(wr("BCR", 99, 10)), TRUE)
expect_equal(is.na(wr("WCR", 99, 10)), TRUE)
expect_equal(is.na(wr("MK", 379, 10)), TRUE)
expect_equal(is.na(wr("RE", 69, 10)), TRUE)
expect_equal(is.na(wr("RB", 79, 10)), TRUE)
expect_equal(is.na(wr("BG", 79, 10)), TRUE)
expect_equal(is.na(wr("WM", 79, 10)), TRUE)
expect_equal(is.na(wr("GS", 59, 10)), TRUE)
expect_equal(is.na(wr("BKBH", 129, 10)), TRUE)
expect_equal(is.na(wr("BRBH", 129, 10)), TRUE)

# three walleye examples in second edition of the Techniques book
expect_equal(round(wr("WY", 450, 800),0), 83)
expect_equal(round(wr("WY", 450, 963),0), 100)
expect_equal(round(wr("WY", 450, 1090),0), 113)

})
