rm(list = ls())

peckerwood <- read.csv('~/git/wheelR/tests/peckerwood_1.235cfs.csv', stringsAsFactors = FALSE, na = '.')

test_that ("flow:", {

#Make sure that returns NA when any one of the depth, velocity, and station arguements are missing
A <- flow(NA, peckerwood$depth_ft, peckerwood$vel_ft_per_sec, peckerwood$stream, peckerwood$site, peckerwood$date)
expect_true(is.na(A))

B <- flow(peckerwood$station_ft, NA, peckerwood$vel_ft_per_sec, peckerwood$stream, peckerwood$site, peckerwood$date)
expect_true(is.na(B))

C <- flow(peckerwood$station_ft, peckerwood$depth_ft, NA, peckerwood$stream, peckerwood$site, peckerwood$date)
expect_true(is.na(C))

#These examples should work
out <- flow(peckerwood$station_ft, peckerwood$depth_ft, peckerwood$vel_ft_per_sec, peckerwood$stream, peckerwood$site, peckerwood$date)

expect_true(is.list(out))

expect_equal(round(out$Discharge[['cfs']],4), 1.2350)
})
