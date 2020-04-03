test_that ("flow:", {

peckerwood <- read.csv('~/git/wheelR/tests/peckerwood_1.235cfs.csv', stringsAsFactors=FALSE, na = '.')

out <- flow(peckerwood$station_ft, peckerwood$depth_ft, peckerwood$vel_ft_per_sec, peckerwood$stream, peckerwood$site, peckerwood$date)

expect_equal(round(out$Discharge[['cfs']],4), 1.2350)
})
