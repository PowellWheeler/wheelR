library(ggmap)

# findCoords pull coordinates from Google Search
# the coordiantes here are from wikipedia

test_that ("findCoords:", {
  expect_equal(round(findCoords("Four Corners"), digits = 4), round(data.frame (lon = -109.0451507, lat = 36.9989402), digits=4))
})
