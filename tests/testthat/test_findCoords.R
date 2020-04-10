library(ggmap)

df <- data.frame(A = c('dog', 'cat'), B = c('fish', 'bird'))
# findCoords pull coordinates from Google Search
# the coordiantes here are from wikipedia

test_that ("findCoords:", {

#Check bad Input
  expect_true(is.na(findCoords(c("The White House", "Virginia Tech")))) #dosen't take vectors of multiple places
  expect_true(is.na(findCoords(df))) #dosen't work on data.frames

#I need some more examples
  expect_equal(round(findCoords("Four Corners Monument"), digits = 4), round(data.frame (lon = -109.0451507, lat = 36.9989402), digits = 4))
})
