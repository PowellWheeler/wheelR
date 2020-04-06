# I want to make a df of all the cities and towns in the zipcode dataframe
# For the cities with multiple zipcodes, I want the average GPS coords of the zipcodes combined
library(doBy)
library(zipcode)
data(zipcode)

df <- zipcode

df <- na.omit(df) # there are 647 towns without coordinates

df$city.and.state <-paste(df$city,', ', df$state,sep='') #make a variable that is a combo of city and state.  There could be the same city name in multiple states

cities.with.multiple.zips <- unique(df$city.and.state[duplicated(df$city.and.state)]) #make a vector of all the cites with multiple zipcodes

df.clean.cities <- df[!df$city.and.state %in% cities.with.multiple.zips,] # make a dataframe without any problem hometowns
df.clean.cities <- df.clean.cities[,c(2,3,6,4,5)]

df.problem.cities <- df[df$city.and.state %in% cities.with.multiple.zips,] #make a new data.frame with all the problem hometowns

df.fixed.cities <- summaryBy(latitude + longitude ~ city + state + city.and.state, data = df.problem.cities, FUN=mean)
colnames(df.fixed.cities)[c(4,5)] <- c('latitude','longitude')

city.and.town.coords <- rbind(df.clean.cities, df.fixed.cities)
city.and.town.coords <- city.and.town.coords[order(city.and.town.coords$city,city.and.town.coords$state),]
rownames(city.and.town.coords) <- NULL

towns <- city.and.town.coords

save(towns,file='~/git/wheelR/data/towns.rda')
