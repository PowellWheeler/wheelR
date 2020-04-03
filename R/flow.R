#' @title Stream flow.
#'
#' @description Calculate stream flow from interval velocity and depth measurements.
#'
#' @param station_ft A numeric column in a data.frame. The transect station points recorded in decimal feet.
#' @param depth_ft A numeric column in a data.frame. The depths at the transect station points recorded in decimal feet.
#' @param vel_ft_per_sec A numeric column in a data.frame. The depths at the transect station points recorded in decimal feet.
#' @param stream Character string for the stream name. This information is only used to add metadata to the output and can be left blank. If you reference a data.frame column, then the function uses the only the value in the first row.
#' @param site Character string for site name. This information is only used to add metadata to the output and can be left blank. If you reference a data.frame column, then the function uses the value in the first row.
#' @param date Character string for date. This information is only used to add metadata to the output and can be left blank. If you reference a data.frame column, then function uses only the value in the first row.
#'
#' @return A list including the Stream Name, Site, Date, Transect Width (ft), Transect Weighted Mean Depth (ft), Transect Cross Sectional Area (sq_ft), Transect Weighted Mean Velocity (ft/s), Interval Calculations, and Discharge (cfs, cms, gps)
#'
#' @section testing: #I checked this against Brady Dodd's Excel Sheet with Data from Peckerwood Creek, NC.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references This uses the Midsection Method from Turnipseed, D. P., and Sauer, V. B. 2010. Discharge measurements at gaging stations. U.S. Geological Survey, Washington.
#'
#' @examples
#' ## Read a CSV example file internal to wheelR package.
#' peckerwood <- read.csv(system.file("extdata", "peckerwood.csv", package = 'wheelR'), na='.', stringsAsFactors=FALSE)))
#'
#' ## In this example the following three calls produce the same results.
#'
#' flow(station_ft = peckerwood$station_ft, depth_ft = peckerwood$depth_ft, vel_ft_per_sec=peckerwood$vel_ft_per_sec, stream="peckerwood", site="site", date="date")
#' flow(station_ft = peckerwood$station_ft, depth_ft = peckerwood$depth_ft, vel_ft_per_sec=peckerwood$vel_ft_per_sec, stream=peckerwood$stream, site=peckerwood$site, date=peckerwood$date)
#' flow(peckerwood$station_ft, peckerwood$depth_ft, peckerwood$vel_ft_per_sec, peckerwood$stream, peckerwood$site, peckerwood$date)
#'
#' @export
flow <- function(station_ft, depth_ft, vel_ft_per_sec, stream='None', site='None', date='None'){

df <- data.frame(stream = stream, site = site, date = date, station_ft = station_ft, depth_ft = depth_ft, vel_ft_per_sec = vel_ft_per_sec)
#warning("If velocity measurement are wrong, its likely a factor because you forgot na='.' in the read_csv")

df$vel_ft_per_sec[is.na(df$vel_ft_per_sec)] <- 0 #replace any missing velocity measurements with Zero - NAs might be recorded for LEW and REW.

first_row <- 1
last_row <- dim(df)[1]
min_station <- min(df[,'station_ft'])
max_station <- max(df[,'station_ft'])

intervals <- data.frame('cell' = 1:last_row,'start_ft' = NA, 'station_ft' = NA, 'end_ft' = NA, 'depth_ft' = NA, 'vel_ft_per_sec' = NA) #initalize a df for interval analysis

for (row in first_row:last_row){
  intervals$start_ft[row]         <- ifelse(row != first_row, df[row - 1, 'station_ft'], df[row, 'station_ft']) #row=1 is special because it starts on itself
  intervals$end_ft[row]           <- ifelse(row != last_row, df[row + 1, 'station_ft'], df[row, 'station_ft']) #the last row is special because it ends on itself
  intervals$station_ft[row]       <- df[row, 'station_ft']
  intervals$depth_ft[row]         <- df[row, 'depth_ft']
  intervals$vel_ft_per_sec[row]   <- df[row, 'vel_ft_per_sec']
}

intervals$width_ft                  <- (intervals$end_ft - intervals$start_ft) / 2 #this is the key to this method.  The intervals are overwide because they span 3 stations, but then you divide by 2.
intervals$area_sqft                 <- intervals$width_ft * intervals$depth_ft
intervals$discharge_cfs             <- intervals$area_sqft * intervals$vel_ft_per_sec
total_discharge_cfs                 <- sum(intervals$discharge_cfs)
intervals$discharge_pct             <- intervals$discharge_cfs / total_discharge_cfs
transect_width_ft                   <- max_station - min_station
transect_mean_depth_ft              <- mean(rep(intervals$depth_ft, trunc(intervals$width_ft * 100))) # this is an average of depth that is weighted by the cell width
transect_xsection_area_sqft         <- transect_mean_depth_ft * transect_width_ft
transect_mean_velocity_ft_per_sec   <- mean(rep(intervals$vel_ft_per_sec, trunc(intervals$width_ft * 100))) # this is an average of velocity that is weighted by the cell width
discharge_measurements              <- c('cfs' = total_discharge_cfs,'cms' = total_discharge_cfs * 0.0283168,'gps' = total_discharge_cfs * 7.4805)

output <- list('Stream Name' = as.character(df$stream[1]),'Site' = as.character(df$site[1]),'Date' = as.character(df$date[1]),'Transect Width (ft)' = transect_width_ft,
   'Transect Weighted Mean Depth (ft)' = transect_mean_depth_ft, 'Transect Cross Sectional Area (sq_ft)' = transect_xsection_area_sqft,
   'Transect Weighted Mean Velocity (ft/s)' = transect_mean_velocity_ft_per_sec,
   'Interval Calculations' = intervals, 'Discharge' = discharge_measurements)
return(output)
}
