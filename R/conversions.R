#' @title Convert millimeters to inches
#'
#' @description Convert millimeters to inches.
#'
#' @param x A numeric vector of measurements in millimeters.
#'
#' @return A numeric vector of measurements in inches.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' mm2in <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname mm2in
#'
#' @export
mm2in <- function(x) x *  0.03937007874

#' @title Convert inches to millimeters
#'
#' @description Convert inches to millimeters.
#'
#' @param x A numeric vector of measurements in inches.
#'
#' @return A numeric vector of measurements in millimeters.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' in2mm <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname in2mm
#'
#' @export
in2mm <- function(x) x * 25.4

#' @title Convert grams to pounds
#'
#' @description Convert grams to pounds.
#'
#' @param x A numeric vector of measurements in grams.
#'
#' @return A numeric vector of measurements in pounds.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' g2lb <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname g2lb
#'
#' @export
g2lb <- function (x) x * 0.0022046226218454

#' @title Convert pounds to grams
#'
#' @description Convert pounds to grams.
#'
#' @param x A numeric vector of measurements in pounds.
#'
#' @return A numeric vector of measurements in grams.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' lb2g <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname lb2g
#'
#' @export
lb2g <- function (x) x * 453.59237

#' @title Convert meters to feet
#'
#' @description Convert meters to feet.
#'
#' @param x A numeric vector of measurements in meters.
#'
#' @return A numeric vector of measurements in feet.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' m2ft <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname m2ft
#'
#' @export
m2ft <- function (x) x * 3.280839895

#' @title Convert feet to meters
#'
#' @description Convert feet to meters.
#'
#' @param x A numeric vector of measurement in feet.
#'
#' @return A numeric vector of measurements in meters.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' ft2m <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname ft2m
#'
#' @export
ft2m <- function (x) x * 0.3048

#' @title Convert degrees Farenheit to degrees celcius
#'
#' @description Convert degrees Farenheit to degrees Celcius.
#'
#' @param x A numeric vector of measurements in degrees Farenheit.
#'
#' @return A numeric vector of measurements in degrees Celcius.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' f2c <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname f2c
#'
#' @export
f2c <- function (x) (x - 32) * (5 / 9)

#' @title Convert degrees Celcius to degrees Farenheit
#'
#' @description Convert degrees Celcius to degrees Farenheit.
#'
#' @param x A numeric vector of measurements in degrees Celcius.
#'
#' @return A numeric vector of measurements in degrees Farenheit.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' c2f <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname c2f
#'
#' @export
c2f <- function (x) (x / (5 / 9)) + 32

#' @title Convert pounds to kilograms
#'
#' @description Convert pounds to kilograms.
#'
#' @param x A numeric vector of measurements in pounds.
#'
#' @return A numeric vector of measurements in kilograms.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' lb2kg <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname lb2kg
#'
#' @export
lb2kg <-function (x) x * 0.45359237

#' @title Convert kilograms to pounds
#'
#' @description Convert kilograms to pounds.
#'
#' @param x A numeric vector of measurements in kilograms.
#'
#' @return A numeric vector of measurements in pounds.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' kg2lb <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname kg2lb
#'
#' @export
kg2lb <-function (x) x * 2.2046226218

#' @title Convert cubic feet to cubic meters
#'
#' @description Convert cubic feet to cubic meters.
#'
#' @param x A numeric vector of measurements in cubic feet.
#'
#' @return A numeric vector of measurements in cubic meters.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' cubft2cubm <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname cubft2cubm
#'
#' @export
cubft2cubm <-function (x) x * 0.028316846592

#' @title Convert cubic meters to cubic feet
#'
#' @description Convert cubic meters to cubic feet.
#'
#' @param x A numeric vector of measurements in cubic meters.
#'
#' @return A numeric vector of measurements in cubic feet.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' cubm2cubft <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname cubm2cubft
#'
#' @export
cubm2cubft <-function (x) x * 35.314666721

#' @title Convert cubic feet to gallons
#'
#' @description Convert cubic feet to gallons.
#'
#' @param x A numeric vector of measurements in cubic feet.
#'
#' @return A numeric vector of measurements in cubic meters.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' cubft2gal <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname cubft2gal
#'
#' @export
cubft2gal <-function (x) x * 7.4805194805

#' @title Convert gallons to cubic feet
#'
#' @description Convert gallons to cubic feet.
#'
#' @param x A numeric vector of measurements in gallons.
#'
#' @return A numeric vector of measurements in cubic feet.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' gal2cubft <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname gal2cubft
#'
#' @export
gal2cubft <-function (x) x * 0.13368055556

#' @title Convert kilometers to miles
#'
#' @description Convert kilometers to miles.
#'
#' @param x A numeric vector of measurements in kilometers.
#'
#' @return A numeric vector of measurements in miles.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' km2mi <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname km2mi
#'
#' @export
km2mi <-function (x) x * 0.62137119224

#' @title Convert miles to kilometers
#'
#' @description Convert miles to kilometers.
#'
#' @param x A numeric vector of measurements in miles.
#'
#' @return A numeric vector of measurements in kilometers.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' mi2km <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname mi2km
#'
#' @export
mi2km <-function (x) x * 1.609344

#' @title Convert hectares to acres
#'
#' @description Convert hectares to acres.
#'
#' @param x A numeric vector of measurements in hectares.
#'
#' @return A numeric vector of measurements in acres.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' ha2ac <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname ha2ac
#'
#' @export
ha2ac <-function (x) x * 2.4710538147

#' @title Convert acres to hectares
#'
#' @description Convert acres to hectares.
#'
#' @param x A numeric vector of measurements in acres.
#'
#' @return A numeric vector of measurements in hectares.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' ac2ha <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname ac2ha
#'
#' @export
ac2ha <-function (x) x * 0.40468564224

#' @title Convert radians to degrees
#'
#' @description Convert radians to degrees.
#'
#' @param x A numeric vector of measurements in radians.
#'
#' @return A numeric vector of measurements in degrees.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' rad2deg <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname rad2deg
#'
#' @export
rad2deg <- function(rad) rad * 180 / pi

#' @title Convert degrees to radians
#'
#' @description Convert degrees to radians.
#'
#' @param x A numeric vector of measurements in degrees.
#'
#' @return A numeric vector of measurements in radians.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @references The converion factor is from www.onlineconversion.com.
#'
#' @family unit conversion functions
#'
#' @examples
#' deg2rad <- c(0, 1, 2, 4, 5, 6, 7, NA, 9, 10)
#'
#' @rdname deg2rad
#'
#' @export
deg2rad <- function(deg) deg * pi / 180
