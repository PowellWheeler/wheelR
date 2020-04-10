#' @title Simple data.frame for testing functions and unfamiliar commands.
#'
#' @description Creates a simple data.frame with a variety of variable types for testing purposes.
#'
#' @param N Numeric vector with length = 1 to describe the number of rows in the output data.frame. Defaults to 20.
#'
#' @return A data.frame with information about fictitious salads.
#'
#' @author A. Powell Wheeler, \email{powell.wheeler@@gmail.com}
#'
#' @examples
#' fruitSalad()
#' fruitSalad(50)
#'
#' @export
fruitSalad <- function(N = 20){

if((is.vector(N)) && (length(N) == 1) && (is.numeric(N))){

  #a function to produce random five digit strings of letters
  nameFun <- function (N=5) paste(replicate(N, sample(letters, 1)), collapse = "")

  start <- as.Date('01/01/1970', format = '%m/%d/%Y')
  stop  <- Sys.Date()

  fruit      <- sample(c('apple', 'pear', 'banana', 'kiwi'), N, replace = TRUE)
  greens     <- sample(c('iceberg', 'romaine', 'springMix'), N, replace = TRUE)
  vegetable  <- sample(c('carrot', 'cucumber', 'broccoli'),  N, replace = TRUE)
  meat       <- sample(c(TRUE, FALSE),                       N, replace = TRUE)
  nuts       <- sample(c('pineNut', 'walnut', 'cashews'),    N, replace = TRUE)
  size       <- sample(c('large', 'small'),                  N, replace = TRUE)
  rank       <- sample(1:N, replace = FALSE)

  df         <- data.frame (fruit = fruit, greens = greens, vegetable = vegetable, meat = meat, nuts = nuts, size = size, rank = rank, stringsAsFactors = TRUE)

  df$date       <- sample(as.Date(seq(start:stop), origin = '1970-01-01'), N, replace = TRUE)
  df$short.code <- replicate (N, nameFun(5))
  df$long.code  <- replicate (N, nameFun(20))

  df$meatType   <- ifelse(df$meat ==  TRUE,   replicate(N, sample(c('chicken', 'salmon'),1)),NA)
  df$weight     <- ifelse(df$size == 'large', replicate(N, rnorm(1, mean = 1,  sd = 0.2)), replicate(N, rnorm(1, mean=0.5, sd=0.1)))
  df$diameter   <- ifelse(df$size == 'large', replicate(N, rnorm(1, mean = 14, sd = 2.0)), replicate(N, rnorm(1, mean=7.0, sd=1.0)))

  #add some random missing values
  df$diameter  [sample(1:N, floor(N / 3),  replace = FALSE)] <- NA
  df$vegetable [sample(1:N, floor(N / 3),  replace = FALSE)] <- NA

  df$bad.data <- sample(c(0/0, 1/0, -1/0, NA), N, replace = TRUE)

  df <- df[, c('date', 'short.code', 'long.code', 'fruit', 'greens', 'vegetable', 'meat', 'meatType', 'nuts', 'size', 'diameter', 'weight', 'rank', 'bad.data')]

  return(df)
} else {
  return(NA)
}
}
