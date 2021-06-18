#' Determine the width and height of the ISO216 A and B series of paper
#'
#' @param n is an integer from 0 to 10
#' @param floor returns an integer by taking the floor of the result
#'
#' @return a vector containing the width and height
#'
#' @aliases aseries bseries papersize
#' @examples
#' aseries(4) # Return A4 size paper
#' bseries(5) # Return B5 size paper
#' @export
#' @describeIn aseries A paper size
aseries = function(n, floor = TRUE) {
  # Function to calculate ISO216 A Series
  if(!(is.numeric(n) | is.integer(n)) | n > 11) stop("Provide a number from 0 to 10.")
  n = as.integer(n)
  wodd = function(n) {
    1 / (2 ^ ((n + 1)/2)) * 1000 * sqrt(sqrt(2))
  }
  hodd = function(n) {
    1 / (2 ^ ((n - 1)/2)) * 1000 / sqrt(sqrt(2))
  }
  weven = function(n) {
    1 / (2 ^ (n / 2)) * 1000 / sqrt(sqrt(2))
  }
  heven = function(n) {
    1 / (2 ^ (n / 2)) * 1000 * sqrt(sqrt(2))
  }
  if(n %% 2)  {
    w = wodd(n)
    h = hodd(n)
  } else {
    w = weven(n)
    h = heven(n)
  }
  if(floor) {return(floor(c("width" = w,"height" = h)))}
  return(c("width" = w,"height" = h))
}

#' @describeIn aseries B paper sizes
#' @export
bseries = function(n) {
  # Function to calculate ISO216 B Series
  if(!(is.numeric(n) | is.integer(n)) | n > 11) stop("Provide a number from 0 to 10.")
  n = as.integer(n)
  if(n == 0) {
    wh = c(1000, 1414)
  } else {
    wh  = aseries(n, floor = F)
    wh2 = aseries(n-1, floor = F)
    w = sqrt(wh[1] * wh2[1])
    h = sqrt(wh[2] * wh2[2])
  }
  return(floor(c("width" = w,"height" = h)))
}
