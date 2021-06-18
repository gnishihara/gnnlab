#' Calculate the standard error of the values in x.
#' If na.rm is TRUE then the missing values are removed before
#' calcuating the error (default).
#'
#' @param x a numeric vector
#' @param na.rm logical. Should missing values be removed? Default na.rm = TRUE.
#'
#' @return a vector containing the width and height
#'
#' @examples
#' se(c(4,1,2,3,4))
#' @export

se = function(x, na.rm = TRUE) {

  if(!is.numeric(x)) warning("Provide numeric values.")
  N = length(x)

  if(any(is.na(x))) {
    N = N - sum(is.na(x))
    return(sd(x, na.rm=TRUE) / sqrt(N))
  }
  return(sd(x) / sqrt(N))
}