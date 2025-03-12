#' Shift months
#' Shift the months by subtracting the shift degree
#' A positive shift moves months forward, while a negative shift moves them backward
#'
#' @param original_month is the original month of the data
#' @param shift_month is the degree of shifting
#' @export

shift_months_general <- function(original_month, shift_month) {
  shifted <- (original_month - shift_month) %% 12
  shifted[shifted == 0] <- 12
  shifted
}


#' Calculate the decimal hours
#'
#' @param datetime a date-time
#' @export
calculate_H = function(datetime) {
  lubridate::hour(datetime)  + lubridate::minute(datetime)/60
}
