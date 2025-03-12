#' Determine dawn and dusk for a specified GPS coordinate.
#'
#' @param datetime the date-time to determine when dawn and dusk occurs.
#' @param gpscoord the gps coordinate to determine when dawn and dusk occurs. It should be provided as a 1x2 matrix, where the first value is the latitude and the second value is the longitude.
#' @return A lubridate interval.
#'
#' @export
#' @importFrom lubridate interval
#' @importFrom suntools crepuscule

calculate_dawn_dusk = function(datetime, gpscoord) {
  # solarDep = 6 is civil twilight
  # solarDep = 18 is astronomical twilight

  if (!is.matrix(gpscoord)) {
    stop("gpscoord should be a matrix [latitude, longitude].")
  }

  tz(datetime) = "Japan"
  dawn = suntools::crepuscule(
    gpscoord,
    datetime,
    solarDep = 6,
    direction = "dawn",
    POSIXct = T
  )[, 2]
  dusk = suntools::crepuscule(
    gpscoord,
    datetime,
    solarDep = 6,
    direction = "dusk",
    POSIXct = T
  )[, 2]
  tz(dawn) = "UCT"
  tz(dusk) = "UCT"
  interval(dawn, dusk)
}

