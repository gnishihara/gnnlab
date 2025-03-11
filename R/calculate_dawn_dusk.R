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
  # 光のデータが十分じゃない時、日中の長さを求められないので、
  # 薄暮と薄明は crepuscule で求める
  # maptools のパッケージが必要
  # solarDep = 6 is civil twilight （市民薄明）
  # solarDep = 18 is astronomical twilight （天文薄明）

  if (!is.matrix(gpscoord)) {
    stop("gpscoord は行列として渡す [x, y]")
  }

  tz(datetime) = "Japan"
  dawn = crepuscule(
    gpscoord,
    datetime,
    solarDep = 6,
    direction = "dawn",
    POSIXct = T
  )[, 2]
  dusk = crepuscule(
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

