#' Read Castaway CTD data
#'
#' This function reads the CTD from the Castaway CTD.
#' The data that is read is the mean start and ending GPS coordinates,
#' the time of the cast, water depth, water temperature, salinity, and water density.
#'
#' @param filename the name of the csv file.
#' @param skip the number of lines to skip. Default is 29.
#' @param ... further arguments passed to read_csv.
#' @return
#' A tibble containing the data.
#'
#' \strong{Returned variables}
#' \describe{
#'   \item{lat}{GPS latitude}
#'   \item{lon}{GPS longitude}
#'   \item{datetime}{Date and time of the cast}
#'   \item{depth}{Water depth (m)}
#'   \item{temperature}{Water temperature (\eqn{^\circ}{degrees}C)}
#'   \item{salinity}{Salinity (PSU)}
#'   \item{density}{Water density (kg / m^3)}
#' }
#' @export
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd_hms
#' @importFrom tools file_ext
#'


read_castaway = function(filename, skip = 29, ...) {
  if (grepl("csv", tools::file_ext(filename), ignore.case = TRUE)) {
    out = suppressMessages(read_csv(filename, skip = skip, ...))
  }
  else {
    stop(paste(filename, "is not a readable file."))
  }

  lines = read_lines(filename, n_max = 50)
  n1 = grep("Cast time *.local.*", lines)
  n2 = grep("Start lat", lines)
  n3 = grep("End lat", lines)
  n4 = grep("Start long", lines)
  n5 = grep("End long", lines)
  cast_time = lines[n1]
  start_gps = c(lines[n2], lines[n4])
  end_gps = c(lines[n3], lines[n5])

  cast_time = str_remove(cast_time, "%.*,") %>% ymd_hms()
  start_gps = str_extract(start_gps, "[0-9].*[0-9]") %>% as.numeric()
  end_gps = str_extract(end_gps, "[0-9].*[0-9]") %>% as.numeric()
  gps = cbind(start_gps, end_gps) %>% rowMeans() # Get the average GPS coordinates

  out = out %>% select(
    depth = matches("Depth"),
    temperature = matches("Temperature"),
    salinity = matches("Salinity"),
    density = matches("Density"))

  out %>% mutate(lat = gps[1], lon = gps[2], datetime = cast_time, .before = 1)
}

