#' Read ALEC instruments data
#' This function can read the CEM and CKU data files.
#'
#' For CEM files, when the absolute x- and y- velocities are below 0 or above 200, all data are NA.
#'
#' For CKU files, when the chlorophyll data is below 0 or above 400, the value is set to NA.
#' When turbidity is below 0 or above 1000, the value is set to NA.
#'
#' For both CEM and CKU files, When the temperature is below 0 or above 40, all data are NA.
#'
#' @param filename the name of the file which the data are to be read from. It can be a CSV file or an XLSX file.
#' @param ... further arguments passed to read_csv or read_xlsx
#'
#' @return A tibble containing the data.
#' The contents of the tibble depend on the data file.
#'
#' \strong{Returned variables}
#' \describe{
#'    \item{datetime}{Date-time}
#'    \item{speed}{Current speed (cm / s)}
#'    \item{dir}{Current direction (radians)}
#'    \item{ew}{East-West velocity (cm / s)}
#'    \item{ns}{North-South velocity (cm / s)}
#'    \item{chla}{Chlorophyll-a (μg / L)}
#'    \item{turbidity}{Turbidity (FTU, Formazin Nephelometric Units)}
#'    \item{temperature}{Water temperature (°C)}
#' }
#' @export
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom readr locale
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_extract
#' @examples
#' \dontrun{
#' fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
#' df = tibble(fnames = fnames)#'
#' df %>% filter(str_detect(fnames, "CEM")) %>% slice(1) %>% pull(fnames) %>% read_alec()
#' df %>% filter(str_detect(fnames, "CKU")) %>% slice(1) %>% pull(fnames) %>% read_alec()
#' }
#'
read_alec = function(filename, ...) {
  test_file = system(paste("file --brief", filename), intern = TRUE)
  if(grepl("Zip", test_file)){
    # If it is xlsx, read as an xlsx file.
    id = read_xlsx(filename, range = "A13")
    out = read_xlsx(filename, skip = 36)
  } else if(grepl("ASCII", test_file)) {
    # If it is csv, read as a csv file.
    id = read_lines(filename, skip = 12, n_max = 1)
    out = suppressMessages(read_csv(filename, skip = 36, locale = locale(encoding = "CP932")))
  } else if(grepl("UTF-8", test_file)) {
    id = read_lines(filename, skip = 12, n_max = 1)
    out = suppressMessages(read_csv(filename, skip = 36))
  } else {
    # Exit if not either.
    stop(paste(filename, "is not a readable file."))
  }
  # Add two attributes to the data frame. One for the type of data (CKU or CEM)
  # and one for the filename.
  attributes(out)$loggertype = str_extract(id, "T[GK].*[B]")
  attributes(out)$filename = basename(filename)
  out = out %>%
    select(ymd = matches("YYYY"),
           hms = matches("hh:mm"),
           speed = matches("Velo"),
           dir = matches("Dir"),
           ew = matches("EW"),
           ns = matches("NS"),
           vx = matches("Vel X\\["),
           vy = matches("Vel Y\\["),
           chla = matches("ｸﾛﾛ\\[   \\]"),
           turbidity = matches("Turb\\[ppm\\]"),
           temperature = matches("Temp\\[")) %>%
    mutate(datetime = paste(.data$ymd, .data$hms)) %>%
    mutate(datetime = ymd_hms(.data$datetime)) %>%
    mutate_at(vars(matches("dir")), ~(. / 360 * 2*pi)) %>%
    select(-.data$ymd, -.data$hms) %>%
    select(.data$datetime, everything()) %>%
    mutate_at(vars(matches("chla")), ~(ifelse(((. < 0) | (. > 400)), NA, .))) %>%
    mutate_at(vars(matches("turbidity")), ~(ifelse(((. < 0) | (. > 1000)), NA, .))) %>%
    mutate_at(vars(matches("vx|vy")), ~(ifelse(abs(.) > 200, NA, .))) %>%
    mutate_at(vars(matches("temperature")), ~(ifelse(((. < 0) | (. > 40)), NA, .)))

  chk = colnames(out)
  if("vx" %in% chk) {
    out %>%
      mutate_at(vars(-matches("datetime")), ~(ifelse((is.na(vx)|is.na(vy)|is.na(temperature)), NA, .))) %>%
      select(-matches("vx|vy"))
  } else {
    out
  }
}
