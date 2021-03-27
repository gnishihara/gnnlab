#' Read Onset data
#'
#' This function reads some of the dataloggers from Onset Inc.
#' The loggers that can be read are the U26 dissolved oxygen logger,
#' the U24 salinity logger, the U20 water depth logger, and the
#' USB Microstation with the wind speed, air pressure, PAR, and insolation
#' sensors.
#'
#' When the water temperature and dissolved oxygen values are
#' below 0 and above 40, the value is set to NA.
#' When the values of any of the other variables are below 0, the value is
#' set to NA.
#'
#' @param filename the name of the file which the data are to be read from. It can be a CSV file or an XLSX file.
#' @param locale the locale to bue used. On linux systems use system("locale -a") to list all of the installed locales. Typically for our systems in the lab, use ja_JP.UTF-8 for a Japanese locale and en_US.UTF-8 for an American English locale.
#' The parameter default is locale = Sys.setlocale("LC_TIME", "ja_JP.UTF-8")
#' @param ... further arguments passed to read_csv or read_xlsx
#' @return
#' A tibble containing the data.
#' The contents of the tibble depend on the data file.
#'
#' \strong{Returned variables}
#' \describe{
#'   \item{datetime}{Date-time}
#'   \item{mgl}{Dissolved oxygen (mg / L)}
#'   \item{wind}{Wind speed (m / s)}
#'   \item{gust}{Wind gust (m / s)}
#'   \item{ppfd}{Photosynthetically active radiation, PAR (μmol photons / m^2 / L)}
#'   \item{insolation}{Insolation (W / m^2)}
#'   \item{lux}{Illuminance (lux)}
#'   \item{mbar}{Depth (mbar)}
#'   \item{kPA or psi}{Air pressure at sea-level (kPa or psi)}
#'   \item{hr_conductance}{High range conductivity (μS / cm)}
#'   \item{spc}{Specific conductance (μS / cm)}
#'   \item{temperature}{Water temperature (°C)}
#' }
#' @export
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom lubridate parse_date_time
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
#' df = tibble(fnames = fnames)#'
#' df %>% filter(str_detect(fnames, "DO")) %>% slice(1) %>% pull(fnames) %>% read_onset()
#' df %>% filter(str_detect(fnames, "Microstation")) %>% slice(1) %>% pull(fnames) %>% read_onset()
#' df %>% filter(str_detect(fnames, "Depth")) %>% slice(1) %>% pull(fnames) %>% read_onset()
#' df %>% filter(str_detect(fnames, "Salinity")) %>% slice(1) %>% pull(fnames) %>% read_onset()
#' df %>% filter(str_detect(fnames, "Temperature")) %>% slice(1) %>% pull(fnames) %>% read_onset()
#' }
#'
read_onset  = function(filename, locale = Sys.setlocale("LC_TIME", "ja_JP.UTF-8"), ...) {

  if(grepl("xlsx", file_ext(filename), ignore.case = TRUE)) {
    out = read_xlsx(filename, skip = 1, ...)
  } else if(grepl("csv", file_ext(filename), ignore.case = TRUE)) {
    out = suppressMessages(read_csv(filename, skip = 1,  ...))
  } else {
    stop(paste(filename, "is not a readable file."))
  }
  attributes(out)$filename = basename(filename)
  out %>%
    select(datetime = matches("GMT"),
           mgl = matches("mg/L"),
           temperature = matches("°C"),
           mbar = matches("mbar"),
           wind = matches("風速|Wind"),
           gust = matches("突風|Gust"),
           ppfd = matches("PAR"),
           insolation = matches("日射"),
           lux = matches("光度"),
           kpa = matches("kPa"),
           psi = matches("psi"),
           hr_conductance = matches("高範囲|High"),
           spc = matches("Specific"),
           psu = matches("ppt")) %>%
    mutate(datetime = parse_date_time(.data$datetime, "mdyT", locale = locale)) %>%
    mutate_at(vars(-.data$datetime), ~(ifelse(. < 0, NA, .))) %>%
    mutate_at(vars(matches("mgl|temperature")), ~(ifelse(. > 40, NA, .)))
}
