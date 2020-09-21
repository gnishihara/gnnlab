#' Read Odyssey data
#' This function reads the Odyssey PAR datalogger data files.
#'
#' When the PAR value is below 0, the value is set to NA.
#' **At this stage, the data is not calibrated!**
#'
#' @param filename the name of the file which the data are to be read from. It can be a CSV file or an XLSX file.
#' @param ... further arguments passed to read_csv or read_xlsx
#'
#' @return A tibble containing the data.
#'
#' \strong{Returned variables}
#' \describe{
#' \item{datetime}{Date-time}
#' \item{ppfd}{Uncalibrated photosynthetically active radiation (-)}
#' }
#'
#' @export
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom tidyr drop_na
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
#' df = tibble(fnames = fnames)#'
#' df %>% filter(str_detect(fnames, "Light")) %>% slice(1) %>% pull(fnames) %>% read_odyssey()
#' }
#'
read_odyssey = function(filename, ...) {
  test_file = system(paste("file --brief", filename), intern = TRUE)
  cnames = c("N", "date", "time", "raw", "calib")
  if(grepl("Zip", test_file)) {
    ctypes = c("numeric", "text", "text", "numeric", "numeric")
    out = read_xlsx(filename, skip = 7, col_names = cnames, col_types=ctypes)
  } else if(grepl("ASCII | Unicode", test_file)) {
    ctypes = c("nccnn")
    out = suppressMessages(read_csv(filename, skip = 7, col_names = cnames, col_types=ctypes))
  } else {
    stop(paste(filename, "is not a readable file."))
  }
  attributes(out)$filename = basename(filename)
  out %>%
    mutate(datetime = paste(.data$date, .data$time)) %>%
    mutate(datetime = parse_date_time(.data$datetime, "dmyT")) %>%
    select(.data$datetime, ppfd = raw) %>% drop_na() %>%
    mutate(ppfd = ifelse(.data$ppfd < 0, NA, .data$ppfd))
}