#' Read ALEC AWH data
#' This function can read the AWH data files.
#' Note that `options(digits.secs = 4)` might needed to be set to view the subseconds.
#'
#'
#'
#' @param filename the name of the file which the data are to be read from. It must be a CSV file
#' @param summarisedata has a default value of FALSE. If set to TRUE, the burst samples will be averaged.
#' @param ... further arguments passed to read_csv
#'
#' @return A tibble containing the data.
#'
#' \strong{Returned variables}
#' \describe{
#'    \item{datetime}{Date-time}
#'    \item{mpa}{Pressure (MPa)}
#'    \item{depth}{Depth (m)}
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
#' @importFrom stringr str_split
#' @importFrom tools file_ext
#' @importFrom stringi stri_detect_regex
#' @examples
#'

read_awh = function(filename,   summarisedata = FALSE, ...) {
  options(digits.secs = 6)

  test_file = system(paste("file --brief", filename), intern = TRUE)

  if(!stri_detect_regex(rawToChar(readBin(filename, "raw", 60)), "AWH-USB")) {
    stop(paste(filename, "is not an AWH Data file."))
  }

  if(grepl("csv", file_ext(filename), ignore.case = TRUE)) {
    # If it is csv, read as a csv file.
    enc = get_encoding(filename)
    id = read_lines(filename, n_max = 30, locale = locale(encoding = enc))
    skip = which(stri_detect_regex(id, "^\\[Item\\]"))
    sampling_interval = id[stri_detect_regex(id, "^Interval")]
    sampling_interval = str_extract(sampling_interval, "[0-9]+") |> as.numeric()
    skip = skip + 1
    cnames = c("datetime", "mpa", "depth")
    out = suppressMessages(read_csv(filename, skip = skip,
                                    col_names = cnames,
                                    col_select = 1:3,
                                    locale = locale(encoding = enc)))
  } else {
    # Exit if not either.
    stop(paste(filename, "must be a readable CSV file."))
  }

  extract_decimals = function(x) {
    x = str_split(x, "\\.")
    x = sapply(x, "[", 2)
    x[is.na(x)] = "0"
    x
  }

  # Add two attributes to the data frame. One for the type of data (CKU or CEM)
  # and one for the filename.
  attributes(out)$loggertype = "AWH"
  attributes(out)$filename = basename(filename)

  if(summarisedata) {
    out |> group_by(datetime) |> summarise(across(c(mpa, depth), mean))
  } else {
    out |>
      mutate(x = 0.1*sampling_interval * seq(0,by = 1, length.out = n())) |>
      mutate(x = (x %% sampling_interval)/sampling_interval) |>
      mutate(x = extract_decimals(x)) |>
      mutate(datetime = str_glue("{datetime}.{x}")) |>
      select(-x) |>
      mutate(datetime = ymd_hms(datetime))
  }
}
