# gnnlab

<!-- badges: start -->
<!-- badges: end -->

This package contains a number of useful functions to read data files from the following 
data loggers:

**Onset Inc.**

* U20 Water depth datalogger
* U24 Conductivity/Salinity datalogger
* U26 Dissolved oxygen datalogger
* Various temperature dataloggers
* USB Microstation equipped with 
    (1) Wind speed sensor
    (2) Barometer
    (3) PAR sensor
    (4) Insolation sensor

**Dataflow Systems**

* Odyssey PAR datalogger

**Alec Electronics**

* CEM Electromagnetic water velocity datalogger
* CKU Chlorophyll-a fluorescence and turbidity datalogger

## Installation

``` r
devtools::install_github("gnishihara/gnnlab")
```

## Important

Check the locale of the system (the server runs on Debian). 
If the locale is in `en_US` and the data files are in ja_JP.UTF-8,
then there will be errors in the date stame when reading the file!

``` r
if(grepl("en_US", Sys.getlocale("LC_TIME"))) { 
  # LC_TIME の設定が en_US.UTF-8 なら、ja_JP.UTF-8 にかえる。
  # 買えない場合、parse_date_time()の午前・午後変換がバグる。
  Sys.setlocale("LC_TIME", "ja_JP.UTF-8")
}
```

## Example

``` r
library(tidyverse)
library(gnnlab)

fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
df = tibble(fnames)

# Dissolved oxygen datalogger example

str_subset(fnames, "DO") %>% read_onset()
```

