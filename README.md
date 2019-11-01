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

## Example

``` r
library(tidyverse)
library(gnnlab)

fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
df = tibble(fnames)

# Dissolved oxygen datalogger example

str_subset(fnames, "DO") %>% read_onset()
```

