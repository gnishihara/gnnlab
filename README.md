# gnnlab

<!-- badges: start -->
<!-- badges: end -->


水圏植物生態学研究室用の便利な関数をまとめています。
研究室でよく使う, オンセット社のデータロガー読み込み関数, アレック電子のCompact CEMとCKUの読み込み関数, 
Dataflow Systems の Odyssey 光量子量ロガーの読み込み関数を準備しています。
This package contains a number of useful functions to read data files from the following 
data loggers:

**Onset Inc.**

* U20 Water depth datalogger（水深ロガー）
* U24 Conductivity/Salinity datalogger（塩分・伝導率ロガー）
* U26 Dissolved oxygen datalogger（溶存酸素濃度ロガー）
* Various temperature dataloggers（水温ロガー）
* USB Microstation equipped with（マイクロステーション, ただし下記のセンサーのみ対応している） 
    (1) Wind speed sensor
    (2) Barometer
    (3) PAR sensor
    (4) Insolation sensor

**Dataflow Systems**

* Odyssey PAR datalogger（光量子量ロガー）

**Alec Electronics**

* CEM Electromagnetic water velocity datalogger（流向流速ロガー）
* CKU Chlorophyll-a fluorescence and turbidity datalogger（クロロフィル蛍光ロガー）
* AWH Wave height data logger (波高ロガー)

## Other functions

* `aseries()` and `bseries()` for paper size dimensions (AとBシリーズの用紙サイズ)
* `se()` for the standard error (標準誤差)

## Installation

``` r
devtools::install_github("gnishihara/gnnlab")
```

## Important

Check the locale of the system (the server runs on Debian). 
If the locale is in `en_US` and the data files are in ja_JP.UTF-8,
then there will be errors in the date stamp when reading the file!

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

## Memo

``` r
devtools::document() # To use roxygen2 to create the man files.
devtools::build() # To build the package.
devtools::install() # To install locally.
```
