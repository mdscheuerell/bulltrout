## data munging for USFWS bull trout SSA

## load libraries
library(here)
library(readxl)
library(dplyr)

## set directories
data_dir <- here("data")

## expected column names
colnames_default <- c("dataset", "recovery unit", "core area", "popn/stream", "metric", "method", "year", "value")

## better column names
colnames_nice <- c("dataset", "recovery_unit", "core_area", "popn_stream", "metric", "method", "year", "value")

## get raw file names
file_names <- dir(data_dir)

i <- file_names[1]

## read raw Excel files by cycling through state file
for (i in file_names) {
  
  ## get state
  st <- sub("(USFWS_bull_trout_SSA_data_)([A-Z]{2})(.*)", "\\2", i)
  
  tmp <- file.path(data_dir, i) %>%
    read_xlsx() %>%
    select(all_of(colnames_default)) %>%
    `colnames<-`(colnames_nice) %>%
    mutate(state = st, .before = 1)

}

