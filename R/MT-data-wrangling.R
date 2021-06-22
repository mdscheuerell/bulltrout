## data munging for USFWS bull trout SSA

#### setup ####

## load libraries
library(here)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

## set directories
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")

## expected column names
colnames_default <- c("dataset", "recovery unit", "core area", "popn/stream",
                      "metric", "method", "year", "value")

## better column names
colnames_nice <- c("dataset", "recovery_unit", "core_area", "popn_stream",
                   "metric", "source", "year", "value")


#### get metadata ####

## get all file names
all_files <- dir(raw_data_dir)

metadata_file <- grep("metadata", all_files, value = TRUE)

metadata <- read_csv(file.path(raw_data_dir, metadata_file))


#### read raw data ####

## get file names
file_name <- grep("MT", all_files, value = TRUE)

tmp <- file.path(raw_data_dir, file_name) %>%
  read_xlsx(col_type = "text", na = c("", "NA", "n/a", "na", ".")) %>%
  mutate(dataset = seq(nrow(.)), .before = 1) %>%
  pivot_longer(cols = c(10:51), names_to = "year", values_to = "redds") %>%
  select(!TotalYearsCounted) %>%
  mutate(n_redds = gsub(pattern = "\\D{1,2}", replacement  = "", x = redds),
         notes = gsub(pattern = "\\d{1,3}", replacement  = "", x = redds)) %>%
  mutate(n_redds = as.integer(n_redds),
         year = as.integer(year)) %>%
  select(-redds) 


tmp$notes[tmp$notes == ""] <- NA
tail(tmp)


