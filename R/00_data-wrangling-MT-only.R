## data munging for USFWS bull trout SSA

## NOTES
## The data from MT cam in a format different than the tidy template I sent
## to state coordinators, so they need to be cleaned before being processed
## with the other states

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

## get Excel file name
mt_file_name <- grep("MT", all_files, value = TRUE)

## read Excel file
mt_data_all <- file.path(raw_data_dir, mt_file_name) %>%
  read_xlsx(col_type = "text", na = c("", "NA", "n/a", "na", ".")) %>%
  ## add integer col for dataset ID to match other states
  ## specific number doesn't matter b/c will be linked to data dictionary
  mutate(dataset = seq(nrow(.)), .before = 1) %>%
  ## years are cols spread out wide; pivot to tidy long format
  pivot_longer(cols = `1979`:`2020`, names_to = "year", values_to = "redds") %>%
  ## drop col with total years sampled
  select(!TotalYearsCounted) %>%
  ## need to extract embedded metadata codes from redd counts
  ## create cols for `n_redds` and `notes`
  mutate(n_redds = gsub(pattern = "\\D{1,2}", replacement  = "", x = redds),
         notes = gsub(pattern = "\\d{1,3}", replacement  = "", x = redds)) %>%
  ## convert empty strings for metadata into NA
  mutate(notes = ifelse(notes == "", NA, notes)) %>%
  ## convert year and n_redds to integer
  mutate(year = as.integer(year),
         n_redds = as.integer(n_redds)) %>%
  ## drop col with orig counts/codes
  select(-redds) 

mt_dataset_lut <- mt_data_all %>%
  filter(year == 2020) %>%
  select(dataset, Kovach_ID, MT_ID)

mt_data_clean <- mt_data_all %>%
  select(!c(LifeHistory, Kovach_ID, MT_ID)) %>%
  mutate(metric = "abundance",
         method = "redd count")

## codes
mt_metadata_codes <- c("a", "b", "d", "e", "f", "g", "z")

which()

tbl <- tibble(x = c(NA, "a", "b", ""))

tbl %>% 
  mutate(x = ifelse(x == "", NA, x)) -> tbl

