## summary info for USFWS bull trout SSA

#### setup ####

## required libraries
## library(here)
## library(readr)
## library(MARSS)
library(tidyr)
library(dplyr)

## set directories
clean_data_dir <- here::here("data", "clean")
output_dir <- here::here("output")

## first & last years of data considered in model
yr_first <- 1991
yr_last <- 2020


#### get observed data ####

## read data
adult_data <- readr::read_csv(file = file.path(clean_data_dir,
                                        "bull_trout_SSA_data_all_states_adults.csv"))

## trim years, reshape to "wide" format & transform for MARSS
yy <- adult_data %>%
  filter(metric == "abundance") %>%
  select(-metric) %>%
  filter(year >= yr_first) %>%
  pivot_wider(names_from = year,
              values_from = value,
              names_prefix = "yr") %>%
  arrange(state, recovery_unit, core_area) %>%
  rowwise(state:source) %>%
  mutate(n_yrs = sum(!is.na(c_across(everything())))) %>%
  ungroup() %>%
  filter(n_yrs >= 10) %>%
  select(-n_yrs) 
  
  
yt <- yy %>%
  ## drop ID cols
  select(-(state:source)) %>%
  ## convert to matrix
  as.matrix() %>%
  + 1 %>%
  ## log-transform
  log() %>%
  ## remove the mean
  MARSS:::zscore(mean.only = FALSE)


#### entire time period ####

## get model fits
mod_fit_CI90 <- readRDS((file = file.path(output_dir, "model_fits_CI90.rds")))


#### summary plots of trends ####

## extract core area names
core_areas <- MARSS:::coef.marssMLE(mod_fit_CI90, matrix)$U %>%
  rownames() %>%
  strsplit(": ") %>%
  do.call(what = rbind) %>%
  `colnames<-`(c("state", "core_area"))







mod_fit_late_CI90 <- readRDS((file = file.path(output_dir, "model_fits_late_CI90.rds")))




