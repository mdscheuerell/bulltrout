## data munging for USFWS bull trout SSA

##-------
## setup
##-------

## load libraries
library(here)
library(readxl)
library(readr)
library(dplyr)

## set directories
data_dir <- here("data")

## expected column names
colnames_default <- c("dataset", "recovery unit", "core area", "popn/stream",
                      "metric", "method", "year", "value")

## better column names
colnames_nice <- c("dataset", "recovery_unit", "core_area", "popn_stream",
                   "metric", "method", "year", "value")


##---------------
## read raw data
##---------------

## get raw file names
file_names <- dir(data_dir)

## empty tibble for full data set
df_all <- NULL

## read raw Excel files by cycling through state files
for (i in file_names) {
  
  ## get state
  st <- sub("(USFWS_bull_trout_SSA_data_)([A-Z]{2})(.*)", "\\2", i)
  
  ## read raw file into tmp
  tmp <- file.path(data_dir, i) %>%
    read_xlsx(range = cell_cols("A:H"),
              col_types = c("guess", "text", "text", "text", "text", "text", "numeric", "numeric"),
              na = c("", "NA", "n/a", "na", ".")) %>%
    ## set column names to lowercase
    rename_with(tolower) %>%
    ## select columns of interest & rename them
    select(all_of(colnames_default)) %>%
    `colnames<-`(colnames_nice) %>%
    ## add state abbreviation
    mutate(state = st, .before = 1) %>%
    ## remove `dataset == "MetaData"`
    filter(dataset != "MetaData")
  
  ## remove "WA." from `dataset` for WA file
  if (st == "WA") {
    tmp$dataset <- sub("([A-Z]{2}\\.)([0-9]{1,})", "\\2", tmp$dataset)
  }
  
  ## convert `dataset` to integer
  tmp <- tmp %>%
    mutate(dataset = as.integer(dataset))
  
  ## add state to regional tibble
  df_all <- rbind.data.frame(df_all, tmp)
  
}


##-----------------------
## clean `method` names
##-----------------------

## change all names to lowercase
df_all$method <- df_all$method %>%
  tolower()

## weir counts
weir_i <- df_all$method %in% c("weir", "wier count", "weir count")
df_all$method[weir_i] <- "weir"

## redd counts
redd_i <- grep("redd", df_all$method)
df_all$method[redd_i] <- "redd"

## snorkel surveys
snorkel_i <- grep("snorkel", df_all$method)
df_all$method[snorkel_i] <- "snorkel"

unique(df_all$method)


##------------
## write data
##------------

## write data for all states to one file
df_all %>% 
  write_csv(file = file.path(data_dir, "bull_trout_SSA_data_all_states.csv"))

