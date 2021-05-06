## data munging for USFWS bull trout SSA

##-------
## setup
##-------

## load libraries
library(here)
library(readxl)
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

i <- file_names[1]

## empty tibble for full data set
df_all <- NULL

## read raw Excel files by cycling through state files
for (i in file_names) {
  
  ## get state
  st <- sub("(USFWS_bull_trout_SSA_data_)([A-Z]{2})(.*)", "\\2", i)
  
  ## read raw file into tmp
  tmp <- file.path(data_dir, i) %>%
    read_xlsx(na = c("", "NA", "n/a")) %>%
    ## set column names to lowercase
    rename_with(tolower) %>%
    ## select columns of interest & rename them
    select(all_of(colnames_default)) %>%
    `colnames<-`(colnames_nice) %>%
    ## add state abbreviation
    mutate(state = st, .before = 1)
  
  ## add state to regional tibble
  df_all <- rbind.data.frame(df_all, tmp)
  
}

xx <- df_all %>% filter(state == "OR")




## write data for all states to one file
df_all %<% 
  write_csv(file = file.path(data_dir, "bull_trout_SSA_data_all_states.csv"))

