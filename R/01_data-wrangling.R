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
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")

## expected column names
colnames_default <- c("dataset", "recovery unit", "core area", "popn/stream",
                      "metric", "method", "year", "value")

## better column names
colnames_nice <- c("dataset", "recovery_unit", "core_area", "popn_stream",
                   "metric", "source", "year", "value")


##--------------
## get metadata
##--------------

## get all file names
all_files <- dir(raw_data_dir)

metadata_file <- grep("metadata", all_files, value = TRUE)

metadata <- read_csv(file.path(raw_data_dir, metadata_file))


##---------------
## read raw data
##---------------

## get file names
file_names <- grep("USFWS", all_files, value = TRUE)

## empty tibble for full data set
df_all <- NULL

## read raw Excel files by cycling through state files
for (i in file_names) {
  
  ## get state
  st <- sub("(USFWS_bull_trout_SSA_data_)([A-Z]{2})(.*)", "\\2", i)
  
  ## read raw file into tmp
  tmp <- file.path(raw_data_dir, i) %>%
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


##----------------------
## clean `metric` names
##----------------------

## `metric` describes the state or transition
## it does not contain info on life stage

## abundance
abund_i <- grep("[A|a]bund", df_all$metric)
df_all$metric[abund_i] <- "abundance"

## survival
surv_i <- grep("survival", df_all$metric)
df_all$metric[surv_i] <- "survival"


##----------------------
## clean `method` names
##----------------------

## `method` is the type or source of data 

## change all names to lowercase
df_all$source <- tolower(df_all$source)

## check methods by state
df_all %>%
  group_by(state) %>%
  summarise(source = unique(source))
# example output
# state source                                                  
# <chr> <chr>                                                   
#  1 ID program mark (barker model)                             
#  2 ID program mark (robust design model - huggins formulation)
#  3 ID weir                                                    
#  4 ID screw trap                                              
#  5 ID snorkel                                                 
#  6 ID electrofishing                                          
#  7 ID fishery catch rates                                     
#  8 ID redd survey                                             
#  9 OR redd surveys                                            
# 10 OR juvenile snorkel surveys                                
# 11 OR dam counts                                              
# 12 OR wier count                                              
# 13 OR weir count                                              
# 14 OR NA                                                      
# 15 WA adult count                                             
# 16 WA cumulative redd count                                   
# 17 WA trap count                                              
# 18 WA escapement (proportion)                                 
# 19 WA index snorkel                                           
# 20 WA weir                                                    

## weir counts
weir_i <- df_all$source %in% c("weir", "wier count", "weir count")
df_all$source[weir_i] <- "weir"

## redd counts
redd_i <- grep("redd", df_all$source)
df_all$source[redd_i] <- "redd_counts"

## snorkel surveys
snorkel_i <- grep("snorkel", df_all$source)
df_all$source[snorkel_i] <- "snorkel"

## MARK estimates
mark_i <- grep("mark", df_all$source)
df_all$source[mark_i] <- "mark_output"

## escapement
esc_i <- grep("escape", df_all$source)
df_all$source[esc_i] <- "escapement"

## replace spaces with underscores
df_all$source <- gsub("\\s", "_", df_all$source)


##----------------
## get adult data
##----------------

## create primary key for filtering adult data
adult_ID <- metadata %>%
  filter(lifestage == "A") %>%
  tidyr::unite("data_ID", state:dataset, remove = TRUE) %>%
  select(data_ID)

## add primary key to df_all
df_tmp <- df_all %>%
  tidyr::unite("data_ID", state:dataset, remove = FALSE)

## filter out non-adult sites
adult_data <- left_join(adult_ID, df_tmp) %>%
  select(-data_ID)
  

##--------------
## data summary
##--------------

## sources of adult data
adults <- c("adult_count", "dam_counts", "escapement", "fishery_catch_rates",
            "mark_output", "redd_counts", "trap_count", "weir")

## sources of juvenile data
juvies <- c("electrofishing", "screw_trap", "snorkel")

## short names for data sources
# data_sources <- c("dam", "efishing", "escape", "redd", "screw", "snorkel", "trap", "weir")

## data summary for adults
year_smry <- adult_data %>%
  group_by(state, 
           recovery_unit, 
           core_area, 
           popn_stream, 
           metric,
           source) %>%
  summarise(first_year = min(year), last_year = max(year))

print(as.data.frame(year_smry))


##------------
## write data
##------------

## write all data for all states to one file
df_all %>% 
  write_csv(file = file.path(clean_data_dir, "bull_trout_SSA_data_all_states.csv"))

## write adult data only for all states to one file
df_all %>% 
  write_csv(file = file.path(clean_data_dir, "bull_trout_SSA_data_all_states_adults.csv"))

