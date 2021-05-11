## model fitting for USFWS bull trout SSA

##-------
## setup
##-------

## load libraries
library(readr)
library(dplyr)
library(tidyr)
library(MARSS)


##-----------
## read data
##-----------

adult_data <- read_csv(file = file.path(clean_data_dir,
                                        "bull_trout_SSA_data_all_states_adults.csv"))


##--------------
## data summary
##--------------

## select only abundance metrics
model_data <- adult_data %>%
  filter(metric == "abundance") %>%
  select(-metric)

## data summary for adults
year_smry <- model_data %>%
  group_by(state, 
           dataset,
           recovery_unit, 
           core_area, 
           popn_stream, 
           source) %>%
  summarise(first_year = min(year), last_year = max(year))

## remove all-NA records
# year_smry <- year_smry[-which(is.na(year_smry$state)),]

## number of sites per core area
core_tbl <- year_smry %>% 
  group_by(state, core_area) %>%
  summarise(n = length(core_area))

## number of sites per core area
core_tbl <- yy %>% 
  group_by(state, core_area) %>%
  summarise(n = length(core_area))

## reshape to "wide" format for MARSS
yy <- model_data %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "yr")

## number of core areas (processes, x)
cc <- length(core_tbl$core_area)
## number of locations (streams/rivers, y)
rr <- length(year_smry$core_area)



yy <- tribble(
  ~ x, ~ y, ~ z, ~ value,
  "a", "b", 1, -1,
  "a", "b", 2, -1,
  "a", "c", 3, -1,
  "a", "c", 4, -1,
  "a", "d", 5, -1,
  "a", "d", 6, -1
)

yy %>%
  pivot_wider(names_from = z, values_from = value, names_prefix = "yr")


##-------------
## MARSS setup
##-------------

## empty Z matrix for mapping obs to processes
ZZ <- matrix(0, rr, cc)

## loop over core areas to set cols of Z
for (jj in 1:cc) {
  ## seq for row indices
  ll <- seq(as.integer(core_tbl[jj, 3]))
  ## last row
  xx <- ifelse(jj == 1, 0, max(ii))
  ## seq for row indices
  ii <- ll + xx
  ## assign 1's to respective rows/obs
  ZZ[ii, jj] <- 1
}

## cov matrix for obs (R)
RR <- matrix(list(0), rr, rr)
diag(RR) <- yy$source