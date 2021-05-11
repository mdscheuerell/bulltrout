## model fitting for USFWS bull trout SSA

##-----------
## read data
##-----------

adult_data <- read_csv(file = file.path(clean_data_dir,
                                        "bull_trout_SSA_data_all_states_adults.csv"))


##--------------
## data summary
##--------------

## data summary for adults
year_smry <- adult_data %>%
  group_by(state, 
           recovery_unit, 
           core_area, 
           popn_stream, 
           metric,
           source) %>%
  summarise(first_year = min(year), last_year = max(year))

## remove all-NA records
year_smry <- year_smry[-which(is.na(year_smry$state)),]

## number of sites per core area
core_tbl <- year_smry %>% 
  group_by(state, core_area) %>%
  summarise(n = length(core_area))

## number of core areas (processes)
cc <- length(core_tbl$core_area)
## number of locations (streams/rivers)
rr <- length(year_smry$core_area)


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

