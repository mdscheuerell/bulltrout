
##-----------
## read data
##-----------

adult_data <- read_csv(file = file.path(clean_data_dir,
                                        "bull_trout_SSA_data_all_states_adults.csv"))



## data summary for adults
year_smry <- adult_data %>%
  group_by(state, 
           recovery_unit, 
           core_area, 
           popn_stream, 
           metric,
           source) %>%
  summarise(first_year = min(year), last_year = max(year))


# dd <- data.frame(
#   state = rep(c("ID", "WA"), ea = 3),
#   core_area = c("a", "a", "b", "c", "d", "d")
# )
# 
# dd

core_tbl <- year_smry %>% 
  group_by(state, core_area) %>%
  summarise(n = length(core_area))

cc <- length(unique(dd$core_area))

rr <- length(dd$core_area)

ZZ <- matrix(0, rr, cc)

for (j in 1:cc) {
  
  ll <- seq(as.integer(core_tbl[j, 2]))
  
  xx <- ifelse(j == 1, 0, max(ii))

  ii <- ll + xx

  ZZ[ii, j] <- 1
  
}
