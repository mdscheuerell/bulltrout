## model fitting for USFWS bull trout SSA
## for sites with data on juveniles only

#### setup ####

## load libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(MARSS)

## set directories
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")
output_dir <- here("output")

## first year of data to consider in model
yr_first <- 1991

## read data
juvie_data <- read_csv(file = file.path(clean_data_dir,
                                        "bull_trout_SSA_data_all_states_juveniles.csv"))


#### data summary ####

## select only abundance metrics
model_data <- juvie_data %>%
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
  summarise(first_year = min(year),
            last_year = max(year))

## write summary info to file
year_smry %>% 
  write_csv(file = file.path(output_dir, "bull_trout_SSA_juvie_data_summary.csv"))

## remove all-NA records
# year_smry <- year_smry[-which(is.na(year_smry$state)),]

## number of sites per core area
core_tbl <- year_smry %>% 
  group_by(state, core_area) %>%
  summarise(n = length(core_area))

## trim years & reshape to "wide" format for MARSS
yy <- model_data %>%
  filter(year >= yr_first) %>%
  pivot_wider(names_from = year,
              values_from = value,
              names_prefix = "yr")

## number of core areas (processes, x)
cc <- length(core_tbl$core_area)
## number of locations (streams/rivers, y)
rr <- length(year_smry$core_area)


#### MARSS setup ####

## observation eqn

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

## offsets for obs (a); data de-meaned so all 0's
AA <- matrix(0, rr, 1)

## covariance matrix for obs (R)
RR <- matrix(list(0), rr, rr)
diag(RR) <- yy$source

## process eqn

## interactions matrix (B); set to I for RW's
BB <- diag(cc)

## bias terms (u); each core area gets a unique bias term
UU <- core_tbl %>%
  select(-n) %>%
  unite("core_area", state:core_area, sep = ": ") %>%
  as.matrix(nrow = cc, ncol = 1)

## cov matrix for processes (Q)
QQ <- matrix(list(0), cc, cc)
## diagonal and unequal
# diag(QQ) <- core_tbl %>%
#   select(-n) %>%
#   unite("core_area", state:core_area, sep = ": ") %>%
#   unlist()
## diagonal and equal (IID)
diag(QQ) <- rep("q", cc)

## data for fitting

## reformat for MARSS
yy <- yy %>%
  ## drop ID cols
  select(-(state:source)) %>%
  ## convert to matrix
  as.matrix() %>%
  + 1 %>%
  ## log-transform
  log() %>%
  ## remove the mean
  zscore(mean.only = TRUE)

## check number of non-NA values by year
# apply(!is.na(yy), 2, sum)


#### model fitting ####

## model list
mod_list <- list(
  B = BB,
  U = UU,
  Q = QQ,
  Z = ZZ,
  A = AA,
  R = RR
)

## control list
con_list <- list(
  maxit = 5000
)

## fit base model
mod_fit <- MARSS(yy, model = mod_list, control = con_list)


#### bootstrapped CI's ####

## bootstrap parameters from the Hessian matrix
mod_fit_CI <- MARSSboot(mod_fit, param.gen = "hessian", nboot = 1000)

## extract bias params
bias_mat <- mod_fit_CI$boot.params[grep("U.", rownames(mod_fit_CI$boot.params)),]

## summary table of bias CI's
bias_smry <- bias_mat %>%
  apply(1, quantile, c(0.025, 0.5, 0.975)) %>%
  round(digits = 3) %>%
  t() %>%
  as.data.frame()
## better row names
rownames(bias_smry) <- gsub("(U.)(.*)", "\\2", rownames(bias_smry))

## summarize trends
## negative
neg <- bias_smry %>%
  apply(1, function(x) x < 0) %>%
  t() %>%
  apply(1, all)
## positive
pos <- bias_smry %>%
  apply(1, function(x) x > 0) %>%
  t() %>%
  apply(1, all)
## add trend col
bias_smry <- bias_smry %>%
  mutate(trend = "0")
bias_smry$trend[neg] <- "-"
bias_smry$trend[pos] <- "+"

## write bias summary to file
bias_smry %>% 
  write.csv(file = file.path(output_dir, "bull_trout_SSA_all_states_juveniles_biases.csv"))


