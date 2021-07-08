## summary info for USFWS bull trout SSA

#### setup ####

## required libraries
## library(here)
## library(readr)
## library(MARSS)
library(tidyr)
library(dplyr)
library(RColorBrewer)

## set directories
clean_data_dir <- here::here("data", "clean")
output_dir <- here::here("output")

## first & last years of data considered in model
yr_first <- 1991
yr_last <- 2020

t_index <- seq(yr_first, yr_last)

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
  as.data.frame() %>%
  `colnames<-`(c("state", "core_area"))

n_states <- length(unique(core_areas$state))
n_cores <- nrow(core_areas)



tmp <- subset(yy, core_area == "LPO") %>%
  select(starts_with("yr")) %>%
  t() %>%
  log() %>%
  scale()


n_cores <- 2

for(i in 1:n_cores) {

  tmp <- yy %>%
    filter(core_area == core_areas[i,"core_area"]) %>%
    ## drop ID cols
    select(-(state:source)) %>%
    ## convert to matrix
    as.matrix() %>%
    + 1 %>%
    ## log-transform
    log() %>%
    ## remove the mean
    MARSS:::zscore(mean.only = FALSE) %>%
    t()
  
  nn <- ncol(tmp)
  clr <- brewer.pal(nn, "Blues")
  
  par(mai = c(0.9, 0.9, 0.6, 0.1))
  
  matplot(seq(1991:2020), tmp,
          type = "o", lty = "solid", pch = 16, col = clr,
          las = 1, xaxt = "n", xlab = "Year", ylab = "Abundance index")
  mtext(paste0(core_areas[i,"state"], ": ", core_areas[i,"core_area"]),
        side = 3, line = 0.5, adj = 0)
  axis(1, seq(5, 30, 5), seq(1995, 2020, 5))
  lines(mod_fit_CI90$states[i,], lwd = 3)
  
}






mod_fit_late_CI90 <- readRDS((file = file.path(output_dir, "model_fits_late_CI90.rds")))




