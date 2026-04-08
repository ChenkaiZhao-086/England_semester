library(tidyverse)
library(patchwork)
library(xml2) # get html data
library(rvest)
library(readxl)
library(lubridate)
library(sf)
## library(CausalImpact)
library(zoo)
library(assertthat)
library(bsts)
library(BoomSpikeSlab)
library(CARBayesST)
library(spdep)
library(coda)
library(future)
library(furrr)
library(future.apply)
library(doParallel)
library(foreach)

# save.image(file = "20260402.Rdata")
load("20260402.Rdata")

source("code/function.R")
source("code/super_ci.R")

# 0. data import ---------------------------------------------------------
source("code/0.data_import.R")

# 1.  Descriptive_analysis -----------------------------------------------
source("code/1.0.Descriptive_analysis.R")


# 2.  BSTS ----------------------------------------------------------------
##  <="2020-11-15" Use this cutoff date
### Prepare open data ---
HEI_dat <- HEI_cam_dat %>%
  dplyr::select(HEI_ID, HEI_name, Campus_name, HEI_opening) %>%
  left_join(., combine1, by = c("Campus_name" = "LTLA_name")) %>%
  mutate(
    HEI_opening = as.Date(HEI_opening),
    LTLA_name = Campus_name
  ) %>%
  drop_na()


### LTLA_case_dat moving average or rolling sum ---
LTLA_rs <- case.cal(
  data = LTLA_case_dat,
  type = "RS",
  start_date = "2020-06-07",
  end_date = "2020-11-15"
)
# LTLA_week <- case.cal(data = LTLA_case_dat, type = "week", start_date = "2020-06-07", end_date = "2020-11-15")
# LTLA_ma <- case.cal(data = LTLA_case_dat, type = "MA", start_date = "2020-06-07", end_date = "2020-11-15")

## Growth rate CI ---
LTLA_gr <- LTLA_rs %>%
  group_by(LTLA_ID) %>%
  mutate(
    newCasesBySpecimenDate = replace(
      newCasesBySpecimenDate,
      newCasesBySpecimenDate == 0,
      0.5
    ),
    lag = lag(newCasesBySpecimenDate, 1),
    gr = (newCasesBySpecimenDate / lag),
    log_gr = log(gr),
    newCasesBySpecimenDate = log_gr
  ) %>%
  ungroup() %>%
  drop_na() %>%
  dplyr::select(
    "areaCode",
    "LTLA_ID",
    "LTLA_name",
    "date",
    "newCasesBySpecimenDate"
  )


### Set case and control condition for the following match ---
region_num <- data.frame(
  N_region = LTLA_dat_combine$N_region %>% unique(., ),
  N_num = 1:12
)


LTLA_mat <- LTLA_dat_combine %>%
  full_join(., region_num) %>%
  dplyr::select(
    !c(
      "UTLA_CD",
      "UTLA_name",
      "LTLA_Lat",
      "LTLA_Long",
      "areaCode",
      "lci",
      "uci",
      "N_region",
      "rt"
    )
  ) %>%
  mutate(type = if_else(hall_num > 0, 1, 0))


LTLA_mat_nolondon <- LTLA_dat_combine %>%
  full_join(., region_num) %>%
  filter(N_region != "London") %>%
  dplyr::select(
    !c(
      "UTLA_CD",
      "UTLA_name",
      "LTLA_Lat",
      "LTLA_Long",
      "areaCode",
      "lci",
      "uci",
      "N_region",
      "rt"
    )
  ) %>%
  mutate(type = if_else(hall_num > 0, 1, 0))


LTLA_mat_hall <- LTLA_dat_combine %>%
  full_join(., region_num) %>%
  filter(N_region != "London") %>%
  dplyr::select(
    !c(
      "UTLA_CD",
      "UTLA_name",
      "LTLA_Lat",
      "LTLA_Long",
      "areaCode",
      "lci",
      "uci",
      "N_region",
      "rt"
    )
  ) %>%
  mutate(type = if_else(hall_num == 1, NA, if_else(hall_num >= 2, 1, 0))) %>%
  drop_na()

######## 1 control LTLA
source("code/2.1.bsts_1_control.R")

######## 3 control LTLAs
source("code/2.2.bsts_3_control.R")

### make table2——Causal impact part
Table2_bsts <- cal.cum.ci(
  data = ci_m_base_gr,
  m.data = mat_base,
  table = ci_m_base_gr_table,
  tidy.out = TRUE
)


# 3. Bayesian spatio-temporal ---------------------------------------------
source("code/3.1.Data_prepare.R")

### 3.5 Construct bayesian spatio-temporal model
# default prior: normal prior N(0,100000)
### ST.CARanova Spatio-temporal main effects and an interaction
results_list <- ST.CARanova(
  formula = log_GR ~ h_index +
    retail_gr +
    Pop_Den_km2 +
    Prosperity +
    week +
    weekend,
  family = "gaussian",
  data = HEI_dec0.4,
  W = W_mat,
  burnin = 10000,
  n.sample = 110000,
  thin = 100,
  n.chains = 4,
  n.cores = 4,
  interaction = TRUE,
  verbose = TRUE
)


#### Check convergence - traceplot
# summary(beta.cov)
# plot(results_list[["samples"]][["beta"]])
# x11.save(beta.cov, file = "outputs/convergence_check.pdf", width = 10, height = 10)
# gelman.diag(results_list[["samples"]][["beta"]])

#### Effects of covariates on disease risk
RR_table <- get.RR(
  results_list,
  data = HEI_dec0.4,
  unit = "student",
  digits = 3
)
rownames(RR_table) <- c(
  "H-index",
  "Growth rate of retail and recreation percent change",
  "Population density/Km2",
  "Prosperity"
)
# get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "sd", coef.num = 4)
# get.RR(chain1 = chain1_ano_den_h, chain2 = chain2_ano_den_h, data = HEI_dec0.4, unit = "1", coef.num = 6)
# get.RR(chain1 = chain1_ano_pro_h, chain2 = chain2_ano_pro_h, data = HEI_dec0.4, unit = "1", coef.num = 6)

coef_mat <- t(rbind(
  results_list[["samples"]][["beta"]][[1]],
  results_list[["samples"]][["beta"]][[2]],
  results_list[["samples"]][["beta"]][[3]],
  results_list[["samples"]][["beta"]][[4]]
)) %>%
  as.matrix()

eff_all <- as.matrix(HEI_dec0.4[, "h_index"]) %*% coef_mat[2, ]
eff_GMR <- as.matrix(HEI_dec0.4[, "retail_gr"]) %*% coef_mat[3, ]

eff_all_ci <- t(apply(eff_all, 1, FUN = function(x) {
  quantile(x, c(0.5, 0.025, 0.975))
}))
eff_GMR_ci <- t(apply(eff_GMR, 1, FUN = function(x) {
  quantile(x, c(0.5, 0.025, 0.975))
}))


### Sensitivity analysis: Only Hall index
source("code/3.2.bayesST.R")

### KNN analysis
source("code/3.3.KNN_analysis.R")


# 4. Generate Table 2 ------------------------------------------------------
### Table2 in main analysis -----------------------------------------------
region_num2 <- LTLA_dat_combine %>%
  dplyr::select(LTLA_ID, LTLA_name, N_region) %>%
  left_join(region_num)


Table2_CAR <- cal.cum.bs(
  rawdata = HEI_dec0.4,
  region_data = region_num2,
  eff_data = eff_all
)


Table2_CAR_GMR <- cal.cum.bs(
  rawdata = HEI_dec0.4,
  region_data = region_num2,
  eff_data = eff_GMR
)

RowOrder <- c(
  "All region",
  "North East",
  "North West",
  "Yorkshire and The Humber",
  "East Midlands",
  "West Midlands",
  "South West",
  "East of England",
  "South East",
  "London",
  "Northern Ireland",
  "Scotland",
  "Wales"
)
Table2 <- bind_cols(Table2_CAR[[1]], Table2_CAR_GMR[[1]])
Table2 <- bind_cols(Table2, Table2_bsts)

Table2 <- subset(
  Table2,
  select = -c(N_num...1, N_num...7, N_region...8, N_num...13, N_region...14)
) %>%
  setNames(
    .,
    c(
      "Region",
      "0-3 days_CAR",
      "0-7 days_CAR",
      "0-14 days_CAR",
      "0-30 days_CAR",
      "0-3 days_GMR",
      "0-7 days_GMR",
      "0-14 days_GMR",
      "0-30 days_GMR",
      "0-3 days_BSTS",
      "0-7 days_BSTS",
      "0-14 days_BSTS",
      "0-30 days_BSTS"
    )
  ) %>%
  mutate(
    Region = factor(Region, levels = RowOrder)
  ) %>%
  arrange(Region)

write.csv(Table2, "outputs/table/Table2.csv")

### Table2 for LTLA level ------------------------------------------------
Table2_CAR_LTLA <- cal.cum.bs.LTLA(
  rawdata = HEI_dec0.4,
  region_data = region_num2,
  eff_data = eff_all
)


Table2_bsts_LTLA <- cal.cum.ci.LTLA(
  data = ci_m_base_gr,
  m.data = mat_base,
  table = ci_m_base_gr_table,
  tidy.out = TRUE
)

colnames(Table2_CAR_LTLA) <- c("ID", "N_region", "d3", "d7", "d14", "d30")


Table2LTLA <- merge(
  Table2_CAR_LTLA,
  Table2_bsts_LTLA,
  by.x = "LTLA_name",
  by.y = "N_region",
  all = TRUE
)
Table2LTLA <- subset(Table2LTLA, select = -c(LTLA_ID, N_num)) %>%
  setNames(
    .,
    c(
      "Region",
      "0-3 days_CAR",
      "0-7 days_CAR",
      "0-14 days_CAR",
      "0-30 days_CAR",
      "0-3 days_BSTS",
      "0-7 days_BSTS",
      "0-14 days_BSTS",
      "0-30 days_BSTS"
    )
  )

write.csv(Table2LTLA, "outputs/table/Table2_LTLA.csv")

# 5. Generate plot ----------------------------------------------------------
source("code/5.1.Fig3.R")
