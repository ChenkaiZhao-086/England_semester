# Packages and shared functions --------------------------------------------
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

source("code/function.R", local = TRUE)
source("code/super_ci.R", local = TRUE)

# Shared settings ----------------------------------------------------------
analysis_workers <- as.integer(getOption("england.workers", 4L))
stopifnot(
  length(analysis_workers) == 1,
  !is.na(analysis_workers),
  analysis_workers >= 1
)
car_cores <- if (analysis_workers >= 4L) 4L else 1L
revision_dir <- "outputs/unified-revision-2026-10-12"
result_dir <- file.path(revision_dir, "results")
table_dir <- file.path(revision_dir, "table")
plot_dir <- file.path(revision_dir, "figures")
core_prepared_file <- file.path(result_dir, "prepared.rds")
prepared_file <- file.path(result_dir, "prepared-per-metric.rds")
bsts_prepared_file <- file.path(result_dir, "bsts-prepared.rds")
original_prepared <- "outputs/results/prepared.rds"
original_bsts <- "outputs/results/bsts-matches.rds"
population_file <- "data/ukpopestimatesmid2020on2020geography.xls"
case_file <- "data/raw/Eng_ltla_case.csv"
study_start <- as.Date("2020-06-01")
study_end <- as.Date("2020-12-05")
hall_reference <- 40485
model_labels <- c(
  main = "Main model",
  binary_weekend = "Binary weekend comparator",
  no_mobility = "Without mobility",
  knn = "KNN adjacency",
  population_absolute = "Restricted sample: absolute Hall",
  population_share = "Restricted sample: population-relative Hall",
  hall_flat = "Day 6 onset without decay",
  hall_day3 = "Day 3 onset with decay",
  window7 = "7-day influence window",
  window14 = "14-day influence window",
  window21 = "21-day influence window",
  mobility_grocery_gr = "Grocery/pharmacy: available LTLAs",
  mobility_parks_gr = "Parks: available LTLAs",
  mobility_transit_gr = "Transit: available LTLAs",
  mobility_work_gr = "Workplaces: available LTLAs",
  mobility_residential_gr = "Residential: available LTLAs"
)

stages <- character()
car_from_exports <- !file.exists(prepared_file) ||
  !file.exists(file.path(result_dir, "car-main.rds"))
plot_from_exports <- car_from_exports
if (car_from_exports) {
  table_dir <- "outputs/latest-results-2026-10-16/table"
  plot_dir <- "outputs/latest-results-2026-10-16/figures"
}
for (d in c(table_dir, plot_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 0. Data import -----------------------------------------------------------
# Reads the original prepared geography/matching and audits raw daily cases.
source("code/0.Data_import.R", local = TRUE)
cases <- raw_cases %>% inner_join(lookup, by = "areaCode")

# 1. Descriptive analysis --------------------------------------------------
source("code/1.0.Descriptive_analysis.R", local = TRUE)

# 2. BSTS ------------------------------------------------------------------
stages <- "bsts"
source("code/2.0.Data_prepare.R", local = TRUE)

### Parallel workers for the two BSTS scripts
bsts_functions <- new.env(parent = globalenv())
sys.source("code/function.R", envir = bsts_functions)
sys.source("code/super_ci.R", envir = bsts_functions)
bsts_cluster <- parallel::makePSOCKcluster(
  analysis_workers,
  rscript = getOption(
    "england.worker_rscript",
    file.path(R.home("bin"), "Rscript")
  )
)

tryCatch(
  {
    plan(future::cluster, workers = bsts_cluster)

    ### 1 control LTLA
    source("code/2.1.bsts_1_control.R", local = TRUE)

    ### 3 control LTLAs
    source("code/2.2.bsts_3_control.R", local = TRUE)
  },
  finally = {
    plan(sequential)
    parallel::stopCluster(bsts_cluster)
  }
)

# 3. Bayesian spatio-temporal ----------------------------------------------
stages <- "car"
list2env(read.analysis(prepared_file), envir = environment())
stopifnot(identical(mobility_panel_rule, "per-metric-v1"))


### Main CAR and binary-weekend comparator
source("code/3.0.Fit_main.R", local = TRUE)

### Sensitivity analyses: mobility, population and Hall profiles
source("code/3.2.bayesST.R", local = TRUE)

### KNN analysis
source("code/3.3.KNN_analysis.R", local = TRUE)

### Power analysis (optional; execute separately in a fresh R session)
# source("code/3.4.Power_Analysis.R")

# 4. Generate tables -------------------------------------------------------
stages <- "summary"
car_from_exports <- !file.exists(prepared_file) ||
  !file.exists(file.path(result_dir, "car-main.rds"))
plot_from_exports <- car_from_exports
table_dir <- file.path(revision_dir, "table")
plot_dir <- file.path(revision_dir, "figures")
if (car_from_exports) {
  table_dir <- "outputs/latest-results-2026-10-16/table"
  plot_dir <- "outputs/latest-results-2026-10-16/figures"
  model_counts <- read.csv(file.path(table_dir, "Model_counts.csv"))
  specs <- read.csv(file.path(table_dir, "Model_specifications.csv"))
  stopifnot(
    nrow(specs) == 16L,
    all(specs$outcome == "log_GR"),
    all(model_counts$n_days == 188L),
    !any(startsWith(specs$model, "common_"))
  )
} else {
  list2env(read.analysis(prepared_file), envir = environment())
  stopifnot(identical(mobility_panel_rule, "per-metric-v1"))
  source("code/1.0.Descriptive_analysis.R", local = TRUE)
}
list2env(read.analysis(bsts_prepared_file), envir = environment())
source("code/4.Summarise.R", local = TRUE)

# 5. Generate plots --------------------------------------------------------

stages <- "plot"
source("code/1.4.Descriptive_plots.R", local = TRUE)
source("code/5.1.Fig3.R", local = TRUE)
