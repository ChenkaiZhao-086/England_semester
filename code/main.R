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

# Stages execute in order; plotting can also read the published numeric exports.
local({
  retired <- c(
    "england.reviewer_revision",
    "england.weekly_growth",
    "england.weekday_effect",
    "england.bsts_daily"
  )
  if (
    any(vapply(retired, function(x) isTRUE(getOption(x, FALSE)), logical(1)))
  ) {
    stop(
      "Retired revision selected. Clear the old revision flags and choose england.stages through code/main.R."
    )
  }
  if (!identical(getOption("england.mobility", "retail_gr"), "retail_gr")) {
    stop(
      "The mobility-level experiment has been removed; use england.mobility = 'retail_gr'."
    )
  }
  stages <- getOption(
    "england.stages",
    c("prepare", "bsts", "car", "summary", "plot")
  )
  allowed <- c("prepare", "bsts", "car", "summary", "plot", "power")
  stopifnot(length(stages) > 0, all(stages %in% allowed))
  analysis_workers <- as.integer(getOption("england.workers", 4L))
  stopifnot(
    length(analysis_workers) == 1,
    !is.na(analysis_workers),
    analysis_workers >= 1
  )
  # CARBayesST 4.0 requires four workers or one worker running all four chains.
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
  source("code/function.R", local = TRUE)
  source("code/super_ci.R", local = TRUE)
  if ("power" %in% stages) {
    stopifnot(identical(stages, "power"))
    analysis_workers <- min(analysis_workers, 2L)
    source("code/3.4.Power_Analysis.R", local = TRUE)
    return(invisible(NULL))
  }
  car_from_exports <- !any(c("prepare", "car") %in% stages) &&
    (!file.exists(prepared_file) ||
      !file.exists(file.path(result_dir, "car-main.rds")))
  plot_from_exports <- car_from_exports
  if (!car_from_exports) {
    for (d in c(result_dir, table_dir, plot_dir)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  if ("prepare" %in% stages) {
    source("code/0.Data_import.R", local = TRUE)
    source("code/2.0.Data_prepare.R", local = TRUE)
    source("code/3.1.Data_prepare.R", local = TRUE)
  } else if (plot_from_exports) {
    # Published numeric exports are sufficient for redrawing without model files.
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
    message(
      "Retaining published CAR numeric exports; CAR checkpoints are unavailable."
    )
  } else {
    if (!file.exists(prepared_file)) {
      stop(
        "Per-metric inputs are not prepared. Run the prepare stage before fitting or reporting; original saved fits have not been changed."
      )
    }
    list2env(read.analysis(prepared_file), envir = environment())
    stopifnot(identical(mobility_panel_rule, "per-metric-v1"))
  }
  if ("summary" %in% stages && !"bsts" %in% stages) {
    list2env(read.analysis(bsts_prepared_file), envir = environment())
  }

  if ("bsts" %in% stages) {
    # BSTS input changes must not invalidate the frozen CAR dependency files.
    source("code/0.Data_import.R", local = TRUE)
    source("code/2.0.Data_prepare.R", local = TRUE)
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
    plan(future::cluster, workers = bsts_cluster)
    source("code/2.1.bsts_1_control.R", local = TRUE)
    source("code/2.2.bsts_3_control.R", local = TRUE)
    plan(sequential)
    parallel::stopCluster(bsts_cluster)
  }
  if ("car" %in% stages) {
    source("code/3.0.Fit_main.R", local = TRUE)
    source("code/3.2.bayesST.R", local = TRUE)
    source("code/3.3.KNN_analysis.R", local = TRUE)
  }
  if ("summary" %in% stages) {
    if (!car_from_exports) {
      source("code/1.0.Descriptive_analysis.R", local = TRUE)
    }
    source("code/4.Summarise.R", local = TRUE)
  }
  if ("plot" %in% stages) {
    if (!car_from_exports || identical(stages, "plot")) {
      source("code/1.4.Descriptive_plots.R", local = TRUE)
    }
    source("code/5.1.Fig3.R", local = TRUE)
  }
})

# Historical comments retained from the previous implementation.
# Stages execute in this order; omitted fit stages only read existing checkpoints.
# CARBayesST 4.0 must use either four parallel workers or one worker for four chains.
# save.image(file = "20260402.Rdata")
# load("20260402.Rdata")
# Worker functions must not capture prepared spatial data or previous fits.
