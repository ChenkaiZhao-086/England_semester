# Summarise saved CAR/BSTS fits, follow-up counts and model diagnostics.
# This script never fits a model; unavailable fits are recorded in Completion.csv.
if (!car_from_exports) {
  # Use the complete geographical lookup for reporting without changing fit inputs.
  old <- read.analysis(original_prepared)
  report_regions <- datasets$raw %>%
    distinct(LTLA_ID, LTLA_name) %>%
    left_join(old$LTLA_dat %>% distinct(LTLA_ID, N_region), by = "LTLA_ID") %>%
    left_join(region_num, by = "N_region")
  stopifnot(
    nrow(report_regions) == n_distinct(datasets$raw$LTLA_ID),
    !anyNA(report_regions$N_num),
    !anyDuplicated(report_regions$LTLA_ID)
  )
  region_corrections <- report_regions %>%
    left_join(
      regions %>% dplyr::select(LTLA_ID, previous_region = N_region),
      by = "LTLA_ID"
    ) %>%
    filter(is.na(previous_region) | previous_region != N_region)
  write.csv(
    region_corrections,
    file.path(table_dir, "Regional_label_corrections.csv"),
    row.names = FALSE
  )
  regions <- report_regions
  car_files <- file.path(result_dir, paste0("car-", specs$model, ".rds"))
  available <- which(file.exists(car_files))
  coefficients <- hall_results <- diagnostics <- beta_diagnostics <- residuals <- cumulative <- ltla_effects <- ltla_numeric <- list()
  for (i in available) {
    spec <- specs[i, ]
    dat <- datasets[[spec$dataset]]
    model <- read.analysis(car_files[i])$model
    beta <- do.call(rbind, model$samples$beta)
    colnames(beta) <- colnames(model$X)
    expected <- model.matrix(attr(model, "fit_settings")$formula, dat)
    stopifnot(
      length(model$samples$beta) == 4L,
      nrow(beta) >= 4000L,
      isTRUE(all.equal(
        unname(model$X),
        unname(expected),
        check.attributes = FALSE
      ))
    )
    coefficients[[spec$model]] <- bind_rows(lapply(
      colnames(beta),
      function(term) {
        increment <- if (term == spec$hall && term != "hall_share") {
          5000 / hall_reference
        } else if (term == "retail_gr") {
          0.6
        } else {
          1
        }
        q <- quantile(beta[, term], c(0.025, 0.5, 0.975))
        bind_cols(
          spec,
          data.frame(
            term = term,
            increment = increment,
            beta_lower = q[1],
            beta = q[2],
            beta_upper = q[3],
            lower = exp(increment * q[1]),
            ratio = exp(increment * q[2]),
            upper = exp(increment * q[3]),
            row.names = NULL
          )
        )
      }
    ))
    hall_results[[spec$model]] <- coefficients[[spec$model]] %>%
      filter(term == spec$hall) %>%
      mutate(
        n_ltla = n_distinct(dat$LTLA_ID),
        n_days = n_distinct(dat$date),
        n_rows = nrow(dat),
        unit = if_else(
          term == "hall_share",
          "1 percentage point of resident population",
          "5000 proxy students at unit temporal weight"
        ),
        growth_interval_days = 1L
      )
    diagnostics[[spec$model]] <- bind_rows(lapply(
      c("beta", "rho", "tau2", "nu2", "phi", "delta"),
      function(term) {
        p <- gelman.diag(
          model$samples[[term]],
          autoburnin = FALSE,
          multivariate = FALSE
        )$psrf
        data.frame(
          model = spec$model,
          parameter = term,
          n_chains = length(model$samples[[term]]),
          retained_draws = nrow(beta),
          max_PSRF = max(p[, 1]),
          max_PSRF_upper = max(p[, 2]),
          min_effective = min(effectiveSize(model$samples[[term]]))
        )
      }
    ))
    beta_psrf <- gelman.diag(
      model$samples$beta,
      autoburnin = FALSE,
      multivariate = FALSE
    )$psrf
    beta_diagnostics[[spec$model]] <- data.frame(
      model = spec$model,
      term = colnames(model$X),
      PSRF = beta_psrf[, 1],
      PSRF_upper = beta_psrf[, 2],
      effective = unname(effectiveSize(model$samples$beta)),
      row.names = NULL
    )
    residuals[[spec$model]] <- bind_rows(lapply(
      split(seq_len(nrow(dat)), dat$LTLA_ID),
      function(rows) {
        a <- acf(model$residuals$response[rows], lag.max = 14, plot = FALSE)$acf
        data.frame(
          model = spec$model,
          LTLA_ID = dat$LTLA_ID[rows[1]],
          lag1 = a[2],
          lag7 = a[8],
          lag14 = a[15]
        )
      }
    ))
    weekday_residual <- dat %>%
      transmute(
        weekday = wday(date, week_start = 1),
        residual = as.numeric(model$residuals$response)
      ) %>%
      summarise(n = n(), mean_residual = mean(residual), .by = weekday)
    write.csv(
      weekday_residual,
      file.path(table_dir, paste0("Residual_weekdays_", spec$model, ".csv")),
      row.names = FALSE
    )
    if (spec$model %in% c("main", "knn", "no_mobility")) {
      for (term in c(spec$hall, spec$mobility[spec$mobility != ""])) {
        effect <- car.effect(model, term, baseline = 0)
        key <- paste(spec$model, term, sep = "_")
        cumulative[[key]] <- cal.cum.bs(dat, regions, effect)$raw_plot_data %>%
          mutate(model = spec$model, term = term)
        ltla_effects[[key]] <- cal.cum.bs.LTLA(dat, regions, effect) %>%
          mutate(model = spec$model, term = term)
        paths <- car.paths(dat, regions, effect)
        ltla_numeric[[key]] <- bind_rows(lapply(paths, function(path) {
          car.curve(list(path), effect$beta) %>%
            mutate(
              LTLA_ID = path$LTLA_ID[1],
              LTLA_name = path$LTLA_name[1],
              model = spec$model,
              term = term
            )
        }))
      }
    }
  }
  coefficients <- bind_rows(coefficients)
  hall_results <- bind_rows(hall_results)
  diagnostics <- bind_rows(diagnostics)
  beta_diagnostics <- bind_rows(beta_diagnostics)
  if (nrow(hall_results)) {
    hall_results <- hall_results %>%
      left_join(
        diagnostics %>%
          summarise(
            max_PSRF_upper = max(max_PSRF_upper),
            min_effective = min(min_effective),
            .by = model
          ),
        by = "model"
      )
  }
  residuals <- bind_rows(residuals)
  cumulative <- bind_rows(cumulative)
  ltla_effects <- bind_rows(ltla_effects)
  ltla_numeric <- bind_rows(ltla_numeric)
  if (nrow(ltla_numeric)) {
    ltla_numeric <- ltla_numeric %>%
      mutate(
        ratio = exp(cum.effect),
        lower = exp(cum.effect.lower),
        upper = exp(cum.effect.upper)
      )
  }
  for (n in c(
    "coefficients",
    "hall_results",
    "diagnostics",
    "beta_diagnostics",
    "residuals",
    "cumulative",
    "ltla_effects",
    "ltla_numeric"
  )) {
    write.csv(
      get(n),
      file.path(table_dir, paste0("CAR_", n, ".csv")),
      row.names = FALSE
    )
  }
  if (length(available)) {
    write.analysis(
      mget(c(
        "coefficients",
        "hall_results",
        "diagnostics",
        "beta_diagnostics",
        "residuals",
        "cumulative",
        "ltla_effects",
        "ltla_numeric",
        "mobility_panel_rule"
      )),
      file.path(result_dir, "car-summary.rds"),
      c(prepared_file, car_files[available])
    )
    print(hall_results)
    print(
      diagnostics %>%
        summarise(
          max_PSRF_upper = max(max_PSRF_upper),
          min_effective = min(min_effective),
          .by = model
        )
    )
  }
} else {
  # Deleted CAR draws cannot be reconstructed; preserve their published estimates.
  for (n in c(
    "coefficients",
    "hall_results",
    "diagnostics",
    "beta_diagnostics",
    "residuals",
    "cumulative",
    "ltla_effects",
    "ltla_numeric"
  )) {
    assign(n, read.csv(file.path(table_dir, paste0("CAR_", n, ".csv"))))
  }
  car_files <- file.path(result_dir, paste0("car-", specs$model, ".rds"))
  mobility_effects <- read.csv(file.path(table_dir, "Mobility_effects.csv"))
  totals <- read.csv(file.path(table_dir, "Descriptive_totals.csv"))
}

LTLA_gr <- observations
bsts_curves <- bsts_ltla <- bsts_ltla_numeric <- list()
bsts_files <- file.path(result_dir, paste0("bsts-", names(matches), ".rds"))
for (i in which(file.exists(bsts_files))) {
  group <- names(matches)[i]
  fit <- read.analysis(bsts_files[i])
  stopifnot(
    identical(fit$matches, matches[[group]]),
    identical(fit$outcome, bsts_outcome)
  )
  bsts_curves[[group]] <- cal.cum.ci.region(
    fit$fits,
    fit$matches,
    fit$report
  ) %>%
    mutate(group = group)
  bsts_ltla[[group]] <- cal.cum.ci.LTLA(
    fit$fits,
    fit$matches,
    fit$report,
    tidy.out = TRUE
  ) %>%
    mutate(group = group)
  paths <- bsts.paths(fit$fits, fit$matches, fit$report)
  bsts_ltla_numeric[[group]] <- bind_rows(lapply(paths, function(path) {
    bsts.curve(list(path)) %>%
      mutate(
        LTLA_ID = path$LTLA_ID,
        LTLA_name = path$LTLA_name,
        N_num = path$N_num,
        group = group
      )
  }))
}
if (length(bsts_curves)) {
  bsts_curves <- bind_rows(bsts_curves) %>%
    mutate(
      ratio = exp(cum.effect),
      lower = exp(cum.effect.lower),
      upper = exp(cum.effect.upper)
    )
  bsts_ltla <- bind_rows(bsts_ltla)
  bsts_ltla_numeric <- bind_rows(bsts_ltla_numeric) %>%
    mutate(
      ratio = exp(cum.effect),
      lower = exp(cum.effect.lower),
      upper = exp(cum.effect.upper)
    )
  write.csv(
    bsts_curves,
    file.path(table_dir, "BSTS_curves.csv"),
    row.names = FALSE
  )
  write.csv(
    filter(bsts_curves, date %in% c(3, 7, 14, 30)),
    file.path(table_dir, "BSTS_horizons.csv"),
    row.names = FALSE
  )
  write.csv(bsts_ltla, file.path(table_dir, "BSTS_LTLA.csv"), row.names = FALSE)
  write.csv(
    bsts_ltla_numeric,
    file.path(table_dir, "BSTS_LTLA_numeric.csv"),
    row.names = FALSE
  )
  bsts_counts <- bsts_ltla_numeric %>%
    filter(date %in% c(3, 7, 14, 30)) %>%
    summarise(
      n_eligible = sum(n_ltla == 1),
      n_positive = sum(cum.effect.lower > 0, na.rm = TRUE),
      n_negative = sum(cum.effect.upper < 0, na.rm = TRUE),
      .by = c(group, date)
    )
  write.csv(
    bsts_counts,
    file.path(table_dir, "BSTS_LTLA_counts.csv"),
    row.names = FALSE
  )
  bsts_any <- bsts_ltla_numeric %>%
    filter(date %in% c(3, 7, 14, 30)) %>%
    summarise(
      eligible_horizons = sum(n_ltla == 1),
      positive_horizons = sum(cum.effect.lower > 0, na.rm = TRUE),
      negative_horizons = sum(cum.effect.upper < 0, na.rm = TRUE),
      .by = c(group, LTLA_ID)
    ) %>%
    summarise(
      n_eligible = sum(eligible_horizons > 0),
      n_any_positive = sum(positive_horizons > 0),
      n_any_negative = sum(negative_horizons > 0),
      n_any_excludes_one = sum(positive_horizons > 0 | negative_horizons > 0),
      n_all_four_positive = sum(positive_horizons == 4),
      .by = group
    )
  write.csv(
    bsts_any,
    file.path(table_dir, "BSTS_any_horizon_counts.csv"),
    row.names = FALSE
  )
  write.analysis(
    list(
      curves = bsts_curves,
      ltla = bsts_ltla,
      ltla_numeric = bsts_ltla_numeric,
      outcome = bsts_outcome
    ),
    file.path(result_dir, "bsts-summary.rds"),
    c(bsts_prepared_file, bsts_files[file.exists(bsts_files)])
  )
}
if (nrow(cumulative)) {
  if (car_from_exports) {
    car_horizons <- read.csv(file.path(table_dir, "CAR_horizons.csv"))
  } else {
    car_horizons <- cumulative %>%
      filter(date %in% c(3, 7, 14, 30)) %>%
      mutate(
        ratio = exp(cum.effect),
        lower = exp(cum.effect.lower),
        upper = exp(cum.effect.upper)
      )
    write.csv(
      car_horizons,
      file.path(table_dir, "CAR_horizons.csv"),
      row.names = FALSE
    )
  }
  if (is.data.frame(bsts_curves) && nrow(bsts_curves)) {
    table2 <- car_horizons %>%
      filter(model == "main", term == "h_index") %>%
      dplyr::select(
        N_num,
        N_region,
        day = date,
        CAR_n_ltla = n_ltla,
        CAR_ratio = ratio,
        CAR_lower = lower,
        CAR_upper = upper
      ) %>%
      full_join(
        bsts_curves %>%
          filter(group == "mat_base", date %in% c(3, 7, 14, 30)) %>%
          dplyr::select(
            N_num,
            N_region,
            day = date,
            BSTS_n_ltla = n_ltla,
            BSTS_ratio = ratio,
            BSTS_lower = lower,
            BSTS_upper = upper
          ),
        by = c("N_num", "N_region", "day")
      )
    write.csv(
      table2,
      file.path(table_dir, "Table2_Hall_and_BSTS.csv"),
      row.names = FALSE
    )
  }
}
completion <- bind_rows(
  data.frame(
    type = "CAR",
    model = specs$model,
    complete = if (car_from_exports) {
      specs$model %in% coefficients$model
    } else {
      file.exists(car_files)
    },
    checkpoint_available = file.exists(car_files),
    source = if (car_from_exports) {
      "saved_numeric_exports"
    } else {
      "model_checkpoint"
    }
  ),
  data.frame(
    type = "BSTS",
    model = names(matches),
    complete = file.exists(bsts_files),
    checkpoint_available = file.exists(bsts_files),
    source = "model_checkpoint"
  )
)
write.csv(completion, file.path(table_dir, "Completion.csv"), row.names = FALSE)
bsts_specs <- data.frame(
  group = names(matches),
  n_matched_cases = lengths(matches),
  outcome = bsts_outcome,
  formula = "log(S7_t / S7_t_minus_1); S7 = trailing 7-day raw case sum",
  zero_handling = "Replace zero totals with 0.5 after summing raw daily counts",
  first = study_start,
  last = study_end,
  n_days = as.integer(study_end - study_start) + 1L,
  lookback_first = study_start - 7L,
  niter = 5000L,
  nseasons = 7L,
  season_duration = 1L
)
write.csv(
  bsts_specs,
  file.path(table_dir, "BSTS_model_specifications.csv"),
  row.names = FALSE
)
if (!car_from_exports) {
  write.csv(
    data.frame(model = names(model_labels), label = unname(model_labels)),
    file.path(table_dir, "Model_labels.csv"),
    row.names = FALSE
  )
  membership <- bind_rows(lapply(names(datasets), function(n) {
    datasets[[n]] %>% distinct(LTLA_ID, LTLA_name) %>% mutate(panel = n)
  }))
  write.csv(
    membership,
    file.path(table_dir, "Panel_membership.csv"),
    row.names = FALSE
  )
  write.csv(
    model_counts,
    file.path(table_dir, "Model_counts.csv"),
    row.names = FALSE
  )
  write.csv(
    model_exclusions,
    file.path(table_dir, "Model_exclusions.csv"),
    row.names = FALSE
  )
  # Retail is the exact main fit, not an independent estimate on another sample.
  mobility_models <- specs %>%
    filter(model == "main" | startsWith(model, "mobility_")) %>%
    transmute(model, metric = mobility)
  mobility_effects <- mobility_models %>%
    left_join(coefficients %>% filter(term == mobility), by = "model") %>%
    left_join(
      model_counts %>%
        dplyr::select(model, n_ltla, n_days, n_rows, excluded_ltla),
      by = "model"
    )
  write.csv(
    mobility_effects,
    file.path(table_dir, "Mobility_effects.csv"),
    row.names = FALSE
  )
}
writeLines(
  c(
    "Current CAR daily-growth / BSTS rolling-seven-day-total-growth analysis",
    sprintf(
      "Saved checkpoints available: %d/%d CAR and %d/%d BSTS.",
      sum(completion$checkpoint_available[completion$type == "CAR"]),
      nrow(specs),
      sum(completion$checkpoint_available[completion$type == "BSTS"]),
      length(matches)
    ),
    sprintf(
      "Outcome dates: %s through %s inclusive (%d days).",
      study_start,
      study_end,
      as.integer(study_end - study_start) + 1L
    ),
    "CAR outcome: unsmoothed daily log growth, replacing daily zero cases with 0.5.",
    "BSTS outcome: adjacent-day log ratio of trailing seven-day raw case totals; replace zero totals with 0.5 after summing.",
    sprintf(
      "Case descriptive sample: %d mapped LTLA rows; this is separate from the 376-area main CAR panel.",
      totals$n_regions
    ),
    "CAR: Monday and weekend terms relative to Tuesday-Friday; binary-weekend comparator retained.",
    "BSTS: unchanged matching sets and seven-day seasonality; see BSTS_model_specifications.csv.",
    "Mobility samples (LTLAs):",
    paste0("  ", mobility_effects$metric, ": ", mobility_effects$n_ltla),
    "Retail reporting reuses the exact 376-LTLA main model; other categories use individually complete panels.",
    "Population-relative and matched absolute Hall analyses retain 369 LTLAs.",
    "See Model_counts.csv, Model_exclusions.csv and Panel_membership.csv for denominators and exclusion details.",
    "Cumulative results sum day 1-D and require complete follow-up; retail contribution is retail_gr * beta, referenced to zero.",
    "Regional labels are restored from the complete original geography lookup; aggregation and fitted inputs are unchanged.",
    "City of London and Hackney still share the agreed merged-case and mobility mapping.",
    "Case totals and descriptive case curves sum mapped LTLA rows, including that shared case curve twice; they are not deduplicated national case totals.",
    "The CAR weekly-growth sensitivity and mobility-level experiments remain withdrawn; power simulation is excluded.",
    "Only current model specifications appear in these summaries; superseded common-panel results have been removed.",
    "See CAR_diagnostics.csv and CAR_beta_diagnostics.csv before interpreting model estimates."
  ),
  file.path(revision_dir, "Results_readme.txt")
)

# Historical comments retained from the previous implementation.
# Compute only summaries and plot inputs from prepared data and saved model draws.
# No model fitting occurs in this stage.
# Uncentred mobility contribution: x * beta, referenced to growth ratio zero.
# A constant index has x = 1 and therefore still contributes beta each day.
# 4. Generate Table 2 ------------------------------------------------------
### Table2 in main analysis -----------------------------------------------
### Table2 for LTLA level ------------------------------------------------
#### Prepare data for plot
# Denominators describe complete follow-up at each requested horizon.
