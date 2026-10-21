# Prepare CAR covariates, Hall profiles, panels and model specifications.
# Main/population samples are unchanged; each other mobility metric uses its own complete areas.
static <- old$HEI_dec0.4 %>%
  distinct(
    LTLA_ID,
    LTLA_name,
    reopen_date,
    Pop_Den_km2,
    Prosperity,
    areaCode
  ) %>%
  mutate(
    reopen_date = as.Date(reopen_date),
    case_id = if_else(LTLA_ID == 295, 284, LTLA_ID)
  )
mob_lookup <- old$LTLA_dat %>%
  mutate(
    Google = if_else(
      LTLA_ID == 260,
      "Merseyside_Metropolitan Borough of St Helens",
      Google
    )
  ) %>%
  distinct(LTLA_ID, LTLA_name, Google)
area_lookup <- static %>%
  dplyr::select(LTLA_ID, LTLA_name) %>%
  left_join(mob_lookup, by = c("LTLA_ID", "LTLA_name"))
area_lookup$Google[area_lookup$LTLA_ID == 295] <- area_lookup$Google[
  area_lookup$LTLA_ID == 284
]
stopifnot(!anyNA(area_lookup$Google), !anyDuplicated(area_lookup$LTLA_ID))
metric_names <- c(
  retail_gr = "retail_and_recreation_percent_change_from_baseline",
  grocery_gr = "grocery_and_pharmacy_percent_change_from_baseline",
  parks_gr = "parks_percent_change_from_baseline",
  transit_gr = "transit_stations_percent_change_from_baseline",
  work_gr = "workplaces_percent_change_from_baseline",
  residential_gr = "residential_percent_change_from_baseline"
)
mobility <- old$gmr %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= study_end)
mob <- crossing(
  area_lookup,
  date = seq(min(mobility$date), study_end, by = "day")
) %>%
  left_join(mobility, by = c("Google", "date")) %>%
  arrange(LTLA_ID, date) %>%
  group_by(LTLA_ID) %>%
  mutate(across(
    all_of(unname(metric_names)),
    ~ zoo::na.locf(.x, na.rm = FALSE)
  )) %>%
  ungroup()
for (v in names(metric_names)) {
  m <- metric_names[[v]]
  mob <- mob %>%
    group_by(LTLA_ID) %>%
    mutate("{v}" := (100 + .data[[m]]) / lag(100 + .data[[m]])) %>%
    ungroup()
}
mob <- mob %>%
  dplyr::select(
    LTLA_ID,
    date,
    all_of(names(metric_names)),
    retail_level = retail_and_recreation_percent_change_from_baseline
  )
panel <- crossing(static, date = seq(study_start, study_end, by = "day")) %>%
  left_join(
    cases %>% dplyr::select(case_id = LTLA_ID, date, case = adjusted, log_GR),
    by = c("case_id", "date")
  ) %>%
  left_join(mob, by = c("LTLA_ID", "date")) %>%
  mutate(
    week = as.integer(date - study_start) + 1L,
    weekend = as.integer(wday(date) %in% c(1, 7)),
    day_type = factor(
      ifelse(
        weekend == 1,
        "weekend",
        ifelse(wday(date) == 2, "Monday", "Tuesday_Friday")
      ),
      levels = c("Tuesday_Friday", "Monday", "weekend")
    )
  ) %>%
  arrange(date, LTLA_ID)
contrasts(panel$day_type) <- contr.treatment(levels(panel$day_type), base = 1)
pulses <- old$HEI_cam_dat %>%
  mutate(LTLA_name = trimws(Campus_name), date = as.Date(HEI_opening)) %>%
  summarise(pulse = sum(Hall_index, na.rm = TRUE), .by = c(LTLA_name, date))
pulse_grid <- panel %>%
  dplyr::select(LTLA_ID, LTLA_name, date) %>%
  mutate(
    LTLA_name = case_when(
      LTLA_ID == 48 ~ "Cornwall and Isles of Scilly",
      LTLA_ID %in% c(284, 295) ~ "Hackney and City of London",
      TRUE ~ LTLA_name
    )
  ) %>%
  left_join(pulses, by = c("LTLA_name", "date")) %>%
  mutate(h_index = replace_na(pulse, 0))
kernels <- list(
  h_index = gen.decay(30, "exp(-0.4*x)", 6),
  hall_flat = gen.decay(30, "1+0*x", 6),
  hall_day3 = gen.decay(30, "exp(-0.4*x)", 3),
  hall_window7 = gen.decay(7, "exp(-0.4*x)", 6),
  hall_window14 = gen.decay(14, "exp(-0.4*x)", 6),
  hall_window21 = gen.decay(21, "exp(-0.4*x)", 6)
)
for (v in names(kernels)) {
  exposure <- bind_rows(lapply(
    split(pulse_grid, pulse_grid$LTLA_ID),
    get.index,
    decay = kernels[[v]]
  )) %>%
    dplyr::select(LTLA_ID, date, !!v := h_index)
  panel <- left_join(panel, exposure, by = c("LTLA_ID", "date"))
}
reference <- old$HEI_dec0.4 %>%
  transmute(
    LTLA_ID,
    date = as.Date(date),
    original_y = log_GR,
    original_h = h_index
  )
overlap <- inner_join(panel, reference, by = c("LTLA_ID", "date"))
stopifnot(
  nrow(overlap) == nrow(reference),
  max(abs(overlap$log_GR - overlap$original_y)) < 1e-12,
  max(abs(overlap$h_index - overlap$original_h)) < 1e-10
)
covars <- c("h_index", "Pop_Den_km2", "Prosperity", "week", "weekend")
raw <- complete.panel(panel, c("log_GR", "retail_gr", covars))
pop <- read_excel(population_file, sheet = "MYE2 - Persons", skip = 7) %>%
  transmute(LTLA_CD = Code, population_2020 = `All ages`)
pop_lookup <- distinct(old$LTLA_dat, LTLA_name, LTLA_CD) %>%
  left_join(pop, by = "LTLA_CD")
population <- raw %>%
  left_join(pop_lookup, by = "LTLA_name") %>%
  filter(!LTLA_ID %in% c(48, 284, 295, 56, 57, 58, 59)) %>%
  mutate(hall_share = 100 * hall_reference * h_index / population_2020)
stopifnot(all(is.finite(population$hall_share)))
datasets <- list(raw = raw, population = population)
# Retail uses the main panel; other categories do not inherit missingness elsewhere.
for (metric in setdiff(names(metric_names), "retail_gr")) {
  datasets[[metric]] <- complete.panel(raw, c("log_GR", metric, covars))
}
specs <- tribble(
  ~model                , ~dataset     , ~outcome , ~hall        , ~mobility   , ~calendar  , ~weights    ,
  "main"                , "raw"        , "log_GR" , "h_index"    , "retail_gr" , "day_type" , "adjacency" ,
  "binary_weekend"      , "raw"        , "log_GR" , "h_index"    , "retail_gr" , "weekend"  , "adjacency" ,
  "no_mobility"         , "raw"        , "log_GR" , "h_index"    , ""          , "day_type" , "adjacency" ,
  "knn"                 , "raw"        , "log_GR" , "h_index"    , "retail_gr" , "day_type" , "knn"       ,
  "population_absolute" , "population" , "log_GR" , "h_index"    , "retail_gr" , "day_type" , "adjacency" ,
  "population_share"    , "population" , "log_GR" , "hall_share" , "retail_gr" , "day_type" , "adjacency" ,
  "hall_flat"           , "raw"        , "log_GR" , "hall_flat"  , "retail_gr" , "day_type" , "adjacency" ,
  "hall_day3"           , "raw"        , "log_GR" , "hall_day3"  , "retail_gr" , "day_type" , "adjacency"
)
specs <- bind_rows(
  specs,
  tibble(
    model = paste0("window", c(7, 14, 21)),
    dataset = "raw",
    outcome = "log_GR",
    hall = paste0("hall_window", c(7, 14, 21)),
    mobility = "retail_gr",
    calendar = "day_type",
    weights = "adjacency"
  ),
  tibble(
    model = paste0("mobility_", setdiff(names(metric_names), "retail_gr")),
    dataset = setdiff(names(metric_names), "retail_gr"),
    outcome = "log_GR",
    hall = "h_index",
    mobility = setdiff(names(metric_names), "retail_gr"),
    calendar = "day_type",
    weights = "adjacency"
  )
)
W_mat <- old$W_mat
W_mat_KNN <- old$W_mat_KNN
panel_weights <- lapply(datasets, function(dat) {
  areas <- dat$LTLA_name[dat$date == study_start]
  W <- W_mat[areas, areas, drop = FALSE]
  stopifnot(
    length(areas) > 0L,
    identical(dat$LTLA_name, rep(areas, 188L)),
    identical(range(dat$date), c(study_start, study_end))
  )
  if (any(rowSums(W) == 0)) {
    stop(
      "Panel has no spatial neighbours for: ",
      paste(areas[rowSums(W) == 0], collapse = ", ")
    )
  }
  W
})
counts <- bind_rows(lapply(names(datasets), function(n) {
  dat <- datasets[[n]]
  data.frame(
    panel = n,
    n_ltla = nrow(panel_weights[[n]]),
    n_days = n_distinct(dat$date),
    n_rows = nrow(dat),
    first = min(dat$date),
    last = max(dat$date)
  )
}))
# Keep existing CAR checkpoint dependencies intact; BSTS has its own inputs.
if (!file.exists(core_prepared_file) && identical(stages, "power")) {
  # Reconstruct only in the isolated power directory, checking published membership.
  stopifnot(identical(result_dir, "outputs/power-2026-10-16/results"))
  published <- read.csv(
    "outputs/latest-results-2026-10-16/table/Panel_membership.csv"
  )
  stopifnot(
    isTRUE(all.equal(
      distinct(raw, LTLA_ID, LTLA_name),
      as_tibble(published[published$panel == "raw", c("LTLA_ID", "LTLA_name")])
    )),
    n_distinct(raw$date) == 188L,
    identical(range(raw$date), c(study_start, study_end))
  )
  write.analysis(
    mget(c(
      "datasets",
      "matches",
      "HEI_dat",
      "W_mat",
      "W_mat_KNN",
      "regions",
      "region_num",
      "kernels"
    )),
    core_prepared_file,
    c(original_prepared, original_bsts, population_file, case_file)
  )
}
core <- read.analysis(core_prepared_file)
for (n in c("raw", "population")) {
  stopifnot(identical(datasets[[n]], core$datasets[[n]]))
}
for (n in c(
  "matches",
  "HEI_dat",
  "W_mat",
  "W_mat_KNN",
  "regions",
  "region_num",
  "kernels"
)) {
  stopifnot(identical(get(n), core[[n]]))
}
stopifnot(n_distinct(raw$LTLA_ID) == 376L)
rm(core)
mobility_panel_rule <- "per-metric-v1"
model_input_files <- setNames(rep(core_prepared_file, nrow(specs)), specs$model)
model_input_files[startsWith(specs$model, "mobility_")] <- prepared_file
# Exclusion denominators are the same 376 candidate LTLAs, not the UK lookup table.
model_exclusions <- bind_rows(lapply(seq_len(nrow(specs)), function(i) {
  spec <- specs[i, ]
  variables <- unique(c(
    spec$outcome,
    spec$hall,
    spec$mobility[spec$mobility != ""],
    "Pop_Den_km2",
    "Prosperity",
    "week",
    "weekend"
  ))
  if (spec$dataset == "population") {
    excluded <- anti_join(
      distinct(raw, LTLA_ID, LTLA_name),
      distinct(population, LTLA_ID),
      by = "LTLA_ID"
    )
    return(
      excluded %>%
        mutate(
          model = spec$model,
          metric = spec$mobility,
          variable = "population_2020",
          reason = if_else(
            LTLA_ID %in% c(56, 57, 58, 59),
            "population_boundary_code_mismatch",
            "population_case_geography_mismatch"
          ),
          invalid_days = NA_integer_,
          first_invalid = as.Date(NA),
          last_invalid = as.Date(NA)
        )
    )
  }
  raw %>%
    dplyr::select(LTLA_ID, LTLA_name, date, all_of(variables)) %>%
    pivot_longer(all_of(variables), names_to = "variable") %>%
    filter(!is.finite(value)) %>%
    mutate(
      reason = if_else(
        is.na(value),
        "missing_model_variable",
        "nonfinite_model_variable"
      )
    ) %>%
    summarise(
      invalid_days = n(),
      first_invalid = first(date),
      last_invalid = last(date),
      .by = c(LTLA_ID, LTLA_name, variable, reason)
    ) %>%
    mutate(model = spec$model, metric = spec$mobility)
}))
model_counts <- specs %>%
  dplyr::select(model, dataset, mobility) %>%
  left_join(counts, by = c("dataset" = "panel")) %>%
  mutate(
    candidate_ltla = n_distinct(raw$LTLA_ID),
    excluded_ltla = candidate_ltla - n_ltla
  )
exclusion_summary <- model_exclusions %>%
  summarise(
    excluded_with_reason = n_distinct(LTLA_ID),
    exclusion_reason = paste(sort(unique(reason)), collapse = "; "),
    .by = model
  )
model_counts <- model_counts %>%
  left_join(exclusion_summary, by = "model") %>%
  mutate(
    excluded_with_reason = replace_na(excluded_with_reason, 0L),
    exclusion_reason = replace_na(exclusion_reason, "none")
  )
stopifnot(all(model_counts$excluded_ltla == model_counts$excluded_with_reason))
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
source_audit <- data.frame(
  overlap_records = nrow(audit),
  differing_cases = 0,
  lookback_first = min(cases$date),
  outcome_first = study_start,
  outcome_last = study_end
)
write.csv(
  source_audit,
  file.path(table_dir, "Source_date_audit.csv"),
  row.names = FALSE
)
write.csv(counts, file.path(table_dir, "Panel_counts.csv"), row.names = FALSE)
write.csv(
  specs,
  file.path(table_dir, "Model_specifications.csv"),
  row.names = FALSE
)
write.analysis(
  mget(c(
    "datasets",
    "specs",
    "counts",
    "cases",
    "observations",
    "matches",
    "region_num",
    "regions",
    "HEI_dat",
    "W_mat",
    "W_mat_KNN",
    "kernels",
    "source_audit",
    "panel_weights",
    "model_counts",
    "model_exclusions",
    "model_input_files",
    "mobility_panel_rule"
  )),
  prepared_file,
  c(
    original_prepared,
    original_bsts,
    population_file,
    case_file,
    core_prepared_file
  )
)
print(counts)

# Historical comments retained from the previous implementation.
### 3.1 Prepare data for ST analysis, calculate H.index in each LTLA ---
### 3.2 Prepare h index and Google mobility report data
# n.hall = sum(Number_of_hall),
# mcf = if_else(is.na(mcf), Number_of_hall / n.hall, mcf),
# h.ref = 40485,
# "HEI_ID", "HEI_name", "Campus_ID", "Economic_Quality", "Business_Environment", "Education", "Health", "Safe_Security", "Social_Capital", "Environment", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline"
# names(HEI)
# Carry the actual opening date through the Cornwall/London geometry splits.
### 3.3 prepare case date, including case, growth rate and log GR. Construct a basic model without sptial correlation
### for matching the spatial data, reconstructed 48.Cornwall, 284.City of London, 295.Hackey. the data of 284 and 295 are exactly same
# get.index(dat = HEI, decay = decay0.4)
# group_by(LTLA_ID, date) %>%
# mutate(week_trend = row_number()) %>%
# summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4))
### 3.4 prepare spatial data and adjacent matrix
# Eng_shp <- st_transform(Eng_shp, crs = 4326)
## !!!!! OBJECTID is not equal to LTLA_ID !!!!!
## Calculate KNN matrix -----------------------------------------
### KNN Neighbor
### Connect Northern Ireland to other region
# Get coords of each LTLA
# Calculate the distance matrix (Euclidean distance, unit meters)
# 这里的矩阵行数是北爱尔兰的数量，列数是大不列颠的数量
# 截取右上角矩阵块：行是NI，列是GB
# 步骤 C: 找到最近的一对点
# 转换回原始数据的索引 ID
# 步骤 D: 手动添加连接 (更新 nb 对象)
# plot(st_geometry(Eng_shp_no_island), border = "grey")
# plot(nb, coords, add = TRUE, col = "red", lwd = 1, pch = 19, cex = 0.5)
# plot(W_adj, coords, add = TRUE, col = "red", lwd = 1, pch = 19, cex = 0.5)
# segments(coords[target_NI_idx, 1], coords[target_NI_idx, 2],
#          coords[target_GB_idx, 1], coords[target_GB_idx, 2],
#          col = "blue", lwd = 3)
