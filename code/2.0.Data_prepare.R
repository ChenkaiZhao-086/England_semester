# Prepare rolling-seven-day-total BSTS growth, openings and saved matching sets.
# Matching is reused exactly; no new matching is performed here.
match_names <- c(
  "mat_base",
  "mat_nolondon",
  "mat_over2hall",
  "mat_base3",
  "mat_nolondon3",
  "mat_over2hall3"
)
matches <- old_bsts[match_names]
region_num <- old$region_num
regions <- old$LTLA_dat_combine %>%
  dplyr::select(LTLA_ID, LTLA_name, N_region) %>%
  left_join(region_num, by = "N_region")
HEI_dat <- old$HEI_dat %>% mutate(HEI_opening = as.Date(HEI_opening))
cases <- raw_cases %>%
  inner_join(lookup, by = "areaCode") %>%
  arrange(LTLA_ID, date) %>%
  group_by(LTLA_ID) %>%
  mutate(
    adjusted = if_else(
      newCasesBySpecimenDate == 0,
      0.5,
      newCasesBySpecimenDate
    ),
    log_GR = log(adjusted / lag(adjusted))
  ) %>%
  ungroup()
stopifnot(all(vapply(
  split(cases$date, cases$LTLA_ID),
  function(d) all(diff(d) == 1),
  logical(1)
)))
bsts_outcome <- "log_growth_trailing_7day_total_zero_after_sum"
# Restore the original BSTS order: sum raw daily counts first, then replace zero
# totals. Daily zero replacement above remains exclusively the CAR outcome.
observations <- cases %>%
  group_by(LTLA_ID) %>%
  mutate(
    total7 = zoo::rollsum(
      newCasesBySpecimenDate,
      7,
      align = "right",
      fill = NA
    ),
    total7 = if_else(total7 == 0, 0.5, total7),
    bsts_growth = log(total7 / lag(total7))
  ) %>%
  ungroup() %>%
  filter(date >= study_start, date <= study_end) %>%
  dplyr::select(
    areaCode,
    LTLA_ID,
    LTLA_name,
    date,
    newCasesBySpecimenDate = bsts_growth
  )
stopifnot(
  nrow(observations) == 380L * 188L,
  all(is.finite(observations$newCasesBySpecimenDate))
)
bsts_inputs <- mget(c(
  "observations",
  "matches",
  "region_num",
  "regions",
  "HEI_dat",
  "bsts_outcome"
))
if (
  !analysis.cached(bsts_prepared_file) ||
    !identical(read.analysis(bsts_prepared_file), bsts_inputs)
) {
  write.analysis(
    bsts_inputs,
    bsts_prepared_file,
    c(original_prepared, original_bsts, case_file)
  )
}
