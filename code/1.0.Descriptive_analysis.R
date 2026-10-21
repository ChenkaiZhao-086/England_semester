# Summarise the agreed study period and opening-date distribution.
old <- read.analysis(original_prepared)
study_cases <- cases %>% filter(date >= study_start, date <= study_end)
totals <- study_cases %>%
  summarise(
    n_regions = n_distinct(LTLA_ID),
    n_days = n_distinct(date),
    total_cases = sum(newCasesBySpecimenDate),
    zero_days = sum(newCasesBySpecimenDate == 0)
  )
write.csv(
  totals,
  file.path(table_dir, "Descriptive_totals.csv"),
  row.names = FALSE
)
# Preserve the existing within-LTLA weekly-summary convention, changing dates only.
weekly_cases <- study_cases %>%
  mutate(week = week(date)) %>%
  group_by(LTLA_ID, week) %>%
  mutate(week_case = sum(newCasesBySpecimenDate)) %>%
  ungroup() %>%
  summarise(mean = median(week_case), .by = LTLA_ID)
desc <- old$LTLA_dat_combine %>%
  left_join(weekly_cases, by = "LTLA_ID") %>%
  mutate(any_hall = hall_num > 0, Prosperity = as.numeric(Prosperity))

source("code/1.3.U_test_for_table1.R", local = TRUE)

openings <- old$HEI_cam_dat %>%
  mutate(date = as.Date(HEI_opening), weekday = wday(date, week_start = 1))
opening_counts <- data.frame(
  weekday = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
    "Missing"
  ),
  n = tabulate(replace_na(openings$weekday, 8L), nbins = 8)
) %>%
  mutate(
    percent_known = if_else(
      weekday == "Missing",
      NA_real_,
      100 * n / sum(n[weekday != "Missing"])
    )
  )
write.csv(
  opening_counts,
  file.path(table_dir, "Opening_weekdays.csv"),
  row.names = FALSE
)
