# Read the validated original inputs and audit the current raw case CSV.
# Prepared geography and frozen matching remain required inputs.
old <- read.analysis(original_prepared)
old_bsts <- read.analysis(original_bsts)
raw_cases <- read_csv(case_file, show_col_types = FALSE) %>%
  transmute(
    areaCode,
    date = as.Date(date, "%Y/%m/%d"),
    newCasesBySpecimenDate
  ) %>%
  filter(date >= study_start - 7, date <= study_end + 1)
original_cases <- old$LTLA_case_dat %>% mutate(date = as.Date(date))
lookup <- original_cases %>% distinct(areaCode, LTLA_ID, LTLA_name)
audit <- original_cases %>%
  left_join(
    raw_cases,
    by = c("areaCode", "date"),
    suffix = c("_original", "_raw")
  )
stopifnot(
  nrow(audit) == nrow(original_cases),
  all(
    audit$newCasesBySpecimenDate_original == audit$newCasesBySpecimenDate_raw
  ),
  !anyDuplicated(raw_cases[c("areaCode", "date")])
)
