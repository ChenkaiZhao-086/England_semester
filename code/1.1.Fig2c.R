# Draw daily descriptive series, correlations and Hall temporal weights as PDFs.
old <- read.analysis(original_prepared)
if (plot_from_exports) {
  series <- read.csv(file.path(table_dir, "Figure2C_cases.csv")) %>%
    mutate(date = as.Date(date))
} else {
  series <- cases %>%
    filter(date >= study_start, date <= study_end) %>%
    summarise(case_total = sum(newCasesBySpecimenDate), .by = date) %>%
    arrange(date)
  before <- cases %>%
    filter(date == study_start - 1) %>%
    summarise(total = sum(newCasesBySpecimenDate)) %>%
    pull(total)
  series <- series %>%
    mutate(log_growth = log(case_total / lag(case_total, default = before)))
}
national_mobility <- read_csv(
  "data/2020_GB_Region_Mobility_Report.csv",
  show_col_types = FALSE
) %>%
  filter(
    country_region == "United Kingdom",
    is.na(sub_region_1),
    date >= study_start,
    date <= study_end
  ) %>%
  dplyr::select(
    date,
    retail = retail_and_recreation_percent_change_from_baseline
  )
openings <- old$HEI_cam_dat %>%
  mutate(date = as.Date(HEI_opening)) %>%
  filter(!is.na(date)) %>%
  count(date)
if (!plot_from_exports) {
  write.csv(
    series,
    file.path(table_dir, "Figure2C_cases.csv"),
    row.names = FALSE
  )
}
if (!plot_from_exports) {
  write.csv(
    openings,
    file.path(table_dir, "Figure2C_campus_openings.csv"),
    row.names = FALSE
  )
}
weekly_openings <- openings %>%
  mutate(date = floor_date(date, "week", week_start = 1)) %>%
  summarise(reopen = sum(n), .by = date)
display_series <- series %>%
  left_join(national_mobility, by = "date") %>%
  left_join(weekly_openings, by = "date") %>%
  mutate(
    reopen = replace_na(reopen, 0L),
    reopen_scaled = reopen * 0.004,
    mobility_display = (retail + 100) / 100 + 0.9,
    growth_display = log_growth + 0.9
  )
write.csv(
  display_series,
  file.path(table_dir, "Figure2C_display.csv"),
  row.names = FALSE
)
# The two vertical bands reproduce the reference figure's display scales only.
p <- ggplot(display_series, aes(date)) +
  geom_hline(
    yintercept = 1.9,
    linetype = "dashed",
    colour = "gray60",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = 0.9,
    colour = "gray60",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_col(
    aes(y = reopen_scaled, fill = "Number of HEIs reopening"),
    position = position_nudge(x = 3),
    width = 6.5,
    alpha = 0.5
  ) +
  geom_line(
    aes(y = mobility_display, colour = "Population mobility"),
    linewidth = 0.7,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = growth_display, colour = "Log growth rate"),
    linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "Log growth rate",
    breaks = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9),
    labels = c(0, 25, 50, 75, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
    sec.axis = sec_axis(
      ~.,
      name = "Mobility change",
      breaks = seq(0.9, 1.9, 0.2),
      labels = seq(0, 100, 20)
    ),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%m/%d",
    expand = expansion(add = c(7, 0))
  ) +
  scale_colour_manual(
    values = c("Log growth rate" = "#003773", "Population mobility" = "#E30118")
  ) +
  scale_fill_manual(values = c("Number of HEIs reopening" = "grey60")) +
  annotate(
    "segment",
    x = study_start,
    xend = study_end,
    y = 0.37,
    yend = 0.37,
    colour = "black",
    linewidth = 0.5
  ) +
  labs(x = "Date", colour = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom", plot.margin = margin(10, 55, 10, 10))
ggsave(file.path(plot_dir, "Fig2c_v2.pdf"), p, width = 8, height = 5.5)
cor_data <- series %>%
  left_join(national_mobility, by = "date") %>%
  left_join(openings, by = "date") %>%
  mutate(n = replace_na(n, 0L)) %>%
  dplyr::select(log_growth, retail, n)
stopifnot(nrow(cor_data) == 188L, all(is.finite(as.matrix(cor_data))))
names(cor_data) <- c(
  "Daily log case growth",
  "Retail mobility (%)",
  "Campus openings"
)
if (!plot_from_exports) {
  write.csv(cor(cor_data), file.path(table_dir, "Figure2D_correlations.csv"))
}
cor_data[[2]] <- (cor_data[[2]] + 100) / 100
names(cor_data) <- c("log_GR", "gmc_retail2", "reopen")
pdf(file.path(plot_dir, "Fig2d.pdf"), width = 6, height = 6)
psych::pairs.panels(
  cor_data,
  method = "pearson",
  stars = FALSE,
  ci = FALSE,
  hist.col = "gray90",
  density = TRUE,
  pch = 20,
  lm = TRUE,
  ellipses = FALSE,
  scale = FALSE,
  cex.cor = 0.5,
  cex.labels = NULL,
  font.labels = 1,
  gap = 0,
  show.points = TRUE
)
dev.off()
if (plot_from_exports) {
  kernels <- list(
    h_index = gen.decay(30, "exp(-0.4*x)", 6),
    hall_flat = gen.decay(30, "1+0*x", 6),
    hall_day3 = gen.decay(30, "exp(-0.4*x)", 3),
    hall_window7 = gen.decay(7, "exp(-0.4*x)", 6),
    hall_window14 = gen.decay(14, "exp(-0.4*x)", 6),
    hall_window21 = gen.decay(21, "exp(-0.4*x)", 6)
  )
}
kernel_labels <- c(
  h_index = "Main: day 6, decay 0.4",
  hall_flat = "Day 6, no decay",
  hall_day3 = "Day 3, decay 0.4",
  hall_window7 = "7-day window",
  hall_window14 = "14-day window",
  hall_window21 = "21-day window"
)
kernel_data <- bind_rows(lapply(names(kernels), function(n) {
  data.frame(
    day = 0:30,
    weight = c(kernels[[n]], rep(0, 31 - length(kernels[[n]]))),
    setting = kernel_labels[[n]]
  )
}))
p <- ggplot(kernel_data, aes(day, weight)) +
  geom_line() +
  geom_point(size = 0.6) +
  facet_wrap(~setting, ncol = 3) +
  theme_bw() +
  labs(x = "Days after reopening", y = "Hall temporal weight")
ggsave(
  file.path(plot_dir, "Hall_temporal_weights.pdf"),
  p,
  width = 11,
  height = 6
)
