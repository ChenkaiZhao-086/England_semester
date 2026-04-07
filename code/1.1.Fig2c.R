## case, reopen, GMR change
case_nation <- LTLA_case_dat %>%
  reframe(
    case = sum(newCasesBySpecimenDate),
    .by = date
  ) %>%
  mutate(
    date = as.Date(date),
    case_scaled = case * 150 / max(case),
    case = if_else(case == 0, 0.5, case),
    diff = lag(case, n = 1),
    log_GR = log(case / diff)
  ) %>%
  filter(date >= "2020-06-02" & date <= "2020-11-15")


gmr_nation <- read_csv("data/2020_GB_Region_Mobility_Report.csv") %>%
  filter(
    country_region == "United Kingdom" &
      is.na(sub_region_1) == TRUE &
      date >= "2020-06-02" &
      date <= "2020-11-15"
  ) %>%
  mutate(
    gmc_retail = na.locf(
      retail_and_recreation_percent_change_from_baseline,
      na.rm = FALSE
    ),
    gmc_retail2 = (gmc_retail + 100) / 100
  ) %>%
  dplyr::select(
    date,
    gmc_retail,
    gmc_retail2
  )

Reopen <- HEI_cam_dat %>%
  mutate(
    date = as.Date(HEI_opening) -
      (as.integer(strftime(as.Date(HEI_opening), "%u")) - 1L),
    HEI_opening = NULL
  ) %>%
  reframe(
    reopen = sum(length(Campus_name)),
    .by = date
  ) %>%
  mutate(reopen_scaled = reopen / 100) %>%
  drop_na()


MergeDat_1c <- merge(case_nation, gmr_nation, by = "date")
MergeDat_1c <- merge(MergeDat_1c, Reopen, by = "date", all.x = TRUE)
MergeDat_1c <- MergeDat_1c %>% mutate(reopen = replace_na(reopen, 0))


Fig1c_v1 <-
  ggplot(MergeDat_1c, aes(x = date)) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray60",
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
    aes(y = gmc_retail2, color = "Mobility change"),
    linewidth = 0.7,
    alpha = 0.6
  ) +
  geom_line(aes(y = log_GR, color = "Log growth rate"), linewidth = 0.7) +
  scale_y_continuous(
    name = "Log growth rate",
    limits = c(-0.55, 1.1),
    breaks = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
    labels = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
    sec.axis = sec_axis(
      transform = ~.,
      name = "Mobility change &\n Number of HEIs reopening",
      breaks = seq(0, 1, by = 0.2),
      labels = seq(0, 100, by = 20)
    ),
    expand = c(0.0, 0.0)
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%m/%d",
    expand = c(0.02, 0.02)
  ) +
  scale_color_manual(
    values = c(
      "Log growth rate" = "#003773",
      "Mobility change" = "#E30118"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Number of HEIs reopening" = "grey70"
    )
  ) +
  # annotate(
  #   "segment",
  #   x = structure(Inf, class = "Date"),
  #   xend = structure(Inf, class = "Date"),
  #   y = 0,
  #   yend = 100,
  #   color = "black",
  #   linewidth = 0.5
  # ) + # 手动绘制仅覆盖底部 2/3 的右轴线
  # coord_cartesian(clip = "off") + # x = Inf 映射到面板右边界，clip = "off" 允许在面板边界处绘制
  labs(x = "Date", color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    # axis.line.y.right = element_blank(),
    plot.margin = margin(10, 55, 10, 10)
  )
ggsave(
  Fig1c_v1,
  filename = "outputs/Fig2c_v1.pdf",
  width = 8,
  height = 5
)


MergeDat_1c_V2 <- MergeDat_1c %>%
  mutate(
    reopen_scaled2 = reopen_scaled * 0.4,
    gmc_retail2 = gmc_retail2 + 0.9,
    log_GR = log_GR + 0.9
  ) %>%
  filter(date >= "2020-06-02" & date <= "2020-11-15")

Fig1c_v2 <-
  ggplot(MergeDat_1c_V2, aes(x = date)) +
  geom_hline(
    yintercept = 1.9,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = 0.9,
    color = "gray60",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_col(
    aes(y = reopen_scaled2, fill = "Number of HEIs reopening"),
    position = position_nudge(x = 3),
    width = 6.5,
    alpha = 0.5
  ) +
  geom_line(
    aes(y = gmc_retail2, color = "Mobility change"),
    linewidth = 0.7,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = log_GR, color = "Log growth rate"),
    linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "Log growth rate",
    limits = c(0, 2),
    breaks = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9),
    labels = c(0, 25, 50, 75, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
    sec.axis = sec_axis(
      transform = ~.,
      name = "Mobility change",
      breaks = seq(0.9, 1.9, by = 0.2),
      labels = seq(0, 100, by = 20)
    ),
    expand = c(0.0, 0.0)
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%m/%d",
    expand = expansion(add = c(7, 0.0))
  ) +
  scale_color_manual(
    values = c(
      "Log growth rate" = "#003773",
      "Mobility change" = "#E30118"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Number of HEIs reopening" = "grey60"
    )
  ) +
  annotate(
    "segment",
    x = as.Date("2020-06-02"),
    xend = as.Date("2020-11-15"),
    y = 0.37,
    yend = 0.37,
    color = "black",
    linewidth = 0.5
  ) +
  labs(x = "Date", color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(10, 55, 10, 10)
  )
ggsave(
  Fig1c_v2,
  filename = "outputs/Fig2c_v2.pdf",
  width = 8,
  height = 5.5
)


CorDat <- MergeDat_1c %>%
  dplyr::select(log_GR, gmc_retail2, reopen)

library(psych)
pdf("outputs/Fig2d.pdf", width = 6, height = 6)
pairs.panels(
  CorDat,
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
