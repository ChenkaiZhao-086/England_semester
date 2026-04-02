## case, reopen, GMR change
case_nation <- LTLA_case_dat %>%
  reframe(
    case = sum(newCasesBySpecimenDate),
    .by = date
  ) %>%
  mutate(
    date = as.Date(date),
    case_scaled = case * 150 / max(case) # scales::rescale(case, to = c(0, 150))
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
    gmc_retail2 = gmc_retail + 100
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
  drop_na()
# HEI_cam_dat %>%
# reframe(
#   reopen = sum(length(Campus_name)),
#   .by = HEI_opening
# ) %>%
# mutate(date = as.Date(HEI_opening), HEI_opening = NULL)

MergeDat_1c <- merge(case_nation, gmr_nation, by = "date")
MergeDat_1c <- merge(MergeDat_1c, Reopen, by = "date", all.x = TRUE)


Fig1c_v1 <- ggplot(MergeDat_1c, aes(x = date)) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_col(
    aes(y = reopen, fill = "Reopened HEIs"),
    position = position_nudge(x = 3),
    width = 6.5,
    alpha = 0.4
  ) +
  geom_line(
    aes(y = gmc_retail2, color = "Mobility change"),
    linewidth = 0.9,
    alpha = 0.8
  ) +
  geom_line(aes(y = case_scaled, color = "Confirmed cases"), linewidth = 0.9) +
  scale_y_continuous(
    name = "Confirmed cases",
    limits = c(0, 155),
    breaks = seq(0, 32000, by = 4000) * 150 / max(case_nation$case),
    labels = seq(0, 32000, by = 4000),
    sec.axis = sec_axis(
      transform = ~.,
      name = "Mobility change &\n Opened HEIs",
      breaks = seq(0, 100, by = 20),
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
      "Confirmed cases" = "#003773",
      "Mobility change" = "#E30118"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Reopened HEIs" = "grey70"
    )
  ) +
  annotate(
    "segment",
    x = structure(Inf, class = "Date"),
    xend = structure(Inf, class = "Date"),
    y = 0,
    yend = 100,
    color = "black",
    linewidth = 0.5
  ) + # 手动绘制仅覆盖底部 2/3 的右轴线
  coord_cartesian(clip = "off") + # x = Inf 映射到面板右边界，clip = "off" 允许在面板边界处绘制
  labs(x = "Date", color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.line.y.right = element_blank(),
    plot.margin = margin(10, 55, 10, 10)
  )
ggsave(
  Fig1c_v1,
  filename = "outputs/Fig2c_v1.pdf",
  width = 8,
  height = 5
)


MergeDat_1c_V2 <- MergeDat_1c %>%
  mutate(reopen_scaled = reopen * 20 / 100) %>%
  filter(date >= "2020-06-02" & date <= "2020-11-15")

Fig1c_v2 <- ggplot(MergeDat_1c_V2, aes(x = date)) +
  geom_hline(
    yintercept = 120,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_col(
    aes(y = reopen_scaled, fill = "Reopened HEIs"),
    position = position_nudge(x = 3),
    width = 6.5
  ) +
  geom_line(
    aes(y = gmc_retail2 + 20, color = "Mobility change"),
    linewidth = 0.9,
    alpha = 0.8
  ) +
  geom_line(
    aes(y = case_scaled + 20, color = "Confirmed cases"),
    linewidth = 0.9
  ) +
  scale_y_continuous(
    name = "Confirmed cases",
    limits = c(0, 175),
    breaks = c(
      0,
      5,
      10,
      15,
      c(seq(0, 32000, by = 4000) * 150 / max(case_nation$case)) + 20
    ),
    labels = c(0, 25, 50, 75, seq(0, 32000, by = 4000)),
    sec.axis = sec_axis(
      transform = ~.,
      name = "Mobility change",
      breaks = seq(20, 120, by = 20),
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
      "Confirmed cases" = "#003773",
      "Mobility change" = "#E30118"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Reopened HEIs" = "grey70"
    )
  ) +
  annotate(
    "segment",
    x = structure(Inf, class = "Date"),
    xend = structure(Inf, class = "Date"),
    y = 20,
    yend = 120,
    color = "black",
    linewidth = 0.5
  ) + # 手动绘制仅覆盖底部 2/3 的右轴线
  annotate(
    "segment",
    x = as.Date("2020-06-02"),
    xend = as.Date("2020-11-15"),
    y = 20,
    yend = 20,
    color = "black",
    linewidth = 0.5
  ) +
  coord_cartesian(clip = "off") + # x = Inf 映射到面板右边界，clip = "off" 允许在面板边界处绘制
  labs(x = "Date", color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",

    axis.line.y.right = element_blank(),
    plot.margin = margin(10, 55, 10, 10)
  )

ggsave(
  Fig1c_v2,
  filename = "outputs/Fig2c_v2.pdf",
  width = 8,
  height = 5
)
