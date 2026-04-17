#### Table2

### table2 sensitivity analysis
# table2_ci_sens <- cal.cum.ci(
#   data = ci_base_3,
#   m.data = mat_base3,
#   table = base_table_3,
#   tidy.out = TRUE
# )
# write.csv(table2_ci_sens, "outputs/table/Table2_sens.csv")

#----------------------------------------------------------------------
#### Prepare data for plot
Result_bsts_main <- cal.cum.ci.region(
  data = ci_m_base_gr,
  m.data = mat_base,
  table = ci_m_base_gr_table
)
Result_bsts_main$group <- "main"


Result_bsts_noM25 <- cal.cum.ci.region(
  data = ci_m_base_gr_nolon,
  m.data = mat_nolondon,
  table = ci_m_base_gr_nolon_table
)
Result_bsts_noM25$group <- "noM25"


Result_bsts_over2 <- cal.cum.ci.region(
  data = ci_m_base_gr_over2,
  m.data = mat_over2hall,
  table = ci_m_base_gr_over2_table
)
Result_bsts_over2$group <- "over2"


Result_bsts_MultiMatch <- cal.cum.ci.region(
  data = ci_base_3,
  m.data = mat_base3,
  table = base_table_3
)
Result_bsts_MultiMatch$group <- "MultiMatch"

### Figure 3 --------------------------------------------
Fig3dat_CAR <- Table2_CAR[[2]]
Fig3dat_CAR$group <- "CAR"
Fig3dat_GMR <- Table2_CAR_GMR[[2]]
Fig3dat_GMR$group <- "GMR"
Result_bsts_main_fig3 <- Result_bsts_main %>%
  filter(date >= 11) %>%
  mutate(date = date - 11)
Fig3dat <- bind_rows(
  Fig3dat_CAR,
  Fig3dat_GMR,
  Result_bsts_main_fig3
)
ZeroDat_main <- expand.grid(
  date = 0,
  N_num = unique(Fig3dat$N_num),
  N_region = unique(Fig3dat$N_region),
  cum.effect = 0,
  cum.effect.lower = 0,
  cum.effect.upper = 0,
  group = unique(Fig3dat$group)
)
Fig3dat <- bind_rows(
  Fig3dat,
  ZeroDat_main
) %>%
  filter(date >= 0) %>%
  mutate(
    N_region = factor(N_region, levels = RowOrder),
    group = factor(group, levels = c("GMR", "CAR", "main"))
  )

for (i in c(unique(Fig3dat$N_region))) {
  FigTitle <- i
  p <- ggplot(Fig3dat[Fig3dat$N_region == i, ], aes(x = date)) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.4,
      linetype = 2,
      colour = "grey70"
    ) +
    geom_hline(yintercept = 1, colour = "grey70") +
    geom_ribbon(
      aes(
        ymin = exp(cum.effect.lower),
        ymax = exp(cum.effect.upper),
        colour = group,
        fill = group
      ),
      alpha = 0.03,
      show.legend = FALSE
    ) +
    geom_line(
      aes(y = exp(cum.effect), colour = group),
      linewidth = 0.5,
      alpha = 0.8
    ) +
    scale_x_continuous(
      name = "Days after HEIs reopening",
      breaks = seq(0, 30, by = 5),
      labels = seq(0, 30, by = 5),
      limits = c(0, 30),
      expand = expansion(add = c(1, 0))
    ) +
    scale_y_log10(
      name = "Cumulative risk ratio (log scale)",
      labels = function(x) format(x, scientific = FALSE),
      limits = c(0.019, 601)
    ) +
    theme_classic() +
    scale_colour_manual(
      name = NULL,
      values = c(
        "GMR" = "#003773",
        "CAR" = "#E30118",
        "main" = "#00D7B6"
      ),
      labels = c(
        "GMR" = "Population mobility (self-controlled)",
        "CAR" = "HEIs reopening (self-controlled)",
        "main" = "HEIs reopening (externally-matched)"
      )
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(
        "GMR" = "#003773",
        "CAR" = "#E30118",
        "main" = "#00D7B6"
      ),
      labels = c(
        "GMR" = "Population mobility (self-controlled)",
        "CAR" = "HEIs reopening (self-controlled)",
        "main" = "HEIs reopening (externally-matched)"
      )
    ) +
    guides(
      colour = guide_legend(override.aes = list(alpha = 1)),
      fill = "none"
    ) +
    facet_wrap(~N_region, scales = "fixed") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 8, colour = "black"),
      axis.title = element_text(size = 10, face = "bold", colour = "black"),
      strip.background = element_blank(),
      strip.text = element_blank(),
      # strip.text = element_text(size = 8, face = "bold", colour = "black"),
      # strip.background = element_rect(fill = "grey95", colour = "grey70"),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      panel.spacing = unit(0.8, "lines"),
      plot.title = element_text(size = 8, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )

  ggsave(
    p,
    filename = paste0("outputs/Fig3_", i, ".pdf"),
    width = 8,
    height = 6
  )
}


### Figure appendix: CAR sensitivity analysis ----------------------
Result_CAR_KNN <- cal.cum.bs(
  rawdata = HEI_dec0.4,
  region_data = region_num2,
  eff_data = eff_all_KNN
)
Result_CAR_KNN_dat <- Result_CAR_KNN[[2]]
Result_CAR_KNN_dat$group <- "KNN"
Result_CAR_dat <- Table2_CAR[[2]]
Result_CAR_dat$group <- "main"

FigS_CAR_dat <- bind_rows(
  Result_CAR_dat,
  Result_CAR_KNN_dat
)
ZeroDat <- expand.grid(
  date = 0,
  N_num = unique(FigS_CAR_dat$N_num),
  N_region = unique(FigS_CAR_dat$N_region),
  cum.effect = 0,
  cum.effect.lower = 0,
  cum.effect.upper = 0,
  group = unique(FigS_CAR_dat$group)
)
FigS_CAR_dat <- bind_rows(
  FigS_CAR_dat,
  ZeroDat
) %>%
  mutate(
    N_region = factor(N_region, levels = RowOrder),
    group = factor(group, levels = c("main", "KNN"))
  )

FigS_CAR_main <- FigS_CAR_dat %>%
  filter(N_region == "All region") %>%
  ggplot(aes(x = date)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = 2,
    colour = "grey70"
  ) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_ribbon(
    aes(
      ymin = exp(cum.effect.lower),
      ymax = exp(cum.effect.upper),
      colour = group,
      fill = group
    ),
    alpha = 0.03,
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = exp(cum.effect), colour = group),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = expansion(add = c(1, 0))
  ) +
  scale_y_log10(
    name = NULL,
    breaks = c(1, 2, 3),
    limits = c(0.83, 2.5)
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "KNN" = "#E30118"
    ),
    labels = c(
      "main" = "Main Analysis",
      "KNN" = "KNN Analysis"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "KNN" = "#E30118"
    ),
    labels = c(
      "main" = "Main Analysis",
      "KNN" = "KNN Analysis"
    )
  ) +
  guides(
    colour = "none",
    fill = "none",
  ) +
  facet_wrap(~N_region) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 10, face = "bold", colour = "black"),
    strip.text = element_text(size = 8, face = "bold", colour = "black"),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(size = 8, face = "bold", colour = "black"),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
  )
ggsave(
  FigS_CAR_main,
  filename = "outputs/FigS_CAR_main.pdf",
  width = 5.9,
  height = 6
)


FigS_CAR <- FigS_CAR_dat %>%
  filter(N_region != "All region") %>%
  ggplot(aes(x = date)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = 2,
    colour = "grey70"
  ) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_ribbon(
    aes(
      ymin = exp(cum.effect.lower),
      ymax = exp(cum.effect.upper),
      colour = group,
      fill = group
    ),
    alpha = 0.03,
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = exp(cum.effect), colour = group),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  facet_wrap(~N_region) +
  scale_x_continuous(
    name = "Days after HEIs reopening",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = expansion(add = c(1, 0))
  ) +
  scale_y_log10(
    name = "Cumulative risk ratio (log scale)"
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "KNN" = "#E30118"
    ),
    labels = c(
      "main" = "Main Analysis",
      "KNN" = "KNN Analysis"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "KNN" = "#E30118"
    ),
    labels = c(
      "main" = "Main Analysis",
      "KNN" = "KNN Analysis"
    )
  ) +
  guides(
    colour = guide_legend(override.aes = list(alpha = 1)),
    fill = "none",
  ) +
  facet_wrap(~N_region) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 10, face = "bold", colour = "black"),
    strip.text = element_text(size = 8, face = "bold", colour = "black"),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(size = 8, face = "bold", colour = "black"),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
  )
ggsave(
  FigS_CAR,
  filename = "outputs/FigS_CAR.pdf",
  width = 12,
  height = 10
)


### Figure appendix: BSTS sensitivity analysis ----------------------
FigS_bsts_dat <- bind_rows(
  Result_bsts_main,
  Result_bsts_noM25,
  Result_bsts_over2,
  Result_bsts_MultiMatch
)
FigS_bsts_dat <- FigS_bsts_dat %>%
  filter(date >= 11) %>%
  mutate(
    date = date - 11,
    N_region = factor(N_region, levels = RowOrder),
    group = factor(group, levels = c("MultiMatch", "over2", "noM25", "main"))
  )


FigS_bsts_main <- FigS_bsts_dat %>%
  filter(N_region == "All region") %>%
  ggplot(aes(x = date)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = 2,
    colour = "grey70"
  ) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_ribbon(
    aes(
      ymin = exp(cum.effect.lower),
      ymax = exp(cum.effect.upper),
      colour = group,
      fill = group
    ),
    alpha = 0.01,
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = exp(cum.effect), colour = group),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  facet_wrap(~N_region) +
  scale_x_continuous(
    name = NULL,
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = expansion(add = c(1, 0))
  ) +
  scale_y_log10(
    name = NULL
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "noM25" = "#E30118",
      "over2" = "#00D7B6",
      "MultiMatch" = "#FED502"
    ),
    labels = c(
      "main" = "Main Analysis",
      "noM25" = "Excluding London",
      "over2" = "Matched LTLAs with \u2265 2 Halls",
      "MultiMatch" = "LTLAs with \u2265 2 Matched Controls"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "noM25" = "#E30118",
      "over2" = "#00D7B6",
      "MultiMatch" = "#FED502"
    ),
    labels = c(
      "main" = "Main Analysis",
      "noM25" = "Excluding London",
      "over2" = "Matched LTLAs with \u2265 2 Halls",
      "MultiMatch" = "LTLAs with \u2265 2 Matched Controls"
    )
  ) +
  guides(
    colour = "none",
    fill = "none"
  ) +
  facet_wrap(~N_region) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 10, face = "bold", colour = "black"),
    strip.text = element_text(size = 8, face = "bold", colour = "black"),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(size = 8, face = "bold", colour = "black"),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
  )
ggsave(
  FigS_bsts_main,
  filename = "outputs/FigS_bsts_main.pdf",
  width = 5.9,
  height = 6
)

FigS_bsts <- FigS_bsts_dat %>%
  filter(N_region != "All region") %>%
  ggplot(aes(x = date)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = 2,
    colour = "grey70"
  ) +
  geom_hline(yintercept = 1, colour = "grey70") +
  geom_ribbon(
    aes(
      ymin = exp(cum.effect.lower),
      ymax = exp(cum.effect.upper),
      colour = group,
      fill = group
    ),
    alpha = 0.01,
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = exp(cum.effect), colour = group),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  facet_wrap(~N_region) +
  scale_x_continuous(
    name = "Days after HEIs reopening",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = expansion(add = c(1, 0))
  ) +
  scale_y_log10(
    name = "Cumulative risk ratio (log scale)"
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "noM25" = "#E30118",
      "over2" = "#00D7B6",
      "MultiMatch" = "#FED502"
    ),
    labels = c(
      "main" = "Main Analysis",
      "noM25" = "Excluding London",
      "over2" = "Matched LTLAs with \u2265 2 Halls",
      "MultiMatch" = "LTLAs with \u2265 2 Matched Controls"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "main" = "#003773",
      "noM25" = "#E30118",
      "over2" = "#00D7B6",
      "MultiMatch" = "#FED502"
    ),
    labels = c(
      "main" = "Main Analysis",
      "noM25" = "Excluding London",
      "over2" = "Matched LTLAs with \u2265 2 Halls",
      "MultiMatch" = "LTLAs with \u2265 2 Matched Controls"
    )
  ) +
  guides(
    colour = guide_legend(reverse = TRUE, override.aes = list(alpha = 1)),
    fill = "none"
  ) +
  facet_wrap(~N_region) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 10, face = "bold", colour = "black"),
    strip.text = element_text(size = 8, face = "bold", colour = "black"),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(size = 8, face = "bold", colour = "black"),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")
  )
ggsave(
  FigS_bsts,
  filename = "outputs/FigS_bsts.pdf",
  width = 12,
  height = 10,
  device = cairo_pdf
)
