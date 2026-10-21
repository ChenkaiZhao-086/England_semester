# Match the reference cumulative-curve appearance without changing saved estimates.
# Inputs: data with date, cum.effect/lower/upper and group; named colour/label vectors.
# Returns a ggplot with exponentiated ordinates; callers supply log scales and facets.
# The optional alpha values control only line/ribbon appearance.
plot.reference.curves <- function(
  data,
  colours,
  labels,
  ribbon_alpha = 0.03,
  line_alpha = 0.8
) {
  ggplot(data, aes(x = date)) +
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
      alpha = ribbon_alpha,
      show.legend = FALSE
    ) +
    geom_line(
      aes(y = exp(cum.effect), colour = group),
      linewidth = 0.5,
      alpha = line_alpha
    ) +
    scale_x_continuous(
      name = "Days after HEIs reopening",
      breaks = seq(0, 30, 5),
      labels = seq(0, 30, 5),
      limits = c(0, 30),
      expand = expansion(add = c(1, 0))
    ) +
    labs(y = "Cumulative risk ratio (log scale)") +
    scale_colour_manual(name = NULL, values = colours, labels = labels) +
    scale_fill_manual(name = NULL, values = colours, labels = labels) +
    guides(
      colour = guide_legend(override.aes = list(alpha = 1)),
      fill = "none"
    ) +
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
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

# Numeric exports support plot-only work when model RDS files have been moved.
car_plot <- if (file.exists(file.path(result_dir, "car-summary.rds"))) {
  read.analysis(file.path(result_dir, "car-summary.rds"))
} else if (plot_from_exports) {
  fields <- c(
    "coefficients",
    "hall_results",
    "diagnostics",
    "residuals",
    "cumulative"
  )
  values <- setNames(
    lapply(fields, function(n) {
      read.csv(file.path(table_dir, paste0("CAR_", n, ".csv")))
    }),
    fields
  )
  values$mobility_panel_rule <- "per-metric-v1"
  values
} else {
  NULL
}
bsts_plot <- if (file.exists(file.path(result_dir, "bsts-summary.rds"))) {
  read.analysis(file.path(result_dir, "bsts-summary.rds"))
} else if (plot_from_exports) {
  list(curves = read.csv(file.path(table_dir, "BSTS_curves.csv")))
} else {
  NULL
}

# Draw cumulative, sensitivity and diagnostic PDFs from saved model summaries.
# Includes regional Figure 3; no model fitting occurs in this script.
if (!is.null(car_plot) && (!car_from_exports || identical(stages, "plot"))) {
  out <- car_plot
  if (!identical(out$mobility_panel_rule, "per-metric-v1")) {
    stop(
      "Saved CAR summary uses the previous mobility panels; run summary before plot."
    )
  }
  slow_models <- out$diagnostics %>%
    filter(max_PSRF_upper > 1.05 | min_effective < 400) %>%
    pull(model) %>%
    unique()
  p <- out$hall_results %>%
    mutate(
      label = unname(model_labels[model]),
      label = if_else(model %in% slow_models, paste0(label, "*"), label),
      contrast = if_else(
        term == "hall_share",
        "Population-relative Hall\nDaily growth",
        "5,000 proxy students\nDaily growth"
      )
    ) %>%
    ggplot(aes(ratio, label, xmin = lower, xmax = upper)) +
    geom_vline(xintercept = 1, colour = "grey60") +
    geom_pointrange() +
    facet_grid(contrast ~ ., scales = "free", space = "free_y") +
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0)) +
    labs(
      x = "Ratio of outcome-specific growth factors (95% CrI)",
      y = NULL,
      caption = if (length(slow_models)) {
        "*Some parameters show slow mixing; interpret these sensitivity estimates with the convergence diagnostics."
      } else {
        NULL
      }
    )
  ggsave(
    file.path(plot_dir, "CAR_Hall_comparisons.pdf"),
    p,
    width = 13,
    height = 10
  )
  if (nrow(out$cumulative)) {
    p <- out$cumulative %>%
      filter(model == "main", N_num == 0) %>%
      mutate(
        ratio = exp(cum.effect),
        lower = exp(cum.effect.lower),
        upper = exp(cum.effect.upper),
        contribution = if_else(
          term == "h_index",
          "Hall Index",
          "Retail and recreation mobility"
        )
      ) %>%
      ggplot(aes(date, ratio)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
      geom_line() +
      facet_wrap(~contribution, scales = "free_y") +
      theme_bw() +
      labs(
        x = "Days after reopening (day 0 excluded from sums)",
        y = "Exponentiated cumulative model contribution"
      )
    ggsave(file.path(plot_dir, "CAR_cumulative.pdf"), p, width = 11, height = 5)
  }
  p <- out$residuals %>%
    pivot_longer(c(lag1, lag7, lag14), names_to = "lag", values_to = "acf") %>%
    mutate(
      label = unname(model_labels[model]),
      lag = factor(
        lag,
        levels = c("lag1", "lag7", "lag14"),
        labels = c("Lag 1 day", "Lag 7 days", "Lag 14 days")
      )
    ) %>%
    ggplot(aes(label, acf)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~lag) +
    coord_flip() +
    theme_bw() +
    labs(x = NULL, y = "Within-LTLA residual autocorrelation")
  ggsave(file.path(plot_dir, "CAR_residual_ACF.pdf"), p, width = 13, height = 9)
  dir.create(file.path(plot_dir, "diagnostics"), showWarnings = FALSE)
  for (model_name in unique(out$coefficients$model)) {
    model_file <- file.path(result_dir, paste0("car-", model_name, ".rds"))
    if (!file.exists(model_file)) {
      stopifnot(file.exists(file.path(
        plot_dir,
        "diagnostics",
        paste0(model_name, "_traceplots.pdf")
      )))
      next
    }
    model <- read.analysis(model_file)$model
    chains <- model$samples$beta
    for (j in seq_along(chains)) {
      colnames(chains[[j]]) <- colnames(model$X)
    }
    pdf(
      file.path(plot_dir, "diagnostics", paste0(model_name, "_traceplots.pdf")),
      width = 10,
      height = 8
    )
    traceplot(chains)
    dev.off()
  }
}
if (!is.null(bsts_plot)) {
  out <- bsts_plot
  bsts_labels <- c(
    mat_base = "One control",
    mat_base3 = "Three controls",
    mat_nolondon = "One control\nExcluding London",
    mat_nolondon3 = "Three controls\nExcluding London",
    mat_over2hall = "One control, at least two halls\nExcluding London",
    mat_over2hall3 = "Three controls, at least two halls\nExcluding London"
  )
  p <- out$curves %>%
    filter(N_num == 0) %>%
    mutate(label = unname(bsts_labels[group])) %>%
    ggplot(aes(date, ratio)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line() +
    facet_wrap(~label) +
    scale_y_log10() +
    theme_bw() +
    labs(
      x = "Days after reopening (day 0 excluded from sums)",
      y = "Cumulative BSTS ratio"
    )
  ggsave(
    file.path(plot_dir, "BSTS_sensitivities.pdf"),
    p,
    width = 11,
    height = 8
  )
  if (!is.null(car_plot)) {
    car_out <- car_plot
    region_order <- c(
      "All region",
      "North East",
      "North West",
      "Yorkshire and The Humber",
      "East Midlands",
      "West Midlands",
      "South West",
      "East of England",
      "South East",
      "London",
      "Northern Ireland",
      "Scotland",
      "Wales"
    )
    joint <- bind_rows(
      car_out$cumulative %>%
        filter(model == "main") %>%
        mutate(group = if_else(term == "h_index", "CAR", "GMR")),
      out$curves %>% filter(group == "mat_base") %>% mutate(group = "main")
    ) %>%
      mutate(
        group = factor(group, levels = c("GMR", "CAR", "main")),
        N_region = factor(N_region, levels = region_order)
      )
    stopifnot(
      !anyNA(joint$N_region),
      !anyDuplicated(joint[c("N_region", "group", "date")])
    )
    write.csv(
      joint,
      file.path(table_dir, "Figure3_display.csv"),
      row.names = FALSE
    )
    colours <- c(GMR = "#003773", CAR = "#E30118", main = "#00D7B6")
    labels <- c(
      GMR = "Mobility (retail_gr reference = 0)",
      CAR = "HEIs reopening (self-controlled)",
      main = "HEIs reopening (externally-matched)"
    )
    for (region in region_order) {
      p <- plot.reference.curves(
        filter(joint, N_region == region),
        colours,
        labels
      ) +
        scale_y_log10(breaks = c(0.1, 10, 1000), labels = function(x) {
          format(x, scientific = FALSE)
        }) +
        coord_cartesian(ylim = c(0.019, 601)) +
        theme(strip.background = element_blank(), strip.text = element_blank())
      ggsave(
        file.path(plot_dir, paste0("Fig3_", region, ".pdf")),
        p,
        width = 8,
        height = 6
      )
    }

    if (!car_from_exports || identical(stages, "plot")) {
      car_sensitivity <- car_out$cumulative %>%
        filter(term == "h_index", model %in% c("main", "knn")) %>%
        mutate(
          group = factor(model, levels = c("main", "knn")),
          N_region = factor(N_region, levels = region_order)
        )
      car_colours <- c(main = "#003773", knn = "#E30118")
      car_labels <- c(main = "Main Analysis", knn = "KNN Analysis")
      p <- plot.reference.curves(
        filter(car_sensitivity, N_region == "All region"),
        car_colours,
        car_labels
      ) +
        scale_y_log10(breaks = c(1, 2, 3)) +
        coord_cartesian(ylim = c(0.83, 2.5)) +
        facet_wrap(~N_region) +
        labs(x = NULL, y = NULL) +
        guides(colour = "none", fill = "none") +
        theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
      ggsave(
        file.path(plot_dir, "FigS_CAR_main.pdf"),
        p,
        width = 5.9,
        height = 6
      )
      p <- plot.reference.curves(
        filter(car_sensitivity, N_region != "All region"),
        car_colours,
        car_labels
      ) +
        scale_y_log10() +
        facet_wrap(~N_region, ncol = 4) +
        theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
      ggsave(file.path(plot_dir, "FigS_CAR.pdf"), p, width = 12, height = 10)
    }

    bsts_colours <- c(
      mat_base = "#003773",
      mat_nolondon = "#E30118",
      mat_over2hall = "#00D7B6",
      mat_base3 = "#FED502"
    )
    bsts_labels <- c(
      mat_base = "Main Analysis",
      mat_nolondon = "Excluding London",
      mat_over2hall = "Matched LTLAs with \u2265 2 Halls",
      mat_base3 = "LTLAs with \u2265 2 Matched Controls"
    )
    bsts_sensitivity <- out$curves %>%
      filter(group %in% names(bsts_colours)) %>%
      mutate(
        group = factor(group, levels = rev(names(bsts_colours))),
        N_region = factor(N_region, levels = region_order)
      )
    p <- plot.reference.curves(
      filter(bsts_sensitivity, N_region == "All region"),
      bsts_colours,
      bsts_labels,
      ribbon_alpha = 0.01,
      line_alpha = 0.5
    ) +
      scale_y_log10() +
      facet_wrap(~N_region) +
      labs(x = NULL, y = NULL) +
      guides(colour = "none", fill = "none") +
      theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
    ggsave(
      file.path(plot_dir, "FigS_bsts_main.pdf"),
      p,
      width = 5.9,
      height = 6
    )
    p <- plot.reference.curves(
      filter(bsts_sensitivity, N_region != "All region"),
      bsts_colours,
      bsts_labels,
      ribbon_alpha = 0.01,
      line_alpha = 0.5
    ) +
      scale_y_log10() +
      facet_wrap(~N_region, ncol = 4) +
      guides(
        colour = guide_legend(reverse = TRUE, override.aes = list(alpha = 1)),
        fill = "none"
      ) +
      theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"))
    ggsave(
      file.path(plot_dir, "FigS_bsts.pdf"),
      p,
      width = 12,
      height = 10,
      device = cairo_pdf
    )
  }
}

# Historical comments retained from the previous implementation.
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
### Figure 3 --------------------------------------------
# Keep a common scale across regions without deleting posterior interval endpoints.
# strip.text = element_text(size = 8, face = "bold", colour = "black"),
# strip.background = element_rect(fill = "grey95", colour = "grey70"),
### Figure appendix: CAR sensitivity analysis ----------------------
### Figure appendix: BSTS sensitivity analysis ----------------------
