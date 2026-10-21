# Compare LTLA characteristics by presence of halls on the descriptive sample.
# Inputs are the descriptive frame prepared in 1.0; outputs include group counts.
descriptions <- bind_rows(lapply(
  c("mean", "GDP_pc", "Pop_Den_km2", "Prosperity"),
  function(v) {
    p <- wilcox.test(
      desc[[v]][desc$any_hall],
      desc[[v]][!desc$any_hall]
    )$p.value
    desc %>%
      summarise(
        n_ltla = n(),
        median = median(.data[[v]]),
        q25 = quantile(.data[[v]], 0.25),
        q75 = quantile(.data[[v]], 0.75),
        .by = any_hall
      ) %>%
      mutate(variable = v, p_value = p)
  }
))
write.csv(
  descriptions,
  file.path(table_dir, "Descriptive_LTLA_comparison.csv"),
  row.names = FALSE
)
