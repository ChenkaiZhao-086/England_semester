# Fit the three saved single-control BSTS matching scenarios.
for (group in c("mat_base", "mat_nolondon", "mat_over2hall")) {
  path <- file.path(result_dir, paste0("bsts-", group, ".rds"))
  if (
    analysis.cached(path) &&
      identical(read.analysis(path)$outcome, bsts_outcome)
  ) {
    next
  }
  message("BSTS: ", group, " / ", length(matches[[group]]), " matched sets")
  fits <- future_map(
    matches[[group]],
    bsts_functions$fit.bsts.match,
    observations = observations,
    openings = HEI_dat,
    .progress = TRUE,
    .options = furrr_options(seed = 997)
  )
  report <- combine.dat(fits, matches[[group]], "table")
  write.analysis(
    list(
      fits = fits,
      matches = matches[[group]],
      report = report,
      outcome = bsts_outcome
    ),
    path,
    bsts_prepared_file
  )
  rm(fits)
  gc()
}

# Historical comments retained from the previous implementation.
### Manual match LTLA
### Fit causal impact ---
### Manual data CI and plot
# reopen_names <- c(paste0("B",rev(1:10)), "Reopen", paste0("A",1:30))
### base group, 93 paired---
# mat_base_out <- ci_m_base_gr_table %>%
#   mutate(
#     abs = paste0(
#       format(exp(abs_eff), digits = 2),
#       " (",
#       format(exp(abs_lci), digits = 2),
#       ", ",
#       format(exp(abs_uci), digits = 3),
#       ")"
#     )
#   ) %>%
#   # ,rel = paste0(format(relative_eff,2), " (", format(rel_lci,2), ", ", format(rel_uci,2), ")")
#   left_join(., region_num, by = "N_num") %>%
#   group_by(N_num) %>%
#   arrange(N_num, int_date, location) %>%
#   ungroup() %>%
#   dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
#   setNames(
#     .,
#     c(
#       "Case LTLA name",
#       "First reopen date",
#       "Absolute effect median (95%CI)",
#       "Region",
#       "Matched LTLA(s)"
#     )
#   ) # ,"Relative effect median (95%CI)"
# write_csv(mat_base_out, "outputs/table/mat_base.csv")
# report.it(ci_m_base_gr, length.dat = mat_base)
# plot.it(
#   ci_m_base_gr,
#   length.dat = mat_base,
#   report_table = ci_m_base_gr_table,
#   select.length = 41,
#   title = "All matched LTLAs",
#   save = T,
#   path = "outputs/allALRI.pdf",
#   width = 14,
#   height = 12
# )
### exclude all LTLA located in London
# mat_nolon_out <- ci_m_base_gr_nolon_table %>%
#   mutate(
#     abs = paste0(
#       format(exp(abs_eff), digits = 2),
#       " (",
#       format(exp(abs_lci), digits = 2),
#       ", ",
#       format(exp(abs_uci), digits = 3),
#       ")"
#     )
#   ) %>%
#   left_join(., region_num, by = "N_num") %>%
#   group_by(N_num) %>%
#   arrange(N_num, int_date, location) %>%
#   ungroup() %>%
#   dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
#   setNames(
#     .,
#     c(
#       "Case LTLA name",
#       "First reopen date",
#       "Absolute effect median (95%CI)",
#       "Region",
#       "Matched LTLA(s)"
#     )
#   )
# write_csv(mat_nolon_out, "outputs/table/mat_nolondon.csv")
# report.it(ci_m_base_gr_nolon, length.dat = mat_nolondon)
# plot.it(
#   ci_m_base_gr_nolon,
#   length.dat = mat_nolondon,
#   report_table = ci_m_base_gr_nolon_table,
#   title = "Not include London (outside of M25)",
#   save = T,
#   path = "outputs/nolondon.pdf",
#   width = 14,
#   height = 12
# )
### find control in LTLA with over 2 halls
# over2_out <- ci_m_base_gr_over2_table %>%
#   mutate(
#     abs = paste0(
#       format(exp(abs_eff), digits = 2),
#       " (",
#       format(exp(abs_lci), digits = 2),
#       ", ",
#       format(exp(abs_uci), digits = 3),
#       ")"
#     )
#   ) %>%
#   left_join(., region_num, by = "N_num") %>%
#   group_by(N_num) %>%
#   arrange(N_num, int_date, location) %>%
#   ungroup() %>%
#   dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
#   setNames(
#     .,
#     c(
#       "Case LTLA name",
#       "First reopen date",
#       "Absolute effect median (95%CI)",
#       "Region",
#       "Matched LTLA(s)"
#     )
#   )
# write_csv(over2_out, "outputs/table/over2.csv")
# report.it(ci_m_base_gr_over2, length.dat = mat_over2hall)
# plot.it(
#   ci_m_base_gr_over2,
#   length.dat = mat_over2hall,
#   report_table = ci_m_base_gr_over2_table,
#   title = "Matched in LTLA with over one hall(s)",
#   save = T,
#   path = "outputs/over2.pdf",
#   width = 14,
#   height = 12
# )
