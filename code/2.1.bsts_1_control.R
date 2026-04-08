### Manual match LTLA
set.seed(997)
mat_base <- m.match(LTLA_mat, prop = 0.2, control_num = 1)

### Fit causal impact ---
### Manual data CI and plot
# reopen_names <- c(paste0("B",rev(1:10)), "Reopen", paste0("A",1:30))
reopen_names <- c(
  paste0("—", seq(1, 10, by = 2)),
  "Reopen",
  paste0("+", seq(2, 30, by = 2))
)
### base group, 93 paired---
ci_m_base_gr <- vector("list", length = length(mat_base))
plan(multicore, workers = 10) # multicore

ci_m_base_gr <- future_map(
  1:length(mat_base),
  function(i) {
    current_name <- names(mat_base)[i]
    res <- do.Causal(
      data = prep.Causal(
        data = LTLA_gr,
        case = get.info(data = mat_base[[i]], type = "case"),
        control = get.info(data = mat_base[[i]], type = "control")
      ),
      intervention_date = get.info(
        data = mat_base[[i]],
        open_data = HEI_dat,
        type = "open"
      ),
      ahead = 10,
      show.fig = FALSE,
      save.fig = FALSE,
      original = FALSE,
      get.table = TRUE,
      raw.data = "series",
      model.args = list(niter = 5000, nseasons = 7, season.duration = 1)
    )
    return(res)
  },
  .progress = TRUE,
  .options = furrr_options(seed = 997)
)

ci_m_base_gr_table <- combine.dat(ci_m_base_gr, mat_base, "table") %>%
  mutate(location = str_remove(.[, "record"], regex("\\d+\\_\\d+\\_"))) # 这一句之前是为了提取case LTLA的名称，现在是为了检查结果

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

set.seed(997)
mat_nolondon <- m.match(LTLA_mat_nolondon, prop = 0.2, control_num = 1)

### exclude all LTLA located in London
ci_m_base_gr_nolon <- vector("list", length = length(mat_nolondon))
ci_m_base_gr_nolon <- future_map(
  1:length(mat_nolondon),
  function(i) {
    current_name <- names(mat_nolondon)[i]
    res <- do.Causal(
      data = prep.Causal(
        data = LTLA_gr,
        case = get.info(data = mat_nolondon[[i]], type = "case"),
        control = get.info(data = mat_nolondon[[i]], type = "control")
      ),
      intervention_date = get.info(
        data = mat_nolondon[[i]],
        open_data = HEI_dat,
        type = "open"
      ),
      ahead = 10,
      show.fig = FALSE,
      save.fig = FALSE,
      original = FALSE,
      get.table = TRUE,
      raw.data = "series",
      path = paste0(
        "outputs/ci_m_base_gr_nolon/",
        names(mat_nolondon[i]),
        ".pdf"
      ),
      model.args = list(niter = 5000, nseasons = 7, season.duration = 1)
    )
    return(res)
  },
  .progress = TRUE,
  .options = furrr_options(seed = 997)
)


ci_m_base_gr_nolon_table <- combine.dat(
  ci_m_base_gr_nolon,
  mat_nolondon,
  "table"
) %>%
  mutate(location = str_remove(.[, "record"], regex("\\d+\\_\\d+\\_")))

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

set.seed(997)
mat_over2hall <- m.match(LTLA_mat_hall, prop = 0.2, control_num = 1)

### find control in LTLA with over 2 halls
ci_m_base_gr_over2 <- vector("list", length = length(mat_over2hall))
ci_m_base_gr_over2 <- future_map(
  1:length(mat_over2hall),
  function(i) {
    current_name <- names(mat_over2hall)[i]
    res <- do.Causal(
      data = prep.Causal(
        data = LTLA_gr,
        case = get.info(data = mat_over2hall[[i]], type = "case"),
        control = get.info(data = mat_over2hall[[i]], type = "control")
      ),
      intervention_date = get.info(
        data = mat_over2hall[[i]],
        open_data = HEI_dat,
        type = "open"
      ),
      ahead = 10,
      show.fig = FALSE,
      save.fig = FALSE,
      original = FALSE,
      get.table = TRUE,
      raw.data = "series",
      model.args = list(niter = 5000, nseasons = 7, season.duration = 1)
    )
    return(res)
  },
  .progress = TRUE,
  .options = furrr_options(seed = 997)
)


ci_m_base_gr_over2_table <- combine.dat(
  ci_m_base_gr_over2,
  mat_over2hall,
  "table"
) %>%
  mutate(location = str_remove(.[, "record"], regex("\\d+\\_\\d+\\_")))

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

plan(sequential)
