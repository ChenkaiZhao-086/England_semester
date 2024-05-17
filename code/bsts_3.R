### Manual match LTLA
set.seed(997)
mat_base3 <- m.match(LTLA_mat, prop = 0.2, control_num = 3)

### Fit causal impact ---
### base group, 93 paired---
ci_base_3 <- vector("list", length = length(mat_base3))
for (i in 1:length(mat_base3)) { 
  ci_base_3[[i]] <- names(mat_base3[i])
  ci_base_3[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                    case = get.info(data = mat_base3[[i]], type = "case"), 
                                                    control = get.info(data = mat_base3[[i]], type = "control")),
                                 intervention_date = get.info(data = mat_base3[[i]], open_data = HEI_dat,type = "open"),
                                 ahead = 10,
                                 show.fig = F,
                                 save.fig = T,
                                 original = F,
                                 get.table = T,
                                 raw.data = "series",
                                 path = paste0("outputs/3 control/3ci_m_base_gr/", names(mat_base3[i]), ".pdf"),
                                 model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}  

base_table_3<- combine.dat(ci_base_3, mat_base3, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_"))) # 这一句之前是为了提取case LTLA的名称，现在是为了检查结果

base_out_3 <- base_table_3 %>% 
  mutate(abs = paste0(format(exp(abs_eff),digits = 2), " (", 
                      format(exp(abs_lci),digits = 2), ", ", 
                      format(exp(abs_uci),digits = 3), ")")) %>% 
  # ,rel = paste0(format(relative_eff,2), " (", format(rel_lci,2), ", ", format(rel_uci,2), ")")
  left_join(., region_num, by = "N_num") %>% 
  group_by(N_num) %>% 
  arrange(N_num, int_date, location) %>% 
  ungroup() %>% 
  dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
  setNames(., c("Case LTLA name", "First reopen date", "Absolute effect median (95%CI)", "Region", "Matched LTLA(s)")) # ,"Relative effect median (95%CI)"
write_csv(base_out_3, "outputs/table/3 control/mat_base3.csv")

#report.it(ci_m_base_gr, length.dat = mat_base)
plot.it(ci_base_3, length.dat = mat_base3, report_table = base_table_3, select.length = 41, title = "All matched LTLAs", 
        save = T, path = "outputs/3 control/allALRI3.pdf", width = 14, height = 12)




set.seed(997)
mat_nolondon3 <- m.match(LTLA_mat_nolondon, prop = 0.2, control_num = 3)

### exclude all LTLA located in London 
ci_base_nolon_3 <- vector("list", length = length(mat_nolondon3))
for (i in 1:length(mat_nolondon3)) {
  ci_base_nolon_3[[i]] <- names(mat_nolondon3[i])
  ci_base_nolon_3[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                          case = get.info(data = mat_nolondon3[[i]], type = "case"), 
                                                          control = get.info(data = mat_nolondon3[[i]], type = "control")),
                                       intervention_date = get.info(data = mat_nolondon3[[i]], open_data = HEI_dat,type = "open"),
                                       ahead = 10,
                                       seed = 1902,
                                       show.fig = F,
                                       save.fig = T,
                                       original = F,
                                       get.table = T,
                                       raw.data = "series",
                                       path = paste0("outputs/3 control/3ci_m_base_gr_nolon/", names(mat_nolondon3[i]), ".pdf"),
                                       model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

ci_base_nolon_table3<- combine.dat(ci_base_nolon_3, mat_nolondon3, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_")))

mat_nolon_out3 <- ci_base_nolon_table3 %>% 
  mutate(abs = paste0(format(exp(abs_eff),digits = 2), " (", 
                      format(exp(abs_lci),digits = 2), ", ", 
                      format(exp(abs_uci),digits = 3), ")")) %>% 
  left_join(., region_num, by = "N_num") %>% 
  group_by(N_num) %>% 
  arrange(N_num, int_date, location) %>% 
  ungroup() %>% 
  dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
  setNames(., c("Case LTLA name", "First reopen date", "Absolute effect median (95%CI)", "Region", "Matched LTLA(s)"))
write_csv(mat_nolon_out3, "outputs/table/3 control/mat_nolondon3.csv")

#report.it(ci_m_base_gr_nolon, length.dat = mat_nolondon)
plot.it(ci_base_nolon_3, length.dat = mat_nolondon3, report_table = ci_base_nolon_table3, title = "Not include London (outside of M25)", 
        save = T, path = "outputs/3 control/nolondon3.pdf", width = 14, height = 12)



set.seed(997)
mat_over2hall3 <- m.match(LTLA_mat_hall, prop = 0.2, control_num = 3)

### find control in LTLA with over 2 halls
over2_3 <- vector("list", length = length(mat_over2hall3))
for (i in 1:length(mat_over2hall3)) {
  over2_3[[i]] <- names(mat_over2hall3[i])
  over2_3[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                          case = get.info(data = mat_over2hall3[[i]], type = "case"), 
                                                          control = get.info(data = mat_over2hall3[[i]], type = "control")),
                                       intervention_date = get.info(data = mat_over2hall3[[i]], open_data = HEI_dat,type = "open"),
                                       ahead = 10,
                                       seed = 1912,
                                       show.fig = F,
                                       save.fig = T,
                                       original = F,
                                       get.table = T,
                                       raw.data = "series",
                                       path = paste0("outputs/3 control/3ci_m_base_gr_over2/", names(mat_over2hall3[i]), ".pdf"),
                                       model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

over2_table3<- combine.dat(over2_3, mat_over2hall3, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_")))

over2_out3 <- over2_table3 %>% 
  mutate(abs = paste0(format(exp(abs_eff),digits = 2), " (", 
                      format(exp(abs_lci),digits = 2), ", ", 
                      format(exp(abs_uci),digits = 3), ")")) %>% 
  left_join(., region_num, by = "N_num") %>% 
  group_by(N_num) %>% 
  arrange(N_num, int_date, location) %>% 
  ungroup() %>% 
  dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
  setNames(., c("Case LTLA name", "First reopen date", "Absolute effect median (95%CI)", "Region", "Matched LTLA(s)"))
write_csv(over2_out3, "outputs/table/3 control/over2_3.csv")

#report.it(ci_m_base_gr_over2, length.dat = mat_over2hall)
plot.it(over2_3, length.dat = mat_over2hall3, report_table = over2_table3, title = "Matched in LTLA with over one hall(s)",
        save = T, path = "outputs/3 control/over2_3.pdf", width = 14, height = 12)
