## This is another type of cal.cum.ci function

matched_tbl <- do.call(bind_rows, mat_base) %>% drop_na() %>% arrange(N_num) %>% split(., .$N_num) %>% 
  map(., \(df) df$subclass)

# 这一段算的是 raw_mean 和 raw_sample
raw_mean_list <- list()
raw_sample_list <- list()
before_open_list <- list()
for (i in 1:length(matched_tbl)) {
  index <- unlist(matched_tbl[i]) %>% unname()
  raw_mean_table_list <- map(index, ~ ci_m_base_gr[[.x]]$raw_mean)
  raw_sample_table_list <- map(index, ~ ci_m_base_gr[[.x]]$raw_sample)
  before_open_table_list <- map(index, function(.x){
    LTLA_gr %>% 
      filter(LTLA_name == ci_m_base_gr_table[.x,"location"] & date >=  (as.Date(ci_m_base_gr_table[.x,"int_date"])-10)) %>% 
      dplyr::select(newCasesBySpecimenDate) %>% 
      t() %>% as.data.frame()
  })
  
  #raw_mean
  raw_mean_all <- do.call(bind_rows, raw_mean_table_list)
  raw_mean_all <- raw_mean_all[, 1:41]
  raw_mean_list[[i]] <- raw_mean_all
  
  #raw_sample
  raw_sample_all <- do.call(bind_rows, raw_sample_table_list)
  raw_sample_all <- raw_sample_all[, 1:41]
  raw_sample_list[[i]] <- raw_sample_all
  
  #before_open
  before_open_all <- do.call(bind_rows, before_open_table_list)
  before_open_all <- before_open_all[, 1:41]
  before_open_list[[i]] <- before_open_all
}

mean_meta_list <- map(raw_mean_list, ~ map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE)))
before_open_meta_list <- map(before_open_list, ~map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE)))

cum_table <- pmap(list(raw_sample_list, mean_meta_list, before_open_meta_list, 12), cum.pred)

before_open <- before.dat(ci_m_base_gr_table, select.length = 41)
raw_mean <- combine.dat(data = ci_m_base_gr, length.dat = mat_base, type = "raw_mean", select.length = 41)
mean_meta <- apply(raw_mean, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
raw_sample <- combine.dat(data = ci_m_base_gr, length.dat = mat_base, type = "raw_sample", select.length = 41)
cum_table_all <- cum.pred(raw_sample, mean_meta, before_open, 12) 

label <- c("day3", "day7", "day14")
region_code <- ci_m_base_gr_table %>% left_join(., region_num, by = "N_num") %>% 
  dplyr::select(N_num, N_region) %>% arrange(N_num) %>% distinct(., .keep_all = T)

cum_table_all <- cum_table_all %>% 
  slice(., c(12+3, 12+7, 12+14)) %>% 
  exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
  mutate(cum.ci = paste0(cum.effect, " (", cum.effect.lower, ", ", cum.effect.upper, ")"),
         label = label, N_num = 0, N_region = "All region") %>% 
  dplyr::select(cum.ci, label, N_num, N_region) %>% 
  pivot_wider(names_from = "label", values_from = cum.ci) %>% 
  relocate(starts_with("N"))

cum_table_tidy <- cum_table %>% 
  map(., ~format(round(exp(slice(.x, c(12+3, 12+7, 12+14))),digits = 2), nsmall = 2) %>% 
        mutate(cum.ci = paste0(cum.effect, " (", cum.effect.lower, ", ", cum.effect.upper, ")"),
               label = label) %>% 
        dplyr::select(cum.ci, label) %>% 
        pivot_wider(names_from = "label", values_from = cum.ci)) %>% 
  do.call(bind_rows, .) %>% 
  bind_cols(., region_code) %>% 
  relocate(starts_with("N"))

cum_table_tidy <- bind_rows(cum_table_all, cum_table_tidy)
