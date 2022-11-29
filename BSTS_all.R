# 4.  BSTS ----------------------------------------------------------------
##  <="2020-11-15" Use this cutoff date
### Prepare open data ---
HEI_dat <- HEI_cam_dat %>% 
  dplyr::select(HEI_ID, HEI_name, Campus_name, HEI_opening) %>% 
  left_join(., combine1, by = c("Campus_name" = "LTLA_name")) %>% 
  mutate(HEI_opening = as.Date(HEI_opening),
         LTLA_name = Campus_name) %>% 
  drop_na() 

### LTLA_case_dat moving average or rolling sum ---
LTLA_ma <- case.cal(data = LTLA_case_dat, type = "MA", start_date = "2020-06-07", end_date = "2020-11-15")
LTLA_rs <- case.cal(data = LTLA_case_dat, type = "RS", start_date = "2020-06-07", end_date = "2020-11-15")

### Set case and control condition for the following match ---
region_num <- data.frame(N_region = LTLA_dat_combine$N_region %>% unique(., ), N_num = 1:12)

LTLA_mat <- LTLA_dat_combine %>% 
  full_join(., region_num) %>% 
  dplyr::select(!c("UTLA_CD", "UTLA_name", "LTLA_Lat", "LTLA_Long", "areaCode", "lci", "uci", "N_region", "rt")) %>% 
  mutate(type = if_else(hall_num>0, 1, 0))


LTLA_mat_nolondon <- LTLA_dat_combine %>% 
  full_join(., region_num) %>% 
  filter(N_region != "London") %>% 
  dplyr::select(!c("UTLA_CD", "UTLA_name", "LTLA_Lat", "LTLA_Long", "areaCode", "lci", "uci", "N_region", "rt")) %>% 
  mutate(type = if_else(hall_num>0, 1, 0))


LTLA_mat_hall <- LTLA_dat_combine %>% 
  full_join(., region_num) %>% 
  filter(N_region != "London") %>% 
  dplyr::select(!c("UTLA_CD", "UTLA_name", "LTLA_Lat", "LTLA_Long", "areaCode", "lci", "uci", "N_region", "rt")) %>% 
  mutate(type = if_else(hall_num>=2, 1, 0))


### Propensity Score Matching ---
psm_base <- matchit(type ~ Pop_Den_km2 + Economic_Quality + Business_Environment + Education + Health + Safe_Security +
                      Social_Capital + Environment + N_num, data = LTLA_mat, method = "nearest", distance = "glm", replace = T) # 在Matchit中，replace用于控制一个对照可否匹配于多个干预，ratio用于控制匹配比例


psm_nolondon <- matchit(type ~ Pop_Den_km2 + Economic_Quality + Business_Environment + Education + Health + Safe_Security +
                          Social_Capital + Environment + N_num, data = LTLA_mat_nolondon, method = "nearest", distance = "glm", replace = T)


psm_over2hall <- matchit(type ~ Pop_Den_km2 + Economic_Quality + Business_Environment + Education + Health + Safe_Security +
                           Social_Capital + Environment + N_num, data = LTLA_mat_hall, method = "nearest", distance = "glm", replace = F)

summary(psm_base) 
summary(psm_nolondon)
summary(psm_over2hall)
# Values of standardized mean differences and eCDF statistics close to zero and values of variance ratios close to one indicate good balance 
# a standardised mean difference great than 0.1 can be considered as substantial difference
#b <- match.data(a)

#plot(psm_base, type = "jitter", interactive = F)
#plot(summary(psm), abs = FALSE)
#plot(psm_nolondon, type = "jitter", interactive = F)
#plot(summary(psm_nolondon), abs = FALSE)
#plot(psm_over2hall, type = "jitter", interactive = F)
#plot(summary(psm_over2hall), abs = FALSE)


psm_base_dat <- prep.psm.dat(psm_base)
psm_nolondon_dat <- prep.psm.dat(psm_nolondon)
psm_over2hall_dat <- prep.psm.dat(psm_over2hall)


### Fit causal impact ---
## base data
ci_psm_base <- vector("list", length = length(psm_base_dat))
for (i in 1:length(psm_base_dat)) {
  ci_psm_base[[i]] <- names(psm_base_dat[i])
  ci_psm_base[[i]] <- do.Causal(data = prep.Causal(data = LTLA_case_dat, 
                                                   case = get.info(data = psm_base_dat[[i]], type = "case"), 
                                                   control = get.info(data = psm_base_dat[[i]], type = "control")),
                                intervention_date = get.info(data = psm_base_dat[[i]], open_data = HEI_dat,type = "open"),
                                show.fig = F,
                                get.table = T,
                                save.fig = T,
                                path = paste0("outputs/ci_base/", names(psm_base_dat[i]), ".pdf"),
                                model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}
ci_psm_base <- do.call(rbind, ci_psm_base)
record <- names(psm_base_dat)
ci_psm_base<-cbind(record, ci_psm_base)
write_csv(ci_psm_base, "ci_result_allLTLA_5000iter.csv")


## no London data
ci_psm_nolondon <- vector("list", length = length(psm_nolondon_dat))
for (i in 1:length(psm_nolondon_dat)) {
  ci_psm_nolondon[[i]] <- names(psm_nolondon_dat[i])
  ci_psm_nolondon[[i]] <- do.Causal(data = prep.Causal(data = LTLA_case_dat, 
                                                       case = get.info(data = psm_nolondon_dat[[i]], type = "case"), 
                                                       control = get.info(data = psm_nolondon_dat[[i]], type = "control")),
                                    intervention_date = get.info(data = psm_nolondon_dat[[i]], open_data = HEI_dat,type = "open"),
                                    show.fig = F,
                                    get.table = T,
                                    save.fig = T,
                                    path = paste0("outputs/ci_nolondon/", names(psm_nolondon_dat[i]), ".pdf"),
                                    model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

ci_psm_nolondon <- do.call(rbind, ci_psm_nolondon)
record <- names(psm_nolondon_dat)
ci_psm_nolondon<-cbind(record, ci_psm_nolondon)
write_csv(ci_psm_nolondon, "ci_result_nolondon_5000iter.csv")


## over 2 hall as case data 
ci_psm_over2hall <- vector("list", length = length(psm_over2hall_dat))
for (i in 1:length(psm_over2hall_dat)) {
  ci_psm_over2hall[[i]] <- names(psm_over2hall_dat[i])
  ci_psm_over2hall[[i]] <- do.Causal(data = prep.Causal(data = LTLA_case_dat, 
                                                        case = get.info(data = psm_over2hall_dat[[i]], type = "case"), 
                                                        control = get.info(data = psm_over2hall_dat[[i]], type = "control")),
                                     intervention_date = get.info(data = psm_over2hall_dat[[i]], open_data = HEI_dat,type = "open"),
                                     show.fig = F,
                                     get.table = T,
                                     save.fig = T,
                                     path = paste0("outputs/ci_over2hall/", names(psm_over2hall_dat[i]), ".pdf"),
                                     model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

ci_psm_over2hall <- do.call(rbind, ci_psm_over2hall)
record<-names(psm_over2hall_dat)
ci_psm_over2hall <- cbind(record, ci_psm_over2hall)
write_csv(ci_psm_over2hall, "ci_result_over2hall_5000iter.csv")


### If only want to save Fig and table, a parallel for loop is much faster 
registerDoParallel(8)
ci_resul2 <- foreach (i = 1:length(psm_base_dat), rbind) %dopar% {
  do.Causal(data = prep.Causal(data = LTLA_case_dat, 
                               case = get.info(data = psm_base_dat[[i]], type = "case"), 
                               control = get.info(data = psm_base_dat[[i]], type = "control")),
            intervention_date = get.info(data = psm_base_dat[[i]], open_data = HEI_dat,type = "open"),
            show.fig = F,
            get.table = T,
            save.fig = T,
            path = paste0("outputs/ci_base/", names(psm_base_dat[i]), ".pdf"),
            model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}
ci_resul2 <- do.call(rbind, ci_resul2)
record <- names(psm_base_dat)


## Growth rate CI ---
LTLA_rs
LTLA_gr <- LTLA_rs %>% 
  group_by(LTLA_ID) %>% 
  mutate(newCasesBySpecimenDate = replace(newCasesBySpecimenDate, newCasesBySpecimenDate == 0, 0.5), 
         lag = lag(newCasesBySpecimenDate,1),
         gr = (newCasesBySpecimenDate/lag),
         log_gr = log(gr),
         newCasesBySpecimenDate = log_gr) %>% 
  ungroup() %>% 
  drop_na() %>% 
  dplyr::select("areaCode", "LTLA_ID", "LTLA_name", "date", "newCasesBySpecimenDate")


mat_base <- m.match(LTLA_mat)
mat_nolondon <- m.match(LTLA_mat_nolondon)
mat_over2hall <- m.match(LTLA_mat_hall)


## Manual data CI
ci_m_base_gr <- vector("list", length = length(mat_base))
job::job({
  for (i in 1:length(mat_base)) { 
    ci_m_base_gr[[i]] <- names(mat_base[i])
    ci_m_base_gr[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                      case = get.info(data = mat_base[[i]], type = "case"), 
                                                      control = get.info(data = mat_base[[i]], type = "control")),
                                   intervention_date = get.info(data = mat_base[[i]], open_data = HEI_dat,type = "open"),
                                   ahead = 10,
                                   show.fig = F,
                                   save.fig = F,
                                   original = F,
                                   get.table = T,
                                   raw.data = "series",
                                   path = paste0("outputs/ci_m_base_gr/", names(mat_base[i]), ".pdf"),
                                   model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
  }
}, title = "base")

ci_m_base_gr_table<- combine_dat(ci_m_base_gr, mat_base, "table")
write_csv(ci_m_base_gr_table, "ci_m_base_gr_5000iter.csv")

ci_m_base_gr_raw_mean <- combine_dat(ci_m_base_gr, mat_base, "raw_mean")
ci_m_base_gr_mean_meta <- apply(ci_m_base_gr_raw_mean, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
ci_m_base_gr_raw_sample <- combine_dat(ci_m_base_gr, mat_base, "raw_sample")
ci_m_base_gr_sample_meta <- apply(ci_m_base_gr_raw_mean, 2, FUN = function(x) quantile(x, c(0.025, 0.975), na.rm = T))

ci_m_base_gr_nolon <- vector("list", length = length(mat_nolondon))
job::job({
  for (i in 1:length(mat_nolondon)) {
    ci_m_base_gr_nolon[[i]] <- names(mat_nolondon[i])
    ci_m_base_gr_nolon[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                            case = get.info(data = mat_nolondon[[i]], type = "case"), 
                                                            control = get.info(data = mat_nolondon[[i]], type = "control")),
                                         intervention_date = get.info(data = mat_nolondon[[i]], open_data = HEI_dat,type = "open"),
                                         show.fig = F,
                                         get.table = T,
                                         save.fig = T,
                                         path = paste0("outputs/ci_m_base_gr_nolon/", names(mat_nolondon[i]), ".pdf"),
                                         model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
  }
}, title = "no london")
ci_m_base_gr_nolon <- do.call(rbind, ci_m_base_gr_nolon)
record<-names(matched)
ci_m_base_gr_nolon <- cbind(record, ci_m_base_gr_nolon)
write_csv(ci_m_base_gr_nolon, "ci_m_base_gr_nolon_5000iter.csv")


ci_m_base_gr_over2 <- vector("list", length = length(mat_over2hall))
job::job({
  for (i in 1:length(mat_over2hall)) {
    ci_m_base_gr_over2[[i]] <- names(mat_over2hall[i])
    ci_m_base_gr_over2[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                            case = get.info(data = mat_over2hall[[i]], type = "case"), 
                                                            control = get.info(data = mat_over2hall[[i]], type = "control")),
                                         intervention_date = get.info(data = mat_over2hall[[i]], open_data = HEI_dat,type = "open"),
                                         show.fig = F,
                                         get.table = T,
                                         save.fig = T,
                                         path = paste0("outputs/ci_m_base_gr_over2/", names(mat_over2hall[i]), ".pdf"),
                                         model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
  }
}, title = "over two hall")


ci_m_base_gr_over2 <- do.call(rbind, ci_m_base_gr_over2)
record<-names(matched)
ci_m_base_gr_over2 <- cbind(record, ci_m_base_gr_over2)
write_csv(ci_m_base_gr_over2, "ci_m_base_gr_over2_5000iter.csv")

