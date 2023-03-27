
# 1.  Data prepare --------------------------------------------------------


date_list <- LTLA_case_dat %>% 
  full_join(., LTLA_dat) %>% 
  left_join(., gmr)

HEI <- HEI_cam_dat %>% 
  group_by(HEI_ID) %>% 
  mutate(n.hall = sum(Number_of_hall),
         mcf = if_else(is.na(mcf), Number_of_hall/n.hall, mcf),
         h.ref = 40485,
         h_index = (Hall_of_student/h.ref)*mcf) %>%  # The university with most students and all halls located in one LTLA
  left_join(., LTLA_dat_combine, by = c("Campus_name" = "LTLA_name")) %>% 
  group_by(Campus_name, HEI_opening) %>% 
  mutate(LTLA_name = Campus_name, # add 2 column named LTLA_name and date
         date = HEI_opening) %>% 
  ungroup() %>% 
  distinct(., LTLA_name, date, .keep_all = T) %>% 
  full_join(., date_list) %>% # combine date and other information like H-index and etc.
  arrange(LTLA_ID, date) %>% 
  mutate(h_index = replace_na(h_index, 0),
         gmc_retail = na.locf(retail_and_recreation_percent_change_from_baseline, na.rm = F),
         gmc_grocery = na.locf(grocery_and_pharmacy_percent_change_from_baseline, na.rm = F),
         gmc_park = na.locf(parks_percent_change_from_baseline, na.rm = F),
         gmc_park = if_else(is.na(gmc_park), 0, gmc_park),
         gmc_transit = na.locf(transit_stations_percent_change_from_baseline, na.rm = F),
         gmc_work = na.locf(workplaces_percent_change_from_baseline, na.rm = F),
         gmc_res = na.locf(residential_percent_change_from_baseline, na.rm = F)) %>% # Use LOCF to replace NA in GMR_redail data
  dplyr::select(LTLA_ID, LTLA_name, date, h_index, GDP_pc, Pop_Den_km2, Prosperity, areaCode, gmc_retail, gmc_grocery, 
                gmc_park, gmc_transit, gmc_work, gmc_res) %>% 
  drop_na() 

case_dat <- ts_dat %>% 
  group_by(LTLA_ID) %>% 
  arrange(LTLA_ID, date) %>% 
  mutate(date = as.Date(date),
         case = if_else(newCasesBySpecimenDate==0, 0.5, newCasesBySpecimenDate),
         case_ma = rollmean(case, 7, align = "right", fill = NA),
         diff = lag(case, n = 1),
         diff2 = lag(case_ma, n=1),
         growth_rate = case/diff*100,
         growth_rate2 = case_ma/diff2*100,
         log_GR = log(case/diff),
         log_GR2 = log(case_ma/diff2),
         case = as.integer(case)) %>% 
  dplyr::select(LTLA_ID, LTLA_name, date, case,case_ma, growth_rate, growth_rate2, log_GR, log_GR2) %>% 
  ungroup() 


### for matching the spatial data, reconstructed 48.Cornwall, 284.City of London, 295.Hackey. the data of 284 and 295 are exactly same
HEI_cornwall <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID == 48) %>% 
  mutate(LTLA_name = "Cornwall")
HEI_london <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID == 284) %>% 
  mutate(LTLA_name = "City of London")
HEI_hackry <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID == 284) %>% 
  mutate(LTLA_ID = 295, LTLA_name = "Hackney")

HEI_all_ma <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID != 48 & LTLA_ID != 284) %>% 
  bind_rows(HEI_cornwall, HEI_london, HEI_hackry) %>% 
  group_by(LTLA_ID) %>% 
  mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
         lag_h = lag(h_ma, n=1),
         h_gr = h_ma/lag_h,
         h_gr = if_else(is.nan(h_gr),0,h_gr),
         h_gr = if_else(is.infinite(h_gr),1,h_gr),
         gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
         gmc_retail2 = gmc_ma+100,
         lag_retail = lag((gmc_ma+100), n=1),
         retail_gr = gmc_retail2/lag_retail,
         week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>% 
  dplyr::select(LTLA_ID, LTLA_name, date, GDP_pc, Pop_Den_km2, Prosperity, areaCode, gmc_retail, case, growth_rate, growth_rate2, 
                log_GR, log_GR2,  h_index, h_gr, retail_gr, week)



HEI_all <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID != 48 & LTLA_ID != 284) %>% 
  bind_rows(HEI_cornwall, HEI_london, HEI_hackry)



# 2.  Run log linear regression -------------------------------------------


result <- list() # 传统TS，简单week，全时段
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec))
  result[[paste0("Lag",i,"_nw")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr, family = "gaussian", data = HEI_dec))
}


result2 <- list() # 传统TS，简单week，分层8.17-9.10
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  HEI_before <- HEI_dec %>% filter(date >= "2020-06-02" & date < "2020-08-17")
  HEI_after <- HEI_dec %>% filter(date > "2020-09-10" & date <= "2020-12-05")
  
  result2[[paste0("Lag",i,"_before")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr + week, 
                                                      family = "gaussian", data = HEI_before))
  result2[[paste0("Lag",i,"_after")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr + week, 
                                                     family = "gaussian", data = HEI_after))
}


result3 <- list() # 传统TS，复杂week，全时段
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = wday(date)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result3[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec))
}


result4 <- list() # 传统TS，复杂week，分层8.17-9.10
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = wday(date)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  HEI_before <- HEI_dec %>% filter(date >= "2020-06-02" & date < "2020-08-17")
  HEI_after <- HEI_dec %>% filter(date > "2020-09-10" & date <= "2020-12-05")
  
  result4[[paste0("Lag",i,"_before")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr + week, 
                                                      family = "gaussian", data = HEI_before))
  result4[[paste0("Lag",i,"_after")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr + week, 
                                                     family = "gaussian", data = HEI_after))
}


result5 <- list() # 传统TS，简单week，删除NA
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result5[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec))
  result5[[paste0("Lag",i,"_nw")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr, family = "gaussian", data = HEI_dec))
}

result6 <- list() # 传统TS，复杂week，删除NA
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = wday(date)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result6[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec))
}


result7 <- list() # 传统TS，简单week，全时段, 交互
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result7[[paste0("Lag",i, "_den*h")]] <- summary(glm(log_GR ~ Pop_Den_km2*h_gr +  Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec))
  result7[[paste0("Lag",i, "_pro*h")]] <- summary(glm(log_GR ~ Pop_Den_km2 + Prosperity*h_gr +  retail_gr + week, family = "gaussian", data = HEI_dec))
}

result8 <- list() # 传统TS，简单week，分层8.17-9.10
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  HEI_before <- HEI_dec %>% filter(date >= "2020-06-02" & date < "2020-08-17")
  HEI_after <- HEI_dec %>% filter(date > "2020-09-10" & date <= "2020-12-05")
  
  result8[[paste0("Lag",i,"_before","_den*h")]] <- summary(glm(log_GR ~ Pop_Den_km2*h_gr +  Prosperity + retail_gr + week, 
                                                      family = "gaussian", data = HEI_before))
  result8[[paste0("Lag",i,"_after","_den*h")]] <- summary(glm(log_GR ~ Pop_Den_km2*h_gr +  Prosperity + retail_gr + week, 
                                                     family = "gaussian", data = HEI_after))
  result8[[paste0("Lag",i,"_before","_pro*h")]] <- summary(glm(log_GR ~ Pop_Den_km2 +  Prosperity*h_gr + retail_gr + week, 
                                                      family = "gaussian", data = HEI_before))
  result8[[paste0("Lag",i,"_after","_pro*h")]] <- summary(glm(log_GR ~ Pop_Den_km2 +  Prosperity*h_gr  + retail_gr + week, 
                                                     family = "gaussian", data = HEI_after))
}
# gmc_grocery, gmc_park, gmc_transit, gmc_work, gmc_res

result9 <- list() # MATS，简单week，全时段
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result9[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma +week, family = "gaussian", data = HEI_dec))
  result9[[paste0("Lag",i,"_nw")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec))
}

result10 <- list() # MATS，简单week，分层8.17-9.10
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  HEI_before <- HEI_dec %>% filter(date >= "2020-06-02" & date < "2020-08-17")
  HEI_after <- HEI_dec %>% filter(date > "2020-09-10" & date <= "2020-12-05")
  
  result10[[paste0("Lag",i,"_before")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, 
                                                      family = "gaussian", data = HEI_before))
  result10[[paste0("Lag",i,"_after")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, 
                                                     family = "gaussian", data = HEI_after))
}

result9 <- list() # MATS，week，全时段
for (i in 0:7) {
  decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.6*x)", incubation = i) 
  HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
    group_by(LTLA_ID) %>% 
    mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
           h_ma = if_else(h_ma<1e-10, 0, h_ma),
           lag_h = lag(h_index, n=1),
           lag_h_ma = lag(h_ma, n=1),
           h_gr = h_index/lag_h,
           h_gr = if_else(is.nan(h_gr),0,h_gr),
           h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
           h_gr_ma = h_ma/lag_h_ma,
           h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
           h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
           gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
           gmc_retail2 = gmc_retail+100,
           lag_retail = lag(gmc_retail2, n=1),
           retail_gr = gmc_retail2/lag_retail,
           gmc_retail2_ma = gmc_ma+100,
           lag_retail_ma = lag(gmc_retail2_ma, n=1),
           retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
           week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
    filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
           date >= "2020-06-02" & date <= "2020-12-05") %>% 
    arrange(LTLA_ID, date) %>% 
    ungroup()
  
  result9[[paste0("Lag",i,"_w")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma +week, family = "gaussian", data = HEI_dec))
  result9[[paste0("Lag",i,"_nw")]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec))
}


# 3.  Construct a lag and decay matrix ------------------------------------


dec_fun <- c("0*x", "1*exp(-0.2*x)", "1*exp(-0.4*x)", "1*exp(-0.6*x)", "1*exp(-0.8*x)", "1+0*x")
result11 <- list() # 传统TS，简单week，全时段，多重衰减
for (i in 0:7) {
  for (j in 1:6) {
    decay <- gen.decay(decay.period = 14, decay.fun = dec_fun[j], incubation = i) 
    HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
      group_by(LTLA_ID) %>% 
      mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
             h_ma = if_else(h_ma<1e-10, 0, h_ma),
             lag_h = lag(h_index, n=1),
             lag_h_ma = lag(h_ma, n=1),
             h_gr = h_index/lag_h,
             h_gr = if_else(is.nan(h_gr),0,h_gr),
             h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
             h_gr_ma = h_ma/lag_h_ma,
             h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
             h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
             gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
             gmc_retail2 = gmc_retail+100,
             lag_retail = lag(gmc_retail2, n=1),
             retail_gr = gmc_retail2/lag_retail,
             gmc_retail2_ma = gmc_ma+100,
             lag_retail_ma = lag(gmc_retail2_ma, n=1),
             retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
             week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
      filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
             date >= "2020-06-02" & date <= "2020-12-05") %>% 
      arrange(LTLA_ID, date) %>% 
      ungroup()
    
    result11[[paste0("Lag",i,"_w",dec_fun[j])]] <- summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec)) 
  }
}

min(
  print(unlist(lapply(result11, function(x) x$aic)), digits = 9)
        )
result11$`Lag2_w1+0*x`
result11$`Lag6_w1*exp(-0.4*x)`


dec_fun <- c("0*x", "1*exp(-0.2*x)", "1*exp(-0.4*x)", "1*exp(-0.6*x)", "1*exp(-0.8*x)", "1+0*x")
result12 <- list() # 传统TS，简单week，全时段，多重衰减
for (i in 0:7) {
  for (j in 1:6) {
    decay <- gen.decay(decay.period = 14, decay.fun = dec_fun[j], incubation = i) 
    HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
      group_by(LTLA_ID) %>% 
      mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
             h_ma = if_else(h_ma<1e-10, 0, h_ma),
             lag_h = lag(h_index, n=1),
             lag_h_ma = lag(h_ma, n=1),
             h_gr = h_index/lag_h,
             h_gr = if_else(is.nan(h_gr),0,h_gr),
             h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
             h_gr_ma = h_ma/lag_h_ma,
             h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
             h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
             gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
             gmc_retail2 = gmc_retail+100,
             lag_retail = lag(gmc_retail2, n=1),
             retail_gr = gmc_retail2/lag_retail,
             gmc_retail2_ma = gmc_ma+100,
             lag_retail_ma = lag(gmc_retail2_ma, n=1),
             retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
             week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
      filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
             date >= "2020-06-02" & date <= "2020-12-05") %>% 
      arrange(LTLA_ID, date) %>% 
      ungroup()
    
    result12[[paste0("Lag",i,"_w",dec_fun[j])]] <- summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec)) 
  }
}

min(
  print(unlist(lapply(result12, function(x) x$aic)), digits = 9)
)


# 4.  Get min AIC regression result ---------------------------------------


decay <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.4*x)", incubation = 6) 
HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay)) %>% 
  group_by(LTLA_ID) %>% 
  mutate(h_ma = rollmean(h_index, 7, align = "right", fill = NA), 
         h_ma = if_else(h_ma<1e-10, 0, h_ma),
         lag_h = lag(h_index, n=1),
         lag_h_ma = lag(h_ma, n=1),
         h_gr = h_index/lag_h,
         h_gr = if_else(is.nan(h_gr),0,h_gr),
         h_gr = if_else(is.infinite(h_gr),exp(-0.6),h_gr),
         h_gr_ma = h_ma/lag_h_ma,
         h_gr_ma = if_else(is.nan(h_gr_ma),0,h_gr_ma),
         h_gr_ma = if_else(is.infinite(h_gr_ma),1+exp(-0.6),h_gr_ma),
         gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
         gmc_retail2 = gmc_retail+100,
         lag_retail = lag(gmc_retail2, n=1),
         retail_gr = gmc_retail2/lag_retail,
         gmc_retail2_ma = gmc_ma+100,
         lag_retail_ma = lag(gmc_retail2_ma, n=1),
         retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
         week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
  filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
         date >= "2020-06-02" & date <= "2020-12-05") %>% 
  arrange(LTLA_ID, date) %>% 
  ungroup()
summary(glm(log_GR ~ Pop_Den_km2*h_index +  Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec)) 
summary(glm(log_GR ~ Pop_Den_km2 + Prosperity*h_index + retail_gr + week, family = "gaussian", data = HEI_dec)) 
summary(glm(log_GR ~ Pop_Den_km2 + Prosperity + h_index + retail_gr + week, family = "gaussian", data = HEI_dec)) 


