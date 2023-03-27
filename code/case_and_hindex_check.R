
# 1.  Case check ----------------------------------------------------------

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
         log_GR = log(growth_rate),
         log_GR2 = log(growth_rate2),
         case = as.integer(case)) %>% 
  dplyr::select(LTLA_ID, LTLA_name, date, case,,case_ma, growth_rate, growth_rate2, log_GR, log_GR2) %>% 
  ungroup() 

case_dat %>% 
  filter(LTLA_ID %in% sample(case_dat$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=case), color = "red") +
  geom_line(aes(x=date, y=case_ma)) +
  facet_grid(~LTLA_ID)

case_dat %>% 
  filter(LTLA_ID %in% sample(case_dat$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=growth_rate), color = "red") +
  geom_line(aes(x=date, y=growth_rate2)) +
  facet_grid(~LTLA_ID)

case_dat %>% 
  filter(LTLA_ID %in% sample(case_dat$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=log_GR), color = "red") +
  geom_line(aes(x=date, y=log_GR2)) +
  facet_grid(~LTLA_ID)


# 2.  h index and retail check --------------------------------------------


HEI_dec <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay))
HEI_dec_nor <- HEI_dec %>% 
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
#  dplyr::select(LTLA_ID, LTLA_name, date, GDP_pc, Pop_Den_km2, Prosperity, areaCode, gmc_retail, case, growth_rate, log_GR,  h_index, h_gr, retail_gr, week) %>% 
  filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
         date >= "2020-09-01" & date <= "2020-12-05") %>% 
 # drop_na() %>% 
  arrange(LTLA_ID, date) %>% 
  ungroup()


HEI_dec_nor %>% 
  dplyr::filter(LTLA_ID %in% sample(HEI_dec_nor$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=h_index), color = "red") +
  geom_line(aes(x=date, y=h_ma)) +
  facet_grid(~LTLA_ID)

HEI_dec_nor %>% 
  filter(LTLA_ID %in% sample(HEI_dec_nor$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=h_gr), color = "red") +
  geom_line(aes(x=date, y=h_gr_ma)) +
  facet_grid(~LTLA_ID)

HEI_dec_nor %>% 
  filter(LTLA_ID %in% sample(HEI_dec_nor$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=gmc_retail), color = "red") +
  geom_line(aes(x=date, y=gmc_ma)) +
  facet_grid(~LTLA_ID)

HEI_dec_nor %>% 
  filter(LTLA_ID %in% sample(HEI_dec_nor$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=gmc_retail2), color = "red") +
  geom_line(aes(x=date, y=gmc_retail2_ma)) +
  facet_grid(~LTLA_ID)

HEI_dec_nor %>% 
  filter(LTLA_ID %in% sample(HEI_dec_nor$LTLA_ID,1)) %>% 
  ggplot() +
  geom_line(aes(x=date, y=retail_gr), color = "red") +
  geom_line(aes(x=date, y=retail_gr_ma)) +
  facet_grid(~LTLA_ID)






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