#rm(list=ls()   )

library(tidyverse)
library(xml2) # get html data
library(rvest)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(httpgd)
##library(CausalImpact)
library(zoo)
library(assertthat)
library(bsts)
library(BoomSpikeSlab)
library(patchwork)
library(CARBayesST)
library(spdep)
library(coda) # for MCMC analysis and diagnose
library(foreach) # parallel computation
library(doParallel) # parallel computation



source("code/function.R")
source("code/super_ci.R")


# 0. data import ---------------------------------------------------------
source("code/data_import.R")


# 1.  Basic character -----------------------------------------------------

### calculate hall number in each LTLA ---
hall_num <- Hall_LTLA_dat %>% 
  reframe(hall_num = n(),  .by = c(LTLA_ID, LTLA_name)) %>% 
  arrange(desc(hall_num)) 


### calculate HEI number in each LTLA ---
hei_num <- HEI_cam_hall_dat %>% 
  mutate(hei_cam = substr(Unique_ID, 1, 9)) %>% 
  dplyr::select(Unique_ID, hei_cam, LTLA_ID, LTLA_name) %>% 
  group_by(LTLA_ID) %>% 
  distinct(hei_cam, .keep_all = T) %>% 
  summarise(hei_num = n(),
            LTLA_name = LTLA_name) %>% 
  distinct(LTLA_ID, .keep_all = T) %>% 
  drop_na() %>% 
  arrange(desc(hei_num))

### match LTLA, UTLA and HEI/Hall number ---
H_C <- data.frame(LTLA_ID = c(48, 284), 
                  LTLA_CD = c("E06000052", "E09000001"),
                  LTLA_name = c("Cornwall and Isles of Scilly", "Hackney and City of London"), 
                  UTLA_CD = c("E06000052", "E09000001"),
                  UTLA_name = c("Cornwall and Isles of Scilly", "Hackney and City of London"),
                  hall_num = c(7, 8), 
                  hei_num = c(2, 8))

combine1 <- reduce(list(hall_num, hei_num, ltla2utla),full_join) %>% 
  mutate(hall_num = replace_na(hall_num, 0),
         hei_num = replace_na(hei_num, 0)) %>% 
  filter(!LTLA_ID %in% c(48,49,284,295)) %>% 
  rbind(., H_C)


# sum(combine1$hall_num, na.rm = T) # check Hall number
# sum(combine1$hei_num, na.rm = T) # check HEI number

Tab1_case <- LTLA_case_dat %>% 
  group_by(LTLA_ID, areaCode, LTLA_name) %>% 
  summarise(case = sum(newCasesBySpecimenDate))

Tab1_rt <- rt_dat %>% 
  group_by(LTLA_name) %>% 
  summarise(lci = mean(lci),
            rt = mean(rt),
            uci = mean(uci))

LTLA_dat_combine <- reduce(list(LTLA_dat, combine1, Tab1_case, Tab1_rt), full_join) %>% 
  drop_na() # deleat Cornwall, Isles of Scilly, Hackney, City of London


LTLA_dat_compare <- LTLA_dat_combine %>% 
  mutate(any_hall = if_else(hall_num>0, "LTLA with hall(s)", "LTLA without hall"),
         Prosperity = as.numeric(Prosperity)) %>% 
  group_by(any_hall) %>% 
  summarise(n.LTLA = as.character(n()),
            across(c(ends_with("num")), sum, .names = "n.{col}"),
            across(c(case, GDP_pc, Pop_Den_km2, Prosperity), function(x){round(median(x, na.rm=T), 2)}, .names = "{col}_m"),
            across(c(case, GDP_pc, Pop_Den_km2, Prosperity), function(x){round(quantile(x, 0.25, na.rm = T), 2)}, .names = "{col}_p25"),
            across(c(case, GDP_pc, Pop_Den_km2, Prosperity), function(x){round(quantile(x, 0.75, na.rm = T), 2)}, .names = "{col}_p75")) %>% 
  mutate(Case_ci = paste0(case_m, " (", case_p25, ", ", case_p75, ")"),
         GDP_ci = paste0(GDP_pc_m, " (", GDP_pc_p25, ", ", GDP_pc_p75, ")"),
         PD_ci = paste0(Pop_Den_km2_m, " (", Pop_Den_km2_p25, ", ", Pop_Den_km2_p75, ")"),
         prop_ci = paste0(Prosperity_m, " (", Prosperity_p25, ", ", Prosperity_p75, ")")) %>% 
  dplyr::select(any_hall, n.LTLA, contains(c("ci"))) %>% 
  set_names("", "","Cumulative Number of cases from start to end", "GDP", "Population density", "Prosperity score") %>% 
  t()
#write.csv(LTLA_dat_compare, "outputs/table/Table1.csv")


# 2.  loction distribution ------------------------------------------------

## 解决Mac画地图的一个兼容性方法：使用 X11(type = "cairo") 搭配进行绘图结果的查看. 导出时使用quartz配合 dev.off() 使用以上的方法，grom_polygon或geom_sf速读均很快

#X11(type = "cairo") # mac画图巨快！！！！
#library(httpgd)
#hgd() # 这个可以用在apple silicon上，之前的x11方案似乎在M2上会失效，另外图形设备切换成AGG似乎比cairo还快，不贵应该没有这个快
#hgd_view() #右下角的图形设备
# hgd_browse() #浏览器中的图形设备

Eng_shp <- st_read(dsn = "LAT_shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp")
# crs_use <- st_crs(Eng_shp) # 得到shp文件中的crs (coordinate reference system)，是参考坐标系，如果设置错误，则地图的投影会错误. 一般crs设置为4326，即WGS84不会出错, 属于全球通用, 也可以设置为专有的


### add information to map data
map_dat <- Eng_shp %>% 
  full_join(., hall_num, by = c("LAD19NM" = "LTLA_name")) %>% 
  mutate(hall_num = replace_na(hall_num, 0),
         hall_num = hall_num+1) %>% 
  st_transform(., crs = 4326) ## sf画图的大坑!!!如果不转换为4326投影(或统一的投影方式), 则无法正确裁切出伦敦

### Hall location
hall_sf <- st_as_sf(Hall_LTLA_dat, coords = c('Hall_Long', 'Hall_Lat'), crs = 4326) #将hall转化为坐标

### plot England map ---
low_col <- colorRampPalette(c("#F6F6F6FF", "#E0B040FF"))
high_col <- colorRampPalette(c("#E0B040FF", "#C87810FF"))

ggsave(ggplot() +
         geom_sf(data = map_dat, aes(fill = hall_num), size = 0.05) +
         scale_fill_gradientn(name = "Number of halls", colors=c(low_col(10), high_col(50)), 
                              breaks = c(1, 20, 40, 60), labels = c("0", "20", "40", "60")) +
         geom_sf(data = hall_sf, color = "#0f2f78", size = 0.3, alpha = 0.8) +
         coord_sf() +
         theme_minimal() +
         theme(panel.grid = element_blank(),
               panel.background = element_blank(), 
               axis.text = element_blank(), 
               axis.ticks = element_blank(), 
               axis.title = element_blank(), 
               legend.position = "right",
               legend.title.align = 0.1,
               legend.title = element_text(face = "bold", size = 12),
               legend.text = element_text(size = 10),
               legend.box.just = "center"), 
       filename = "outputs/uk.pdf", width = 9, height = 14)


### plot London map (inside M25) ---
crop_factor <- st_bbox(c(xmin = -0.545479, xmax = 0.292020, ymax = 51.719041, ymin = 51.254188), crs = 4326)
map_dat_crop <- st_crop(map_dat, crop_factor)
hall_sf_crop <- st_crop(hall_sf, crop_factor)

ggsave(ggplot(map_dat_crop) +
         geom_sf(data = map_dat_crop, aes(fill = hall_num), size = 0.07) +
         scale_fill_gradientn(name = "Number of halls", colors=c(low_col(10), high_col(50)), 
                              breaks = c(1, 20, 40, 60), labels = c("0", "20", "40", "60")) +
         geom_sf(data = hall_sf_crop, color = "#0f2f78", size = 0.3, alpha = 0.8) +
         geom_sf_text(data = map_dat_crop, aes(label = LAD19NM), alpha = 1) +
         #geom_label(data = map_dat_crop, aes(x = LONG, y = LAT,label = LAD19NM), size = 5) + # 另一种标记地名方法
         #coord_sf(xlim = c(-0.545479,0.292020), ylim = c(51.254188,51.719041), expand = F, crs = 4326) + # 另一种裁切方式，不如直接在数据层面裁切渲染速读快
         theme_minimal() +
         labs(title = "London") + 
         theme(panel.grid = element_blank(),
               panel.background = element_blank(), 
               text = element_text(size = 16, face = "bold"),
               axis.text.x = element_text(vjust = 1), 
               axis.text.y = element_text(hjust = 0), 
               axis.title = element_blank(), 
               legend.position = "none"), 
       filename = "outputs/london.pdf", width = 10, height = 10, device = "pdf")

# write.csv(map.metadata, file = "linkage/Map.csv")
# save(map.data, file = "cleaned/map.RData")


# 3. Time series heatmap --------------------------------------------------
#source("code/Time_series_heatmap.R")


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
LTLA_rs <- case.cal(data = LTLA_case_dat, type = "RS", start_date = "2020-06-07", end_date = "2020-11-15")
#LTLA_week <- case.cal(data = LTLA_case_dat, type = "week", start_date = "2020-06-07", end_date = "2020-11-15")
# LTLA_ma <- case.cal(data = LTLA_case_dat, type = "MA", start_date = "2020-06-07", end_date = "2020-11-15")

## Growth rate CI ---
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
  mutate(type = if_else(hall_num==1, NA, if_else(hall_num>=2, 1, 0))) %>% 
  drop_na()


### Manual match LTLA
set.seed(997)
mat_base <- m.match(LTLA_mat, prop = 0.2, control_num = 1)

### Fit causal impact ---
### Manual data CI and plot
#reopen_names <- c(paste0("B",rev(1:10)), "Reopen", paste0("A",1:30))
reopen_names <- c(paste0("—",seq(1,10,by=2)), "Reopen", paste0("+",seq(2,30,by=2)))
### base group, 93 paired---
ci_m_base_gr <- vector("list", length = length(mat_base))
for (i in 1:length(mat_base)) { 
  ci_m_base_gr[[i]] <- names(mat_base[i])
  ci_m_base_gr[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                    case = get.info(data = mat_base[[i]], type = "case"), 
                                                    control = get.info(data = mat_base[[i]], type = "control")),
                                 intervention_date = get.info(data = mat_base[[i]], open_data = HEI_dat,type = "open"),
                                 ahead = 10,
                                 show.fig = F,
                                 save.fig = T,
                                 original = F,
                                 get.table = T,
                                 raw.data = "series",
                                 path = paste0("outputs/ci_m_base_gr/", names(mat_base[i]), ".pdf"),
                                 model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}  

ci_m_base_gr_table<- combine.dat(ci_m_base_gr, mat_base, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_"))) # 这一句之前是为了提取case LTLA的名称，现在是为了检查结果

mat_base_out <- ci_m_base_gr_table %>% 
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
write_csv(mat_base_out, "outputs/table/mat_base.csv")

#report.it(ci_m_base_gr, length.dat = mat_base)
plot.it(ci_m_base_gr, length.dat = mat_base, report_table = ci_m_base_gr_table, select.length = 41, title = "All matched LTLAs", 
        save = T, path = "outputs/allALRI.pdf", width = 14, height = 12)


set.seed(997)
mat_nolondon <- m.match(LTLA_mat_nolondon, prop = 0.2, control_num = 1)

### exclude all LTLA located in London 
ci_m_base_gr_nolon <- vector("list", length = length(mat_nolondon))
for (i in 1:length(mat_nolondon)) {
  ci_m_base_gr_nolon[[i]] <- names(mat_nolondon[i])
  ci_m_base_gr_nolon[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                          case = get.info(data = mat_nolondon[[i]], type = "case"), 
                                                          control = get.info(data = mat_nolondon[[i]], type = "control")),
                                       intervention_date = get.info(data = mat_nolondon[[i]], open_data = HEI_dat,type = "open"),
                                       ahead = 10,
                                       seed = 1902,
                                       show.fig = F,
                                       save.fig = T,
                                       original = F,
                                       get.table = T,
                                       raw.data = "series",
                                       path = paste0("outputs/ci_m_base_gr_nolon/", names(mat_nolondon[i]), ".pdf"),
                                       model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

ci_m_base_gr_nolon_table<- combine.dat(ci_m_base_gr_nolon, mat_nolondon, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_")))

mat_nolon_out <- ci_m_base_gr_nolon_table %>% 
  mutate(abs = paste0(format(exp(abs_eff),digits = 2), " (", 
                      format(exp(abs_lci),digits = 2), ", ", 
                      format(exp(abs_uci),digits = 3), ")")) %>% 
  left_join(., region_num, by = "N_num") %>% 
  group_by(N_num) %>% 
  arrange(N_num, int_date, location) %>% 
  ungroup() %>% 
  dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
  setNames(., c("Case LTLA name", "First reopen date", "Absolute effect median (95%CI)", "Region", "Matched LTLA(s)"))
write_csv(mat_nolon_out, "outputs/table/mat_nolondon.csv")

#report.it(ci_m_base_gr_nolon, length.dat = mat_nolondon)
plot.it(ci_m_base_gr_nolon, length.dat = mat_nolondon, report_table = ci_m_base_gr_nolon_table, title = "Not include London (outside of M25)", 
        save = T, path = "outputs/nolondon.pdf", width = 14, height = 12)


set.seed(997)
mat_over2hall <- m.match(LTLA_mat_hall, prop = 0.2, control_num = 1)

### find control in LTLA with over 2 halls
ci_m_base_gr_over2 <- vector("list", length = length(mat_over2hall))
for (i in 1:length(mat_over2hall)) {
  ci_m_base_gr_over2[[i]] <- names(mat_over2hall[i])
  ci_m_base_gr_over2[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                          case = get.info(data = mat_over2hall[[i]], type = "case"), 
                                                          control = get.info(data = mat_over2hall[[i]], type = "control")),
                                       intervention_date = get.info(data = mat_over2hall[[i]], open_data = HEI_dat,type = "open"),
                                       ahead = 10,
                                       seed = 1912,
                                       show.fig = F,
                                       save.fig = T,
                                       original = F,
                                       get.table = T,
                                       raw.data = "series",
                                       path = paste0("outputs/ci_m_base_gr_over2/", names(mat_over2hall[i]), ".pdf"),
                                       model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
}

ci_m_base_gr_over2_table<- combine.dat(ci_m_base_gr_over2, mat_over2hall, "table") %>% 
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_")))

over2_out <- ci_m_base_gr_over2_table %>% 
  mutate(abs = paste0(format(exp(abs_eff),digits = 2), " (", 
                      format(exp(abs_lci),digits = 2), ", ", 
                      format(exp(abs_uci),digits = 3), ")"))%>% 
  left_join(., region_num, by = "N_num") %>% 
  group_by(N_num) %>% 
  arrange(N_num, int_date, location) %>% 
  ungroup() %>% 
  dplyr::select(location, int_date, abs, N_region, mated) %>% # , rel
  setNames(., c("Case LTLA name", "First reopen date", "Absolute effect median (95%CI)", "Region", "Matched LTLA(s)"))
write_csv(over2_out, "outputs/table/over2.csv")

#report.it(ci_m_base_gr_over2, length.dat = mat_over2hall)
plot.it(ci_m_base_gr_over2, length.dat = mat_over2hall, report_table = ci_m_base_gr_over2_table, title = "Matched in LTLA with over one hall(s)",
        save = T, path = "outputs/over2.pdf", width = 14, height = 12)


######## 3 control LTLA
source("code/bsts_3.R") 

### make table2——Causal impact part
# source("code/cal.cum.ci.R") # This is another type of cal.cum.cl()
table2_ci <- cal.cum.ci(data = ci_m_base_gr, m.data = mat_base, table = ci_m_base_gr_table, tidy.out = T)


# 5. Bayesian spatio-temporal ---------------------------------------------

### 5.1 Prepare data for ST analysis, calculate H.index in each LTLA ---
date_list <- LTLA_case_dat %>% # This is the dataset include case number, Rt and LTLA information
  full_join(., LTLA_dat) %>% 
  left_join(., gmr)
  
### 5.2 Prepare h index and Google mobility report data 
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
# "HEI_ID", "HEI_name", "Campus_ID", "Economic_Quality", "Business_Environment", "Education", "Health", "Safe_Security", "Social_Capital", "Environment", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline" 
#names(HEI)

### 5.3 prepare case date, including case, growth rate and log GR. Construct a basic model without sptial correlation

case_dat <- LTLA_case_dat %>% 
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
  dplyr::select(LTLA_ID, LTLA_name, date, case, growth_rate, log_GR, log_GR2) %>% 
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

HEI_all <- HEI %>% 
  left_join(., case_dat) %>% 
  filter(LTLA_ID != 48 & LTLA_ID != 284) %>% 
  bind_rows(HEI_cornwall, HEI_london, HEI_hackry)


decay0.4 <- gen.decay(decay.period = 14, decay.fun = "1*exp(-0.4*x)", incubation = 6) # supposed decay fun type: y(t) = yf+(y0-yf)e^(-at) 0.6 OR 0.8

# get.index(dat = HEI, decay = decay0.4)


HEI_dec0.4 <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay0.4)) %>% 
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
         gmc_retail2_ma = gmc_ma+100,
         lag_retail_ma = lag(gmc_retail2_ma, n=1),
         retail_gr_ma = gmc_retail2_ma/lag_retail_ma,
         gmc_retail2 = gmc_retail+100,
         lag_retail = lag(gmc_retail2, n=1),
         retail_gr = gmc_retail2/lag_retail,
         week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
  filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
         date >= "2020-06-02" & date <= "2020-12-05") %>% 
  arrange(date, LTLA_ID) %>% 
  ungroup() %>% 
  left_join(., hall_num) %>% 
  dplyr::select(LTLA_ID, LTLA_name, date, Pop_Den_km2, Prosperity, areaCode, growth_rate, log_GR, log_GR2, h_index, h_ma,
                h_gr, h_gr_ma, retail_gr_ma, retail_gr, week, case, gmc_retail2) # case, gmc_retail2 用来画log_GR和retail_gr的变化



#summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
#summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
#summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec0.4))

#summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 
#summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 


### 5.4 prepare spatial data and adjacent matrix
#Eng_shp <- st_transform(Eng_shp, crs = 4326) 
Eng_shp_no_island <- Eng_shp %>% 
  filter(!OBJECTID %in% c(44, 50, 334, 341, 343, 361)) # These locations are island, if do not remove these locations, the adjacent matrix will return an error
## !!!!! OBJECTID is not equal to LTLA_ID !!!!!


W_adj <- poly2nb(Eng_shp_no_island, row.names = Eng_shp_no_island$LAD19NM) # This step could be slow, about 7 mins in this case
W_mat <- nb2mat(W_adj, style = "B", zero.policy = T) # This step generate a 382*382 matrix


### 5.5 Construct bayesian spatio-temporal model

# default prior: normal prior N(0,100000) 
### ST.CARanova Spatio-temporal main effects and an interaction
set.seed(458)
chain1_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
chain2_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
chain3_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
chain4_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)

### n.effect means effective number of independent samples; Geweke.diag is another MCMC convergence diagnostic that should lie between -2 and 2 to indicate convergence.

#### Check convergence - traceplot
beta.cov <- mcmc.list(chain1_anova$samples$beta, chain2_anova$samples$beta, chain3_anova$samples$beta, chain4_anova$samples$beta)

#summary(beta.cov)
#plot(beta.cov)
x11.save(beta.cov, file = "outputs/convergence_check.pdf", width = 10, height = 10)
gelman.diag(beta.cov) #### Check convergence - Gelman-Rubin plot less than 1.1indicate good mixing of the chain

#### Effects of covariates on disease risk
RR_table <- get.RR(chain1 = chain1_anova, chain2 = chain2_anova, chain3 = chain3_anova, chain4 = chain4_anova, data = HEI_dec0.4, unit = "1", coef.num = 4, digits = 2)
rownames(RR_table) <- c('Population density/Km2', "H-index", "Prosperity", "Growth rate of retail and recreation percent change")

#get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "sd", coef.num = 4) ### Here we use the standard deviation of each covariate as the increase ξ, because they represent realistic increases in each covariates value.

#get.RR(chain1 = chain1_ano_den_h, chain2 = chain2_ano_den_h, data = HEI_dec0.4, unit = "1", coef.num = 6)
#get.RR(chain1 = chain1_ano_pro_h, chain2 = chain2_ano_pro_h, data = HEI_dec0.4, unit = "1", coef.num = 6)


coef_mat <- t(rbind(chain1_anova[["samples"]][["beta"]], chain2_anova[["samples"]][["beta"]], 
                    chain3_anova[["samples"]][["beta"]], chain4_anova[["samples"]][["beta"]])) %>% as.matrix()

eff_all <-as.matrix(HEI_dec0.4[, "h_index"]) %*% coef_mat[3,] 

eff_all_ci <- t(apply(eff_all, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))))


### Prepare for map
island_name <- Eng_shp %>% 
  filter(OBJECTID %in% c(44, 50, 334, 341, 343, 361)) %>% 
  as.data.frame() %>% 
  dplyr::select(LAD19NM)


island_dat <- expand.grid(LTLA_name = island_name$LAD19NM, 
                          week = 23:49,
                          acc_risk = 1, 
                          cum = 1)


region_num2 <- LTLA_dat_combine %>% 
  dplyr::select(LTLA_ID, LTLA_name, N_region) %>% 
  left_join(region_num)


HEI_risk <- HEI_dec0.4 %>% 
  left_join(region_num2) %>% 
  bind_cols(., eff_all_ci)


HEI_risk4map <- HEI_risk %>% # maybe this is the effect of HEI
  mutate(week = isoweek(date)) %>% 
  group_by(LTLA_name, week) %>% 
  arrange(LTLA_ID) %>% 
  bind_cols(., cum = exp(unlist(tapply(eff_all_ci[,1], HEI_dec0.4$LTLA_ID, cumsum)))) %>% 
  mutate(acc_risk = exp(sum(`50%`)),
         cum = max(cum)) %>% 
  distinct(LTLA_name, week, .keep_all = T) %>% 
  dplyr::select(LTLA_name, week, acc_risk, cum) %>%   # , `2.5%`, `97.5%`
  ungroup() %>% 
  bind_rows(., island_dat)

  
risk_map <- Eng_shp %>% 
  full_join(., HEI_risk4map, by = c("LAD19NM"="LTLA_name")) %>% 
  st_transform(., crs = 4326)


### plot risk map
ggsave(
  risk_map %>% filter(week %in%  c(35:44)) %>%  
    ggplot() +
    geom_sf(aes(fill = acc_risk), size = 0.02) +
    scale_fill_gradientn(name = "Effect of HEI(s) reopen", 
                         colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey90"),
                         values = c(1,0.8,0.6,0.4,0.2,0.1,0.0001,0)) + #rev(seq(0,1,by=0.05)), breaks = c(-93, 0, 100, 200, 300, 400), labels = c("-100", "0", "100", "200", "300", "400")
    coord_sf() +
    theme_minimal() +
    facet_wrap(~week, ncol = 5) + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          legend.title.align = 0.5,
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 12),
          legend.box.just = "center",
          strip.text = element_text(size = 18)), 
  filename = "outputs/risk35_44.pdf", width = 14, height = 12)


ggsave(
  risk_map %>% filter(week %in%  c(35:44)) %>%  
    ggplot() +
    geom_sf(aes(fill = cum), size = 0.02) +
    scale_fill_gradientn(name = "Cumulative effect of\nHEI(s) reopen", 
                         colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey90"),
                         values = c(1,0.8,0.5,0.3,0.2,0.1,0.0001,0)) + #rev(seq(0,1,by=0.05)), breaks = c(-93, 0, 100, 200, 300, 400), labels = c("-100", "0", "100", "200", "300", "400")
    coord_sf() +
    theme_minimal() +
    facet_wrap(~week, ncol = 5) + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          legend.title.align = 0.5,
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 12),
          legend.box.just = "center",
          strip.text = element_text(size = 18)), 
  filename = "outputs/cumrisk35_44.pdf", width = 14, height = 12)

### plot animation 
source("code/gif.R")

### plot spatial and temporal distribution of log growth rate and retail
source("code/plot log GR and retail.R")

### make table2
table2_bs <- cal.cum.bs(HEI_dec0.4, region_num2, eff_all)
#source("code/cal.cum.bs.R") # same with cal.cum.bs() function
table2 <- bind_cols(table2_ci, table2_bs)
write.csv(table2, "outputs/table/table2.csv")

### plot region distribution
source("code/plot_region.R")

# median(HEI$h_index[HEI$h_index>0], na.rm = T) # 0.03111585 This is the median of h-index
# decay0.4 * 0.03111585
# exp(0.0311158500)
# function(x) {1*exp(-0.4*x)}

# int_coef <- array(NA, c(4000, 15))
# for (i in 0:14) {
#   for (j in 1:4000) { # 抽样得到h-index的4000个系数
#     int_coef[j,i+1] <- exp(integrate(function(x) {1*exp(-0.4*x)*coef_mat[3,][j]}, lower = 0, upper = i)$value) 
#     
#   }
# }
# 
# 
# ggsave(
#   data.frame(date = 0:14, t(apply(int_coef, 2, quantile, c(0.5, 0.025, 0.975)))) %>% 
#     setNames(., c("date","median", "lci", "uci")) %>% 
#     ggplot(aes(x = date)) +
#     geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
#     geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
#     geom_line(aes(y = median), linewidth = 2, colour = "#082243") +
#     scale_x_continuous(name = "Date", breaks = seq(0,14, by=2), labels = c("Reopen", paste0("A",seq(2,14,by=2)))) +
#     scale_y_continuous(name = "Cumulative effect related to HEIs reopen") + 
#     theme_bw() +
#     theme(axis.text = element_text(size = 14, colour = "black"),
#           axis.title = element_text(size = 16, colour = "black"),
#           plot.title = element_text(size = 18, colour = "black"),
#           plot.margin = unit(c(0.5,1,0.5,1),"cm")),
#   filename = "outputs/cum_coef.pdf", width = 14, height = 8)


