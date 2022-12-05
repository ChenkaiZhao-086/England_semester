#rm(list=ls())
stop() # 防误触

library(tidyverse)
library(lubridate)
library(broom)
library(readxl)
library(job) # free console
library(sf)
library(TTR) # calculate moving average
##library(CausalImpact)
library(zoo)
library(assertthat)
library(bsts)
library(BoomSpikeSlab)
library(Boom)
library(MASS)
library(spdep)
library(spDataLarge)
library(CARBayesST)
library(coda) # for MCMC analysis and diagnose
library(foreach) # parallel computation in for loop
library(doParallel) # parallel computation in for loop
library(finalfit)
library(paletteer) # palette
library(forcats) # reorder plot axis
library(MatchIt)
library(xml2) # get html data
library(rvest)
library(jsonlite)
library(epiR)
library(RColorBrewer)
library(patchwork)
library(khroma)



source("function.R")
source("super_ci.R")

stop()
# 0. data import ---------------------------------------------------------

ExcelToUse <- "data/England_hall-v1.4.xlsx"
# 在V1.2版本中LTLA数据中，Cornwall和Isles of Scilly以及Hackney和City of London被分别记录为4个LTLA, 但是英国日报数据将上述地点分别合并为了Cornwall and Isles of Scilly和Hackney and city of London, 此处要考虑后续处理问题, 暂时前者使用Cornwall_48以及City of London_284的编号
# 在V1.2版本中，案例开始和结束的时间为"2020-08-08", "2020-11-23". 在V1.3版本中将拓展为"2020-07-13" & "2020-12-06"
# V1.4:删除HS、BAME、UR lable与年龄组；使用平均的方式添加Cornwall and Isles of Scilly和Hackney and City of London,编号分别为48和284; 数据向前拓展至"2020-06-01"；添加了固定的MCF指数

ltla2utla <- read_csv("data/raw/LTLA_UTLA_lookup.csv") # for match LTLA and UTLA

HEI_cam_hall_dat <- read_excel(ExcelToUse, 
                               sheet = "1.HEI-campus-hall")
HEI_cam_dat <- read_excel(ExcelToUse, 
                          sheet = "1.1HEI-campus")
Hall_LTLA_dat <- read_excel(ExcelToUse, 
                            sheet = "1.2Hall-LTLA")
LTLA_dat <- read_excel(ExcelToUse, 
                       sheet = "1.3 LTLA") %>% dplyr::select(!starts_with(c("HS","BAME"))) 
LTLA_case_dat <- read_excel(ExcelToUse, 
                       sheet = "1.4LTLA-case-rt")
rt_dat <- read_csv("rt.dat.csv")

gmr <- read_csv("data/2020_GB_Region_Mobility_Report.csv") %>% 
  mutate(Google = if_else(is.na(sub_region_2) == T, paste0(sub_region_1, "_"), paste(sub_region_1, sub_region_2, sep = "_"))) %>% 
  dplyr::select(Google, date, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline, parks_percent_change_from_baseline, transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline, residential_percent_change_from_baseline)


# 1.  Basic character -----------------------------------------------------

### calculate hall number in each LTLA ---
hall_num <- Hall_LTLA_dat %>% 
  group_by(LTLA_ID) %>% 
  summarise(hall_num = n(),
            LTLA_name = LTLA_name) %>% 
  distinct(LTLA_ID, .keep_all = T) %>% 
  arrange(desc(hall_num)) %>% 
  ungroup()

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

combine1 <- full_join(hall_num, hei_num) %>% 
  full_join(ltla2utla, .) %>% 
  mutate(hall_num = replace_na(hall_num, 0),
         hei_num = replace_na(hei_num, 0)) %>% 
  filter(LTLA_ID != 48 & LTLA_ID != 49 & LTLA_ID != 295 & LTLA_ID != 284) %>% 
  rbind(., H_C)

# sum(combine1$hall_num, na.rm = T) # check Hall number
# sum(combine1$hei_num, na.rm = T) # check HEI number

Tab1_case <- LTLA_case_dat %>% 
  group_by(LTLA_ID, areaCode, LTLA_name) %>% 
  summarise(case = sum(newCasesBySpecimenDate))

Tab1_rt <- rt_dat %>% 
  group_by(LTLA_name) %>% 
  summarise(LTLA_name = LTLA_name,
            lci = mean(lci),
            rt = mean(rt),
            uci = mean(uci)) %>% 
  distinct(., LTLA_name, .keep_all = T)

LTLA_dat_combine <- LTLA_dat %>% 
  full_join(., combine1) %>% 
  full_join(., Tab1_case) %>% 
  full_join(., Tab1_rt) %>% 
  drop_na() # deleat Cornwall, Isles of Scilly, Hackney, City of London

LTLA_dat_compare <- LTLA_dat_combine %>% 
  mutate(any_hall = if_else(hall_num>0, "LTLA with hall(s)", "LTLA without hall"),
         Prosperity = as.numeric(Prosperity)) %>% 
  group_by(any_hall) %>% 
  summarise(n.LTLA = as.character(n()),
            n.HEI = sum(hei_num),
            n.Hall = sum(hall_num),
            Case_m = median(case),
            Case_p25 = quantile(case, 0.25, na.rm = T),
            Case_p75 = quantile(case, 0.75, na.rm = T),
            Rt_m = round(median(rt), 2),
            Rt_p25 = round(quantile(rt, 0.25, na.rm = T), 2),
            Rt_p75 = round(quantile(rt, 0.75, na.rm = T), 2),
            GDP_m = round(median(GDP_pc, na.rm = T), 2),
            GDP_p25 = round(quantile(GDP_pc, 0.25, na.rm = T),2),
            GDP_p75 = round(quantile(GDP_pc, 0.75, na.rm = T),2),
            PD_m = round(median(Pop_Den_km2, na.rm = T),2),
            PD_p25 = round(quantile(Pop_Den_km2, 0.25, na.rm = T),2),
            PD_p75 = round(quantile(Pop_Den_km2, 0.75, na.rm = T),2),
            Prop_m = round(median(Prosperity, na.rm = T),2),
            Prop_p25 = round(quantile(Prosperity, 0.25, na.rm = T),2),
            Prop_p75 = round(quantile(Prosperity, 0.75, na.rm = T),2)) %>% 
  mutate(Case_ci = paste0(Case_m, " (", Case_p25, "-", Case_p75, ")"),
         Rt_ci = paste0(Rt_m, " (", Rt_p25, "-", Rt_p75, ")"),
         GDP_ci = paste0(GDP_m, " (", GDP_p25, "-", GDP_p75, ")"),
         PD_ci = paste0(PD_m, " (", PD_p25, "-", PD_p75, ")"),
         prop_ci = paste0(Prop_m, " (", Prop_p25, "-", Prop_p75, ")")) %>% 
  dplyr::select(any_hall, n.LTLA, contains(c("ci"))) %>% 
  t()
# write.csv(LTLA_dat_compare, "outputs/table/Table1.csv")


# 2.  loction distribution ------------------------------------------------

## 解决Mac画地图的一个兼容性方法：使用 X11(type = "cairo") 搭配进行绘图结果的查看. 导出时使用quartz配合 dev.off() 使用以上的方法，grom_polygon或geom_sf速读均很快

#X11(type = "cairo") # mac画图巨快！！！！

Eng_shp <- st_read(dsn = "LAT_shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp")
# crs_use <- st_crs(Eng_shp) # 得到shp文件中的crs (coordinate reference system)，是参考坐标系，如果设置错误，则地图的投影会错误. 一般crs设置为4326，即WGS84不会出错, 属于全球通用, 也可以设置为专有的
Eng_shp <- st_transform(Eng_shp, crs = 4326) ## sf画图的大坑!!!如果不转换为4326投影(或统一的投影方式), 则无法正确裁切出伦敦


### add information to map data
map_dat <- Eng_shp %>% 
  full_join(., hall_num, by = c("LAD19NM" = "LTLA_name")) %>% 
  mutate(hall_num = replace_na(hall_num, 0),
         hall_num = hall_num+1) %>% 
  st_transform(., crs = 4326) 

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


# dev.copy2pdf(file = "outputs/map_lon.pdf", width = 8, height = 8, out.type = "pdf")
# dev.off()
# graphics.off()

# write.csv(map.metadata, file = "linkage/Map.csv")
# save(map.data, file = "cleaned/map.RData")


# 3.  Time series heatmap -------------------------------------------------

### prepare dataset for Fig.heatmap ---
ts_dat <- LTLA_case_dat
rt_dat2 <- rt_dat %>% 
  mutate(Date = as.Date(Date),
         week_num = isoweek(Date)) %>% 
  group_by(LTLA_name, week_num) %>% 
  summarise(LTLA_name = LTLA_name, 
            lci = mean(lci),
            rt = mean(rt),
            uci = mean(uci)) %>% 
  distinct(., .keep_all = T)


ts_dat2 <- ts_dat %>% 
  mutate(Date = date,
         Date = as.Date(Date),
         week_num = isoweek(Date)) %>% # ISO 8601 week start at Monday
  group_by(LTLA_ID, week_num) %>% 
  summarise(week_case = if_else(sum(newCasesBySpecimenDate)==0, 0.5, sum(newCasesBySpecimenDate)), # calculate case number per week per LTLA. If sum cases=0, replace by 0.5
            LTLA_name = LTLA_name) %>%  
  distinct() %>%  # a totle of 382 LTLA. However, there were 380 LTLA here only
  ungroup() %>% # ungroup to calculate link relative (环比)
  group_by(LTLA_ID) %>% 
  mutate(diff = lag(week_case, n = 1),
         growth_rate = week_case/diff*100,
         log_GR = log(growth_rate),
         link_relative = (week_case - diff)/diff*100,
         log_case = log(week_case),
         log_dif = log(diff),
         log_link_relative = (log_case - log_dif)/log_dif*100) %>% 
  full_join(., rt_dat2) %>% 
  mutate(rt_dif = lag(rt, n = 1),
         rt_GR = (rt-rt_dif)/rt_dif*100)


ts_dat2_plot <- ts_dat2 %>% 
  full_join(., combine1) %>% 
  mutate(link_relative = if_else(link_relative>=400, 400, link_relative)) %>% 
  filter(diff >= 0 & week_num >= 30)  %>% 
  arrange(desc(hall_num))

ts_dat2_plot_rt <- ts_dat2 %>% 
  full_join(., combine1) %>% 
  filter(rt_dif > 0.0000001 & week_num >= 30) %>% 
  arrange(desc(hall_num))

### plot for growth rate/link relative --- 
# X11(type = "cairo") 

ggsave(
  ggplot(ts_dat2_plot)+
    geom_tile(aes(x = week_num, y = fct_reorder(LTLA_name, hall_num), fill=link_relative), color= "grey70",size=0.02) + 
    scale_fill_gradientn(name = "Growth rate", colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF","#082243"),
                         values = c(1.0,0.9,0.65,0.4,0.12,0), breaks = c(-93, 0, 100, 200, 300, 400), 
                         labels = c("-100", "0", "100", "200", "300", "400")) +
    scale_x_continuous(name = "Weeks", breaks = c(30:49), labels = paste0("W", 30:49), expand = c(0,0)) +
    scale_y_discrete(name = "LTLA name") +
    theme_minimal() + 
    theme(axis.text.y = element_text(size = 4, colour = "black"),
          axis.text.x = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, colour = "black"),
          plot.title = element_text(size = 16, colour = "black"),
          legend.title = element_text(size=16), 
          legend.text = element_text(size=14)),
  filename = "outputs/growth_rate.pdf", width = 14, height = 18)


### plot for Rt ---
ggsave(
  ggplot(ts_dat2_plot_rt)+
    geom_tile(aes(x = week_num, y = LTLA_name, fill=rt), color= "grey70",size=0.02) + 
    scale_fill_gradientn(name = expression("R"[t]), 
                         colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF","#082243"),
                         values = c(1.0,0.6,0.5,0.3,0.12,0)) +
    scale_x_continuous(name = "Weeks", breaks = c(30:49), labels = paste0("W", 30:49), expand = c(0,0)) +
    scale_y_discrete(name = "LTLA") +
    theme_minimal() + # base font size
    theme(axis.text.y = element_text(size = 4, colour = "black"),
          axis.text.x = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, colour = "black"),
          plot.title = element_text(size = 16, colour = "black"), 
          legend.title = element_text(size=16), 
          legend.text = element_text(size=14)),
  filename = "outputs/rt.pdf", width = 14, height = 18)
#  limits = c(-50,200), limits = c(2,8),
#dev.off()


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


### Fit causal impact ---


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

set.seed(1912)
mat_base <- m.match(LTLA_mat, prop = 0.2)
mat_nolondon <- m.match(LTLA_mat_nolondon, prop = 0.2)
mat_over2hall <- m.match(LTLA_mat_hall, prop = 0.2)


### Manual data CI and plot
#reopen_names <- c(paste0("B",rev(1:10)), "Reopen", paste0("A",1:30))
reopen_names <- c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,30,by=2)))
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
  mutate(location = str_remove(.[,"record"], regex("\\d+\\_\\d+\\_")))
#report.it(ci_m_base_gr, length.dat = mat_base)
plot.it(ci_m_base_gr, length.dat = mat_base, report_table = ci_m_base_gr_table, title = "All matched LTLA", save = T, path = "outputs/allALRI.pdf", width = 12, height = 11)

mat_base_out <- ci_m_base_gr_table %>% 
  mutate(abs = paste0(format(abs_eff,2), " (", format(abs_lci,2), "-", format(abs_uci,2), ")"),
         rel = paste0(format(relative_eff,2), " (", format(rel_lci,2), "-", format(rel_uci,2), ")")) %>% 
  dplyr::select(location, int_date, abs, rel) %>% 
  setNames(., c("LTLA name", "First reopen date", "Absolute effect median (95%CI)","Relative effect median (95%CI)"))
write_csv(mat_base_out, "outputs/table/mat_base.csv")



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
#report.it(ci_m_base_gr_nolon, length.dat = mat_nolondon)
plot.it(ci_m_base_gr_nolon, length.dat = mat_nolondon, report_table = ci_m_base_gr_nolon_table, title = "Not include London (outside of M25)", save = T, path = "outputs/nolondon.pdf", width = 12, height = 11)

mat_nolon_out <- ci_m_base_gr_nolon_table %>% 
  mutate(abs = paste0(format(abs_eff,2), " (", format(abs_lci,2), "-", format(abs_uci,2), ")"),
         rel = paste0(format(relative_eff,2), " (", format(rel_lci,2), "-", format(rel_uci,2), ")")) %>% 
  dplyr::select(location, int_date, abs, rel) %>% 
  setNames(., c("LTLA name", "First reopen date", "Absolute effect median (95%CI)","Relative effect median (95%CI)"))
write_csv(mat_nolon_out, "outputs/table/mat_nolondon.csv")


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
#report.it(ci_m_base_gr_over2, length.dat = mat_over2hall)
plot.it(ci_m_base_gr_over2, length.dat = mat_over2hall, report_table = ci_m_base_gr_over2_table, title = "Matched in LTLA with over one hall(s)", save = T, path = "outputs/over2.pdf", width = 12, height = 11)

over2_out <- ci_m_base_gr_over2_table %>% 
  mutate(abs = paste0(format(abs_eff,2), " (", format(abs_lci,2), "-", format(abs_uci,2), ")"),
         rel = paste0(format(relative_eff,2), " (", format(rel_lci,2), "-", format(rel_uci,2), ")")) %>% 
  dplyr::select(location, int_date, abs, rel) %>% 
  setNames(., c("LTLA name", "First reopen date", "Absolute effect median (95%CI)","Relative effect median (95%CI)"))
write_csv(over2_out, "outputs/table/over2.csv")


# 5. Bayesian spatio-temporal ---------------------------------------------

### 5.1 Prepare data for ST analysis, calculate H.index in each LTLA ---
date_list <- LTLA_case_dat %>% 
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
case_dat <- ts_dat %>% 
  group_by(LTLA_ID) %>% 
  arrange(LTLA_ID, date) %>% 
  mutate(date = as.Date(date),
         case = if_else(newCasesBySpecimenDate==0, 0.5, newCasesBySpecimenDate),
         diff = lag(case, n = 1),
         growth_rate = case/diff*100,
         log_GR = log(growth_rate),
         case = as.integer(case)) %>% 
  dplyr::select(LTLA_ID, LTLA_name, date, case, growth_rate, log_GR) %>% 
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

# get.index(dat = a.HEI, decay = decay)


HEI_dec0.4 <- do.call(rbind, by(HEI_all, HEI_all$LTLA_ID, get.index, decay0.4)) %>% 
  group_by(LTLA_ID) %>% 
  mutate(gmc_retail2 = gmc_retail+100,
         lag_retail = lag(gmc_retail2, n=1),
         retail_gr = gmc_retail2/lag_retail,
         week = if_else((wday(date) == 1 |wday(date) == 7), 1,0)) %>%
  filter(LTLA_ID != 43 & LTLA_ID != 333 & LTLA_ID != 340 & LTLA_ID != 342 & LTLA_ID != 360 , 
         date >= "2020-06-02" & date <= "2020-12-05") %>% 
  arrange(date, LTLA_ID) %>% 
  ungroup() %>% 
  left_join(., hall_num)


summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 
summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 

#summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4 %>% filter(hall_num > 0)))
#summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4 %>% filter(hall_num > 0))) 
#summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4 %>% filter(hall_num > 0))) 



#a <- glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4)
#b <- glm(log_GR ~ Pop_Den_km2 + Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4)
#summary(a)
#summary(b)
#exp(a$fitted.values - b$fitted.values)

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
job::job({
  set.seed(526)
  chain1_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
  chain2_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
  chain3_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
  chain4_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 110000, thin = 100, interaction = T)
}, title = "ST.CARanova")

set.seed(9453)
chain1_ano_den_h <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 * h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 220000, thin = 100, interaction = T)
chain2_ano_den_h <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 * h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 220000, thin = 100, interaction = T)
chain1_ano_pro_h <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index *  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 220000, thin = 100, interaction = T)
chain2_ano_pro_h <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index *  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 10000, n.sample = 220000, thin = 100, interaction = T)
### n.effect means effective number of independent samples; Geweke.diag is another MCMC convergence diagnostic that should lie between -2 and 2 to indicate convergence.

#### Check convergence - traceplot
beta.cov <- mcmc.list(chain1_anova$samples$beta, chain2_anova$samples$beta, chain3_anova$samples$beta, chain4_anova$samples$beta)
#plot(beta.cov)
x11.save(beta.cov, file = "outputs/beta_cov.pdf", width = 6, height = 6)
gelman.diag(beta.cov) #### Check convergence - Gelman-Rubin plot less than 1.1indicate good mixing of the chain

#### Effects of covariates on disease risk
get.RR(chain1 = chain1_anova, chain2 = chain2_anova, chain3 = chain3_anova, chain4 = chain4_anova, data = HEI_dec0.4, unit = "1", coef.num = 4)
#get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "sd", coef.num = 4) ### Here we use the standard deviation of each covariate as the increase ξ, because they represent realistic increases in each covariates value.

#get.RR(chain1 = chain1_ano_den_h, chain2 = chain2_ano_den_h, data = HEI_dec0.4, unit = "1", coef.num = 6)
#get.RR(chain1 = chain1_ano_pro_h, chain2 = chain2_ano_pro_h, data = HEI_dec0.4, unit = "1", coef.num = 6)


coef_mat <- t(rbind(chain1_anova[["samples"]][["beta"]], chain2_anova[["samples"]][["beta"]], 
                    chain3_anova[["samples"]][["beta"]], chain4_anova[["samples"]][["beta"]])) %>% as.matrix()

eff_all <-as.matrix(HEI_dec0.4[, "h_index"]) %*% coef_mat[3,] # as.matrix(HEI_dec0.4[, "Pop_Den_km2"]) %*% coef_mat[2,] + as.matrix(HEI_dec0.4[, "Prosperity"]) %*% coef_mat[4,] + as.matrix(HEI_dec0.4[, "retail_gr"]) %*% coef_mat[5,] + as.matrix(HEI_dec0.4[, "week"]) %*% coef_mat[6,] + coef_mat[1,]

# eff_noh <- as.matrix(HEI_dec0.4[, "Pop_Den_km2"]) %*% coef_mat[2,] + as.matrix(HEI_dec0.4[, "Prosperity"]) %*% coef_mat[4,] + as.matrix(HEI_dec0.4[, "retail_gr"]) %*% coef_mat[5,] + as.matrix(HEI_dec0.4[, "week"]) %*% coef_mat[6,] 

eff_all_ci <- t(apply(eff_all, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))))
#eff_noh_ci <- t(apply(eff_noh, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))))
exp(eff_all_ci)

#diff_eff <- eff_all-eff_noh
#eff <- t(apply(diff_eff, 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))))

unlist(tapply(eff_all_ci[,1], HEI_dec0.4$LTLA_ID, cumsum))



HEI_risk <- HEI_dec0.4 %>% bind_cols(., eff_all_ci)
HEI_risk4map <- HEI_risk %>% 
  mutate(week = isoweek(date)) %>% 
  group_by(LTLA_name, week) %>% 
  arrange(LTLA_ID) %>% 
  bind_cols(., cum = unlist(tapply(eff_all_ci[,1], HEI_dec0.4$LTLA_ID, cumsum))) %>% 
  mutate(acc_risk = sum(`50%`),
         cum = max(cum)) %>% 
  distinct(LTLA_name, week, .keep_all = T) %>% 
  dplyr::select(LTLA_name, week, acc_risk, cum) %>%   # , `2.5%`, `97.5%`
  ungroup()
  
  
risk_map <- Eng_shp %>% 
  full_join(., HEI_risk4map, by = c("LAD19NM"="LTLA_name")) %>% 
  st_transform(., crs = 4326)


### plot risk map
ggsave(
  risk_map %>% filter(week %in%  c(35:44)) %>%  
    ggplot() +
    geom_sf(aes(fill = acc_risk), size = 0.05) +
    scale_fill_gradientn(name = "Risk", 
                         colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF"),
                         values = c(1,0.8,0.5,0.3,0.2,0.1,0.0001)) + #rev(seq(0,1,by=0.05)), breaks = c(-93, 0, 100, 200, 300, 400), labels = c("-100", "0", "100", "200", "300", "400")
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
  filename = "outputs/risk35_44.pdf", width = 12, height = 11)

ggsave(
  risk_map %>% filter(week %in%  c(35:44)) %>%  
    ggplot() +
    geom_sf(aes(fill = cum), size = 0.05) +
    scale_fill_gradientn(name = "Cumulative risk", 
                         colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF"),
                         values = c(1,0.8,0.5,0.3,0.2,0.1,0.0001)) + #rev(seq(0,1,by=0.05)), breaks = c(-93, 0, 100, 200, 300, 400), labels = c("-100", "0", "100", "200", "300", "400")
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
  filename = "outputs/cumrisk35_44.pdf", width = 12, height = 11)



#### Plot the average risk trends
dat_break <- seq.Date(from = as.Date("2020-06-02"),
                      to = as.Date("2020-12-04"),
                      by = "7 day")
dat_label <- paste0("W", isoweek(dat_break))


fit_mat <- rbind(chain1_anova[["samples"]][["fitted"]], chain2_anova[["samples"]][["fitted"]], 
                  chain3_anova[["samples"]][["fitted"]], chain4_anova[["samples"]][["fitted"]]) %>% as.matrix()
fit_mat[5,1:20]


risk.trends <- array(NA, c(4000, length(table(HEI_dec0.4$date))))
# t_eff_all <- t(eff_all)
for (i in 1:4000) {
  risk.trends[i,] <- t(tapply(eff_all[,i], HEI_dec0.4$date, mean))
}

for (i in 1:4000) {
  risk.trends[i,] <- tapply(fit_mat[i,], HEI_dec0.4$date, mean)
}

time.trends <- t(apply(risk.trends, 2, quantile, c(0.5, 0.025, 0.975))) %>% 
  as.data.frame() %>% 
  mutate(date=as.Date(names(table(HEI_dec0.4$date))))%>% 
  setNames(.,c("median","lci", "uci","date")) 

ggsave(
  ggplot(time.trends) + 
    geom_line(aes(x = date, y = median), size = 1.2, colour = "#082243", alpha = 0.7) +
    # geom_vline(xintercept = 11, size = 1.2, linetype = 3, colour = "#bc3b29") +
    #  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(aes(x = date, ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
    scale_x_date(name = "Date", breaks = dat_break, labels = dat_label) +
    scale_y_continuous(name = "Relationship between reopen") + 
    theme_bw() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, colour = "black"),
          plot.title = element_text(size = 18, colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"),
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1)), filename = "outputs/time.trend.pdf", width = 18, height = 12)



eff_all <-as.matrix(HEI_dec0.4[, "h_index"]) %*% coef_mat[3,] 


# edian(HEI$h_index[HEI$h_index>0], na.rm = T) # 0.03111585

decay0.4 * 0.03111585

exp(0.0311158500)

function(x) {1*exp(-0.4*x)}


int_coef <- array(NA, c(4000, 14))
for (i in 1:14) {
  for (j in 1:4000) {
    int_coef[j,i] <- integrate(function(x) {0.03111585*exp(-0.4*x)*coef_mat[3,][j]}, lower = 0, upper = i)$value
  }
}

t(apply(int_coef, 2, quantile, c(0.5, 0.025, 0.975)))
fig5 <- data.frame(date = 1:14, t(apply(int_coef, 2, quantile, c(0.5, 0.025, 0.975)))) %>% setNames(., c("date","median", "lci", "uci"))
fig5[15, ] <- c(0,0,0,0)
fig5 %>% arrange(date)
ggplot(fig5, aes(x = date)) +
 # geom_vline(xintercept = 11, size = 1.2, linetype = 2, colour = "#bc3b29") +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
  geom_line(aes(y = median), size = 2, colour = "#082243") +
 # scale_x_continuous(name = "Date", breaks = seq(1,41, by=2), labels = c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,30,by=2)))) +
#  scale_y_continuous(name = "Cumulative effect related to HEIs reopen", breaks = -3:5,limits = c(-3,5), expand = c(0,0)) + 
  labs(tag = "B.") +
  theme_bw() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black"),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        plot.tag = element_text(size = 18),
        plot.tag.position = c(0, 1))



