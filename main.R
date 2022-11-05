#rm(list=ls())
stop() # 防误触

library(tidyverse)
library(lubridate)
library(broom)
library(readxl)
library(job) # free console
library(sf)
library(TTR) # calculate moving average
library(CausalImpact)
library(spdep)
library(spDataLarge)
library(CARBayesST)
library(coda) # for MCMC analysis and diagnose
library(parallel) # parallel computation in *apply family
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



source("function.R")
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

### tools ---
{
  ##Eng_ltla_case <- read_csv("data/raw/Eng_ltla_case.csv")
  ##Eng_case <- Eng_ltla_case %>% 
  #mutate(date = as.Date(date)) %>% 
  #  filter(date >= "2020-06-01" & date <= "2020-12-06") %>% 
  #  left_join(., LTLA_case_dat, by = c("areaName" = "LTLA_name")) %>% 
  #  dplyr::select(areaCode.x, LTLA_ID, areaName, date, newCasesBySpecimenDate.x) %>% 
  #  group_by(areaName, date) %>% 
  #  distinct(areaName, date, .keep_all = T) %>% 
  #  rename(areaCode = areaCode.x, CLTLA_name = areaName, newCasesBySpecimenDate = newCasesBySpecimenDate.x)
  ##write_csv(Eng_case, "Eng_case.csv")
} # expand data
{
  # prepare LTLA code, otherwise the memorary will exhaust
  ##LTLA_code <- LTLA_case_dat %>% 
  # dplyr::select(LTLA_ID, LTLA_name) %>% 
  #  distinct(., .keep_all = T) %>% 
  #  rename(areaName = LTLA_name)
  
  ##age_dat <- read_csv("ltla_age_demo.csv")
  
  ##Eng_age_dat <- age_dat %>% 
  # mutate(date = as.Date(date)) %>% 
  #  filter(date >= "2020-07-27" & date <= "2020-12-06") %>% 
  # full_join(., LTLA_code) %>% 
  #dplyr::select(areaCode, LTLA_ID, areaName, date, age, cases, rollingSum, rollingRate)
  
  ##write_csv(Eng_age_dat, "Eng_age_dat.csv")
} # prepare age structure data


# 0.1 get Rt data ---------------------------------------------------------

linkage <- read_excel("data/raw/Linkage_data.xlsx") %>% dplyr::select(Map, Imperial)

getRt <- full_join(LTLA_dat, linkage, by = c("LTLA_name" = "Map"))
getRt[getRt$LTLA_name == "Cornwall and Isles of Scilly", "Imperial"] <- "Cornwall_and_Isles_of_Scilly.html"
getRt[getRt$LTLA_name == "Hackney and City of London", "Imperial"] <- "Hackney_and_City_of_London.html"


R.data <- do.call(rbind, lapply(getRt$Imperial, FUN = crawlOne))
R.data <- R.data %>% 
  mutate(date = as.Date(x, origin = "1970-01-01")) %>% # 数据最开始的一列是以数字记录的日期，需要进行转换
  filter(date >= "2020-06-01" & date <= "2020-12-06") %>% 
  dplyr::select(date, Imperial, contains("ci"))
R.data1 <- R.data
{
  rt_dat <- read_excel("rt_data.xlsx") %>% 
    dplyr::select(LTLA_name= area, Date, lci = Rt_2_5, rt = Rt_50, uci = Rt_97_5) %>% 
    filter(LTLA_name != c("Hackney", "City of London", "Cornwall", "Isles of Scilly"),
           Date >= "2020-07-13" & Date <= "2020-12-06") # Rt data 的分类与LTLA的分类方式不同，Table1暂时不做Rt
} # the other type of Rt dataset
write_csv(R.data, "dat1.csv")

### sample Rt data
#N.sample <- 1000
#R.data1 <- R.data1[rep(1:nrow(R.data1), each = N.sample),]
#R.data1$sample.id <- rep(1:N.sample, nrow(R.data1))

set.seed(17725)
R.data1$R.1 <- mapply(runif, n = 1,min = R.data1$ci.90_lower, max = R.data1$ci.60_lower)
set.seed(17725)
R.data1$R.2 <- mapply(runif, n = 1,min = R.data1$ci.60_lower, max = R.data1$ci.30_lower)
set.seed(17725)
R.data1$R.3 <- mapply(runif, n = 1,min = R.data1$ci.30_lower, max = R.data1$ci.30_upper)
set.seed(17725)
R.data1$R.4 <- mapply(runif, n = 1,min = R.data1$ci.30_upper, max = R.data1$ci.60_upper)
set.seed(17725)
R.data1$R.5 <- mapply(runif, n = 1,min = R.data1$ci.60_upper, max = R.data1$ci.90_upper)

set.seed(17725)
R.data1$sample.R <- mapply(reSample5, A = R.data1$R.1, B = R.data1$R.2, C = R.data1$R.3, D = R.data1$R.4, E = R.data1$R.5)

## job::job({R.data1 <- read_csv("sampled_Rt.csv")})
rt_dat <- R.data1 %>%  # type 1: sample 1 Rt from dataset
  dplyr::select(date, Imperial, sample.R) %>% 
  group_by(date, Imperial) %>% 
  sample_n(., 1) %>% 
  ungroup()

Rt <- R.data1 %>% 
  dplyr::select(date, Imperial, sample.R)
rt_dat2 <- do.call(rbind, by(Rt, Rt[c("date", "Imperial")], FUN = CI.cal)) # type 2: calculate mean Rt from 1000 sample
rt_dat <- rt_dat2 %>% 
  full_join(., getRt) %>% 
  dplyr::select(LTLA_name, Date = date, lci = lower, rt = est, uci = upper) %>% 
  mutate(lci = as.numeric(lci), rt = as.numeric(rt), uci = as.numeric(uci))


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
         Rt_ci = paste0(Rt_m, "(", Rt_p25, "-", Rt_p75, ")"),
         GDP_ci = paste0(GDP_m, " (", GDP_p25, "-", GDP_p75, ")"),
         PD_ci = paste0(PD_m, " (", PD_p25, "-", PD_p75, ")"),
         prop_ci = paste0(Prop_m, " (", Prop_p25, "-", Prop_p75, ")")) %>% 
  dplyr::select(any_hall, n.LTLA, contains(c("ci"))) %>% 
  t()
# write.csv(LTLA_dat_compare, "outputs/table/Table1.csv")


# 2.  loction distribution ------------------------------------------------

## 解决Mac画地图的一个兼容性方法：使用 X11(type = "cairo") 搭配进行绘图结果的查看. 导出时使用quartz配合 dev.off() 使用以上的方法，grom_polygon或geom_sf速读均很快

X11(type = "cairo") # mac画图巨快！！！！

Eng_shp <- st_read(dsn = "LAT_shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp")
crs_use <- st_crs(Eng_shp) # 得到shp文件中的crs
Eng_shp <- st_transform(Eng_shp, crs = 4326) ## sf画图的大坑!!!如果不转换为4326投影(或统一的投影方式), 则无法正确裁切出伦敦


### add information to map data
map_dat <- Eng_shp %>% 
  full_join(., hall_num, by = c("LAD19NM" = "LTLA_name")) %>% 
  mutate(hall_num = replace_na(hall_num, 0),
         hall_num = hall_num+1)
map_dat <- st_transform(map_dat, crs = 4326) 

### Hall location
hall_sf <- st_as_sf(Hall_LTLA_dat, coords = c('Hall_Long', 'Hall_Lat'), crs = 4326) %>%  # 将hall转化为坐标, crs是coordinate reference system，是参考坐标系，如果设置错误，则地图的投影会错误. 一般crs设置为4326，即WGS84不会出错, 属于全球通用, 另外需要注意，此处crs的数值和下一步transform时的crs不一样
  st_transform(crs = crs_use) # 这一语句可选，如果不裁切地图则需要添加这一句, 如果要裁切出伦敦的范围, 则截止于上一句转换为4326体系即可

### plot England map ---
low_col <- colorRampPalette(c("#F6F6F6FF", "#E0B040FF"))
high_col <- colorRampPalette(c("#E0B040FF", "#C87810FF"))

x11.save(
  ggplot() +
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
          legend.position = c(0.15,0.23),
          legend.title.align = 0.5,
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 10),
          legend.box.just = "center"), 
  file = "outputs/England.pdf", width = 8, height = 10)


### plot London map (inside M25) ---
crop_factor <- st_bbox(c(xmin = -0.545479, xmax = 0.292020, ymax = 51.719041, ymin = 51.254188), crs = 4326)
map_dat_crop <- st_crop(map_dat, crop_factor)
hall_sf_crop <- st_crop(hall_sf, crop_factor)

x11.save(
    ggplot(map_dat_crop) +
      geom_sf(data = map_dat_crop, aes(fill = hall_num), size = 0.07) +
      scale_fill_gradientn(name = "Number of halls", colors=c(low_col(10), high_col(50)), 
                           breaks = c(1, 20, 40, 60), labels = c("0", "20", "40", "60")) +
      geom_sf(data = hall_sf_crop, color = "#0f2f78", size = 0.3, alpha = 0.8) +
      geom_sf_text(data = map_dat_crop, aes(label = LAD19NM), alpha = 0.6) +
      #geom_label(data = map_dat_crop, aes(x = LONG, y = LAT,label = LAD19NM), size = 5) + # 另一种标记地名方法
      #coord_sf(xlim = c(-0.545479,0.292020), ylim = c(51.254188,51.719041), expand = F, crs = 4326) + # 另一种裁切方式，不如直接在数据层面裁切渲染速读快
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            text = element_text(size = 16),
            #axis.text = element_blank(), 
            #axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            legend.position = "none"), 
    file = "outputs/London.pdf", width = 8, height = 8)

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


### Propensity Score Matching ----
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


### Fit causal impact ----

### Do casual impact ---
{
  #cas_dat <- prep.Causal(data = LTLA_case_dat, case = 1, control = c(249)) #case = 349, control = c(331,121)
  #cas_dat <- cas_dat %>% dplyr::select(!Date)
  
  #impact <- do.Causal(data = cas_dat, 
  #                    start_date = "2020-06-01", 
  #                    intervention_date = "2020-09-14", 
  #                    end_date = "2020-12-06",
  #                    model.args = list(niter = 1000, nseasons = 7, season.duration = 1))  
  #impact2 <- do.Causal(data = cas_dat, 
  #                     intervention_date = "2020-09-14", 
  #                     model.args = list(niter = 1000, nseasons = 7, season.duration = 1)) # type 2, need dataset include date
} # test Causal code


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


### Manual match ----
m_mat <- LTLA_dat_combine %>% 
  full_join(., region_num) %>% 
  dplyr::select(!c("UTLA_CD", "UTLA_name", "LTLA_Lat", "LTLA_Long", "areaCode", "lci", "uci", "N_region", "rt")) %>% 
  mutate(type = if_else(hall_num>0, 1, 0)) 

case_data <- m_mat %>% filter(type == 1) %>% mutate(index1 = NA, index2 = NA, index3 = NA)
control_data <- m_mat %>% filter(type == 0)


for (i in 1:length(case_data$type)) {
  mat <- which(between(control_data$Pop_Den_km2, case_data$Pop_Den_km2[i]*0.9, case_data$Pop_Den_km2[i]*1.1) &
                 between(control_data$Prosperity, case_data$Prosperity[i]*0.9, case_data$Prosperity[i]*1.1) &
                 control_data$N_num == case_data$N_num[i])
  mat1 <- m_mat[mat, "LTLA_ID"] %>% unlist()
  if (length(mat1) > 0) {
    #set.seed(1)
    case_data$index1[i] <- as.numeric(sample(mat1, 3, replace = T)[1])
    #set.seed(1)
    case_data$index2[i] <- as.numeric(sample(mat1, 3, replace = T)[2])
    #set.seed(1)
    case_data$index3[i] <- as.numeric(sample(mat1, 3, replace = T)[3])
  } else{
    case_data$index1[i] <- 999
  }
}

con_name <- LTLA_dat_combine %>% 
  mutate(c.LTLA_ID = LTLA_ID, c.LTLA_name = LTLA_name) %>% 
  dplyr::select(c.LTLA_ID, c.LTLA_name)
  
  
matched <- case_data %>% 
  filter(index1 != 999) %>% 
  mutate(subclass = 1:length(LTLA_ID), c.type = 0, c.LTLA_ID = index1,
         k.LTLA_ID = LTLA_ID, k.LTLA_name = LTLA_name, k.type = type) %>% 
  left_join(., con_name) %>% 
  dplyr::select(subclass, k.LTLA_ID, k.LTLA_name, k.type, c.LTLA_ID, c.LTLA_name, c.type) %>% 
  as.data.frame() %>% 
  reshape(., direction = "long", 
          varying = list(LTLA_ID = c(2,5), name = c(3,6), type = c(4,7)),
          v.names = c("LTLA_ID", "LTLA_name", "type"),
          idvar = "subclass") %>% 
  dplyr::select(! time) %>% 
  arrange(subclass, desc(type))

m_name <- matched %>% 
  distinct(., subclass, .keep_all = T) %>% 
  transmute(matched = paste(subclass, LTLA_ID, LTLA_name, sep = "_")) %>%
  as.data.frame() %>% .$matched

matched <- setNames(split(matched, matched$subclass), m_name)


## Manual data CI ----
ci_m_base <- vector("list", length = length(matched))
job::job({
  for (i in 1:length(matched)) {
    ci_m_base[[i]] <- names(matched[i])
    ci_m_base[[i]] <- do.Causal(data = prep.Causal(data = LTLA_ma, 
                                                   case = get.info(data = matched[[i]], type = "case"), 
                                                   control = get.info(data = matched[[i]], type = "control")),
                                intervention_date = get.info(data = matched[[i]], open_data = HEI_dat,type = "open"),
                                show.fig = F,
                                get.table = T,
                                save.fig = T,
                                path = paste0("outputs/ci_m_base/", names(matched[i]), ".pdf"),
                                model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
  }
}, title = "Moving average case")


ci_m_base <- do.call(rbind, ci_m_base)
record<-names(matched)
ci_m_base <- cbind(record, ci_m_base)
write_csv(ci_m_base, "ci_result_m_5000iter.csv")

## Growth rate CI ----
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
i <- 2

## Manual data CI
ci_m_base_gr <- vector("list", length = length(mat_base))
job::job({
  for (i in 1:length(mat_base)) {
    ci_m_base_gr[[i]] <- names(mat_base[i])
    ci_m_base_gr[[i]] <- do.Causal(data = prep.Causal(data = LTLA_gr, 
                                                      case = get.info(data = mat_base[[i]], type = "case"), 
                                                      control = get.info(data = mat_base[[i]], type = "control")),
                                   intervention_date = get.info(data = mat_base[[i]], open_data = HEI_dat,type = "open"),
                                   show.fig = F,
                                   get.table = T,
                                   save.fig = T,
                                   path = paste0("outputs/ci_m_base_gr/", names(mat_base[i]), ".pdf"),
                                   model.args = list(niter = 5000, nseasons = 7, season.duration = 1))
  }
}, title = "base")
ci_m_base_gr <- do.call(rbind, ci_m_base_gr)
record<-names(matched)
ci_m_base_gr <- cbind(record, ci_m_base_gr)
write_csv(ci_m_base_gr, "ci_m_base_gr_5000iter.csv")


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
  ungroup()

summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 
summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4)) 


### 5.4 prepare spatial data and adjacent matrix
Eng_shp <- st_transform(Eng_shp, crs = 4326) 

Eng_shp_no_island <- Eng_shp %>% 
  filter(OBJECTID != 44 & OBJECTID != 50 & OBJECTID != 334 & OBJECTID != 341 & OBJECTID != 343 & OBJECTID != 361) # These locations are island, if do not remove these locations, the adjacent matrix will return an error
## !!!!! OBJECTID is not equal to LTLA_ID !!!!!

W_adj <- poly2nb(Eng_shp_no_island, row.names = Eng_shp_no_island$LAD19NM) # This step could be slow, about 7 mins in this case
W_mat <- nb2mat(W_adj, style = "B", zero.policy = T) # This step generate a 382*382 matrix


### 5.5 Construct bayesian spatio-temporal model

# default prior: normal prior N(0,100000) 
### ST.CARlinear Correlated linear time trends
job::job({
  set.seed(139)
  chain1_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain2_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain3_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain4_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
}, title = "ST.CARlinear")


### ST.CARanova Spatio-temporal main effects and an interaction
job::job({
  set.seed(186)
  chain1_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100, interaction = T)
  chain2_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100, interaction = T)
  chain3_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100, interaction = T)
  chain4_anova <- ST.CARanova(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100, interaction = T)
}, title = "ST.CARanova")

### ST.CARar Spatially autocorrelated first-order autoregressive process
job::job({
  set.seed(1995)
  chain1_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
  chain2_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
}, title = "ST.CARar1-2")

job::job({
  set.seed(922)
  chain3_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
  chain4_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
}, title = "ST.CARar3-4")
### n.effect means effective number of independent samples; Geweke.diag is another MCMC convergence diagnostic that should lie between -2 and 2 to indicate convergence.

#### Check convergence - traceplot
beta.sample <- mcmc.list(chain_test$samples$beta, chain2$samples$beta)
plot(beta.sample)
gelman.diag(beta.sample) #### Check convergence - Gelman-Rubin plot less than 1.1indicate good mixing of the chain

#### Effects of covariates on disease risk
beta.samples.combined <- rbind(chain_test$samples$beta, chain2$samples$beta)
round(quantile(exp(sd(HEI_dec0.6$GDP_pc) * beta.samples.combined[ ,2]), c(0.5, 0.025, 0.975)),3)
round(quantile(exp(sd(HEI_dec0.6$Pop_Den_km2) * beta.samples.combined[ ,3]), c(0.5, 0.025, 0.975)),3)
round(quantile(exp(sd(HEI_dec0.6$Prosperity) * beta.samples.combined[ ,4]), c(0.5, 0.025, 0.975)),3)
round(quantile(exp(sd(HEI_dec0.6$h_index) * beta.samples.combined[ ,5]), c(0.5, 0.025, 0.975)),3)
round(quantile(exp(sd(HEI_dec0.6$gmc_retail) * beta.samples.combined[ ,6]), c(0.5, 0.025, 0.975)),3)
### Here we use the standard deviation of each covariate as the increase ξ, because they represent realistic increases in each covariates value.



#### Compute the risk distributions
fitted.samples.combined <- rbind(chain_test$samples$fitted, chain2$samples$fitted)
n.samples <- nrow(fitted.samples.combined)
n.all <- ncol(fitted.samples.combined)
risk.samples.combined <- fitted.samples.combined / matrix(rep(HEI_dec0.6$log_GR, n.samples), nrow=n.samples, ncol=n.all, byrow=TRUE) 


#### Compute the areal unit average risk for each year
N <- length(table(HEI_dec0.6$date))
risk.trends <- array(NA, c(n.samples, N))
for(i in 1:n.samples)
{
  risk.trends[i, ] <- tapply(risk.samples.combined[i, ], HEI_dec0.6$date, mean)
}


#### Plot the average risk trends
time.trends <- as.data.frame(t(apply(risk.trends, 2, quantile, c(0.5, 0.025, 0.975))))
time.trends <- time.trends %>% mutate(date=names(table(HEI_dec0.6$date)))
colnames(time.trends)[1:3] <- c("Median","LCI", "UCI")

ggplot(time.trends, aes(x = factor(date), y = Median, group=1)) +
  geom_line(col="red") + 
  geom_line(aes(x=factor(date), y=LCI), col="red", lty=2) +
  geom_line(aes(x=factor(date), y=UCI), col="red", lty=2) + 
  scale_x_discrete(name = "Year", breaks=c(2002, 2005, 2008, 2011, 2014, 2017), labels=c("2002", "2005", "2008", "2011", "2014", "2017")) +
  scale_y_continuous(name = "Risk") + 
  theme(text=element_text(size=16), plot.title=element_text(size=18, face="bold")) 


#### Spatial pattern in disease risk in the last year - mean and PEP
risk.samples.2010 <- risk.samples.combined[ ,HEI_dec0.6$date==as.Date("2020-09-17")]
risk.2010 <- apply(risk.samples.2010, 2, median)
pep.2010 <- apply(risk.samples.2010 > 1, 2, mean)


#### Map the results
residuals2010.LA$risk.2010 <- summary(risk.2010)
residuals2010.LA$pep.2010 <- pep.2010
residuals2010.LA.ll <- spTransform(residuals2010.LA, CRS("+proj=longlat +datum=WGS84 +no_defs"))

Eng_shp_no_island$risk.2010 <- risk.2010
residuals2010.LA.ll <- Eng_shp_no_island
  spTransform(Eng_shp_no_island, crs=4326)

  Eng_shp_no_island

colours <- colorNumeric(palette = "YlOrBr", domain = residuals2010.LA.ll@data$risk.2010, reverse=FALSE)
leaflet(data=residuals2010.LA.ll) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~colours(risk.2010), 
              color="",
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>%
  addLegend(pal = colours, values = risk.2010, 
            opacity = 1, title="Risk") %>%
  addScaleBar(position="bottomleft")

ggplot(Eng_shp_no_island) +
  geom_sf(data = Eng_shp_no_island, aes(fill = risk.2010), size = 0.07) +
  scale_fill_gradientn(name = "Log growth rate", colors=c(low_col(10), high_col(50)), 
                       breaks = c(0, 1, 2, 3), labels = c("0", "1", "2", "3")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(), 
        text = element_text(size = 16),
        axis.title = element_blank())

colours <- colorNumeric(palette = "YlOrBr", domain = residuals2010.LA.ll@data$pep.2010, reverse=FALSE)
leaflet(data=residuals2010.LA.ll) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~colours(pep.2010), 
              color="",
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>%
  addLegend(pal = colours, values = pep.2010, 
            opacity = 1, title="PEP") %>%
  addScaleBar(position="bottomleft")



#### Compute the median risk for each area
risk.median <- apply(risk.samples.combined, 2, median)
inequality <- tapply(risk.median, dat.ordered$Year, IQR)
ggplot(data.frame(inequality, Year=names(inequality)), aes(x = factor(Year), y = inequality, group=1)) +
  geom_line(col="red") + 
  scale_x_discrete(name = "Year", breaks=c(2002, 2005, 2008, 2011, 2014, 2017), labels=c("2002", "2005", "2008", "2011", "2014", "2017")) +
  scale_y_continuous(name = "Inequality") + 
  theme(text=element_text(size=16), plot.title=element_text(size=18, face="bold")) 



### plot England map ---
low_col <- colorRampPalette(c("#F6F6F6FF", "#E0B040FF"))
high_col <- colorRampPalette(c("#E0B040FF", "#C87810FF"))

X11(type = "cairo") 

x11.save(
  ggplot() +
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
          legend.position = c(0.15,0.23),
          legend.title.align = 0.5,
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 10),
          legend.box.just = "center"), 
  file = "outputs/England.pdf", width = 8, height = 10)



