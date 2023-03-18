
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
rt_dat <- read_csv("data/rt.dat.csv")

gmr <- read_csv("data/2020_GB_Region_Mobility_Report.csv") %>% 
  mutate(Google = if_else(is.na(sub_region_2) == T, paste0(sub_region_1, "_"), paste(sub_region_1, sub_region_2, sep = "_"))) %>% 
  dplyr::select(Google, date, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline, parks_percent_change_from_baseline, transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline, residential_percent_change_from_baseline)