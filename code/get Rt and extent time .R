###########################################
##  This script is used to extent the range of study  and get Rt from a database
###########################################


# 0. data import ---------------------------------------------------------

### tools ---
{
  Eng_ltla_case <- read_csv("data/raw/Eng_ltla_case.csv")
  Eng_case <- Eng_ltla_case %>% 
  mutate(date = as.Date(date)) %>% 
    filter(date >= "2020-06-01" & date <= "2020-12-06") %>% 
    left_join(., LTLA_case_dat, by = c("areaName" = "LTLA_name")) %>% 
    dplyr::select(areaCode.x, LTLA_ID, areaName, date, newCasesBySpecimenDate.x) %>% 
    group_by(areaName, date) %>% 
    distinct(areaName, date, .keep_all = T) %>% 
    rename(areaCode = areaCode.x, CLTLA_name = areaName, newCasesBySpecimenDate = newCasesBySpecimenDate.x)
  write_csv(Eng_case, "Eng_case.csv")
} # expand data

{
  # prepare LTLA code, otherwise the memorary will exhaust
  LTLA_code <- LTLA_case_dat %>% 
   dplyr::select(LTLA_ID, LTLA_name) %>% 
    distinct(., .keep_all = T) %>% 
    rename(areaName = LTLA_name)
  
  age_dat <- read_csv("ltla_age_demo.csv")
  
  Eng_age_dat <- age_dat %>% 
   mutate(date = as.Date(date)) %>% 
    filter(date >= "2020-07-27" & date <= "2020-12-06") %>% 
   full_join(., LTLA_code) %>% 
  dplyr::select(areaCode, LTLA_ID, areaName, date, age, cases, rollingSum, rollingRate)
  
  write_csv(Eng_age_dat, "Eng_age_dat.csv")
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
R.data1$R.2 <- mapply(runif, n = 1,min = R.data1$ci.60_lower, max = R.data1$ci.30_lower)
R.data1$R.3 <- mapply(runif, n = 1,min = R.data1$ci.30_lower, max = R.data1$ci.30_upper)
R.data1$R.4 <- mapply(runif, n = 1,min = R.data1$ci.30_upper, max = R.data1$ci.60_upper)
R.data1$R.5 <- mapply(runif, n = 1,min = R.data1$ci.60_upper, max = R.data1$ci.90_upper)

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

