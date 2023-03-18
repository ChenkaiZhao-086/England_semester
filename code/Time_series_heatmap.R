# 3.  Time series heatmap -------------------------------------------------
### prepare dataset for Fig.heatmap ---
rt_dat2 <- rt_dat %>% 
  mutate(Date = as.Date(Date),
         week_num = isoweek(Date)) %>% 
  group_by(LTLA_name, week_num) %>% 
  summarise(LTLA_name = LTLA_name, 
            lci = mean(lci),
            rt = mean(rt),
            uci = mean(uci)) %>% 
  distinct(., .keep_all = T)
# 计算这一周的Rt以及对应上下限的均值

ts_dat2 <- LTLA_case_dat %>% 
  mutate(Date = date,
         Date = as.Date(Date),
         week_num = isoweek(Date)) %>% # ISO 8601 week start at Monday
  group_by(LTLA_ID, week_num) %>% 
  summarise(week_case = if_else(sum(newCasesBySpecimenDate)==0, 0.5, sum(newCasesBySpecimenDate)), # calculate case number per week per LTLA. If sum cases=0, replace by 0.5
            LTLA_name = LTLA_name) %>%  
  distinct() %>%  # a totle of 382 LTLA. However, there were 380 LTLA here only
  ungroup() %>% 
  group_by(LTLA_ID) %>% 
  mutate(diff = lag(week_case, n = 1),
         growth_rate = week_case/diff*100,
         log_GR = log(week_case/diff)) %>% 
  full_join(., rt_dat2) %>% 
  mutate(rt_dif = lag(rt, n = 1),
         rt_GR = rt/rt_dif*100) %>% 
  full_join(., combine1) %>% 
  filter(week_num >= 30) %>% 
  mutate(growth_rate = if_else(growth_rate>=300, 300, growth_rate)) %>% 
  arrange(desc(hall_num)) %>% 
  drop_na()


### plot for growth rate/link relative --- 
# min(ts_dat2$log_GR) # -2.772589
# max(ts_dat2$log_GR) # 3.73767
ggsave(
  ggplot(ts_dat2)+
    geom_tile(aes(x = week_num, y = fct_reorder(LTLA_name, hall_num), fill=log_GR), color= "grey70",linewidth = 0.02) + 
    scale_fill_gradientn(name = "Log growth rate", 
                         colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF", "#115b80","#082243"), 
                         values = c(1,0.9,0.8, 0.75,0.55,0.40,0.30,0.2,0), breaks = seq(-3,4, by=1),
                         labels = c("-3", "-2", "-1", "0", "1", "2", "3", "4")) +
    # scale_fill_gradientn(name = "Growth rate", colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF","#082243"),
    #                      values = c(1,0.9,0.8, 0.75,0.55,0.35,0.24,0), breaks = seq(0,300, by=50), 
    #                      labels = c("0", "50", "100", "150", "200", "250", "300")) +
    scale_x_continuous(name = "Weeks", breaks = c(30:49), labels = paste0("W", 30:49), expand = c(0,0)) +
    scale_y_discrete(name = "Lower Tier Local Authorities") +
    theme_minimal() + 
    theme(axis.text.y = element_text(size = 4, colour = "black"),
          axis.text.x = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 24, colour = "black"),
          legend.title = element_text(size=16), 
          legend.text = element_text(size=14)),
  filename = "outputs/log_growth_rate.pdf", width = 18, height = 20) # growth_rate.pdf


### plot for Rt ---
ggsave(
  ggplot(ts_dat2)+
    geom_tile(aes(x = week_num, y = fct_reorder(LTLA_name, hall_num), fill=rt), color= "grey70",linewidth = 0.02) + 
    scale_fill_gradientn(name = expression("R"[t]), 
                         colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF","#082243"),
                         values = c(1.0,0.6,0.5,0.3,0.12,0)) +
    scale_x_continuous(name = "Weeks", breaks = c(30:49), labels = paste0("W", 30:49), expand = c(0,0)) +
    scale_y_discrete(name = "Lower Tier Local Authorities") +
    theme_minimal() + # base font size
    theme(axis.text.y = element_text(size = 4, colour = "black"),
          axis.text.x = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 24, colour = "black"),
          legend.title = element_text(size=16), 
          legend.text = element_text(size=14)),
  filename = "outputs/rt.pdf", width = 18, height = 20)
#  limits = c(-50,200), limits = c(2,8),
#dev.off()