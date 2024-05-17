island_dat2 <- expand.grid(LTLA_name = island_name$LAD19NM, 
                           week = 23:49,
                           log_GR = 0, 
                           retail_w = 100,
                           retail_gr = 1)

gr_map <- HEI_dec0.4 %>% 
  mutate(week = isoweek(date)) %>% 
  arrange(LTLA_ID, week) %>% 
  group_by(LTLA_name, week) %>% 
  mutate(case_w = sum(case),
         retail_w = mean(gmc_retail2)) %>% 
  distinct(LTLA_name, week, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(lag_case = lag(case_w, n=1),
         growth_rate = case_w/lag_case*100,
         log_GR = log(case_w/lag_case), 
         log_GR = if_else(is.infinite(log_GR) & log_GR>0, 3, log_GR),
         log_GR = if_else(is.infinite(log_GR) & log_GR<0, -2.5, log_GR),
         growth_rate = if_else(growth_rate>=300, 300, growth_rate),
         lag_retail = lag(retail_w),
         retail_gr = retail_w/lag_retail,
         case_w2 = if_else(case_w>=501, 501, case_w)) %>% 
  dplyr::select(contains("LTLA"), week, log_GR, retail_w, retail_gr, case_w2) %>% 
  bind_rows(., island_dat2) 


gr_map <- Eng_shp %>% 
  full_join(., gr_map, by = c("LAD19NM"="LTLA_name")) %>% 
  st_transform(., crs = 4326)

#x11(type="cairo")

ggsave(
  gr_map %>% filter(week %in%  c(35:44)) %>%  # 
    ggplot() +
    geom_sf(aes(fill = log_GR), size = 0.02) +
    scale_fill_gradientn(name = "Log growth rate", 
                         colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF", "#115b80","#082243"), 
                         values = c(1,0.9,0.8, 0.7,0.56,0.43,0.22,0.11,0), breaks = seq(-2,3, by=1),
                         labels = c("-2", "-1", "0", "1", "2", "3")) +
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
  filename = "outputs/log_gr35_44.pdf", width = 14, height = 12)


# ggsave(
#   gr_map %>% filter(week %in%  c(35:44)) %>%  # 
#     ggplot() +
#     geom_sf(aes(fill = retail_w), size = 0.02) +
#     scale_fill_gradientn(name = "Retail and recreation\npercent change", 
#                          colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF", "#115b80","#082243"), 
#                          values = c(1,0.9,0.8, 0.7,0.58,0.38,0.28,0.11,0)) +#  , breaks = seq(-2,3, by=1),labels = c("-2", "-1", "0", "1", "2", "3")
#     coord_sf() +
#     theme_minimal() +
#     facet_wrap(~week, ncol = 5) + 
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(), 
#           axis.text = element_blank(), 
#           axis.ticks = element_blank(), 
#           axis.title = element_blank(), 
#           legend.title.align = 0.5,
#           legend.title = element_text(face = "bold", size = 14),
#           legend.text = element_text(size = 12),
#           legend.box.just = "center",
#           strip.text = element_text(size = 18)), 
#   filename = "outputs/retail35_44.pdf", width = 14, height = 12)
# 
# 
# ggsave(
#   gr_map %>% filter(week %in%  c(35:44)) %>%  # 
#     ggplot() +
#     geom_sf(aes(fill = retail_gr), size = 0.02) +
#     scale_fill_gradientn(name = "Retail and recreation\npercent change", 
#                          colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF", "#115b80","#082243"), 
#                          values = c(1,0.95,0.88,0.79,0.71, 0.55,0.28,0)) +#  , breaks = seq(-2,3, by=1),labels = c("-2", "-1", "0", "1", "2", "3")
#     coord_sf() +
#     theme_minimal() +
#     facet_wrap(~week, ncol = 5) + 
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(), 
#           axis.text = element_blank(), 
#           axis.ticks = element_blank(), 
#           axis.title = element_blank(), 
#           legend.title.align = 0.5,
#           legend.title = element_text(face = "bold", size = 14),
#           legend.text = element_text(size = 12),
#           legend.box.just = "center",
#           strip.text = element_text(size = 18)), 
#   filename = "outputs/retailgr35_44.pdf", width = 14, height = 12)


# gr_map %>% filter(week %in%  c(35:44)) %>%  # 
#   ggplot() +
#   geom_sf(aes(fill = case_w2), size = 0.02) +
#   scale_fill_gradientn(name = "Confirmed cases", 
#                        colours = c("#662506FF","#CC4C02FF","#FB9A29FF","#FAEFD1FF", "#F6F6F6FF"), 
#                        values = c(1, 0.7,0.5,0.3,0.22,0.11,0), breaks = seq(0,500, by=100)) +
#   coord_sf() +
#   theme_minimal() +
#   facet_wrap(~week, ncol = 5) + 
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(), 
#         axis.text = element_blank(), 
#         axis.ticks = element_blank(), 
#         axis.title = element_blank(), 
#         legend.title.align = 0.5,
#         legend.title = element_text(face = "bold", size = 14),
#         legend.text = element_text(size = 12),
#         legend.box.just = "center",
#         strip.text = element_text(size = 18))

