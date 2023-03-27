library(animation)

island_dat_day <- expand.grid(LTLA_name = island_name$LAD19NM, 
                          date = unique(HEI_risk$date),
                          acc_risk = 1, 
                          cum = 1)

HEI_risk4map_day <- HEI_risk %>% 
  mutate(week = isoweek(date)) %>% 
  group_by(LTLA_name) %>% 
  arrange(LTLA_ID) %>% 
  bind_cols(., cum = exp(unlist(tapply(eff_all_ci[,1], HEI_dec0.4$LTLA_ID, cumsum)))) %>% 
  mutate(acc_risk = exp(`50%`)) %>% 
  filter(week %in% 35:44) %>% 
  dplyr::select(LTLA_name, date, acc_risk, cum) %>%   # , `2.5%`, `97.5%`
  ungroup() %>% 
  bind_rows(., island_dat_day)


risk_map_day <- Eng_shp %>% 
  full_join(., HEI_risk4map_day, by = c("LAD19NM"="LTLA_name")) %>% 
  st_transform(., crs = 4326) %>% 
  filter(date != is.na(date)) %>% 
  mutate(date = as.Date(date))

risk_map_day <- risk_map_day %>% filter(date >= as.Date("2020-08-24") & date <= as.Date("2020-11-01"))


date_lab <- unique(risk_map_day$date)
crop_factor <- st_bbox(c(xmin = -0.545479, xmax = 0.292020, ymax = 51.719041, ymin = 51.254188), crs = 4326)
map_crop <- st_crop(risk_map_day, crop_factor)
map_crop <- map_crop %>% filter(date >= as.Date("2020-08-24") & date <= as.Date("2020-11-01"))


saveGIF({
  for (i in date_lab) {
    g1 <- risk_map_day %>% filter(date == i) %>%  
      ggplot() +
      geom_sf(aes(fill = acc_risk), size = 0.05) +
      scale_fill_gradientn(name = "Effect of HEI(s) reopen", 
                           colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey80"),
                           values = c(1,0.8,0.6,0.4,0.2,0.1,0.0001,0), 
                           limits = c(1,1.2), 
                           breaks = c(1.00, 1.05, 1.10, 1.15, 1.20)) + 
      coord_sf() +
      theme_minimal() +
      labs(title = paste0("Date: ", as.Date(i))) + 
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            title = element_text(face = "bold", size = 38),
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            legend.position = c(0.2, 0.2),
            legend.title.align = 0.5,
            legend.title = element_text(face = "bold", size = 34),
            legend.text = element_text(size = 32),
            legend.box.just = "center",
            legend.key.size = unit(2, 'cm'))
    
    g1s <- map_crop %>% filter(date == i) %>% 
      ggplot() +
      geom_sf(aes(fill = acc_risk), size = 0.05) +
      scale_fill_gradientn(name = "Effect of HEI(s) reopen", 
                           colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey80"),
                           values = c(1,0.8,0.6,0.4,0.2,0.1,0.0001,0), 
                           limits = c(1,1.2), 
                           breaks = c(1.00, 1.05, 1.10, 1.15, 1.20)) +
      theme_minimal() +
      labs(title = "London") +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            plot.title = element_text(face = "bold", size = 34, hjust = 0.2, vjust = 0),
            legend.position = "none")
    
    g1c <- g1 + g1s + plot_layout(design = c(patchwork::area(t = 1, l = 1, b = 9, r = 7), patchwork::area(t = 2, l = 6, b = 4.5, r = 9)))
    print(g1c)
  }
}, movie.name = "daily.gif", interval = 0.1, ani.width=1440, ani.height=1920)



saveGIF({
  for (i in date_lab) {
    g2 <- risk_map_day %>% filter(date == i) %>%  
      ggplot() +
      geom_sf(aes(fill = cum), size = 0.05) +
      scale_fill_gradientn(name = "Cumulative effect of\nHEI(s) reopen", 
                           colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey80"),
                           values = c(1,0.8,0.6,0.4,0.2,0.1,0.0001,0), 
                           limits = c(1,2), 
                           breaks = c(1.00, 1.20, 1.40, 1.60, 1.80, 2.00)) + 
      coord_sf() +
      theme_minimal() +
      labs(title = paste0("Date: ", as.Date(i))) + 
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            title = element_text(face = "bold", size = 38),
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            legend.position = c(0.2, 0.2),
            legend.title.align = 0.5,
            legend.title = element_text(face = "bold", size = 34),
            legend.text = element_text(size = 32),
            legend.box.just = "center",
            legend.key.size = unit(2, 'cm'))
    
    g2s <- map_crop %>% filter(date == i) %>% 
      ggplot() +
      geom_sf(aes(fill = cum), size = 0.05) +
      scale_fill_gradientn(name = "Cumulative effect of HEI(s) reopen", 
                           colours = c("#662506FF","#993404FF", "#CC4C02FF", "#EC7014FF", "#FB9A29FF","#FEC44FFF","#FAEFD1FF","#FFF7BCFF", "#FFFFE5FF", "grey80"),
                           values = c(1,0.8,0.6,0.4,0.2,0.1,0.0001,0), 
                           limits = c(1,2), 
                           breaks = c(1.00, 1.20, 1.40, 1.60, 1.80, 2.00)) +
      theme_minimal() +
      labs(title = "London") +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            plot.title = element_text(face = "bold", size = 34, hjust = 0.2, vjust = 0),
            legend.position = "none")
    
    g2c <- g2 + g2s + plot_layout(design = c(patchwork::area(t = 1, l = 1, b = 9, r = 7), patchwork::area(t = 2, l = 6, b = 4.5, r = 9)))
    print(g2c)
  }
}, movie.name = "daily_cum.gif", interval = 0.1, ani.width=1440, ani.height=1920)



