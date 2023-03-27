### This scrip is used to plot the distribution of 12 regions

map_dat_region <- Eng_shp %>% 
  full_join(., LTLA_dat_combine %>% dplyr::select("LTLA_name", "N_region") %>% 
              bind_rows(., data.frame(LTLA_name = c("Cornwall", "Isles of Scilly", "Hackney", "City of London"), N_region = c("South West", "South West", "London", "London"))), by = c("LAD19NM" = "LTLA_name")) %>% 
  st_transform(., crs = 4326) 


ggsave(ggplot() +
         geom_sf(data = map_dat_region, aes(fill = N_region), size = 0.05) +
         coord_sf() +
         labs(fill = "Region name") +
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
       filename = "outputs/region_name.pdf", width = 9, height = 14)