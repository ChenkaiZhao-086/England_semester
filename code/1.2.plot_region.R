### This scrip is used to plot the distribution of 12 regions

map_dat_region <- Eng_shp %>%
  left_join(
    .,
    old$LTLA_dat %>% distinct(LTLA_name, N_region),
    by = c("LAD19NM" = "LTLA_name")
  ) %>%
  st_transform(., crs = 4326)


ggsave(
  ggplot() +
    geom_sf(data = map_dat_region, aes(fill = N_region), size = 0.05) +
    coord_sf() +
    labs(fill = "Region name") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "right",
      legend.title.align = 0.1,
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10),
      legend.box.just = "center"
    ),
  filename = file.path(plot_dir, "appendix_region_name.pdf"),
  width = 9,
  height = 14
)
