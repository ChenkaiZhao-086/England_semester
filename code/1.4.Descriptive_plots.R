# Draw descriptive maps and daily figures from the saved original geography.
old <- read.analysis(original_prepared)
Eng_shp <- old$Eng_shp
hall_num <- old$hall_num
Hall_LTLA_dat <- old$Hall_LTLA_dat

### add information to map data
map_dat <- Eng_shp %>%
  full_join(., hall_num, by = c("LAD19NM" = "LTLA_name")) %>%
  mutate(hall_num = replace_na(hall_num, 0)) %>%
  st_transform(., crs = 4326)

### Hall location
hall_sf <- st_as_sf(
  Hall_LTLA_dat,
  coords = c("Hall_Long", "Hall_Lat"),
  crs = 4326
)

### plot England map ---
low_col <- colorRampPalette(c("#F6F6F6FF", "#E0B040FF"))
high_col <- colorRampPalette(c("#E0B040FF", "#C87810FF"))

ggsave(
  ggplot() +
    geom_sf(data = map_dat, aes(fill = hall_num), size = 0.05) +
    scale_fill_gradientn(
      name = "Number of halls",
      colors = c(low_col(10), high_col(50)),
      breaks = c(0, 20, 40, 60),
      labels = c("0", "20", "40", "60")
    ) +
    geom_sf(data = hall_sf, color = "#0f2f78", size = 0.3, alpha = 0.8) +
    coord_sf() +
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
  filename = file.path(plot_dir, "Fig2a_uk.pdf"),
  width = 9,
  height = 14
)


### plot London map (inside M25) ---
crop_factor <- st_bbox(
  c(xmin = -0.545479, xmax = 0.292020, ymax = 51.719041, ymin = 51.254188),
  crs = 4326
)
map_dat_crop <- st_crop(map_dat, crop_factor)
hall_sf_crop <- st_crop(hall_sf, crop_factor)

ggsave(
  ggplot(map_dat_crop) +
    geom_sf(data = map_dat_crop, aes(fill = hall_num), size = 0.07) +
    scale_fill_gradientn(
      name = "Number of halls",
      colors = c(low_col(10), high_col(50)),
      breaks = c(0, 20, 40, 60),
      labels = c("0", "20", "40", "60")
    ) +
    geom_sf(data = hall_sf_crop, color = "#0f2f78", size = 0.3, alpha = 0.8) +
    geom_sf_text(
      data = map_dat_crop %>%
        filter(
          LAD19NM %in%
            old$LTLA_dat$LTLA_name[old$LTLA_dat$N_region == "London"]
        ),
      aes(label = LAD19NM),
      size = 2.6,
      check_overlap = TRUE,
      # Input/output: sf geometry; locate label points in metres, then return lon/lat.
      fun.geometry = function(x) {
        st_transform(st_point_on_surface(st_transform(x, 27700)), 4326)
      }
    ) +
    # geom_label(data = map_dat_crop, aes(x = LONG, y = LAT,label = LAD19NM), size = 5) + # 另一种标记地名方法
    # coord_sf(xlim = c(-0.545479,0.292020), ylim = c(51.254188,51.719041), expand = FALSE, crs = 4326) + # 另一种裁切方式，不如直接在数据层面裁切渲染速读快
    theme_minimal() +
    labs(
      title = "London",
      caption = "London borough labels shown where space permits."
    ) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(vjust = 1),
      axis.text.y = element_text(hjust = 0),
      axis.title = element_blank(),
      legend.position = "none"
    ),
  filename = file.path(plot_dir, "Fig2b_london.pdf"),
  width = 10,
  height = 10,
  device = "pdf"
)

# write.csv(map.metadata, file = "linkage/Map.csv")
# save(map.data, file = "cleaned/map.RData")

source("code/1.1.Fig2c.R", local = TRUE)

# 3. Time series heatmap --------------------------------------------------
# source("code/Time_series_heatmap.R")

### plot region distribution
source("code/1.2.plot_region.R", local = TRUE)

source("code/1.5.plot log GR and retail.R", local = TRUE)
