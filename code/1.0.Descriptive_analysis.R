# 1.  Basic character -----------------------------------------------------
### calculate hall number in each LTLA ---
hall_num <- Hall_LTLA_dat %>%
  reframe(hall_num = n(), .by = c(LTLA_ID, LTLA_name)) %>%
  arrange(desc(hall_num))


### calculate HEI number in each LTLA ---
hei_num <- HEI_cam_hall_dat %>%
  mutate(hei_cam = substr(Unique_ID, 1, 9)) %>%
  dplyr::select(Unique_ID, hei_cam, LTLA_ID, LTLA_name) %>%
  group_by(LTLA_ID) %>%
  distinct(hei_cam, .keep_all = TRUE) %>%
  reframe(
    hei_num = n(),
    LTLA_name = LTLA_name
  ) %>%
  distinct(LTLA_ID, .keep_all = TRUE) %>%
  drop_na() %>%
  arrange(desc(hei_num))

### match LTLA, UTLA and HEI/Hall number ---
H_C <- data.frame(
  LTLA_ID = c(48, 284),
  LTLA_CD = c("E06000052", "E09000001"),
  LTLA_name = c("Cornwall and Isles of Scilly", "Hackney and City of London"),
  UTLA_CD = c("E06000052", "E09000001"),
  UTLA_name = c("Cornwall and Isles of Scilly", "Hackney and City of London"),
  hall_num = c(7, 8),
  hei_num = c(2, 8)
)

combine1 <- reduce(list(hall_num, hei_num, ltla2utla), full_join) %>%
  mutate(
    hall_num = replace_na(hall_num, 0),
    hei_num = replace_na(hei_num, 0)
  ) %>%
  filter(!LTLA_ID %in% c(48, 49, 284, 295)) %>%
  rbind(., H_C)


# sum(combine1$hall_num, na.rm = TRUE) # check Hall number
# sum(combine1$hei_num, na.rm = TRUE) # check HEI number

Tab1_case <- LTLA_case_dat %>%
  reframe(
    case = sum(newCasesBySpecimenDate),
    .by = c(LTLA_ID, areaCode, LTLA_name)
  )

Tab1_rt <- rt_dat %>%
  reframe(
    lci = mean(lci),
    rt = mean(rt),
    uci = mean(uci),
    .by = LTLA_name
  )

LTLA_dat_combine <- reduce(
  list(LTLA_dat, combine1, Tab1_case, Tab1_rt),
  full_join
) %>%
  drop_na() # deleat Cornwall, Isles of Scilly, Hackney, City of London


Tab1_case_mean <- LTLA_case_dat %>%
  group_by(LTLA_ID, areaCode, LTLA_name) %>%
  filter(date > as.Date("2020-06-02") & date <= as.Date("2020-12-05")) %>%
  mutate(week = week(date)) %>%
  group_by(LTLA_ID, week) %>%
  mutate(week_case = sum(newCasesBySpecimenDate)) %>%
  ungroup() %>%
  reframe(mean = median(week_case), .by = c(LTLA_ID, LTLA_name))


LTLA_dat_compare <- LTLA_dat_combine %>%
  left_join(., Tab1_case_mean) %>%
  mutate(
    any_hall = if_else(hall_num > 0, "LTLA with hall(s)", "LTLA without hall"), # nolint: line_length_linter.
    Prosperity = as.numeric(Prosperity)
  ) %>%
  group_by(any_hall) %>%
  summarise(
    n.LTLA = as.character(n()),
    across(c(ends_with("num")), sum, .names = "n.{col}"),
    across(
      c(mean, GDP_pc, Pop_Den_km2, Prosperity),
      function(x) {
        round(median(x, na.rm = TRUE), 2)
      },
      .names = "{col}_m"
    ),
    across(
      c(mean, GDP_pc, Pop_Den_km2, Prosperity),
      function(x) {
        round(quantile(x, 0.25, na.rm = TRUE), 2)
      },
      .names = "{col}_p25"
    ),
    across(
      c(mean, GDP_pc, Pop_Den_km2, Prosperity),
      function(x) {
        round(quantile(x, 0.75, na.rm = TRUE), 2)
      },
      .names = "{col}_p75"
    )
  ) %>%
  mutate(
    Case_ci = paste0(mean_m, " (", mean_p25, ", ", mean_p75, ")"),
    GDP_ci = paste0(GDP_pc_m, " (", GDP_pc_p25, ", ", GDP_pc_p75, ")"),
    PD_ci = paste0(
      Pop_Den_km2_m,
      " (",
      Pop_Den_km2_p25,
      ", ",
      Pop_Den_km2_p75,
      ")"
    ),
    prop_ci = paste0(
      Prosperity_m,
      " (",
      Prosperity_p25,
      ", ",
      Prosperity_p75,
      ")"
    )
  ) %>%
  dplyr::select(any_hall, n.LTLA, contains(c("ci"))) %>%
  set_names(
    "",
    "",
    "Median Number of confirmed cases per week per LTLA",
    "GDP",
    "Population density",
    "Prosperity score"
  ) %>%
  t()
write.csv(LTLA_dat_compare, "outputs/table/Table1.csv")


# 2.  loction distribution ------------------------------------------------
Eng_shp <- st_read(
  dsn = "LAT_shp/Local_Authority_Districts__December_2019__Boundaries_UK_BFE.shp"
)
# crs_use <- st_crs(Eng_shp) # 得到shp文件中的crs (coordinate reference system)，是参考坐标系，如果设置错误，则地图的投影会错误. 一般crs设置为4326，即WGS84不会出错, 属于全球通用, 也可以设置为专有的

### add information to map data
map_dat <- Eng_shp %>%
  full_join(., hall_num, by = c("LAD19NM" = "LTLA_name")) %>%
  mutate(
    hall_num = replace_na(hall_num, 0),
    hall_num = hall_num + 1
  ) %>%
  st_transform(., crs = 4326) ## sf画图的大坑!!!如果不转换为4326投影(或统一的投影方式), 则无法正确裁切出伦敦

### Hall location
hall_sf <- st_as_sf(
  Hall_LTLA_dat,
  coords = c("Hall_Long", "Hall_Lat"),
  crs = 4326
) # 将hall转化为坐标

### plot England map ---
low_col <- colorRampPalette(c("#F6F6F6FF", "#E0B040FF"))
high_col <- colorRampPalette(c("#E0B040FF", "#C87810FF"))

ggsave(
  ggplot() +
    geom_sf(data = map_dat, aes(fill = hall_num), size = 0.05) +
    scale_fill_gradientn(
      name = "Number of halls",
      colors = c(low_col(10), high_col(50)),
      breaks = c(1, 20, 40, 60),
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
  filename = "outputs/Fig2a_uk.pdf",
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
      breaks = c(1, 20, 40, 60),
      labels = c("0", "20", "40", "60")
    ) +
    geom_sf(data = hall_sf_crop, color = "#0f2f78", size = 0.3, alpha = 0.8) +
    geom_sf_text(data = map_dat_crop, aes(label = LAD19NM), alpha = 1) +
    # geom_label(data = map_dat_crop, aes(x = LONG, y = LAT,label = LAD19NM), size = 5) + # 另一种标记地名方法
    # coord_sf(xlim = c(-0.545479,0.292020), ylim = c(51.254188,51.719041), expand = FALSE, crs = 4326) + # 另一种裁切方式，不如直接在数据层面裁切渲染速读快
    theme_minimal() +
    labs(title = "London") +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(vjust = 1),
      axis.text.y = element_text(hjust = 0),
      axis.title = element_blank(),
      legend.position = "none"
    ),
  filename = "outputs/Fig2b_london.pdf",
  width = 10,
  height = 10,
  device = "pdf"
)

# write.csv(map.metadata, file = "linkage/Map.csv")
# save(map.data, file = "cleaned/map.RData")

source("code/1.1.Fig2c.R")

# 3. Time series heatmap --------------------------------------------------
# source("code/Time_series_heatmap.R")

### plot region distribution
source("code/1.2.plot_region.R")
