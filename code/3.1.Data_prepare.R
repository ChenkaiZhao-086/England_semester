### 3.1 Prepare data for ST analysis, calculate H.index in each LTLA ---
date_list <- LTLA_case_dat %>% # This is the dataset include case number, Rt and LTLA information
  full_join(., LTLA_dat) %>%
  left_join(., gmr)

### 3.2 Prepare h index and Google mobility report data
HEI <- HEI_cam_dat %>%
  group_by(HEI_ID) %>%
  mutate(
    # n.hall = sum(Number_of_hall),
    # mcf = if_else(is.na(mcf), Number_of_hall / n.hall, mcf),
    # h.ref = 40485,
    h_index = Hall_index # (Hall_of_student / h.ref) * mcf #
  ) %>% # The university with most students and all halls located in one LTLA
  left_join(., LTLA_dat_combine, by = c("Campus_name" = "LTLA_name")) %>%
  group_by(Campus_name, HEI_opening) %>%
  mutate(
    LTLA_name = Campus_name, # add 2 column named LTLA_name and date
    date = HEI_opening
  ) %>%
  ungroup() %>%
  distinct(., LTLA_name, date, .keep_all = TRUE) %>%
  full_join(., date_list) %>% # combine date and other information like H-index and etc.
  arrange(LTLA_ID, date) %>%
  mutate(
    h_index = replace_na(h_index, 0),
    gmc_retail = na.locf(
      retail_and_recreation_percent_change_from_baseline,
      na.rm = FALSE
    ),
    gmc_grocery = na.locf(
      grocery_and_pharmacy_percent_change_from_baseline,
      na.rm = FALSE
    ),
    gmc_park = na.locf(parks_percent_change_from_baseline, na.rm = FALSE),
    gmc_park = if_else(is.na(gmc_park), 0, gmc_park),
    gmc_transit = na.locf(
      transit_stations_percent_change_from_baseline,
      na.rm = FALSE
    ),
    gmc_work = na.locf(workplaces_percent_change_from_baseline, na.rm = FALSE),
    gmc_res = na.locf(residential_percent_change_from_baseline, na.rm = FALSE)
  ) %>% # Use LOCF to replace NA in GMR_redail data
  dplyr::select(
    LTLA_ID,
    LTLA_name,
    date,
    h_index,
    GDP_pc,
    Pop_Den_km2,
    Prosperity,
    areaCode,
    gmc_retail,
    gmc_grocery,
    gmc_park,
    gmc_transit,
    gmc_work,
    gmc_res
  ) %>%
  drop_na()
# "HEI_ID", "HEI_name", "Campus_ID", "Economic_Quality", "Business_Environment", "Education", "Health", "Safe_Security", "Social_Capital", "Environment", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline"
# names(HEI)

### 3.3 prepare case date, including case, growth rate and log GR. Construct a basic model without sptial correlation

case_dat <- LTLA_case_dat %>%
  group_by(LTLA_ID) %>%
  arrange(LTLA_ID, date) %>%
  mutate(
    date = as.Date(date),
    case = if_else(newCasesBySpecimenDate == 0, 0.5, newCasesBySpecimenDate),
    case_ma = rollmean(case, 7, align = "right", fill = NA),
    diff = lag(case, n = 1),
    diff2 = lag(case_ma, n = 1),
    growth_rate = case / diff * 100,
    growth_rate2 = case_ma / diff2 * 100,
    log_GR = log(case / diff),
    log_GR2 = log(case_ma / diff2),
    case = as.integer(case)
  ) %>%
  dplyr::select(
    LTLA_ID,
    LTLA_name,
    date,
    case,
    growth_rate,
    log_GR,
    log_GR2
  ) %>%
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


decay0.4 <- gen.decay(
  decay.period = 30,
  decay.fun = "1*exp(-0.4*x)",
  incubation = 6
) # supposed decay fun type: y(t) = yf+(y0-yf)e^(-at) 0.6 OR 0.8

# get.index(dat = HEI, decay = decay0.4)

HEI_dec0.4 <- do.call(
  rbind,
  by(HEI_all, HEI_all$LTLA_ID, get.index, decay0.4)
) %>%
  group_by(LTLA_ID) %>%
  mutate(
    h_ma = rollmean(h_index, 7, align = "right", fill = NA),
    h_ma = if_else(h_ma < 1e-10, 0, h_ma),
    lag_h = lag(h_index, n = 1),
    lag_h_ma = lag(h_ma, n = 1),
    h_gr = h_index / lag_h,
    h_gr = if_else(is.nan(h_gr), 0, h_gr),
    h_gr = if_else(is.infinite(h_gr), exp(-0.6), h_gr),
    h_gr_ma = h_ma / lag_h_ma,
    h_gr_ma = if_else(is.nan(h_gr_ma), 0, h_gr_ma),
    h_gr_ma = if_else(is.infinite(h_gr_ma), 1 + exp(-0.6), h_gr_ma),
    gmc_ma = rollmean(gmc_retail, 7, align = "right", fill = NA),
    gmc_retail2_ma = gmc_ma + 100,
    lag_retail_ma = lag(gmc_retail2_ma, n = 1),
    retail_gr_ma = gmc_retail2_ma / lag_retail_ma,
    gmc_retail2 = gmc_retail + 100,
    lag_retail = lag(gmc_retail2, n = 1),
    retail_gr = gmc_retail2 / lag_retail,
    week = row_number(),
    weekend = if_else((wday(date) == 1 | wday(date) == 7), 1, 0)
  ) %>%
  filter(
    LTLA_ID != 43 &
      LTLA_ID != 333 &
      LTLA_ID != 340 &
      LTLA_ID != 342 &
      LTLA_ID != 360,
    date >= "2020-06-02" & date <= "2020-12-05"
  ) %>%
  arrange(date, LTLA_ID) %>%
  ungroup() %>%
  left_join(., hall_num) %>%
  # group_by(LTLA_ID, date) %>%
  # mutate(week_trend = row_number()) %>%
  dplyr::select(
    LTLA_ID,
    LTLA_name,
    date,
    Pop_Den_km2,
    Prosperity,
    areaCode,
    growth_rate,
    log_GR,
    log_GR2,
    h_index,
    h_ma,
    h_gr,
    h_gr_ma,
    retail_gr_ma,
    retail_gr,
    week,
    case,
    gmc_retail2,
    weekend
  ) # case, gmc_retail2 用来画log_GR和retail_gr的变化


# summary(glm(log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR ~ Pop_Den_km2 + h_gr +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR2 ~ Pop_Den_km2 + h_gr_ma +  Prosperity + retail_gr_ma, family = "gaussian", data = HEI_dec0.4))

# summary(glm(log_GR ~ Pop_Den_km2 * h_index + Prosperity + retail_gr + week, family = "gaussian", data = HEI_dec0.4))
# summary(glm(log_GR ~ Pop_Den_km2 + Prosperity * h_index + retail_gr + week, family = "gaussian", data = HEI_dec0.4))

### 3.4 prepare spatial data and adjacent matrix
# Eng_shp <- st_transform(Eng_shp, crs = 4326)
Eng_shp_no_island <- Eng_shp %>%
  filter(!OBJECTID %in% c(44, 50, 334, 341, 343, 361)) # These locations are island, if do not remove these locations, the adjacent matrix will return an error
## !!!!! OBJECTID is not equal to LTLA_ID !!!!!

W_adj <- poly2nb(Eng_shp_no_island, row.names = Eng_shp_no_island$LAD19NM)
W_mat <- nb2mat(W_adj, style = "B", zero.policy = TRUE) # This step generate a 382*382 matrix


## Calculate KNN matrix -----------------------------------------
coords <- st_coordinates(st_centroid(Eng_shp_no_island))

### KNN Neighbor
k6 <- knearneigh(coords, k = 6)
nb <- knn2nb(k6)

### Connect Northern Ireland to other region
codes <- as.character(Eng_shp_no_island$LAD19CD)

id_NI <- which(substr(codes, 1, 1) == "N") # Northern Ireland
id_GB <- which(substr(codes, 1, 1) != "N")

# Get coords of each LTLA
coords_NI <- coords[id_NI, , drop = FALSE]
coords_GB <- coords[id_GB, , drop = FALSE]

# Calculate the distance matrix (Euclidean distance, unit meters)
# 这里的矩阵行数是北爱尔兰的数量，列数是大不列颠的数量
dist_matrix <- as.matrix(dist(rbind(coords_NI, coords_GB)))
n_ni <- length(id_NI)
n_gb <- length(id_GB)

# 截取右上角矩阵块：行是NI，列是GB
cross_sea_dists <- dist_matrix[1:n_ni, (n_ni + 1):(n_ni + n_gb)]

# 步骤 C: 找到最近的一对点
min_pos <- which(cross_sea_dists == min(cross_sea_dists), arr.ind = TRUE)

# 转换回原始数据的索引 ID

target_NI_idx <- id_NI[min_pos[1]]
target_GB_idx <- id_GB[min_pos[2]]


# 步骤 D: 手动添加连接 (更新 nb 对象)
nb[[target_NI_idx]] <- as.integer(sort(c(nb[[target_NI_idx]], target_GB_idx)))
nb[[target_GB_idx]] <- as.integer(sort(c(nb[[target_GB_idx]], target_NI_idx)))

nb_sym <- make.sym.nb(nb)
W_mat_KNN <- nb2mat(nb_sym, style = "B", zero.policy = T)

# plot(st_geometry(Eng_shp_no_island), border = "grey")
# plot(nb, coords, add = TRUE, col = "red", lwd = 1, pch = 19, cex = 0.5)
# plot(W_adj, coords, add = TRUE, col = "red", lwd = 1, pch = 19, cex = 0.5)
# segments(coords[target_NI_idx, 1], coords[target_NI_idx, 2],
#          coords[target_GB_idx, 1], coords[target_GB_idx, 2],
#          col = "blue", lwd = 3)
