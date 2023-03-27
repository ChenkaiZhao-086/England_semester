## Calculate cumulative effect of HEI(s) reopen in 0-3, 0-7, 0-14 days
## This script have a function version called cal.cum.bs()
  
first_row <- HEI_dec0.4 %>% 
  left_join(region_num2) %>% 
  dplyr::select(starts_with("LTLA"),  date, h_index) %>% 
  arrange(LTLA_ID) %>% 
  group_by(LTLA_ID) %>% 
  mutate(row_num = row_number()) %>% 
  filter(h_index > 0) %>% 
  slice(1) %>% 
  dplyr::select(-c(h_index,date))

# 给eff_all加一个识别列，然后先操作HEIdec，之后在合并两个
eff_name <- suppressMessages(
  HEI_dec0.4 %>% 
    dplyr::select(starts_with("LTLA"), date) %>% 
    bind_cols(eff_all)
)


split_dat <- HEI_dec0.4 %>% 
  left_join(region_num2) %>% 
  dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>% 
  left_join(., first_row) %>% 
  arrange(LTLA_ID) %>% 
  group_by(LTLA_ID) %>% 
  filter(row_number() >= row_num &
           row_number() <= row_num+14) %>% 
  arrange(N_num) %>% 
  left_join(., eff_name) %>% 
  split(., .$N_num) %>% 
  map(., function(x) {
    ungroup(x) %>% 
      group_by(LTLA_ID) %>% 
      split(., .$LTLA_ID)
  })


label <- c("day3", "day7", "day14")

cl <- makeCluster(12)
registerDoParallel(cl)
table2_bs <- foreach (df = split_dat, # i in 1:12
                      .packages = c("tidyverse"),
                      .combine = rbind) %dopar% {
                        do.call(cbind, df) %>% 
                          #split_dat %>% 
                          # filter(N_num == i) %>% 
                          # group_by(LTLA_ID) %>% 
                          # split(., .$LTLA_ID) %>% 
                          # bind_cols() %>% 
                          dplyr::select(-c(contains("_"), contains("date"))) %>% # 
                          apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>% 
                          t() %>% 
                          apply(., 2, cumsum) %>% as.data.frame() %>%
                          slice(., c(4,8,15)) %>% 
                          exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
                          mutate(cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"), label = label) %>% 
                          dplyr::select(cum.bs, label) %>% 
                          pivot_wider(names_from = "label", values_from = cum.bs)
                      }
stopCluster(cl)
# closeAllConnections()
table2_bs_tidy <- region_num2 %>% dplyr::select(contains("N_")) %>% distinct() %>% bind_cols(., table2_bs) %>% relocate(N_num)


## Calculate effect of HEI(s) reopen in all region
data_block<- HEI_dec0.4 %>% 
  left_join(region_num2) %>% 
  dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>% 
  left_join(., first_row) %>% 
  arrange(LTLA_ID) %>% 
  group_by(LTLA_ID) %>% 
  filter(row_number() >= row_num &
           row_number() <= row_num+14) %>% 
  left_join(., eff_name) %>% 
  split(., .$LTLA_ID) %>% 
  map(., function(x) ungroup(x) %>% dplyr::select(-c(contains("_"), contains("date")))) %>% 
  split(., ceiling(seq_along(.)/12)) # 将数据分成12个块，使用下面的foreach提升计算速度，此处的12是指电脑的12个核

## 使用foreach提升合并的计算速度
cl <- makeCluster(12)
registerDoParallel(cl)
combine_block <- foreach (block=data_block, .combine = cbind) %dopar% {do.call(cbind, block)}
stopCluster(cl)


table2_bs_all <- combine_block %>% 
  apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>% 
  t() %>% 
  apply(., 2, cumsum) %>% as.data.frame() %>% 
  slice(., c(4, 8, 15)) %>% 
  exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
  mutate(cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
         label = label) %>% 
  dplyr::select(cum.bs, label) %>% 
  pivot_wider(names_from = "label", values_from = cum.bs) %>% 
  mutate(N_num = 0, N_region = "All region") %>% 
  relocate(starts_with("N"))


rbind(table2_bs_all, table2_bs_tidy)





