real_result_beta <- do.call(rbind, results_list[["samples"]][["beta"]])
real_result_tau2 <- do.call(rbind, results_list[["samples"]][["tau2"]])
real_result_nu2 <- do.call(rbind, results_list[["samples"]][["nu2"]])
real_result_rho <- do.call(rbind, results_list[["samples"]][["rho"]])
real_result_delta <- do.call(rbind, results_list[["samples"]][["delta"]])


# ==========================================
# 1. 准备参数
# ==========================================
params_from_real <- list(
  intercept = mean(real_result_beta[, 1]),
  # h_index_real = mean(real_result_beta[, 2]), # 这个不用，我们要替换成 1.83
  retail_gr = mean(real_result_beta[, 3]),
  Pop_Den_km2 = mean(real_result_beta[, 4]),
  Prosperity = mean(real_result_beta[, 5]),
  week = mean(real_result_beta[, 6]),
  weekend = mean(real_result_beta[, 7]),
  tau2 = mean(real_result_tau2), # 空间方差
  sigma2 = mean(real_result_nu2), # 误差方差
  rho = mean(real_result_rho),
  var_delta = var(colMeans(real_result_delta))
)

# 基础数据结构
base_data <- HEI_dec0.4
n_areas <- nrow(W_mat)
n_times <- length(unique(base_data$date))
# 预计算空间矩阵部分 (放在循环外以节省时间)
# 使用 Leroux 结构的精度矩阵公式: Q = rho * (D - W_mat) + (1 - rho) * I
D_mat <- diag(rowSums(W_mat))
I_mat <- diag(1, n_areas)
# 注意：为了生成数据，我们需要协方差矩阵 Sigma = tau2 * Q^-1
# 在循环内根据 rho 计算 Q 会更严谨，但如果固定 rho，可以在这里算好 Q_inv
Q_leroux <- params_from_real$rho *
  (D_mat - W_mat) +
  (1 - params_from_real$rho) * I_mat
Sigma_sp <- params_from_real$tau2 * solve(Q_leroux)


# ==========================================
# 2. 功效分析function
# ==========================================
run_single_sim <- function(sim_id, effect = 1.083) {
  effect_size <- log(effect) / (5000 / 40382)

  ## Generate simulation data

  # 1. 生成空间随机效应 phi (考虑 rho)
  phi_sim <- mvrnorm(1, mu = rep(0, n_areas), Sigma = Sigma_sp)

  # 2. 生成时间随机效应 delta
  delta_sim <- rnorm(n_times, 0, params_from_real$var_delta)

  # 3. 将随机效应映射回数据框 (安全做法)
  # 创建临时数据框用于合并
  sp_df <- data.frame(LTLA_name = rownames(W_mat), phi = phi_sim)
  tm_df <- data.frame(date = unique(base_data$date), delta = delta_sim)

  sim_data <- base_data %>%
    left_join(sp_df, by = "LTLA_name") %>%
    left_join(tm_df, by = "date")

  # 4. 计算Y (h_index 效应为 1.083)
  sim_data$linear_predictor <- params_from_real$intercept +
    effect_size * sim_data$h_index + # 设定的效应量
    params_from_real$retail_gr * sim_data$retail_gr +
    params_from_real$Pop_Den_km2 * sim_data$Pop_Den_km2 +
    params_from_real$Prosperity * sim_data$Prosperity +
    params_from_real$week * sim_data$week +
    params_from_real$weekend * sim_data$weekend +
    sim_data$phi +
    sim_data$delta

  sim_data$log_GR <- sim_data$linear_predictor +
    rnorm(nrow(sim_data), 0, sqrt(params_from_real$sigma2))

  # --- B. 拟合模型 ---

  res <- tryCatch(
    {
      model <- ST.CARanova(
        formula = log_GR ~ h_index +
          retail_gr +
          Pop_Den_km2 +
          Prosperity +
          week +
          weekend,
        family = "gaussian",
        data = sim_data,
        W = W_mat,
        burnin = 1000,
        n.sample = 5000,
        thin = 1,
        verbose = FALSE
      )

      beta_samples <- model$samples$beta[, 2]

      list(
        sim_id = sim_id,
        estimate = mean(beta_samples),
        lower = quantile(beta_samples, 0.025),
        upper = quantile(beta_samples, 0.975),
        is_sig = ifelse(
          quantile(beta_samples, 0.025) > 0 ||
            quantile(beta_samples, 0.975) < 0,
          1,
          0
        ),
        status = "success"
      )
    },
    error = function(e) {
      list(
        sim_id = sim_id,
        estimate = NA,
        lower = NA,
        upper = NA,
        is_sig = NA,
        status = "failed"
      )
    }
  )

  return(res)
}

set.seed(33)
plan(multisession, workers = 4)
power_list1083 <- future_lapply(
  X = 1:100,
  effect = 1.083,
  FUN = run_single_sim,
  future.seed = TRUE
)
# power_list1018 <- future_lapply(
#   X = 1:100,
#   effect = 1.018,
#   FUN = run_single_sim,
#   future.seed = TRUE
# )
# power_list1027 <- future_lapply(
#   X = 1:100,
#   effect = 1.027,
#   FUN = run_single_sim,
#   future.seed = TRUE
# )
# power_list1043 <- future_lapply(
#   X = 1:100,
#   effect = 1.043,
#   FUN = run_single_sim,
#   future.seed = TRUE
# )
power_list1042 <- future_lapply(
  X = 1:100,
  effect = 1.042,
  FUN = run_single_sim,
  future.seed = TRUE
)
plan(sequential)

# ==========================================
# 4. 结果汇总
# ==========================================
power_result_1083 <- bind_rows(power_list1083) %>% filter(status == "success")
# power_result_1018 <- bind_rows(power_list1018) %>% filter(status == "success")
# power_result_1027 <- bind_rows(power_list1027) %>% filter(status == "success")
# power_result_1043 <- bind_rows(power_list1043) %>% filter(status == "success")
power_result_1042 <- bind_rows(power_list1042) %>% filter(status == "success")

# 计算统计量
power1083 <- mean(power_result_1083$is_sig)
# power1018 <- mean(power_result_1018$is_sig)
# power1027 <- mean(power_result_1027$is_sig)
# power1043 <- mean(power_result_1043$is_sig)
power1042 <- mean(power_result_1042$is_sig)

avg_est <- mean(power_result$estimate)
bias <- avg_est - 1.083
