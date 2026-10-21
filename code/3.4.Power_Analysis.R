# Run complete expressions from top to bottom in a fresh R session at project root.
# This script also remains available through england.stages = "power" in main.R.
# 1. Load the saved current panel and main CAR; never refit them implicitly.
Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  MKL_NUM_THREADS = "1"
)
library(CARBayesST)
library(coda)
library(future)
library(future.apply)
source("code/function.R", local = TRUE)

analysis_workers <- min(as.integer(getOption("england.workers", 2L)), 2L)
stopifnot(
  length(analysis_workers) == 1L,
  !is.na(analysis_workers),
  analysis_workers >= 1L
)
worker_rscript <- getOption(
  "england.worker_rscript",
  normalizePath("outputs/bsts-rolling-2026-10-16/runtime/R-worker")
)
hall_reference <- 40485
input_dir <- "outputs/power-2026-10-16/results"
prepared_file <- file.path(input_dir, "prepared-per-metric.rds")
main_file <- file.path(input_dir, "car-main.rds")
result_dir <- "outputs/power-2026-10-17/results"
table_dir <- "outputs/power-2026-10-17/table"
for (d in c(result_dir, table_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
prepared <- read.analysis(prepared_file)
model <- read.analysis(main_file)$model
base_data <- prepared$datasets$raw
W <- prepared$panel_weights$raw
formula <- reformulate(
  c("h_index", "retail_gr", "Pop_Den_km2", "Prosperity", "week", "day_type"),
  response = "log_GR",
  env = baseenv()
)
X <- model.matrix(formula, base_data)
stopifnot(
  identical(dim(X), dim(model$X)),
  nrow(X) == 376L * 188L,
  identical(base_data$LTLA_name, rep(rownames(W), 188L))
)
# Coefficients are selected by design-matrix name, including both weekday terms.
beta <- vapply(
  colnames(X),
  function(term) mean(car.effect(model, term)$beta),
  numeric(1)
)
tau2 <- colMeans(do.call(rbind, model$samples$tau2))
rho <- colMeans(do.call(rbind, model$samples$rho))
nu2 <- mean(do.call(rbind, model$samples$nu2))
Q_sp <- rho["rho.S"] *
  (diag(rowSums(W)) - W) +
  (1 - rho["rho.S"]) * diag(nrow(W))
W_time <- 1 * (abs(outer(seq_len(188L), seq_len(188L), "-")) == 1)
Q_time <- rho["rho.T"] *
  (diag(rowSums(W_time)) - W_time) +
  (1 - rho["rho.T"]) * diag(188L)
# 2. Restore the old simulation budget; retain the current generating model.
settings <- list(
  burnin = 1000L,
  n.sample = 5000L,
  thin = 1L,
  n.chains = 1L,
  n.cores = 1L
)
power_inputs <- list(
  data = as.data.frame(base_data[, all.vars(formula)]),
  X = X,
  formula = formula,
  W = W,
  beta = beta,
  nu2 = nu2,
  # Cholesky factors are reused across replicates instead of repeated eigendecompositions.
  chol_sp = chol(unname(tau2["tau2.S"]) * solve(Q_sp)),
  chol_time = chol(unname(tau2["tau2.T"]) * solve(Q_time)),
  increment = 5000 / hall_reference,
  settings = settings,
  # Parsed code ignores whitespace/comments but still rejects changes to analysis logic.
  signature = c(
    tools::md5sum(c(prepared_file, main_file)),
    code = digest::digest(parse(
      "code/3.4.Power_Analysis.R",
      keep.source = FALSE
    ))
  )
)
write.csv(
  data.frame(
    parameter = c(names(beta), names(tau2), names(rho), "nu2"),
    value = c(beta, tau2, rho, nu2)
  ),
  file.path(table_dir, "Generating_parameters.csv"),
  row.names = FALSE
)
write.analysis(
  list(inputs = power_inputs),
  file.path(result_dir, "power-inputs.rds"),
  c(prepared_file, main_file)
)
# Retain base_data, X and parameters for inspection during interactive execution.
rm(model, prepared)
gc()

# 3. Simulate and refit one Hall RR on the fixed observed panel.
# Inputs: replicate ID, RR per 5,000 students, compact generating inputs and directory.
# Returns/saves a small result with a 95% credible interval, ESS and errors.
# PSRF is NA for the restored single-chain simulation fits.
# Existing successful replicates are reused only when inputs/settings match exactly.
run_single_sim <- function(sim_id, effect, inputs, output_dir) {
  path <- file.path(output_dir, sprintf("rr-%.3f-sim-%03d.rds", effect, sim_id))
  seed <- 330000L + as.integer(round(1000 * effect)) * 1000L + sim_id
  if (file.exists(path)) {
    previous <- readRDS(path)
    stopifnot(
      identical(previous$signature, inputs$signature),
      identical(previous$settings, inputs$settings),
      previous$effect == effect,
      previous$seed == seed
    )
    if (previous$status == "success") return(previous)
  }
  set.seed(seed)
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(
    {
      n_areas <- nrow(inputs$W)
      n_times <- nrow(inputs$chol_time)
      phi <- as.numeric(t(inputs$chol_sp) %*% stats::rnorm(n_areas))
      delta <- as.numeric(t(inputs$chol_time) %*% stats::rnorm(n_times))
      phi <- phi - mean(phi)
      delta <- delta - mean(delta)
      beta <- inputs$beta
      beta["h_index"] <- log(effect) / inputs$increment
      data <- inputs$data
      data$log_GR <- as.numeric(inputs$X %*% beta) +
        rep(phi, n_times) +
        rep(delta, each = n_areas) +
        stats::rnorm(nrow(data), sd = sqrt(inputs$nu2))
      fit <- do.call(
        CARBayesST::ST.CARanova,
        c(
          list(
            formula = inputs$formula,
            family = "gaussian",
            data = data,
            W = inputs$W,
            verbose = FALSE
          ),
          inputs$settings
        )
      )
      chains <- fit$samples$beta
      draws <- if (inherits(chains, "mcmc.list")) {
        do.call(rbind, chains)
      } else {
        as.matrix(chains)
      }
      # CARBayesST leaves beta chains unnamed; its design matrix fixes column order.
      hall <- match("h_index", colnames(fit$X))
      stopifnot(!is.na(hall), all(is.finite(draws[, hall])))
      interval <- unname(stats::quantile(draws[, hall], c(0.025, 0.975)))
      psrf <- if (inherits(chains, "mcmc.list")) {
        coda::gelman.diag(chains, autoburnin = FALSE)$psrf[hall, 1]
      } else {
        NA_real_
      }
      list(
        estimate = mean(draws[, hall]),
        lower = interval[1],
        upper = interval[2],
        is_sig = interval[1] > 0 || interval[2] < 0,
        psrf = psrf,
        effective = unname(coda::effectiveSize(chains)[hall]),
        status = "success",
        error = ""
      )
    },
    error = function(e) {
      list(
        estimate = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        is_sig = NA,
        psrf = NA_real_,
        effective = NA_real_,
        status = "failed",
        error = conditionMessage(e)
      )
    }
  )
  result <- c(
    list(
      sim_id = sim_id,
      effect = effect,
      seed = seed,
      true_beta = log(effect) / inputs$increment
    ),
    result,
    list(
      seconds = proc.time()[["elapsed"]] - started,
      signature = inputs$signature,
      settings = inputs$settings
    )
  )
  temp <- tempfile(tmpdir = output_dir)
  on.exit(unlink(temp), add = TRUE)
  saveRDS(result, temp)
  if (!file.rename(temp, path)) {
    stop("Could not save simulation: ", path)
  }
  message(sprintf(
    "Hall RR %.3f: replicate %d %s (%.1f minutes)",
    effect,
    sim_id,
    result$status,
    result$seconds / 60
  ))
  result
}
# Keep the workspace out of worker exports; future requires globalenv ancestry.
environment(run_single_sim) <- new.env(parent = globalenv())

# Summarise detection among successful fits, retaining the failure denominator.
# Input: flat replicate table. Output: one row per RR with Wilson/Monte Carlo uncertainty.
summarise.power <- function(results) {
  do.call(
    rbind,
    lapply(split(results, results$effect), function(x) {
      ok <- x$status == "success"
      n <- sum(ok)
      detected <- sum(x$is_sig[ok])
      p <- if (n > 0) detected / n else NA_real_
      z <- qnorm(0.975)
      centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
      half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
      data.frame(
        effect = x$effect[1],
        increment_students = 5000L,
        n_attempted = nrow(x),
        n_success = n,
        n_failed = sum(!ok),
        n_detected = detected,
        power = p,
        mc_se = sqrt(p * (1 - p) / n),
        mc_lower = centre - half,
        mc_upper = centre + half,
        mean_beta = mean(x$estimate[ok]),
        bias = mean(x$estimate[ok]) - x$true_beta[1],
        n_psrf_available = sum(is.finite(x$psrf[ok])),
        n_psrf_above_1.1 = if (any(is.finite(x$psrf[ok]))) {
          sum(x$psrf[ok] > 1.1, na.rm = TRUE)
        } else {
          NA_integer_
        }
      )
    })
  )
}

# 4. Run the two effects separately, as in the original script.
# Each worker runs one chain on one core; never start a second cluster concurrently.
power_cluster <- parallel::makePSOCKcluster(
  4,
  rscript = worker_rscript
)
future::plan(future::cluster, workers = power_cluster)
set.seed(33)
tryCatch(
  {
    power_list1051 <- future.apply::future_lapply(
      X = seq_len(100L),
      FUN = run_single_sim,
      effect = 1.051,
      inputs = power_inputs,
      output_dir = result_dir,
      future.seed = TRUE,
      future.globals = FALSE,
      future.chunk.size = 1L
    )
    power_list1013 <- future.apply::future_lapply(
      X = seq_len(100L),
      FUN = run_single_sim,
      effect = 1.013,
      inputs = power_inputs,
      output_dir = result_dir,
      future.seed = TRUE,
      future.globals = FALSE,
      future.chunk.size = 1L
    )
  },
  finally = {
    future::plan(future::sequential)
    parallel::stopCluster(power_cluster)
  }
)

# 5. Keep both per-effect tables and scalar powers visible in the working environment.
power_replicates <- dplyr::bind_rows(lapply(
  c(power_list1051, power_list1013),
  function(x) x[setdiff(names(x), c("signature", "settings"))]
))
power_result_1051 <- subset(
  power_replicates,
  effect == 1.051 & status == "success"
)
power_result_1013 <- subset(
  power_replicates,
  effect == 1.013 & status == "success"
)
power1051 <- mean(power_result_1051$is_sig)
power1013 <- mean(power_result_1013$is_sig)
power_summary <- summarise.power(power_replicates)
write.csv(
  power_replicates,
  file.path(table_dir, "Power_replicates.csv"),
  row.names = FALSE
)
write.csv(
  power_summary,
  file.path(table_dir, "Power_summary.csv"),
  row.names = FALSE
)
print(power_summary)

# Historical comments retained from the previous implementation.
# ==========================================
# 1. 准备参数
# ==========================================
# h_index_real = mean(real_result_beta[, 2]), # 这个不用，我们要替换成 1.83
# 基础数据结构
# 预计算空间矩阵部分 (放在循环外以节省时间)
# 使用 Leroux 结构的精度矩阵公式: Q = rho * (D - W_mat) + (1 - rho) * I
# 注意：为了生成数据，我们需要协方差矩阵 Sigma = tau2 * Q^-1
# 在循环内根据 rho 计算 Q 会更严谨，但如果固定 rho，可以在这里算好 Q_inv
# Gaussian CARanova uses a Leroux precision on adjacent days as well as areas.
# Keep spatial/temporal variance and dependence parameters separate.
# ==========================================
# 2. 功效分析function
# ==========================================
# Simulate one conditional Gaussian CARanova dataset and refit its Hall effect.
# sim_id labels the replicate; effect is the RR per 5,000 proxy students.
# Uses the fitted spatial/temporal Leroux parameters and observed covariates;
# returns coefficient estimates, interval, detection flag and fit status.
## Generate simulation data
# 1. 生成空间随机效应 phi (考虑 rho)
# 2. 生成时间随机效应 delta
# 3. 将随机效应映射回数据框 (安全做法)
# 创建临时数据框用于合并
# 4. 计算Y (h_index 效应为 1.083)
# --- B. 拟合模型 ---
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
# ==========================================
# 4. 结果汇总
# ==========================================
# power_result_1018 <- bind_rows(power_list1018) %>% filter(status == "success")
# power_result_1027 <- bind_rows(power_list1027) %>% filter(status == "success")
# power_result_1043 <- bind_rows(power_list1043) %>% filter(status == "success")
# 计算统计量
# power1018 <- mean(power_result_1018$is_sig)
# power1027 <- mean(power_result_1027$is_sig)
# power1043 <- mean(power_result_1043$is_sig)
# Compare coefficients on the log-growth scale, not a coefficient against an RR.
