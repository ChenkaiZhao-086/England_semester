# Fit the daily main CAR and the same-panel binary-weekend comparator.
selected_car_models <- getOption(
  "england.car_models",
  getOption("england.reviewer_models", specs$model)
)
stopifnot(all(selected_car_models %in% specs$model))
# Preserve per-model seeds when scripts or execution order change.
fit_seeds <- c(
  main = 3101,
  binary_weekend = 3102,
  no_mobility = 3103,
  knn = 3104,
  population_absolute = 3106,
  population_share = 3107,
  hall_flat = 3108,
  hall_day3 = 3109,
  window7 = 3110,
  window14 = 3111,
  window21 = 3112,
  mobility_grocery_gr = 3115,
  mobility_parks_gr = 3116,
  mobility_transit_gr = 3117,
  mobility_work_gr = 3118,
  mobility_residential_gr = 3119
)
for (i in which(
  specs$model %in% intersect(selected_car_models, c("main", "binary_weekend"))
)) {
  spec <- specs[i, ]
  fit.car.specification(
    spec,
    datasets[[spec$dataset]],
    W_mat,
    unname(fit_seeds[spec$model]),
    model_input_files[[spec$model]],
    car_cores
  )
}

# Historical comments retained from the previous implementation.
### 3.5 Construct bayesian spatio-temporal model
# default prior: normal prior N(0,100000)
### ST.CARanova Spatio-temporal main effects and an interaction
#### Check convergence - traceplot
# summary(beta.cov)
# plot(results_list[["samples"]][["beta"]])
# x11.save(beta.cov, file = "outputs/convergence_check.pdf", width = 10, height = 10)
# gelman.diag(results_list[["samples"]][["beta"]])
#### Effects of covariates on disease risk
# get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "sd", coef.num = 4)
# get.RR(chain1 = chain1_ano_den_h, chain2 = chain2_ano_den_h, data = HEI_dec0.4, unit = "1", coef.num = 6)
# get.RR(chain1 = chain1_ano_pro_h, chain2 = chain2_ano_pro_h, data = HEI_dec0.4, unit = "1", coef.num = 6)
