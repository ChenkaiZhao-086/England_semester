# Fit no-mobility, six-category, population and Hall-profile CAR sensitivities.
# The no-mobility models retain the other covariates; they are not Hall-only fits.
for (i in which(
  specs$model %in%
    setdiff(selected_car_models, c("main", "binary_weekend", "knn"))
)) {
  spec <- specs[i, ]
  fit.car.specification(
    spec,
    datasets[[spec$dataset]],
    panel_weights[[spec$dataset]],
    unname(fit_seeds[spec$model]),
    model_input_files[[spec$model]],
    car_cores
  )
}
