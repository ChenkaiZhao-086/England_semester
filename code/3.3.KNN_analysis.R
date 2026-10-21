# Fit the current daily KNN sensitivity with the main-model covariates.
# Both matrices use the same geometry order; KNN originally had numeric names.
stopifnot(identical(dim(W_mat_KNN), dim(W_mat)))
dimnames(W_mat_KNN) <- dimnames(W_mat)
for (i in which(specs$model %in% intersect(selected_car_models, "knn"))) {
  spec <- specs[i, ]
  fit.car.specification(
    spec,
    datasets[[spec$dataset]],
    W_mat_KNN,
    unname(fit_seeds[spec$model]),
    model_input_files[[spec$model]],
    car_cores
  )
}

# Historical comments retained from the previous implementation.
#######################################
# With covariates
#######################################
# summary(beta.cov_KNN)
# plot(beta.cov_KNN)
# x11.save(beta.cov_KNN, file = "outputs/convergence_check.pdf", width = 10, height = 10)
#### Effects of covariates on disease risk
#######################################
# Only Hall index
#######################################
#### Check convergence - traceplot
# summary(beta.cov_Hindex_KNN)
# plot(beta.cov_Hindex_KNN)
# x11.save(beta.cov_Hindex_KNN, file = "outputs/convergence_check_Hindex.pdf", width = 10, height = 10)
#### Effects of covariates on disease risk
