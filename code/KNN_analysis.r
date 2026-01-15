#######################################
# With covariates
#######################################
results_list_KNN <- ST.CARanova(
  formula = log_GR ~ h_index + retail_gr + Pop_Den_km2 + Prosperity + week + weekend,
  family = "gaussian",
  data = HEI_dec0.4,
  W = W_mat_KNN,
  burnin = 10000,
  n.sample = 110000,
  thin = 100,
  n.chains = 4,
  n.cores = 4,
  interaction = TRUE,
  verbose = TRUE
)

# summary(beta.cov_KNN)
# plot(beta.cov_KNN)
# x11.save(beta.cov_KNN, file = "outputs/convergence_check.pdf", width = 10, height = 10)
gelman.diag(results_list_KNN[["samples"]][["beta"]])

#### Effects of covariates on disease risk
RR_table_KNN <- get.RR(results_list_KNN, data = HEI_dec0.4, unit = "student", digits = 3)
rownames(RR_table_KNN) <- c("H-index", "Growth rate of retail and recreation percent change", "Population density/Km2", "Prosperity")


#######################################
# Only Hall index
#######################################
results_list_Hindex_KNN <- ST.CARanova(
  formula = log_GR ~ h_index + week + weekend,
  family = "gaussian",
  data = HEI_dec0.4,
  W = W_mat_KNN,
  burnin = 10000,
  n.sample = 110000,
  thin = 100,
  n.chains = 4,
  n.cores = 4,
  interaction = TRUE,
  verbose = TRUE
)

### n.effect means effective number of independent samples; Geweke.diag is another MCMC convergence diagnostic that should lie between -2 and 2 to indicate convergence.

#### Check convergence - traceplot
# summary(beta.cov_Hindex_KNN)
# plot(beta.cov_Hindex_KNN)
# x11.save(beta.cov_Hindex_KNN, file = "outputs/convergence_check_Hindex.pdf", width = 10, height = 10)
gelman.diag(results_list_Hindex_KNN[["samples"]][["beta"]])

#### Effects of covariates on disease risk
RR_table_Hindex_KNN <- get.RR(results_list_Hindex_KNN, data = HEI_dec0.4, unit = "student", digits = 3)
rownames(RR_table_Hindex_KNN) <- c("H-index")
