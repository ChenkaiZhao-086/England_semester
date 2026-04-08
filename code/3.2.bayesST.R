results_list_Hindex <- ST.CARanova(
  formula = log_GR ~ h_index + week + weekend,
  family = "gaussian",
  data = HEI_dec0.4,
  W = W_mat,
  burnin = 10000,
  n.sample = 110000,
  thin = 100,
  n.chains = 4,
  n.cores = 4,
  interaction = TRUE,
  verbose = TRUE
)


#### Check convergence - traceplot
# summary(beta.cov_Hindex)
# plot(beta.cov_Hindex)
# x11.save(beta.cov_Hindex, file = "outputs/convergence_check_Hindex.pdf", width = 10, height = 10)
gelman.diag(results_list_Hindex[["samples"]][["beta"]])

#### Effects of covariates on disease risk
RR_table_Hindex <- get.RR(
  results_list_Hindex,
  data = HEI_dec0.4,
  unit = "student",
  digits = 3
)
rownames(RR_table_Hindex) <- c("H-index")
