# bayesian spatio-temporal code 

### ST.CARlinear Correlated linear time trends
job::job({
  set.seed(139)
  chain1_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain2_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain3_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
  chain4_linear <- ST.CARlinear(formula = log_GR ~ Pop_Den_km2 + h_index +  Prosperity + retail_gr +week, family = "gaussian", data = HEI_dec0.4, W = W_mat, burnin = 20000, n.sample = 220000, thin = 100)
}, title = "ST.CARlinear")


### ST.CARar Spatially autocorrelated first-order autoregressive process
job::job({
  set.seed(1995)
  chain1_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
  chain2_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
  gc()
  chain3_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
  gc()
  chain4_ar <- ST.CARar(formula = log_GR ~ Pop_Den_km2 + h_index + Prosperity + retail_gr +week, family = "gaussian", 
                        data = HEI_dec0.4, W = W_mat, AR = 1, burnin = 20000, n.sample = 220000, thin = 100)
}, title = "ST.CARar3-4")

### cheak convergence
#-----linear
beta.sample2 <- mcmc.list(chain1_linear$samples$beta, chain2_linear$samples$beta, chain3_linear$samples$beta, chain4_linear$samples$beta)
plot(beta.sample2)
gelman.diag(beta.sample2) 

#-----ar
beta.sample <- mcmc.list(chain1_ar$samples$beta, chain2_ar$samples$beta, chain3_ar$samples$beta, chain4_ar$samples$beta)
plot(beta.sample)
gelman.diag(beta.sample) 


#### Effects of covariates on disease risk
#-----linear
get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "1", coef.num = 4)
#get.RR(chain1 = chain1_linear, chain2 = chain2_linear, data = HEI_dec0.4, unit = "sd", coef.num = 4) ### Here we use the standard deviation of each covariate as the increase ξ, because they represent realistic increases in each covariates value.

#-----ar
get.RR(chain1 = chain1_ar, chain2 = chain2_ar, chain3 = chain3_ar, chain4 = chain4_ar, data = HEI_dec0.4, unit = "1", coef.num = 4)
