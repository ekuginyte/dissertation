#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "betareg", "R.matlab", "glmnet", "dplyr", 
                  "cowplot", "coda", "car", "igraph", "R6", "nimble", "MASS", "xgboost",
                  "caret", "spikeslab", "SSLASSO", "horseshoe", "bayesreg", "Hmisc",
                  "LaplacesDemon", "BayesS5", "monomvn", "Hmisc", "gridExtra", "maps")

# Un-comment the following lines if you need to install the packages
# for (i in library_list) {
#   install.packages(i, character.only = TRUE)
# }

# Load the libraries
for (i in library_list) {
  library(i, character.only = TRUE)
}

# Set working directory (assuming you want to set it to the 'main' directory)
setwd("~/Documents/Dissertation/main/Dissertation")

# Remove unwanted objects
rm(library_list, i)




#### SOURCE SIMULATED DATA ####

# Source the file that contains the simulation functions
source("simulate_data.R")





#### SOURCE FUNCTIONS ####

# Source the file that contains the simulation functions
source("functions.R")





#### SIM DATA. LASSO AND ELASTIC NET FITTING 'glmnet' ####

# Extract BOTH the Lasso and Elastic net penalisation models using all types 
#   of simulated data 

# List of dataset prefixes
prefixes <- c("T1", "T2", "T3", "T4")
# List of dataset suffixes
suffixes <- c("LD", "ED", "HD", "VD")
# List of methods and corresponding functions
methods <- list("_lasso" = fit_lasso, "_elnet" = fit_elnet)

# Function to fit model
fit_model <- function(data, prefix, method) {
  if (prefix %in% c("T3", "T4")) {
    cat_var = TRUE
  } else {
    cat_var = FALSE
  }
  return(methods[[method]](data, cat_var))
}

# Loop through prefixes, suffixes and methods
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the data variable name
    data_var_name <- paste0(prefix, "_", suffix)
    data_var <- get(data_var_name)
    
    for (method in names(methods)) {
      # Construct the model variable name
      model_var_name <- paste0(prefix, "_", suffix, method)
      # Fit the model and assign it to a variable in the global environment
      assign(model_var_name, fit_model(data_var, prefix, method), envir = .GlobalEnv)
    }
  }
}

# Remove the functions and prefixes, suffixes, methods as they won't be used anymore
rm(fit_elnet, fit_lasso, prefixes, suffixes, methods, fit_lasso, fit_elnet,
   data_var_name, data_var, model_var_name, prefix, suffix, method,)




#### SIM DATA. XGBOOST 'caret' ####

# Set the initial parameters for XGBoost

# Define an extensive grid for hyperparameter tuning
# This grid consists of multiple values for each parameter, allowing for more refined tuning
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),                  # Number of boosting rounds
  max_depth = c(3, 5, 7, 9),                  # Maximum depth of the trees
  eta = c(0.01, 0.1, 0.3),                    # Learning rate
  gamma = c(0, 0.1, 1),                       # Minimum loss reduction required
  colsample_bytree = c(0.6, 0.8, 1),          # Fraction of features to be randomly sampled for each tree
  min_child_weight = c(1, 3, 5),              # Minimum sum of instance weight needed in a leaf
  subsample = c(0.8, 1)                      # Fraction of observations to be randomly sampled for each tree
)


# Define cross-validation strategy
# This helps in assessing the model's performance in an unbiased way using a subset of the data
xgb_cv <- trainControl(
  method = "repeatedcv",     # Repeated cross-validation
  number = 5,                # Number of folds
  repeats = 3,               # Number of complete sets of folds to compute
  verboseIter = TRUE,        # Display training progress
  returnData = FALSE,        # Do not return the training data
  returnResamp = "all",      # Save all resampling scores
  allowParallel = TRUE,      # Allow parallel processing
)

# Fit the function
# Type 1 data
T1_LD_xgboost <- train_evaluate_xgb(data = T1_LD, xgb_cv, xgb_grid)
T1_ED_xgboost <- train_evaluate_xgb(data = T1_ED, xgb_cv, xgb_grid)
T1_VD_xgboost <- train_evaluate_xgb(data = T1_HD, xgb_cv, xgb_grid)
T1_HD_xgboost <- train_evaluate_xgb(data = T1_VD, xgb_cv, xgb_grid)
T1_XD_xgboost <- train_evaluate_xgb(data = T1_XD, xgb_cv, xgb_grid)

# Type 2 data
T2_LD_xgboost <- train_evaluate_xgb(data = T2_LD, xgb_cv, xgb_grid)
T2_ED_xgboost <- train_evaluate_xgb(data = T2_ED, xgb_cv, xgb_grid)
T2_VD_xgboost <- train_evaluate_xgb(data = T2_HD, xgb_cv, xgb_grid)
T2_HD_xgboost <- train_evaluate_xgb(data = T2_VD, xgb_cv, xgb_grid)
T2_XD_xgboost <- train_evaluate_xgb(data = T2_XD, xgb_cv, xgb_grid)

# Type 3 data
T3_LD_xgboost <- train_evaluate_xgb(data = T3_LD, cat_var = TRUE, xgb_cv, xgb_grid)
T3_ED_xgboost <- train_evaluate_xgb(data = T3_ED, cat_var = TRUE, xgb_cv, xgb_grid)
T3_VD_xgboost <- train_evaluate_xgb(data = T3_HD, cat_var = TRUE, xgb_cv, xgb_grid)
T3_HD_xgboost <- train_evaluate_xgb(data = T3_VD, cat_var = TRUE, xgb_cv, xgb_grid)
T3_XD_xgboost <- train_evaluate_xgb(data = T3_XD, cat_var = TRUE, xgb_cv, xgb_grid)

# Type 4 data
T4_LD_xgboost <- train_evaluate_xgb(data = T4_LD, cat_var = TRUE, xgb_cv, xgb_grid)
T4_ED_xgboost <- train_evaluate_xgb(data = T4_ED, cat_var = TRUE, xgb_cv, xgb_grid)
T4_VD_xgboost <- train_evaluate_xgb(data = T4_HD, cat_var = TRUE, xgb_cv, xgb_grid)
T4_HD_xgboost <- train_evaluate_xgb(data = T4_VD, cat_var = TRUE, xgb_cv, xgb_grid)
T4_XD_xgboost <- train_evaluate_xgb(data = T4_XD, cat_var = TRUE, xgb_cv, xgb_grid)



#### SIM DATA. SPIKE AND SLAB PRIOR 'spikeslab' #### 

# Extract the selected variables
# T1 data
ssp_T1_LD <- fit_spikeslab_prior(data = T1_LD, bigp_smalln = FALSE)
ssp_T1_ED <- fit_spikeslab_prior(data = T1_ED, bigp_smalln = FALSE)
ssp_T1_HD <- fit_spikeslab_prior(data = T1_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_VD <- fit_spikeslab_prior(data = T1_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T2 data
ssp_T2_LD <- fit_spikeslab_prior(data = T2_LD, bigp_smalln = FALSE)
ssp_T2_ED <- fit_spikeslab_prior(data = T2_ED, bigp_smalln = FALSE)
ssp_T2_HD <- fit_spikeslab_prior(data = T2_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_VD <- fit_spikeslab_prior(data = T2_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T3 data
ssp_T3_LD <- fit_spikeslab_prior(data = T3_LD, cat_var = TRUE, bigp_smalln = FALSE)
ssp_T3_ED <- fit_spikeslab_prior(data = T3_ED, cat_var = TRUE, bigp_smalln = FALSE)
ssp_T3_HD <- fit_spikeslab_prior(data = T3_HD, cat_var = TRUE, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_VD <- fit_spikeslab_prior(data = T3_VD, cat_var = TRUE, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T4 data
ssp_T4_LD <- fit_spikeslab_prior(data = T4_LD, cat_var = TRUE, bigp_smalln = FALSE)
ssp_T4_ED <- fit_spikeslab_prior(data = T4_ED, cat_var = TRUE, bigp_smalln = TRUE,
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_HD <- fit_spikeslab_prior(data = T4_HD, cat_var = TRUE, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_VD <- fit_spikeslab_prior(data = T4_VD, cat_var = TRUE, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# Remove functions, and large spikeslab objects
#rm(fit_spikeslab_prior, ssp_T1_LD, ssp_T1_ED, ssp_T1_HD, ssp_T1_VD,
#ssp_T2_LD, ssp_T2_ED, ssp_T2_HD, ssp_T2_VD,
#ssp_T3_LD, ssp_T3_ED, ssp_T3_HD, ssp_T3_VD,
#ssp_T4_LD, ssp_T4_ED, ssp_T4_HD, ssp_T4_VD)


#### SIM DATA. SPIKE-AND-SLAB LASSO 'SSLASSO' ####

# Call the function with the simulated data
# T1 Data
ssl_T1_LD <- fit_sslasso(T1_LD)
ssl_T1_ED <- fit_sslasso(T1_ED)
ssl_T1_HD <- fit_sslasso(T1_HD)
ssl_T1_VD <- fit_sslasso(T1_VD)

# T2 Data
ssl_T2_LD <- fit_sslasso(T2_LD)
ssl_T2_ED <- fit_sslasso(T2_ED)
ssl_T2_HD <- fit_sslasso(T2_HD)
ssl_T2_VD <- fit_sslasso(T2_VD, var = "unknown")

# T3 Data
ssl_T3_LD <- fit_sslasso(T3_LD, cat_var = TRUE, var = "unknown")
ssl_T3_ED <- fit_sslasso(T3_ED, cat_var = TRUE)
ssl_T3_HD <- fit_sslasso(T3_HD, cat_var = TRUE)
ssl_T3_VD <- fit_sslasso(T3_VD, cat_var = TRUE)

# T4 Data
ssl_T4_LD <- fit_sslasso(T4_LD, cat_var = TRUE, var = "unknown")
ssl_T4_ED <- fit_sslasso(T4_ED, cat_var = TRUE)
ssl_T4_HD <- fit_sslasso(T4_HD, cat_var = TRUE)
ssl_T4_VD <- fit_sslasso(T4_VD, cat_var = TRUE)

# The output contains coefficients, ever_selected, and plot
#ssl_T1_LD$coefficients
#ssl_T3_LD$ever_selected
ssl_T4_LD$selected_variable_names

# Remove functions, and large SSLASSO objects
#rm(fit_sslasso, ssl_T1_LD, ssl_T1_ED, ssl_T1_HD, ssl_T1_VD,
#ssl_T2_LD, ssl_T2_ED, ssl_T2_HD, ssl_T2_VD,
#ssl_T3_LD, ssl_T3_ED, ssl_T3_HD, ssl_T3_VD,
#ssl_T4_LD, ssl_T4_ED, ssl_T4_HD, ssl_T4_VD)


#### SIM DATA. HORSESHOE PRIOR. 'horseshoe' ####

# Fit the models with Truncated Cauchy priors
# T1 Data
hshst_T1_LD <- fit_hs_horseshoe_model(data = T1_LD, method.tau = "truncatedCauchy")
hshst_T1_ED <- fit_hs_horseshoe_model(data = T1_ED, method.tau = "truncatedCauchy")
hshst_T1_HD <- fit_hs_horseshoe_model(data = T1_HD, method.tau = "truncatedCauchy")
hshst_T1_VD <- fit_hs_horseshoe_model(data = T1_VD, method.tau = "truncatedCauchy")

# T2 Data
hshst_T2_LD <- fit_hs_horseshoe_model(data = T2_LD, method.tau = "truncatedCauchy")
hshst_T2_ED <- fit_hs_horseshoe_model(data = T2_ED, method.tau = "truncatedCauchy")
hshst_T2_HD <- fit_hs_horseshoe_model(data = T2_HD, method.tau = "truncatedCauchy")
hshst_T2_VD <- fit_hs_horseshoe_model(data = T2_VD, method.tau = "truncatedCauchy")

# T3 Data
hshst_T3_LD <- fit_hs_horseshoe_model(data = T3_LD, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T3_ED <- fit_hs_horseshoe_model(data = T3_ED, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T3_HD <- fit_hs_horseshoe_model(data = T3_HD, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T3_VD <- fit_hs_horseshoe_model(data = T3_VD, cat_var = TRUE, method.tau = "truncatedCauchy")

# T4 Data
hshst_T4_LD <- fit_hs_horseshoe_model(data = T4_LD, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T4_ED <- fit_hs_horseshoe_model(data = T4_ED, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T4_HD <- fit_hs_horseshoe_model(data = T4_HD, cat_var = TRUE, method.tau = "truncatedCauchy")
hshst_T4_VD <- fit_hs_horseshoe_model(data = T4_VD, cat_var = TRUE, method.tau = "truncatedCauchy")

hshst_T4_VD$sel_var
HS.var.select(hshst_T1_VD$model$BetaMedian, y, method = "intervals")

# Fit the models with Half Cauchy priors
# T1 Data
hshsh_T1_LD <- fit_hs_horseshoe_model(data = T1_LD, method.tau = "halfCauchy")
hshsh_T1_ED <- fit_hs_horseshoe_model(data = T1_ED, method.tau = "halfCauchy")
hshsh_T1_HD <- fit_hs_horseshoe_model(data = T1_HD, method.tau = "halfCauchy")
hshsh_T1_VD <- fit_hs_horseshoe_model(data = T1_VD, method.tau = "halfCauchy")

# T2 Data
hshsh_T2_LD <- fit_hs_horseshoe_model(data = T2_LD, method.tau = "halfCauchy")
hshsh_T2_ED <- fit_hs_horseshoe_model(data = T2_ED, method.tau = "halfCauchy")
hshsh_T2_HD <- fit_hs_horseshoe_model(data = T2_HD, method.tau = "halfCauchy")
hshsh_T2_VD <- fit_hs_horseshoe_model(data = T2_VD, method.tau = "halfCauchy")

# T3 Data
hshsh_T3_LD <- fit_hs_horseshoe_model(data = T3_LD, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T3_ED <- fit_hs_horseshoe_model(data = T3_ED, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T3_HD <- fit_hs_horseshoe_model(data = T3_HD, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T3_VD <- fit_hs_horseshoe_model(data = T3_VD, cat_var = TRUE, method.tau = "halfCauchy")

# T4 Data
hshsh_T4_LD <- fit_hs_horseshoe_model(data = T4_LD, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T4_ED <- fit_hs_horseshoe_model(data = T4_ED, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T4_HD <- fit_hs_horseshoe_model(data = T4_HD, cat_var = TRUE, method.tau = "halfCauchy")
hshsh_T4_VD <- fit_hs_horseshoe_model(data = T4_VD, cat_var = TRUE, method.tau = "halfCauchy")

hshsh_T4_VD$sel_var

# Remove functions, and large horseshoe objects
#rm(fit_hs_horseshoe_model, hshsh_T1_LD, hshsh_T1_ED, hshsh_T1_HD, hshsh_T1_VD,
#hshsh_T2_LD, hshsh_T2_ED, hshsh_T2_HD, hshsh_T2_VD,
#hshsh_T3_LD, hshsh_T3_ED, hshsh_T3_HD, hshsh_T3_VD,
#hshsh_T4_LD, hshsh_T4_ED, hshsh_T4_HD, hshsh_T4_VD)


#### SIM DATA. HORSESHOE PRIOR 'bayesreg' ####
# Fit the model
# T1 Data
hs_bs_T1_LD <- fit_horseshoe_bs_model(data = T1_LD)
hs_bs_T1_ED <- fit_horseshoe_bs_model(data = T1_ED)
hs_bs_T1_HD <- fit_horseshoe_bs_model(data = T1_HD)
hs_bs_T1_VD <- fit_horseshoe_bs_model(data = T1_VD)

# T2 Data
hs_bs_T2_LD <- fit_horseshoe_bs_model(data = T2_LD)
hs_bs_T2_ED <- fit_horseshoe_bs_model(data = T2_ED)
hs_bs_T2_HD <- fit_horseshoe_bs_model(data = T2_HD)
hs_bs_T2_VD <- fit_horseshoe_bs_model(data = T2_VD)

# T3 Data
hs_bs_T3_LD <- fit_horseshoe_bs_model(data = T3_LD, cat_var = TRUE)
hs_bs_T3_ED <- fit_horseshoe_bs_model(data = T3_ED, cat_var = TRUE)
hs_bs_T3_HD <- fit_horseshoe_bs_model(data = T3_HD, cat_var = TRUE)
hs_bs_T3_VD <- fit_horseshoe_bs_model(data = T3_VD, cat_var = TRUE)

# T4 Data
hs_bs_T4_LD <- fit_horseshoe_bs_model(data = T4_LD, cat_var = TRUE)
hs_bs_T4_ED <- fit_horseshoe_bs_model(data = T4_ED, cat_var = TRUE)
hs_bs_T4_HD <- fit_horseshoe_bs_model(data = T4_HD, cat_var = TRUE)
hs_bs_T4_VD <- fit_horseshoe_bs_model(data = T4_VD, cat_var = TRUE)



#### SIM DATA. HORSESHOE + PRIOR 'bayesreg' ####

# Fit the model
# T1 Data
hsp_bs_T1_LD <- fit_horseshoe_bs_model(data = T1_LD, prior = "hs+")
hsp_bs_T1_ED <- fit_horseshoe_bs_model(data = T1_ED, prior = "hs+")
hsp_bs_T1_HD <- fit_horseshoe_bs_model(data = T1_HD, prior = "hs+")
hsp_bs_T1_VD <- fit_horseshoe_bs_model(data = T1_VD, prior = "hs+")

# T2 Data
hsp_bs_T2_LD <- fit_horseshoe_bs_model(data = T2_LD, prior = "hs+")
hsp_bs_T2_ED <- fit_horseshoe_bs_model(data = T2_ED, prior = "hs+")
hsp_bs_T2_HD <- fit_horseshoe_bs_model(data = T2_HD, prior = "hs+")
hsp_bs_T2_VD <- fit_horseshoe_bs_model(data = T2_VD, prior = "hs+")

# T3 Data
hsp_bs_T3_LD <- fit_horseshoe_bs_model(data = T3_LD, cat_var = TRUE, prior = "hs+")
hsp_bs_T3_ED <- fit_horseshoe_bs_model(data = T3_ED, cat_var = TRUE, prior = "hs+")
hsp_bs_T3_HD <- fit_horseshoe_bs_model(data = T3_HD, cat_var = TRUE, prior = "hs+")
hsp_bs_T3_VD <- fit_horseshoe_bs_model(data = T3_VD, cat_var = TRUE, prior = "hs+")

# T4 Data
hsp_bs_T4_LD <- fit_horseshoe_bs_model(data = T4_LD, cat_var = TRUE, prior = "hs+")
hsp_bs_T4_ED <- fit_horseshoe_bs_model(data = T4_ED, cat_var = TRUE, prior = "hs+")
hsp_bs_T4_HD <- fit_horseshoe_bs_model(data = T4_HD, cat_var = TRUE, prior = "hs+")
hsp_bs_T4_VD <- fit_horseshoe_bs_model(data = T4_VD, cat_var = TRUE, prior = "hs+")


hsp_bs_T4_VD[["rank"]]

#### SIM DATA. SSS WITH SCREENING 'BayesS5' ####

print(result(S5_T1_LD)$hppm) # the MAP model
#print(result(S5_T1_LD)$hppm.prob) # the posterior probability of the hppm
#plot(result(S5_T1_LD)$marg.prob, ylim = c(0, 1), ylab = "marginal inclusion probability")


# Fit the model
# T1 Data
S5_T1_LD <- fit_S5_model(data = T1_LD)# %>% results()
S5_T1_ED <- fit_S5_model(data = T1_ED)# %>% results()
S5_T1_HD <- fit_S5_model(data = T1_HD)# %>% results()
S5_T1_VD <- fit_S5_model(data = T1_VD)# %>% results()

# T2 Data
S5_T2_LD <- fit_S5_model(data = T2_LD)# %>% results()
S5_T2_ED <- fit_S5_model(data = T2_ED)# %>% results()
S5_T2_HD <- fit_S5_model(data = T2_HD)# %>% results()
S5_T2_VD <- fit_S5_model(data = T2_VD)# %>% results()

# T3 Data
S5_T3_LD <- fit_S5_model(data = T3_LD, cat_var = TRUE)# %>% results()
S5_T3_ED <- fit_S5_model(data = T3_ED, cat_var = TRUE)# %>% results()
S5_T3_HD <- fit_S5_model(data = T3_HD, cat_var = TRUE)# %>% results()
S5_T3_VD <- fit_S5_model(data = T3_VD, cat_var = TRUE)# %>% results()

# T4 Data
S5_T4_LD <- fit_S5_model(data = T4_LD, cat_var = TRUE)# %>% results()
S5_T4_ED <- fit_S5_model(data = T4_ED, cat_var = TRUE)# %>% results()
S5_T4_HD <- fit_S5_model(data = T4_HD, cat_var = TRUE)# %>% results()
S5_T4_VD <- fit_S5_model(data = T4_VD, cat_var = TRUE)# %>% results()




#### SIM DATA. BAYESIAN LASSO 'monomvn' ####


# T1 Data
blasso_T1_LD <- fit_blasso_model(data = T1_LD)
blasso_T1_ED <- fit_blasso_model(data = T1_ED)
blasso_T1_HD <- fit_blasso_model(data = T1_HD)
blasso_T1_VD <- fit_blasso_model(data = T1_VD)

# T2 Data
blasso_T2_LD <- fit_blasso_model(data = T2_LD)
blasso_T2_ED <- fit_blasso_model(data = T2_ED)
blasso_T2_HD <- fit_blasso_model(data = T2_HD)
blasso_T2_VD <- fit_blasso_model(data = T2_VD)

# T3 Data
blasso_T3_LD <- fit_blasso_model(data = T3_LD, cat_var = TRUE)
blasso_T3_ED <- fit_blasso_model(data = T3_ED, cat_var = TRUE)
blasso_T3_HD <- fit_blasso_model(data = T3_HD, cat_var = TRUE)
blasso_T3_VD <- fit_blasso_model(data = T3_VD, cat_var = TRUE)

# T4 Data
blasso_T4_LD <- fit_blasso_model(data = T4_LD, cat_var = TRUE)
blasso_T4_ED <- fit_blasso_model(data = T4_ED, cat_var = TRUE)
blasso_T4_HD <- fit_blasso_model(data = T4_HD, cat_var = TRUE)
blasso_T4_VD <- fit_blasso_model(data = T4_VD, cat_var = TRUE)




data <- T1_LD
# Extract the target
y <- data$y

# Remove the original categorical variables
data <- data.frame(scale(subset(data, select = -y)))

# Combine scaled continuous variables, intercept
X <- model.matrix(~ . - 1, data = data)

fit_blasso <- monomvn::blasso(X = X, y = y, T, thin = NULL, RJ = FALSE, M = ncol(data),
                              beta = rep(-500, ncol(data)), lambda2 = 0.1, verb = 1)

fit_blasso <- monomvn::blasso(X = X, y = y, T = 1200, lambda = 1)
fit_blasso0 <- monomvn::blasso(X = X, y = y, T = 1000, lambda = 0)


#colMeans(fit_blasso$beta) == colMeans(fit_blasso1$beta)
## summarize the beta (regression coefficients) estimates
plot(fit_blasso, burnin=1000)
points(drop(fit_blasso$b), col=2, pch=20)
points(drop(fit_blasso0$b), col=3, pch=18)
legend("topleft", c("blasso-map", "lasso", "lsr"),
       col=c(2,2,3), pch=c(21,20,18))


colMeans(abs(fit_blasso$beta)) > 0.5







## CONVERGENCE OF CHAINS #

# Number of chains
n_chains <- 4

# Store the results of each chain in a list
chains <- vector("list", n_chains)

# Run through all the chains and extract posterior distributions
for(i in 1:n_chains) {
  # Run blasso with different initial values for beta
  chains[[i]] <- monomvn::blasso(X = X, y = y, T = 2000,
                                 lambda2 = 1, verb = 1)
}

# Combine the chains into a mcmc.list object
mcmc_list <- mcmc.list(lapply(chains, function(chain) 
  mcmc(chain$beta, thin = chain$thin)))

# Compute the Gelman-Rubin diagnostic
gelman_res <- gelman.diag(mcmc_list)








#### CRIME ANALYSIS ####

#### SOURCE DATA FRAMES AND VECTORS OF CRIME DATA ####
# Source the file that contains the crime data
source("data_crime_raw.R")

check_data <- function(data) {
  # Loop over columns
  for (col in names(data)) {
    # Check if the column is numeric
    if (!is.numeric(data[[col]])) {
      stop(paste("Column", col, "is not numeric."))
    }
    # Check for missing values
    if (anyNA(data[[col]])) {
      stop(paste("Column", col, "contains missing values."))
    }
  }
  # If no problems found, print a success message
  print("All columns are numeric and contain no missing values.")
}

# Does the data have any missing values?
check_data(df_stand)

# Standardise the predictors
df_st <- cbind(df[, 1], scale(df[, -1], center = TRUE, scale = TRUE))


# Check for outliers

# Define a function to detect outliers based on the IQR
# INPUTS:
#         data - the data frame containing the data
#         columns - the columns in which to look for outliers
#         factor - the factor to multiply the IQR by to find the bounds (default is 2)
# OUTPUT:
#         outlier_indices - indices of outliers.
# 
detect_outliers_iqr <- function(data, columns, factor = 2){
  
  # Initialize a vector to hold the indices of outlier rows
  outlier_indices <- c()
  
  # Loop over each specified column
  for(col in columns){
    
    # Calculate the first quartile (25th percentile)
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    
    # Calculate the third quartile (75th percentile)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    
    # Calculate the interquartile range (IQR)
    IQR <- Q3 - Q1
    
    # Calculate the lower bound for what will be considered an outlier
    lower_bound <- Q1 - factor * IQR
    
    # Calculate the upper bound for what will be considered an outlier
    upper_bound <- Q3 + factor * IQR
    
    # Identify the indices of rows where the column value is an outlier
    outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    
    # Add the indices of the outliers to the list of outlier indices
    outlier_indices <- c(outlier_indices, outliers)
  }
  
  # Return the unique outlier indices
  return(unique(outlier_indices))
}


# Call the detect_outliers_iqr function, passing the data frame and column names of the numeric columns.
outlier_indices <- detect_outliers_iqr(df_stand, names(df_stand))

# Print the number of outliers and their indices
cat(paste0("Number of outliers detected: ", length(outlier_indices)), "\n")
cat(paste0("Outlier indices: ", outlier_indices), "\n")

# Remove the outliers from the data frame by subsetting the data frame to exclude these rows.
# The negative sign before outlier_indices means "all rows EXCEPT these indices".
df_no_outliers <- df[-outlier_indices, ]




# Check for Multicollinearity
# Fit the regression model
model <- lm(y ~ ., data = df_stand)
car::vif(model)

# Find predictors with near zero variance
nzv <- nearZeroVar(df_stand, saveMetrics= TRUE)
print(nzv)




#### CRIME. LASSO PENALISED REGRESSION 'glmnet' ####

# Run the LASSO function and extract the selected coefficients
crime_lasso <- fit_lasso(data = df_stand)

# Remove function
rm(fit_crime_lasso)




#### CRIME. ELNET PENALISED REGRESSION 'glmnet' ####

# Run the elnet function and extract the selected coefficients
crime_elnet <- fit_crime_elnet(data = df_stand)

# Remove function
rm(fit_crime_elnet)




#### CRIME. XGBOOST ####

# Set the initial parameters for XGBoost

# Define an extensive grid for hyperparameter tuning
# This grid consists of multiple values for each parameter, allowing for more refined tuning
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),                  # Number of boosting rounds
  max_depth = c(3, 5, 7, 9),                  # Maximum depth of the trees
  eta = c(0.01, 0.1, 0.3),                    # Learning rate
  gamma = c(0, 0.1, 1),                       # Minimum loss reduction required
  colsample_bytree = c(0.6, 0.8, 1),          # Fraction of features to be randomly sampled for each tree
  min_child_weight = c(1, 3, 5),              # Minimum sum of instance weight needed in a leaf
  subsample = c(0.8, 1) ,                      # Fraction of observations to be randomly sampled for each tree
  objective = "reg:linear"                    # Specify the learning task and the corresponding learning objective
)

# Define cross-validation strategy
# This helps in assessing the model's performance in an unbiased way using a subset of the data
xgb_cv <- trainControl(
  method = "repeatedcv",     # Repeated cross-validation
  number = 5,                # Number of folds
  repeats = 3,               # Number of complete sets of folds to compute
  verboseIter = TRUE,        # Display training progress
  returnData = FALSE,        # Do not return the training data
  returnResamp = "all",      # Save all resampling scores
  allowParallel = TRUE,      # Allow parallel processing
)

# Run the function to extract XGBoost model and feature importances
#crime_xgboost <- fit_crime_xgboost(data = df_stand,
#                                   xgb_cv = xgb_cv, xgb_grid = xgb_grid)


#### CRIME. SPIKE AND SLAB PRIOR 'spikeslab' ####
crime_spikeslab_prior <- fit_crime_spikeslab_prior(data, 
                                                   bigp_smalln = FALSE, 
                                                   seed = 42)




#### CRIME. SPIKE AND SLAB LASSO 'SSLASSO' ####

# Call the function with the Crimes data
ssl_crime <- fit_crime_sslasso(data = df_stand)
#sslasso_crime

# The output contains coefficients, ever_selected, and plot
ssl_crime$coefficients
ssl_crime$ever_selected




#### CRIME. HORSESHOE PRIOR 'horseshoe' ####

# Run the functions an extract the results
# Truncated Cauchy prior
crime_horseshoe_t_model <- fit_crime_horseshoe_model(data = df_stand,
                                                     method.tau = "truncatedCauchy",
                                                     tau = 10,
                                                     method.sigma = "Jeffreys", 
                                                     burn = 1000, 
                                                     nmc = 5000, 
                                                     thin = 10, 
                                                     alpha = 0.05)


# Half Cauchy prior
crime_horseshoe_t_model <- fit_crime_horseshoe_model(data = df_stand,
                                                     method.tau = "halfCauchy",
                                                     tau = 10,
                                                     method.sigma = "Jeffreys", 
                                                     burn = 1000, 
                                                     nmc = 5000, 
                                                     thin = 10, 
                                                     alpha = 0.05)




#### CRIME. HORSESHOE PRIOR 'bayesreg' ####

# Fit the model
hs_bs_crime <- fit_horseshoe_bs_crime(data = df_stand, prior = "hs")




#### CRIME. HORSESHOE + PRIOR 'bayesreg' ####

# Fit the model
hsp_bs_crime <- fit_horseshoe_bs_crime(data = df_stand, prior = "hs+")




#### CRIME. SSS 'BayesS5' ####

# Fit the model
S5_crime <- fit_S5_crime(data = df_stand)# %>% results()

# Print the MAP model
print(result(S5_crime)$hppm) 
print(result(S5_crime)$hppm.prob) # the posterior probability of the hppm
plot(result(S5_crime)$marg.prob, ylim = c(0, 1), ylab = "marginal inclusion probability")




#### CRIME. BAYESIAN LASSO 'monomvn' ####

# Fit the model
blasso_crime <- fit_blasso_crime(data = df_stand)
blasso_crime %>% plot()

## summarize the beta (regression coefficients) estimates
plot(blasso_crime, burnin=100)
points(drop(blasso_crime$b), col=2, pch=20)
points(drop(blasso_crime$b), col=3, pch=18)
legend("topleft", c("blasso-map", "lasso", "lsr"),
       col=c(2,2,3), pch=c(21,20,18))

# Plot the density of the coefficient values
dev.new(width=18, height=10)
plot(colMeans(blasso_crime$beta))




#### CRIME. RJMCMC 'rjmcmc' ####




