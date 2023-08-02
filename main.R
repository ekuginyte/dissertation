#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "glmnet", "cowplot", "car", "MASS", 
                  "caret", "spikeslab", "SSLASSO", "horseshoe", "bayesreg", 
                  "Hmisc", "BayesS5", "monomvn", "gridExtra", "ggpubr")

#ipgrah, R6, xgboost, kableExtra, knitr

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
# Initialize an empty list to store the models
models_list_glmnet <- list()

# Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model for each alpha value (0.5 and 1)
    for (alpha in c(0.5, 1)) {
      # Fit the model
      model <- fit_glmnet(dataset, alpha = alpha)
      
      # Generate a model name
      model_name <- paste(prefix, suffix, ifelse(alpha == 0.5, "elnet", "lasso"), sep = "_")
      
      # Store the model in the list
      models_list_glmnet[[model_name]] <- model
    }
  }
}

#### SIM DATA. XGBOOST 'caret' ####

# Set the initial parameters for XGBoost
# Define an extensive grid for hyperparameter tuning
# This grid consists of multiple values for each parameter, allowing for more refined tuning
xgb_grid <- expand.grid(
  # Number of boosting rounds
  nrounds = c(50, 100, 150),
  # Maximum depth of the trees
  max_depth = c(3, 5, 7, 9),
  # Learning rate
  eta = c(0.01, 0.1, 0.3),
  # Minimum loss reduction required
  gamma = c(0, 0.1, 1),
  # Fraction of features to be randomly sampled for each tree
  colsample_bytree = c(0.6, 0.8, 1),
  # Minimum sum of instance weight needed in a leaf
  min_child_weight = c(1, 3, 5),
  # Fraction of observations to be randomly sampled for each tree
  subsample = c(0.8, 1))


# Define cross-validation strategy
# This helps in assessing the model's performance in an unbiased way using a subset of the data
xgb_cv <- trainControl(
  # Repeated cross-validation
  method = "repeatedcv",
  # Number of folds
  number = 5,
  # Number of complete sets of folds to compute
  repeats = 3,
  # Display training progress
  verboseIter = TRUE,
  # Do not return the training data
  returnData = FALSE,
  # Save all resampling scores
  returnResamp = "all",
  # Allow parallel processing
  allowParallel = TRUE)

# Fit the function
# Prefixes
prefixesx <- c("T1", "T2", "T3", "T4")
# List of dataset suffixes
suffixesx <- c("LD", "ED", "HD", "VD", "XD")

# Initialize an empty list to store the models
models_xgboost_list <- list()

# Loop over prefixes and suffixes
for (prefix in prefixesx) {
  for (suffix in suffixesx) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_xgb(data = dataset, xgb_cv = xgb_cv, xgb_grid = xgb_grid)
    
    # Generate a model name
    model_name <- paste(dataset_name, "xgboost", sep = "_")
    
    # Store the model in the global environment
    assign(model_name, model)
    
    # Store the model in the list
    models_xgboost_list[[model_name]] <- model
    
    # Save the model as RDS
    saveRDS(model, file = paste0("~/Downloads/", model_name, ".rds"))
  }
}

#### SIM DATA. SPIKE AND SLAB PRIOR 'spikeslab' #### 

# Extract the selected variables
# T1 data
ssp_T1_LD <- fit_spikeslab_prior(data = T1_LD, bigp_smalln = FALSE)
ssp_T1_ED <- fit_spikeslab_prior(data = T1_ED, bigp_smalln = TRUE,
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_HD <- fit_spikeslab_prior(data = T1_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_VD <- fit_spikeslab_prior(data = T1_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T2 data
ssp_T2_LD <- fit_spikeslab_prior(data = T2_LD, bigp_smalln = FALSE)
ssp_T2_ED <- fit_spikeslab_prior(data = T2_ED, bigp_smalln = TRUE,
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_HD <- fit_spikeslab_prior(data = T2_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_VD <- fit_spikeslab_prior(data = T2_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T3 data
ssp_T3_LD <- fit_spikeslab_prior(data = T3_LD, bigp_smalln = FALSE)
ssp_T3_ED <- fit_spikeslab_prior(data = T3_ED, bigp_smalln = TRUE,
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_HD <- fit_spikeslab_prior(data = T3_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_VD <- fit_spikeslab_prior(data = T3_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T4 data
ssp_T4_LD <- fit_spikeslab_prior(data = T4_LD, bigp_smalln = FALSE)
ssp_T4_ED <- fit_spikeslab_prior(data = T4_ED, bigp_smalln = TRUE,
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_HD <- fit_spikeslab_prior(data = T4_HD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_VD <- fit_spikeslab_prior(data = T4_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

#### SIM DATA. SPIKE-AND-SLAB LASSO 'SSLASSO' ####

# Call the function with the simulated data
#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_sslasso(dataset)
    
    # Generate a model name
    model_name <- paste("ssl", dataset_name, sep = "_")
    
    # Store the model in the global environment
    assign(model_name, model)
    
    # Save the model as RDS
    saveRDS(model, file = paste0("~/Downloads/", model_name, ".rds"))
  }
}

ssl_T1_ED$selected_variable_names

#### SIM DATA. HORSESHOE PRIOR. 'horseshoe' ####

# Fit the models with Truncated Cauchy priors
# T1 Data
hshst_T1_LD <- fit_hs_horseshoe(data = T1_LD, method.tau = "truncatedCauchy")
hshst_T1_ED <- fit_hs_horseshoe(data = T1_ED, method.tau = "truncatedCauchy")
hshst_T1_HD <- fit_hs_horseshoe(data = T1_HD, method.tau = "truncatedCauchy")
hshst_T1_VD <- fit_hs_horseshoe(data = T1_VD, method.tau = "truncatedCauchy")

# T2 Data
hshst_T2_LD <- fit_hs_horseshoe(data = T2_LD, method.tau = "truncatedCauchy")
hshst_T2_ED <- fit_hs_horseshoe(data = T2_ED, method.tau = "truncatedCauchy")
hshst_T2_HD <- fit_hs_horseshoe(data = T2_HD, method.tau = "truncatedCauchy")
hshst_T2_VD <- fit_hs_horseshoe(data = T2_VD, method.tau = "truncatedCauchy")

# T3 Data
hshst_T3_LD <- fit_hs_horseshoe(data = T3_LD, method.tau = "truncatedCauchy")
hshst_T3_ED <- fit_hs_horseshoe(data = T3_ED, method.tau = "truncatedCauchy")
hshst_T3_HD <- fit_hs_horseshoe(data = T3_HD, method.tau = "truncatedCauchy")
hshst_T3_VD <- fit_hs_horseshoe(data = T3_VD, method.tau = "truncatedCauchy")

# T4 Data
hshst_T4_LD <- fit_hs_horseshoe(data = T4_LD, method.tau = "truncatedCauchy")
hshst_T4_ED <- fit_hs_horseshoe(data = T4_ED, method.tau = "truncatedCauchy")
hshst_T4_HD <- fit_hs_horseshoe(data = T4_HD, method.tau = "truncatedCauchy")
hshst_T4_VD <- fit_hs_horseshoe(data = T4_VD, method.tau = "truncatedCauchy")

hshst_T4_VD$sel_var
HS.var.select(hshst_T1_VD$model$BetaMedian, y, method = "intervals")

# Fit the models with Half Cauchy priors
# T1 Data
hshsh_T1_LD <- fit_hs_horseshoe(data = T1_LD, method.tau = "halfCauchy")
hshsh_T1_ED <- fit_hs_horseshoe(data = T1_ED, method.tau = "halfCauchy")
hshsh_T1_HD <- fit_hs_horseshoe(data = T1_HD, method.tau = "halfCauchy")
hshsh_T1_VD <- fit_hs_horseshoe(data = T1_VD, method.tau = "halfCauchy")

# T2 Data
hshsh_T2_LD <- fit_hs_horseshoe(data = T2_LD, method.tau = "halfCauchy")
hshsh_T2_ED <- fit_hs_horseshoe(data = T2_ED, method.tau = "halfCauchy")
hshsh_T2_HD <- fit_hs_horseshoe(data = T2_HD, method.tau = "halfCauchy")
hshsh_T2_VD <- fit_hs_horseshoe(data = T2_VD, method.tau = "halfCauchy")

# T3 Data
hshsh_T3_LD <- fit_hs_horseshoe(data = T3_LD, method.tau = "halfCauchy")
hshsh_T3_ED <- fit_hs_horseshoe(data = T3_ED, method.tau = "halfCauchy")
hshsh_T3_HD <- fit_hs_horseshoe(data = T3_HD, method.tau = "halfCauchy")
hshsh_T3_VD <- fit_hs_horseshoe(data = T3_VD, method.tau = "halfCauchy")

# T4 Data
hshsh_T4_LD <- fit_hs_horseshoe(data = T4_LD, method.tau = "halfCauchy")
hshsh_T4_ED <- fit_hs_horseshoe(data = T4_ED, method.tau = "halfCauchy")
hshsh_T4_HD <- fit_hs_horseshoe(data = T4_HD, method.tau = "halfCauchy")
hshsh_T4_VD <- fit_hs_horseshoe(data = T4_VD, method.tau = "halfCauchy")

hshsh_T4_VD$sel_var

#### SIM DATA. HORSESHOE PRIOR 'bayesreg' ####

# Fit the model
# T1 Data
hs_bs_T1_LD <- fit_horseshoe_bs(data = T1_LD)
hs_bs_T1_ED <- fit_horseshoe_bs(data = T1_ED)
hs_bs_T1_HD <- fit_horseshoe_bs(data = T1_HD)
hs_bs_T1_VD <- fit_horseshoe_bs(data = T1_VD)

# T2 Data
hs_bs_T2_LD <- fit_horseshoe_bs(data = T2_LD)
hs_bs_T2_ED <- fit_horseshoe_bs(data = T2_ED)
hs_bs_T2_HD <- fit_horseshoe_bs(data = T2_HD)
hs_bs_T2_VD <- fit_horseshoe_bs(data = T2_VD)

# T3 Data
hs_bs_T3_LD <- fit_horseshoe_bs(data = T3_LD)
hs_bs_T3_ED <- fit_horseshoe_bs(data = T3_ED)
hs_bs_T3_HD <- fit_horseshoe_bs(data = T3_HD)
hs_bs_T3_VD <- fit_horseshoe_bs(data = T3_VD)

# T4 Data
hs_bs_T4_LD <- fit_horseshoe_bs(data = T4_LD)
hs_bs_T4_ED <- fit_horseshoe_bs(data = T4_ED)
hs_bs_T4_HD <- fit_horseshoe_bs(data = T4_HD)
hs_bs_T4_VD <- fit_horseshoe_bs(data = T4_VD)

#### SIM DATA. HORSESHOE + PRIOR 'bayesreg' ####

# Fit the model
# T1 Data
hsp_bs_T1_LD <- fit_horseshoe_bs(data = T1_LD, prior = "hs+")
hsp_bs_T1_ED <- fit_horseshoe_bs(data = T1_ED, prior = "hs+")
hsp_bs_T1_HD <- fit_horseshoe_bs(data = T1_HD, prior = "hs+")
hsp_bs_T1_VD <- fit_horseshoe_bs(data = T1_VD, prior = "hs+")

# T2 Data
hsp_bs_T2_LD <- fit_horseshoe_bs(data = T2_LD, prior = "hs+")
hsp_bs_T2_ED <- fit_horseshoe_bs(data = T2_ED, prior = "hs+")
hsp_bs_T2_HD <- fit_horseshoe_bs(data = T2_HD, prior = "hs+")
hsp_bs_T2_VD <- fit_horseshoe_bs(data = T2_VD, prior = "hs+")

# T3 Data
hsp_bs_T3_LD <- fit_horseshoe_bs(data = T3_LD, prior = "hs+")
hsp_bs_T3_ED <- fit_horseshoe_bs(data = T3_ED, prior = "hs+")
hsp_bs_T3_HD <- fit_horseshoe_bs(data = T3_HD, prior = "hs+")
hsp_bs_T3_VD <- fit_horseshoe_bs(data = T3_VD, prior = "hs+")

# T4 Data
hsp_bs_T4_LD <- fit_horseshoe_bs(data = T4_LD, prior = "hs+")
hsp_bs_T4_ED <- fit_horseshoe_bs(data = T4_ED, prior = "hs+")
hsp_bs_T4_HD <- fit_horseshoe_bs(data = T4_HD, prior = "hs+")
hsp_bs_T4_VD <- fit_horseshoe_bs(data = T4_VD, prior = "hs+")


hsp_bs_T4_VD[["rank"]]

#### SIM DATA. SSS WITH SCREENING 'BayesS5' ####

# Fit the model
# T1 Data
S5_T1_LD <- fit_S5(data = T1_LD)
S5_T1_ED <- fit_S5(data = T1_ED)
S5_T1_HD <- fit_S5(data = T1_HD)
S5_T1_VD <- fit_S5(data = T1_VD)

# T2 Data
S5_T2_LD <- fit_S5(data = T2_LD)
S5_T2_ED <- fit_S5(data = T2_ED)
S5_T2_HD <- fit_S5(data = T2_HD)
S5_T2_VD <- fit_S5(data = T2_VD)

# T3 Data
S5_T3_LD <- fit_S5(data = T3_LD, has_binary = TRUE)
S5_T3_ED <- fit_S5(data = T3_ED, has_binary = TRUE)
S5_T3_HD <- fit_S5(data = T3_HD, has_binary = TRUE)
S5_T3_VD <- fit_S5(data = T3_VD, has_binary = TRUE)

# T4 Data
S5_T4_LD <- fit_S5(data = T4_LD, has_binary = TRUE)
S5_T4_ED <- fit_S5(data = T4_ED, has_binary = TRUE)
S5_T4_HD <- fit_S5(data = T4_HD, has_binary = TRUE)
S5_T4_VD <- fit_S5(data = T4_VD, has_binary = TRUE)

#### SIM DATA. BAYESIAN LASSO 'monomvn' ####

# T1 Data
blasso_T1_LD <- fit_blasso(data = T1_LD)
blasso_T1_ED <- fit_blasso(data = T1_ED)
blasso_T1_HD <- fit_blasso(data = T1_HD)
blasso_T1_VD <- fit_blasso(data = T1_VD)

# T2 Data
blasso_T2_LD <- fit_blasso(data = T2_LD)
blasso_T2_ED <- fit_blasso(data = T2_ED)
blasso_T2_HD <- fit_blasso(data = T2_HD)
blasso_T2_VD <- fit_blasso(data = T2_VD)

# T3 Data
blasso_T3_LD <- fit_blasso(data = T3_LD)
blasso_T3_ED <- fit_blasso(data = T3_ED)
blasso_T3_HD <- fit_blasso(data = T3_HD)
blasso_T3_VD <- fit_blasso(data = T3_VD)

# T4 Data
blasso_T4_LD <- fit_blasso(data = T4_LD)
blasso_T4_ED <- fit_blasso(data = T4_ED)
blasso_T4_HD <- fit_blasso(data = T4_HD)
blasso_T4_VD <- fit_blasso(data = T4_VD)

blasso_T4_VD$sel_var

#### SOURCE CRIME DATA ####

# Source the file that contains the crime data
source("data_crime_raw.R")

#### CRIME. LASSO PENALISED REGRESSION 'glmnet' ####

# Run the LASSO function and extract the selected coefficients
crime_lasso <- fit_glmnet(data = df_t, alpha = 1)

#### CRIME. ELNET PENALISED REGRESSION 'glmnet' ####

# Run the elnet function and extract the selected coefficients
crime_elnet <- fit_glmnet(data = df_t, alpha = 0.5)

#### CRIME. XGBOOST ####

# Initial parameters for xgboost were already defined

# Run the function to extract XGBoost model and feature importances
crime_xgboost <- fit_xgb(data = df_t, xgb_cv = xgb_cv, xgb_grid = xgb_grid)

# Save the model object to a RDS file
saveRDS(crime_xgboost, file = "~Downloads/crime_xgboost.rds")

#### CRIME. SPIKE AND SLAB PRIOR 'spikeslab' ####

# Fit spikeslab model
crime_spikeslab_prior <- fit_spikeslab_prior(data = df_t, bigp_smalln = FALSE)

#### CRIME. SPIKE AND SLAB LASSO 'SSLASSO' ####

# Call the function with the Crimes data
crime_ssl <- fit_sslasso(data = df_t, var = "fixed")

#### CRIME. HORSESHOE PRIOR 'horseshoe' ####

# Run the functions an extract the results
# Truncated Cauchy prior
crime_horseshoe_tc <- fit_hs_horseshoe(data = df_t, method.tau = "truncatedCauchy",
                                         method.sigma = "Jeffreys", burn = 5000, 
                                         nmc = 10000, thin = 1, alpha = 0.05)

# Half Cauchy prior
crime_horseshoe_hc <- fit_hs_horseshoe(data = df_t, method.tau = "halfCauchy",
                                             method.sigma = "Jeffreys", 
                                             burn = 5000, nmc = 10000, 
                                             thin = 1, alpha = 0.05)

#### CRIME. HORSESHOE PRIOR 'bayesreg' ####

# Fit the model
hs_bs_crime <- fit_horseshoe_bs(data = df_t, prior = "hs")

#### CRIME. HORSESHOE + PRIOR 'bayesreg' ####

# Fit the model
hsp_bs_crime <- fit_horseshoe_bs(data = df_t, prior = "hs+")

#### CRIME. SSS 'BayesS5' ####

# Fit the model
S5_crime <- fit_S5(data = df_t)

# Convert to a data frame
S5_crime_df <- data.frame(Variable = names(S5_crime$marg.prob), 
                 Probability = S5_crime$marg.prob)
# Create the plot
ggplot(S5_crime_df, aes(x = reorder(Variable, Probability), y = Probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Variable", y = "Marginal Inclusion Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()


#### CRIME. BAYESIAN LASSO 'monomvn' ####

# Fit the model
blasso_crime <- fit_blasso(data = df_t)

saveRDS(blasso_crime, file = "~/Downloads/blasso/blasso_crime.rds")
