#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "glmnet", "cowplot", "car", "MASS", 
                  "caret", "spikeslab", "SSLASSO", "horseshoe", "bayesreg", 
                  "Hmisc", "BayesS5", "monomvn", "gridExtra", "ggpubr")

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
# Initialize an empty list to store the models
models_xgboost_list <- list()

# Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_xgboost(dataset)
    
    # Generate a model name
    model_name <- paste(prefix, suffix, "xgboost", sep = "_")
    
    # Store the model in the list
    models_xgboost_list[[model_name]] <- model
  }
}

#### SIM DATA. SPIKE AND SLAB PRIOR 'spikeslab' #### 

# Extract the selected variables
# T1 data
ssp_T1_LD <- fit_spikeslab_prior(data = T1_LD, bigp_smalln = FALSE)
ssp_T1_ED <- fit_spikeslab_prior(data = T1_ED, bigp_smalln = FALSE)
ssp_T1_HD <- fit_spikeslab_prior(data = T1_HD, bigp_smalln = FALSE)
ssp_T1_VD <- fit_spikeslab_prior(data = T1_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T2 data
ssp_T2_LD <- fit_spikeslab_prior(data = T2_LD, bigp_smalln = FALSE)
ssp_T2_ED <- fit_spikeslab_prior(data = T2_ED, bigp_smalln = FALSE)
ssp_T2_HD <- fit_spikeslab_prior(data = T2_HD, bigp_smalln = FALSE)
ssp_T2_VD <- fit_spikeslab_prior(data = T2_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T3 data
ssp_T3_LD <- fit_spikeslab_prior(data = T3_LD, bigp_smalln = FALSE)
ssp_T3_ED <- fit_spikeslab_prior(data = T3_ED, bigp_smalln = FALSE)
ssp_T3_HD <- fit_spikeslab_prior(data = T3_HD, bigp_smalln = FALSE)
ssp_T3_VD <- fit_spikeslab_prior(data = T3_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

# T4 data
ssp_T4_LD <- fit_spikeslab_prior(data = T4_LD, bigp_smalln = FALSE)
ssp_T4_ED <- fit_spikeslab_prior(data = T4_ED, bigp_smalln = FALSE)
ssp_T4_HD <- fit_spikeslab_prior(data = T4_HD, bigp_smalln = FALSE)
ssp_T4_VD <- fit_spikeslab_prior(data = T4_VD, bigp_smalln = TRUE, 
                                 bigp_smalln_factor = 1, screen = TRUE)

#### SIM DATA. SPIKE-AND-SLAB LASSO 'SSLASSO' ####

# List of dataset prefixes
prefixes_ss <- c("T1", "T2")

# Call the function with the simulated data
# Initialize an empty list to store the models
models_sslasso_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes_ss) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_sslasso(dataset)
    
    # Generate a model name
    model_name <- paste("ssl", dataset_name, sep = "_")
    
    # Store the model in the list
    models_sslasso_list[[model_name]] <- model
  }
}

#### SIM DATA. HORSESHOE PRIOR. 'horseshoe' ####

# Fit the models with Truncated Cauchy priors
# Call the function with the simulated data
# Initialize an empty list to store the models
models_hs_tc_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_hs_horseshoe(dataset, method.tau = "truncatedCauchy")
    
    # Generate a model name
    model_name <- paste("hs_tc", dataset_name, sep = "_")
    
    # Store the model in the list
    models_hs_tc_list[[model_name]] <- model
  }
}

# Fit the models with Half Cauchy priors
# Call the function with the simulated data
# Initialize an empty list to store the models
models_hs_hc_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_hs_horseshoe(dataset, method.tau = "halfCauchy")
    
    # Generate a model name
    model_name <- paste("hs_tc", dataset_name, sep = "_")
    
    # Store the model in the list
    models_hs_hc_list[[model_name]] <- model
  }
}

#### SIM DATA. HORSESHOE PRIOR 'bayesreg' ####

# Fit the models with horseshoe prior
# Call the function with the simulated data
# Initialize an empty list to store the models
models_bs_h_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_horseshoe_bs(dataset)
    
    # Generate a model name
    model_name <- paste("bs_h", dataset_name, sep = "_")
    
    # Store the model in the list
    models_bs_h_list[[model_name]] <- model
  }
}

# Fit the models with horseshoe prior
# Call the function with the simulated data
# Initialize an empty list to store the models
models_bs_hp_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_horseshoe_bs(dataset, prior = "hs+")
    
    # Generate a model name
    model_name <- paste("bs_hp", dataset_name, sep = "_")
    
    # Store the model in the list
    models_bs_hp_list[[model_name]] <- model
  }
}

#### SIM DATA. S5 'BayesS5' ####

# Set prefices
prefixes_s5 <- c("T1", "T2")

# Fit the model with data with only continuous variables
# Call the function with the simulated data
# Initialize an empty list to store the models
models_s5c_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes_s5) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_S5(dataset)
    
    # Generate a model name
    model_name <- paste("s5c", dataset_name, sep = "_")
    
    # Store the model in the list
    models_s5c_list[[model_name]] <- model
  }
}

#### SIM DATA. BAYESIAN LASSO 'monomvn' ####

# Fit the model with data with only continuous variables
# Call the function with the simulated data
# Initialize an empty list to store the models
models_blasso_list <- list()

#   Loop over prefixes and suffixes
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # Construct the dataset name
    dataset_name <- paste(prefix, suffix, sep = "_")
    
    # Access the dataset from the global environment
    dataset <- get(dataset_name)
    
    # Fit the model
    model <- fit_blasso(dataset)
    
    # Generate a model name
    model_name <- paste("blasso", dataset_name, sep = "_")
    
    # Store the model in the list
    models_blasso_list[[model_name]] <- model
  }
}

#### SOURCE CRIME DATA ####

# Source the file that contains the crime data
source("data_crime_raw.R")

#### CRIME. PENALISED REGRESSION 'glmnet' ####

# Run the LASSO function and extract the selected coefficients
crime_lasso <- fit_glmnet(data = df_t, alpha = 1)

# Run the elnet function and extract the selected coefficients
crime_elnet <- fit_glmnet(data = df_t, alpha = 0.5)

#### CRIME. XGBOOST ####

# Initial parameters for xgboost were already defined
# Run the function to extract XGBoost model and feature importances
crime_xgboost <- fit_xgb(data = df_t, xgb_cv = xgb_cv, xgb_grid = xgb_grid)

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
                                         method.sigma = "Jeffreys", burn = 1000, 
                                         nmc = 5000, thin = 1, alpha = 0.05)

# Half Cauchy prior
crime_horseshoe_hc <- fit_hs_horseshoe(data = df_t, method.tau = "halfCauchy",
                                             method.sigma = "Jeffreys", 
                                             burn = 1000, nmc = 5000, 
                                             thin = 1, alpha = 0.05)

#### CRIME. HORSESHOE AND PLUS PRIOR 'bayesreg' ####

# Fit the model with "hs" prior
crime_hs_bs <- fit_horseshoe_bs(data = df_t, n.samples = 5000, burnin = 1000, 
                                prior = "hs")


# Fit the model with "hs+" prior
crime_hsp_bs <- fit_horseshoe_bs(data = df_t, n.samples = 5000, burnin = 1000, 
                                 prior = "hs+")

#### CRIME. S5 'BayesS5' ####

# Fit the model
crime_S5 <- fit_S5(data = df_t)

#### CRIME. BAYESIAN LASSO 'monomvn' ####

# Fit the model
crime_blasso <- fit_blasso(data = df_t)

