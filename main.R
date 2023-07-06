#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "betareg", "R.matlab", "glmnet", "dplyr", 
                  "cowplot", "coda", "igraph", "R6", "nimble", "MASS", "xgboost",
                  "caret", "spikeslab", "SSLASSO", "horseshoe", "bayesreg", "Hmisc",
                  "LaplacesDemon", "BayesS5", "monomvn")

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





#### SIM DATA. LASSO PENALISED REGRESSION 'glment' ####

# Function to fit Lasso regression on different datasets and extract the selected predictors
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
# OUTPUT:
#         selected_predictors - selected predictor data frame with coefficients.
#
fit_lasso <- function(data, nfolds = 10) {
  
  # Create the matrix of predictors by excluding the intercept term, 
  #   and standardise the predictors. Add an intercept column with all values equal to 1.
  xmatrix <- cbind(Intercept = 1, scale(model.matrix(~ . - 1, data = data[, -1])))
  
  # Fit the Lasso model on the design matrix with alpha = 1 (Lasso penalty)
  initial_lasso <- glmnet(x = xmatrix, y = data$y, alpha = 1)
  
  # Perform k-fold cross-validation to find the optimal value of the regularisation 
  #   parameter lambda that minimizes the cross-validation error
  lasso_cv <- cv.glmnet(x = xmatrix, y = data$y, alpha = 1, nfolds = nfolds)
  
  # Plot the regularisation path which is the coefficient profiles of the Lasso model
  #   as a function of lambda. The vertical line represents the optimal lambda value 
  #   that minimizes the cross-validation error.
  #par(mfrow = c(1, 2))
  #plot(initial_lasso, xvar = "lambda")
  #abline(v = log(initial_lasso$lambda.min), lwd = 4, lty = 2)
  
  # Plot the cross-validation errors as a function of lambda.
  plot(lasso_cv)
  abline(v = log(lasso_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Lasso model using the optimal lambda value obtained from cross-validation
  lasso_model <- glmnet(x = xmatrix, y = data$y, alpha = 1, lambda = lasso_cv$lambda.min)
  
  # Extract the coefficients from the lasso model
  coefficients <- coef(lasso_model, s = lasso_cv$lambda.min)
  
  # Find the names of the variables with non-zero coefficients
  selected_variable_names <- rownames(coefficients)[coefficients[, 1] != 0]
  
  # Extract the non-zero coefficients
  selected_predictors <- coefficients[selected_variable_names, 1] %>% data.frame()
  
  # Return the selected predictors
  return(selected_predictors)
}




#### SIM DATA. ELASTIC-NET PENALISED REGRESSION 'glmnet' ####

# Function to fit Elastic net regression on different datasets and extract the best-fitted model.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
# OUTPUT:
#         selected_predictors - selected predictor data frame with coefficients.
#
fit_elnet <- function(data, nfolds = 10) {
  
  # Create the matrix of predictors by excluding the intercept term, 
  # and standardise the predictors. Add an intercept column with all values equal to 1.
  xmatrix <- cbind(Intercept = 1, scale(model.matrix(~ . - 1, data = data[, -1])))
  
  # Fit the Elastic net model on the design matrix with alpha = 0.5 (L1 + L2 penalties)
  initial_elnet <- glmnet(x = xmatrix, y = data$y, alpha = 0.5)
  
  # Perform k-fold cross-validation to find the optimal value of the regularization 
  # parameter lambda that minimizes the cross-validation error.
  elnet_cv <- cv.glmnet(x = xmatrix, y = data$y, alpha = 0.5, nfolds = nfolds)
  
  # Plot the regularisation path which is the coefficient profiles of the Elastic net model
  # as a function of lambda. The vertical line represents the optimal lambda value 
  # that minimises the cross-validation error.
  #par(mfrow = c(1, 2))
  #plot(initial_elnet, xvar = "lambda")
  #abline(v = log(initial_elnet$lambda.min), lwd = 4, lty = 2)
  
  # Plot the cross-validation errors as a function of lambda.
  plot(elnet_cv)
  abline(v = log(elnet_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Elastic net model using the optimal lambda value obtained from cross-validation.
  elnet_model <- glmnet(x = xmatrix, y = data$y, alpha = 1, lambda = elnet_cv$lambda.min)
  
  # Extract the coefficients from the lasso model
  coefficients <- coef(elnet_model, s = elnet_cv$lambda.min)
  
  # Find the names of the variables with non-zero coefficients
  selected_variable_names <- rownames(coefficients)[coefficients[, 1] != 0]
  
  # Extract the non-zero coefficients
  selected_predictors <- coefficients[selected_variable_names, 1] %>% data.frame()
  
  # Return the selected predictors
  return(selected_predictors)
}




#### SIM DATA. LASSO AND ELASTIC NET FITTING 'glmnet' ####

# Extract BOTH the Lasso and Elastic net penalisation models using all types 
#   of simulated data 

# List of dataset prefixes
prefixes <- c("T1", "T2", "T3", "T4")
# List of dataset suffixes
suffixes <- c("LD", "ED", "HD", "VD")
# List of methods ("_lasso" or "_elnet")
methods <- c("_lasso", "_elnet")

# Loop through prefixes, suffixes and methods
for (prefix in prefixes) {
  for (suffix in suffixes) {
    # construct the data variable name
    data_var_name <- paste0(prefix, "_", suffix)
    data_var <- get(data_var_name)
    
    # construct the model variable names for lasso and elastic net
    for (method in methods) {
      model_var_name <- paste0(prefix, "_", suffix, method)
      if (method == "_lasso") {
        assign(model_var_name, fit_lasso(data_var))
      } else if (method == "_elnet") {
        assign(model_var_name, fit_elnet(data_var))
      }
    }
  }
}

# Remove the functions and prefixes, suffixes, methods as they won't be used anymore
rm(fit_elnet, fit_lasso, prefixes, suffixes, methods, 
   data_var_name, data_var, model_var_name, prefix, suffix, method)
















#### SIM DATA. AIC ####

# Fit a full model with all predictors
full_model <- lm(y ~ ., data = T1_LD)

# Perform stepwise selection
selected_model <- step(full_model, direction = "both")

# Display the selected model
summary(selected_model)











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


# Function to train and evaluate an XGBoost model from 'caret' package on different 
#   datasets and plot feature importance
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#                The response variable should be named "y".
#         xgb_cv   - a trainControl object defining the cross-validation strategy.
#         xgb_grid - a data frame defining the grid of hyperparameters to search over.
# OUTPUT:
#         A list containing:
#               model - The trained XGBoost model.
#               rmse  - The root mean squared error (RMSE) of the model on the test set.
#
train_evaluate_xgb <- function(data, xgb_cv, xgb_grid) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data should be a data frame.")
  }
  
  if (!"y" %in% names(data)) {
    stop("data should contain a column named 'y' as the response variable.")
  }
  
  if (!is.list(xgb_cv) || !("method" %in% names(xgb_cv))) {
    stop("xgb_cv should be a trainControl object with a specified method.")
  }
  
  if (!is.data.frame(xgb_grid) || !all(c("nrounds", "max_depth", "eta") %in% names(xgb_grid))) {
    stop("xgb_grid should be a data frame with hyperparameters to be tuned, including nrounds, max_depth, and eta.")
  }
  
  # Separate features and target from the dataset
  # Features (exclude the target variable 'y')
  X <- as.matrix(data[, -1])
  # Target variable
  y <- data$y
  
  # Split the dataset into training and testing sets
  # createDataPartition helps in creating stratified random samples
  index <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[index,]           # Extract training features
  y_train <- y[index]            # Extract training target
  X_test <- X[-index,]           # Extract testing features
  y_test <- y[-index]            # Extract testing target
  
  # Train the XGBoost model with cross-validation and parameter tuning
  xgb_model <- train(
    x = X_train,               # Feature matrix
    y = y_train,               # Target vector
    trControl = cv,            # Cross-validation strategy
    tuneGrid = grid,           # Grid of hyperparameters to tune
    method = "xgbTree"         # XGBoost model
  )
  
  # Make predictions on the test set using the trained model
  predictions <- predict(xgb_model, X_test)
  
  # Calculate the Root Mean Squared Error (RMSE) on the test set
  # RMSE is a measure of the differences between predicted and actual values
  rmse <- sqrt(mean((predictions - y_test)^2))
  cat("Root Mean Squared Error on Test Set:", rmse, "\n")
  
  # Extract feature importance from the trained model
  # Feature importance helps in understanding which features are most influential in making predictions
  importance_matrix <- xgb.importance(feature_names = colnames(X), model = xgb_model$finalModel)
  
  # Plot feature importance
  # This helps in visualizing the importance of each feature in the model
  xgb.plot.importance(importance_matrix)
  
  # Return the model
  return(xgb_model)
}

# List of all datasets
datasets <- list(T1_LD, T1_ED, T1_HD, T1_VD, T1_XD, 
                 T2_LD, T2_ED, T2_HD, T2_VD, T2_XD, 
                 T3_LD, T3_ED, T3_HD, T3_VD, T3_XD, 
                 T4_LD, T4_ED, T4_HD, T4_VD, T4_XD)

# Loop through each dataset and train the model
#for (data in datasets) {
#  train_evaluate_xgb(data, cv, grid)
#}





















#### SIM DATA. SPIKE AND SLAB PRIOR 'spikeslab' #### 

# Function to fit a Spike and Slab prior model using 'spikeslab' package and 
#   evaluate it on different datasets. It also plots the path of the 
#   estimates for the Spike and Slab model.
#
# INPUTS:
#     data               - A data frame containing the predictors and the response variable.
#                          The response variable should be named "y".
#     bigp_smalln        - A logical indicating if the high-dimensional low sample size adjustments
#                          should be made. Should be either TRUE or FALSE.
#     bigp_smalln_factor - A numeric adjustment factor to be used when bigp.smalln is TRUE.
#     seed               - An integer used for setting the seed for reproducibility. Default is 42.
#
# OUTPUT:
#     ss_results - The fitted Spike and Slab model.
#
fit_spikeslab_prior <- function(data, bigp_smalln, bigp_smalln_factor = 0, screen = FALSE,
                                K = 10, seed = 42) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data should be a data frame.")
  }
  
  if (!"y" %in% names(data)) {
    stop("data should contain a column named 'y' as the response variable.")
  }
  
  if (!is.logical(bigp_smalln) || length(bigp_smalln) != 1) {
    stop("bigp_smalln should be a logical value (either TRUE or FALSE).")
  }
  
  if (!is.numeric(bigp_smalln_factor) || length(bigp_smalln_factor) != 1) {
    stop("bigp_smalln_factor should be a single numeric value.")
  }
  
  if (!is.logical(screen) || length(screen) != 1) {
    stop("screen should be a logical value (either TRUE or FALSE).")
  }
  
  if (!is.numeric(seed) || length(seed) != 1 || seed < 0 || seed != as.integer(seed)) {
    stop("seed should be a single non-negative integer value.")
  }
  
  # Extract the response variable and predictors from the data
  y <- data$y
  x <- data[, !(names(data) %in% "y")]
  
  # Define the formula for the model
  # y ~ . indicates that y is the response variable and . represents all other variables as predictors
 # formula <- as.formula("y ~ .")
  
  # Run the spikeslab model
  ss_results <- spikeslab::cv.spikeslab(
            # Formula representing the relationship between predictors and response
            #formula,  
            # The dataset containing the variables in the formula
            #data = data,   
            x = x,
            y = y,
            K = K,
            # The number of iterations in the two MCMC chains used in spikeslab.
            # n.iter1 is for the first chain, and n.iter2 is for the second chain.
            n.iter1 = 1000,        
            n.iter2 = 1000,        
            # Calculate the mean squared error as part of the model evaluation
            mse = TRUE,           
            # High-dimensional low sample size adjustments.
            # bigp.smalln - logical flag, if TRUE adjustments for high-dimensional low sample size are made.
            # bigp.smalln.factor - controls the magnitude of the adjustments.
            bigp.smalln = bigp_smalln,                 
            bigp.smalln.factor = bigp_smalln_factor,   
            # To screen the variables when p is big
            screen = screen,
            # Random effects. If specified, adds random effects to the model
            r.effects = NULL,      
            # Maximum number of variables to be retained in the model
            max.var = 500,         
            # If TRUE, the predictors are centered by subtracting their means
            center = TRUE,         
            # If TRUE, an intercept term is included in the model
            intercept = TRUE,      
            # If TRUE, a fast approximation algorithm is used for speed up
            fast = TRUE,           
            # The number of blocks into which the coefficients are split for Gibbs sampling
            beta.blocks = 5,       
            # If TRUE, outputs progress and additional information while fitting the model
            verbose = FALSE,       
            # The number of trees in the ensemble (used if using Bayesian Additive Regression Trees prior)
            ntree = 300,           
            # Seed for random number generator, for reproducibility of results
            seed = seed            
  )
  
  
  # Plot the path of the estimates for the Spike and Slab model
  plot(ss_results, plot.type = "path")
  
  # Return the result
  return(ss_results)
}


# Extract the selected variables
# T1 data
ssp_T1_LD <- fit_spikeslab_prior(data = T1_LD, bigp_smalln = FALSE)
ssp_T1_ED <- fit_spikeslab_prior(data = T1_ED, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_HD <- fit_spikeslab_prior(data = T1_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_VD <- fit_spikeslab_prior(data = T1_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T2 data
ssp_T2_LD <- fit_spikeslab_prior(data = T2_LD, bigp_smalln = FALSE)
ssp_T2_ED <- fit_spikeslab_prior(data = T2_ED, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_HD <- fit_spikeslab_prior(data = T2_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_VD <- fit_spikeslab_prior(data = T2_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T3 data
ssp_T3_LD <- fit_spikeslab_prior(data = T3_LD, bigp_smalln = FALSE)
ssp_T3_ED <- fit_spikeslab_prior(data = T3_ED, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_HD <- fit_spikeslab_prior(data = T3_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_VD <- fit_spikeslab_prior(data = T3_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T4 data
ssp_T4_LD <- fit_spikeslab_prior(data = T4_LD, bigp_smalln = FALSE)
ssp_T4_ED <- fit_spikeslab_prior(data = T4_ED, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_HD <- fit_spikeslab_prior(data = T4_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T4_VD <- fit_spikeslab_prior(data = T4_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)




#### SIM DATA. SPIKE-AND-SLAB LASSO 'SSLASSO' ####

# Function to fit the Spike-and-Slab LASSO model, plot the coefficients,
# and extract selected variables from a given data frame.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, and the rest are predictors.
#     lambda1 - Slab variance parameter.
#     lambda0 - Vector of spike penalty parameters.
#     theta - Prior mixing proportion.
#     eps - Convergence criterion.
#     plot_width - Width of the plot in inches.
#     plot_height - Height of the plot in inches.
# OUTPUTS:
#     A list containing:
#         coefficients - The fitted matrix of coefficients.
#         ever_selected - A binary vector indicating which variables were
#                        ever selected along the regularization path.
#         plot - A plot of the coefficient paths for the fitted model.
fit_sslasso <- function(data, lambda1 = 1, lambda0 = NULL, 
                                           theta = 0.5, eps = 0.001,
                                           plot_width = 6, plot_height = 4) {
  
  # Separate data into X and y
  X <- as.matrix(data[, -1])  # Design matrix (excluding the y column)
  y <- data[[1]]              # Response vector (first column)
  
  # If lambda0 is not provided, create a sequence
  if (is.null(lambda0)) {
    lambda0 <- seq(lambda1, length(X), length.out = 100)
  }
  
  # Fit the SSLASSO model
  result <- SSLASSO(X = X, y = y, penalty = "adaptive", variance = "fixed",
                    lambda1 = lambda1, lambda0 = lambda0, theta = theta)
  
  # Set plot margins (bottom, left, top, right)
  par(mar = c(6, 6, 2, 2))
  
  # Set the plot dimensions (width, height) in inches
  par(pin = c(plot_width, plot_height))
  
  # Create the plot of coefficients
  plot(result)
  
  # Extract selection indicators and determine which variables were ever selected
  selected_variables <- result$select
  ever_selected <- apply(selected_variables, 1, max)
  
  # Return the results as a list
  return(list(coefficients = result$beta, ever_selected = ever_selected, plot = result))
}


# Simulate data (for example, T1_LD)
# Call the function with the simulated data
output <- fit_sslasso(T1_LD)

# The output contains coefficients, ever_selected, and plot
coefficients <- output$coefficients
ever_selected <- output$ever_selected




#### SIM DATA. HORSESHOE PRIOR. 'horseshoe' ####

# Function to fit the Horseshoe model, plot predicted values against observed values,
# and plot credible intervals for coefficients.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, and the rest are predictors.
#     method.tau - Method for handling tau (truncatedCauchy, halfCauchy, or fixed).
#     tau - The (estimated) value of tau in case "fixed" is selected for method.tau.
#     method.sigma - Method for handling sigma (Jeffreys or fixed).
#     burn - Number of burn-in MCMC samples.
#     nmc - Number of posterior draws to be saved.
#     thin - Thinning parameter of the chain.
#     alpha - Level for the credible intervals.
# OUTPUTS:
#     The fitted horseshoe model.
#
fit_horseshoe_model <- function(data, method.tau, tau, method.sigma, burn, nmc, thin, alpha) {
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Ensure method.tau is one of the allowed values
  if (!method.tau %in% c("truncatedCauchy", "halfCauchy", "fixed")) {
    stop("method.tau must be one of 'truncatedCauchy', 'halfCauchy', or 'fixed'.")
  }
  
  # Ensure tau is a positive number if method.tau is "fixed"
  if (method.tau == "fixed" && (!is.numeric(tau) || tau <= 0)) {
    stop("tau must be a positive number when method.tau is 'fixed'.")
  }
  
  # Ensure method.sigma is one of the allowed values
  if (!method.sigma %in% c("Jeffreys", "fixed")) {
    stop("method.sigma must be one of 'Jeffreys' or 'fixed'.")
  }
  
  # Ensure burn, nmc, and thin are positive integers
  if (!is.numeric(burn) || burn <= 0 || floor(burn) != burn ||
      !is.numeric(nmc) || nmc <= 0 || floor(nmc) != nmc ||
      !is.numeric(thin) || thin <= 0 || floor(thin) != thin) {
    stop("burn, nmc, and thin must be positive integers.")
  }
  
  # Ensure alpha is a number between 0 and 1
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a number between 0 and 1.")
  }
  
  # Separate data into X and y
  X <- as.matrix(data[, -1])  # Design matrix (excluding the y column)
  y <- data[[1]]              # Response vector (first column)
  
  # Fit the horseshoe model using the horseshoe package
  fit_horseshoe <- horseshoe::horseshoe(y = y, X = X, 
                                        method.tau = method.tau,
                                        tau = tau, 
                                        method.sigma = method.sigma, 
                                        burn = burn,
                                        nmc = nmc, 
                                        thin = thin, 
                                        alpha = alpha)
  
  # Plot predicted values against the observed data
  plot(y, X %*% fit_horseshoe$BetaHat, col = rep("blue", 20), 
       xlab = "Observed values", ylab = "Predicted values",
       main = "Horseshoe Model: Predicted vs Observed Values")
  
  # Print the posterior mean of tau
  cat("Posterior mean of tau:", fit_horseshoe$TauHat, "\n")
  
  # Load the Hmisc package for plotting credible intervals
  library(Hmisc)
  
  # Plot the credible intervals for coefficients
  xYplot(Cbind(fit_horseshoe$BetaHat, fit_horseshoe$LeftCI, fit_horseshoe$RightCI) ~ 1:ncol(X),
         type = c("p", "g", "g"), ylab = "Coefficients", xlab = "Variables",
         main = "Credible Intervals for Coefficients")
  
  # Return the fitted horseshoe model
  return(fit_horseshoe)
}

# Loop through each dataset in the 'datasets' vector
for (i in seq_along(datasets)) {
  
  # Fit the horseshoe model for the current dataset
  result <- fit_horseshoe_model(data = datasets[[i]], 
                                method.tau = "truncatedCauchy", 
                                tau = 1, 
                                method.sigma = "Jeffreys", 
                                burn = 1000, 
                                nmc = 5000, 
                                thin = 1, 
                                alpha = 0.05)
  
}

# Loop through each dataset in the 'datasets' vector
for (i in seq_along(datasets)) {
  
  # Fit the horseshoe model for the current dataset
  result_1 <- fit_horseshoe_model(data = datasets[[i]], 
                                method.tau = "halfCauchy", 
                                tau = 1, 
                                method.sigma = "Jeffreys", 
                                burn = 1000, 
                                nmc = 5000, 
                                thin = 1, 
                                alpha = 0.05)
  
}




#### SIM DATA. HORSESHOE PRIOR 'bayesreg' ####

# Function to fit the Horseshoe prior (or HS+) model with bayesreg package, 
#   extract selected variables based on coefficient threshold, and refit 
#   the model using only the selected variables.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, 
#            and the rest are predictors.
#     n.samples - Number of posterior samples to draw.
#     burnin - Number of burn-in samples.
#     thin - Thinning parameter of the chain.
#     coef_threshold - Threshold for coefficients to select variables.
# OUTPUTS:
#     The summary of the refitted horseshoe model with selected variables.
#
fit_bayesreg_horseshoe_model <- function(data, n.samples, burnin, thin, 
                                         coef_threshold, prior) {
  
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Fit the initial horseshoe model using the bayesreg package
  fit_horseshoe_b <- bayesreg::bayesreg(y ~ ., data = data, 
                                        # Distribution of the target
                                        model = "gaussian",
                                        prior = prior,
                                        n.samples = n.samples,
                                        burnin = burnin,
                                        thin = thin)
  
  # Extract the coefficients from the fitted model
  coefficients <- fit_horseshoe_b$mu.beta
  coefficients <- coefficients[abs(coefficients[,1]) > coef_threshold,]
  
  # Extract the names of the variables with non-zero coefficients
  selected_variables <- names(coefficients)
  
  # Create a formula for the new model using only the selected variables
  selected_formula <- as.formula(paste("y ~", paste(selected_variables, collapse=" + ")))
  
  # Fit the new model with the selected variables
  fit_selected_vars <- bayesreg::bayesreg(selected_formula, data = data, 
                                          model = "gaussian",
                                          prior = "hs",
                                          n.samples = n.samples,
                                          burnin = burnin,
                                          thin = thin)
  
  # Display summary of the refitted model
  selected_summary <- summary(fit_selected_vars)
  print(selected_summary)
  
  # Display WAIC
  cat(sprintf("Linear regression WAIC=%g", fit_selected_vars$waic), "\n")
  
  # Return the summary of the refitted model
  return(selected_summary)
}

# Example usage with a single dataset

# Here T1_LD should be a data frame where the first column is the response variable
fit_bayesreg_horseshoe_model(data = T1_LD, 
                             n.samples = 1000, 
                             burnin = 1000, 
                             thin = 5, 
                             coef_threshold = 1,
                             prior = "hs")




#### SIM DATA. HORSESHOE + PRIOR 'bayesreg' ####

# Here T1_LD should be a data frame where the first column is the response variable
fit_bayesreg_horseshoe_model(data = T1_LD, 
                                  n.samples = 1000, 
                                  burnin = 1000, 
                                  thin = 5, 
                                  coef_threshold = 1,
                                  prior = "hs+")



#### SIM DATA. SSS WITH SCREENING 'BayesS5' ####

# Separate data into X and y
X <- as.matrix(T1_LD[, -1])  # Design matrix (excluding the y column)
y <- T1_LD[[1]]              # Response vector (first column)


fit_S5 <- BayesS5::S5(X = X, y = y, ind_fun = ind_fun_pemom,
                      model = Uniform,
                      tuning = 100,
                      C0 = 2)


res_default <- result(fit_S5)
#print(res_default$hppm) # the MAP model
#print(res_default$hppm.prob) # the posterior probability of the hppm
plot(res_default$marg.prob, ylim = c(0, 1), ylab = "marginal inclusion probability")

selected_variables <- names(res_default$marg.prob)[res_default$marg.prob > 0.005]






#### SIM DATA. LAPLACE APPROXIMATION 'LaplacesDemon' ####



#### SIM DATA. BAYESIAN LASSO 'monomvn' ####

# Separate data into X and y
X <- as.matrix(T1_LD[, -1])  # Design matrix (excluding the y column)
y <- T1_LD[[1]]              # Response vector (first column)

fit_blasso <- monomvn::blasso(X = X, y = y, T = 5000, RJ = FALSE, verb = 1)
# View the summary of the fit
print(fit_blasso)

# Extract the coefficients
coefficients <- fit_blasso$beta

summary(fit_blasso)



#### SIM DATA. RJMCMC 'rjmcmc' ####







#### CRIME ANALYSIS ####

#### SOURCE DATA FRAMES AND VECTORS OF CRIME DATA ####
# Source the file that contains the crime data
source("data_crime_raw.R")

#### CRIME. LASSO PENALISED REGRESSION 'glment' ####

# Function to fit Lasso regression on Crime data and extract the selected predictors.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
#         y - prediction vector.
# OUTPUT:
#         selected_predictors - selected predictor data frame with coefficients.
#
fit_crime_lasso <- function(data, nfolds = 10, y) {
  
  # Create the matrix of predictors
  xmatrix <- model.matrix(~ ., data = data[, -ncol(data)])
  
  # Fit the Lasso model on the design matrix with alpha = 1 (Lasso penalty).
  # Note: alpha = 0 corresponds to Ridge and alpha between 0 and 1 corresponds to ElasticNet.
  initial_lasso <- glmnet(x = xmatrix, y = y, alpha = 1)
  
  # Perform k-fold cross-validation to find the optimal value of the regularization 
  # parameter lambda that minimizes the cross-validation error.
  lasso_cv <- cv.glmnet(x = xmatrix, y = y, alpha = 1, nfolds = nfolds)
  
  # Plot the regularisation path which is the coefficient profiles of the Lasso model
  # as a function of lambda. The vertical line represents the optimal lambda value 
  # that minimizes the cross-validation error.
  #par(mfrow = c(1, 2))
  #plot(initial_lasso, xvar = "lambda")
  #abline(v = log(initial_lasso$lambda.min), lwd = 4, lty = 2)
  
  # Plot the cross-validation errors as a function of lambda.
  plot(lasso_cv)
  abline(v = log(lasso_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Lasso model using the optimal lambda value obtained from cross-validation.
  lasso_model <- glmnet(x = xmatrix, y = y, alpha = 1, lambda = lasso_cv$lambda.min)
  
  # Extract the coefficients from the lasso model
  coefficients <- coef(lasso_model, s = lasso_cv$lambda.min)
  
  # Find the names of the variables with non-zero coefficients
  selected_variable_names <- rownames(coefficients)[coefficients[, 1] != 0]
  
  # Extract the non-zero coefficients
  selected_predictors <- coefficients[selected_variable_names, 1] %>% data.frame()

  # Return the fitted Lasso model
  return(selected_predictors)
}

# Run the LASSO function and extract the selected coefficients
crime_lasso <- fit_crime_lasso(data = df, nfolds = 10, y = df$ViolentCrimesPerPop)

# Remove function
rm(fit_crime_lasso)




#### CRIME. ELNET PENALISED REGRESSION 'glmnet' ####

# Function to fit Elastic net regression on Crime data and extract the selected predictors.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
#         y - prediction vector.
# OUTPUT:
#         selected_predictors - selected predictor data frame with coefficients.
#
fit_crime_elnet <- function(data, nfolds = 10, y) {
  
  # Create the matrix of predictors
  xmatrix <- model.matrix(~ ., data = data[, -ncol(data)])
  
  # Fit the Elastic Net model on the design matrix with alpha = 0.5 (L1 + L2 penalties)
  initial_elnet <- glmnet(x = xmatrix, y = y, alpha = 0.5)
  
  # Perform k-fold cross-validation to find the optimal value of the regularization 
  # parameter lambda that minimizes the cross-validation error.
  elnet_cv <- cv.glmnet(x = xmatrix, y = y, alpha = 0.5, nfolds = nfolds)
  
  # Plot the regularisation path which is the coefficient profiles of the Elastic net model
  # as a function of lambda. The vertical line represents the optimal lambda value 
  # that minimises the cross-validation error.
  #par(mfrow = c(1, 2))
  #plot(initial_elnet, xvar = "lambda")
  #abline(v = log(initial_elnet$lambda.min), lwd = 4, lty = 2)
  
  # Plot the cross-validation errors as a function of lambda.
  plot(elnet_cv)
  abline(v = log(elnet_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Lasso model using the optimal lambda value obtained from cross-validation.
  elnet_model <- glmnet(x = xmatrix, y = y, alpha = 1, lambda = elnet_cv$lambda.min)
  
  # Extract the coefficients from the elnet model
  coefficients <- coef(elnet_model, s = elnet_cv$lambda.min)
  
  # Find the names of the variables with non-zero coefficients
  selected_variable_names <- rownames(coefficients)[coefficients[, 1] != 0]
  
  # Extract the non-zero coefficients
  selected_predictors <- coefficients[selected_variable_names, 1] %>% data.frame()
  
  # Return the selected coefficients
  return(selected_predictors)
}

# Run the elnet function and extract the selected coefficients
crime_elnet <- fit_crime_elnet(data = df, nfolds = 10, y = df$ViolentCrimesPerPop)

# Remove function
rm(fit_crime_elnet)




#### CRIME. XGBOOST 'caret' ####

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


# Function to train and evaluate an XGBoost model from 'caret' package on Crimes
#   data and plot feature importance
# INPUTS:
#         data - a data frame containing the predictors.
#         y - the response variable.
#         xgb_cv   - a trainControl object defining the cross-validation strategy.
#         xgb_grid - a data frame defining the grid of hyperparameters to search over.
# OUTPUT:
#         A list containing:
#               model - The trained XGBoost model.
#               rmse  - The root mean squared error (RMSE) of the model on the test set.
#               coefficients - Coefficients of all predictors.
#
fit_crime_xgboost <- function(data, y, xgb_cv, xgb_grid) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data should be a data frame.")
  }
  
  if (!"y" %in% names(data)) {
    stop("data should contain a column named 'y' as the response variable.")
  }
  
  if (!is.list(xgb_cv) || !("method" %in% names(xgb_cv))) {
    stop("xgb_cv should be a trainControl object with a specified method.")
  }
  
  if (!is.data.frame(xgb_grid) || !all(c("nrounds", "max_depth", "eta") %in% names(xgb_grid))) {
    stop("xgb_grid should be a data frame with hyperparameters to be tuned, including nrounds, max_depth, and eta.")
  }
  
  # Separate features and target from the dataset
  X <- model.matrix(~ ., data = data[, -ncol(data)])
  
  # Split the dataset into training and testing sets
  # createDataPartition helps in creating stratified random samples
  index <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[index,]           # Extract training features
  y_train <- y[index]            # Extract training target
  X_test <- X[-index,]           # Extract testing features
  y_test <- y[-index]            # Extract testing target
  
  # Train the XGBoost model with cross-validation and parameter tuning
  xgb_model <- train(
    x = X_train,               # Feature matrix
    y = y_train,               # Target vector
    trControl = cv,            # Cross-validation strategy
    tuneGrid = grid,           # Grid of hyperparameters to tune
    method = "xgbTree"         # XGBoost model
  )
  
  # Make predictions on the test set using the trained model
  predictions <- predict(xgb_model, X_test)
  
  # Calculate the Root Mean Squared Error (RMSE) on the test set
  # RMSE is a measure of the differences between predicted and actual values
  rmse <- sqrt(mean((predictions - y_test)^2))
  cat("Root Mean Squared Error on Test Set:", rmse, "\n")
  
  # Extract feature importance from the trained model
  # Feature importance helps in understanding which features are most influential in making predictions
  importance_matrix <- xgb.importance(feature_names = colnames(X), model = xgb_model$finalModel)
  
  # Plot feature importance
  # This helps in visualizing the importance of each feature in the model
  xgb.plot.importance(importance_matrix)
  
  # Return the model
  return(xgb_model)
}

# Run the function to extract XGBoost model and feature importances
#crime_xgboost <- fit_crime_xgboost(data = df, y = df$ViolentCrimesPerPop, 
#                                   xgb_cv = xgb_cv, xgb_grid = xgb_grid)


#### CRIME. SPIKE AND SLAB PRIOR 'spikeslab' ####

# Function to fit a Spike and Slab model using 'spikeslab' package on Crimes data.
#   It also plots the path of the estimates for the Spike and Slab model.
#
# INPUTS:
#     data               - A data frame containing the predictors and the response variable.
#                          The response variable should be named "y".
#     bigp_smalln        - A logical indicating if the high-dimensional low sample size adjustments
#                          should be made. Should be either TRUE or FALSE.
#     bigp_smalln_factor - A numeric adjustment factor to be used when bigp.smalln is TRUE.
#     y                  - Prediction variable.
#     seed               - An integer used for setting the seed for reproducibility. Default is 42.
#
# OUTPUT:
#     A list containing:
#         result - The fitted Spike and Slab model.
#
fit_crime_spikeslab_prior <- function(data, bigp_smalln, bigp_smalln_factor, y, seed = 42) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data should be a data frame.")
  }
  
  if (!"y" %in% names(data)) {
    stop("data should contain a column named 'y' as the response variable.")
  }
  
  if (!is.logical(bigp_smalln) || length(bigp_smalln) != 1) {
    stop("bigp_smalln should be a logical value (either TRUE or FALSE).")
  }
  
  if (!is.numeric(bigp_smalln_factor) || length(bigp_smalln_factor) != 1) {
    stop("bigp_smalln_factor should be a single numeric value.")
  }
  
  if (!is.numeric(y) || length(y) != 1) {
    stop("y should be a single numeric value representing the response variable.")
  }
  
  if (!is.numeric(seed) || length(seed) != 1 || seed < 0 || seed != as.integer(seed)) {
    stop("seed should be a single non-negative integer value.")
  }
  
  # Define the formula for the model
  # y ~ . indicates that y is the response variable and . represents all other variables as predictors
  formula <- as.formula("y ~ .")
  
  # Run the spikeslab model
  result <- spikeslab::spikeslab(
    # Formula representing the relationship between predictors and response
    formula,  
    # The dataset containing the variables in the formula
    data = data[, -ncol(data)],           
    # The number of iterations in the two MCMC chains used in spikeslab.
    # n.iter1 is for the first chain, and n.iter2 is for the second chain.
    n.iter1 = 1000,        
    n.iter2 = 1000,        
    # Calculate the mean squared error as part of the model evaluation
    mse = TRUE,           
    # High-dimensional low sample size adjustments.
    # bigp.smalln - logical flag, if TRUE adjustments for high-dimensional low sample size are made.
    # bigp.smalln.factor - controls the magnitude of the adjustments.
    bigp.smalln = bigp_smalln,                 
    bigp.smalln.factor = bigp_smalln_factor,   
    # Random effects. If specified, adds random effects to the model
    r.effects = NULL,      
    # Maximum number of variables to be retained in the model
    max.var = 500,         
    # If TRUE, the predictors are centered by subtracting their means
    center = TRUE,         
    # If TRUE, an intercept term is included in the model
    intercept = TRUE,      
    # If TRUE, a fast approximation algorithm is used for speed up
    fast = TRUE,           
    # The number of blocks into which the coefficients are split for Gibbs sampling
    beta.blocks = 5,       
    # If TRUE, outputs progress and additional information while fitting the model
    verbose = FALSE,       
    # The number of trees in the ensemble (used if using Bayesian Additive Regression Trees prior)
    ntree = 300,           
    # Seed for random number generator, for reproducibility of results
    seed = seed            
  )
  
  # Print the result summary
  print(result)
  
  # Plot the path of the estimates for the Spike and Slab model
  plot(result, plot.type = "path")
  
  # Return the result
  return(list(result = result))
}

crime_spikeslab_prior <- fit_crime_spikeslab_prior(data, 
                                                   bigp_smalln = FALSE,
                                                   y = df$ViolentCrimesPerPop, 
                                                   seed = 42)


#### CRIME. SPIKE AND SLAB LASSO 'SSLASSO' ####
#### CRIME. HORSESHOE PRIOR 'horseshoe' ####

# Function to fit the Horseshoe prior model, plot predicted values against observed values,
#   and plot credible intervals for coefficients.
#
# INPUTS:
#     data - Crime dataset.
#     method.tau - Method for handling tau (truncatedCauchy, halfCauchy, or fixed).
#     tau - The (estimated) value of tau in case "fixed" is selected for method.tau.
#     method.sigma - Method for handling sigma (Jeffreys or fixed).
#     burn - Number of burn-in MCMC samples.
#     nmc - Number of posterior draws to be saved.
#     thin - Thinning parameter of the chain.
#     alpha - Level for the credible intervals.
# OUTPUTS:
#     fit_horseshoe - The fitted horseshoe model.
#
fit_crime_horseshoe_model <- function(data, method.tau, tau, method.sigma, burn, nmc, thin, alpha) {
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Ensure method.tau is one of the allowed values
  if (!method.tau %in% c("truncatedCauchy", "halfCauchy", "fixed")) {
    stop("method.tau must be one of 'truncatedCauchy', 'halfCauchy', or 'fixed'.")
  }
  
  # Ensure tau is a positive number if method.tau is "fixed"
  if (method.tau == "fixed" && (!is.numeric(tau) || tau <= 0)) {
    stop("tau must be a positive number when method.tau is 'fixed'.")
  }
  
  # Ensure method.sigma is one of the allowed values
  if (!method.sigma %in% c("Jeffreys", "fixed")) {
    stop("method.sigma must be one of 'Jeffreys' or 'fixed'.")
  }
  
  # Ensure burn, nmc, and thin are positive integers
  if (!is.numeric(burn) || burn <= 0 || floor(burn) != burn ||
      !is.numeric(nmc) || nmc <= 0 || floor(nmc) != nmc ||
      !is.numeric(thin) || thin <= 0 || floor(thin) != thin) {
    stop("burn, nmc, and thin must be positive integers.")
  }
  
  # Ensure alpha is a number between 0 and 1
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a number between 0 and 1.")
  }
  
  # Separate data into X and y
  X <- as.matrix(data[, -1])  # Design matrix (excluding the y column)
  y <- data[[1]]              # Response vector (first column)
  
  # Fit the horseshoe model using the horseshoe package
  fit_horseshoe <- horseshoe::horseshoe(y = y, X = X, 
                                        method.tau = method.tau,
                                        tau = tau, 
                                        method.sigma = method.sigma, 
                                        burn = burn,
                                        nmc = nmc, 
                                        thin = thin, 
                                        alpha = alpha)
  
  # Plot predicted values against the observed data
  plot(y, X %*% fit_horseshoe$BetaHat, col = rep("blue", 20), 
       xlab = "Observed values", ylab = "Predicted values",
       main = "Horseshoe Model: Predicted vs Observed Values")
  
  # Print the posterior mean of tau
  cat("Posterior mean of tau:", fit_horseshoe$TauHat, "\n")
  
  # Load the Hmisc package for plotting credible intervals
  library(Hmisc)
  
  # Plot the credible intervals for coefficients
  xYplot(Cbind(fit_horseshoe$BetaHat, fit_horseshoe$LeftCI, fit_horseshoe$RightCI) ~ 1:ncol(X),
         type = c("p", "g", "g"), ylab = "Coefficients", xlab = "Variables",
         main = "Credible Intervals for Coefficients")
  
  # Return the fitted horseshoe model
  return(fit_horseshoe)
}

# Loop through each dataset in the 'datasets' vector
for (i in seq_along(datasets)) {
  
  # Fit the horseshoe model for the current dataset
  result <- fit_horseshoe_model(data = datasets[[i]], 
                                method.tau = "truncatedCauchy", 
                                tau = 1, 
                                method.sigma = "Jeffreys", 
                                burn = 1000, 
                                nmc = 5000, 
                                thin = 1, 
                                alpha = 0.05)
  
}

# Run the functions an extract the results
crime_horseshoe_model <- fit_crime_horseshoe_model(data,
                                                   method.tau,
                                                   tau,
                                                   method.sigma,
                                                   burn,
                                                   nmc,
                                                   thin,
                                                   alpha)
#### CRIME. HORSESHOE PRIOR 'bayesreg' ####
#### CRIME. HORSESHOE + PRIOR 'bayesreg' ####
#### CRIME. SSS 'BayesS5' ####
#### CRIME. LAPLACE APPROXIMATION 'LaplacesDemon' ####
#### CRIME. BAYESIAN LASSO 'monomvn' ####
#### CRIME. RJMCMC 'rjmcmc' ####





