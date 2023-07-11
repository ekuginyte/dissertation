#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "betareg", "R.matlab", "glmnet", "dplyr", 
                  "cowplot", "coda", "igraph", "R6", "nimble", "MASS", "xgboost",
                  "caret", "spikeslab", "SSLASSO", "horseshoe", "bayesreg", "Hmisc",
                  "LaplacesDemon", "BayesS5", "monomvn", "Hmisc")

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
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'nfolds' is a function
  if (!is.numeric(nfolds) || nfolds <= 0 || round(nfolds) != nfolds) {
    stop("'nfolds' must be a positive integer.")
  }
  
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
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'nfolds' is a function
  if (!is.numeric(nfolds) || nfolds <= 0 || round(nfolds) != nfolds) {
    stop("'nfolds' must be a positive integer.")
  }
  
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
    trControl = xgb_cv,            # Cross-validation strategy
    tuneGrid = xgb_grid,           # Grid of hyperparameters to tune
    method = "xgbTree",         # XGBoost model
    metric = "RMSE",
    maximize = FALSE,
    objective = "reg:linear"                    # Specify the learning task and the corresponding learning objective
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
T3_LD_xgboost <- train_evaluate_xgb(data = T3_LD, xgb_cv, xgb_grid)
T3_ED_xgboost <- train_evaluate_xgb(data = T3_ED, xgb_cv, xgb_grid)
T3_VD_xgboost <- train_evaluate_xgb(data = T3_HD, xgb_cv, xgb_grid)
T3_HD_xgboost <- train_evaluate_xgb(data = T3_VD, xgb_cv, xgb_grid)
T3_XD_xgboost <- train_evaluate_xgb(data = T3_XD, xgb_cv, xgb_grid)

# Type 4 data
T4_LD_xgboost <- train_evaluate_xgb(data = T4_LD, xgb_cv, xgb_grid)
T4_ED_xgboost <- train_evaluate_xgb(data = T4_ED, xgb_cv, xgb_grid)
T4_VD_xgboost <- train_evaluate_xgb(data = T4_HD, xgb_cv, xgb_grid)
T4_HD_xgboost <- train_evaluate_xgb(data = T4_VD, xgb_cv, xgb_grid)
T4_XD_xgboost <- train_evaluate_xgb(data = T4_XD, xgb_cv, xgb_grid)

















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
#     seed               - An NEGATIVE integer used for setting the seed for reproducibility.
#
# OUTPUT:
#     ss_results - The fitted Spike and Slab model.
#
fit_spikeslab_prior <- function(data, bigp_smalln, bigp_smalln_factor = 0, screen = FALSE,
                                K = 10, seed = -42) {
  
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
  
  if (!is.numeric(seed) || length(seed) != 1 || seed > 0 || seed != as.integer(seed)) {
    stop("seed should be a single negative integer value.")
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
ssp_T1_ED <- fit_spikeslab_prior(data = T1_ED, bigp_smalln = FALSE)
ssp_T1_HD <- fit_spikeslab_prior(data = T1_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T1_VD <- fit_spikeslab_prior(data = T1_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T2 data
ssp_T2_LD <- fit_spikeslab_prior(data = T2_LD, bigp_smalln = FALSE)
ssp_T2_ED <- fit_spikeslab_prior(data = T2_ED, bigp_smalln = FALSE)
ssp_T2_HD <- fit_spikeslab_prior(data = T2_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T2_VD <- fit_spikeslab_prior(data = T2_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T3 data
ssp_T3_LD <- fit_spikeslab_prior(data = T3_LD, bigp_smalln = FALSE)
ssp_T3_ED <- fit_spikeslab_prior(data = T3_ED, bigp_smalln = FALSE)
ssp_T3_HD <- fit_spikeslab_prior(data = T3_HD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)
ssp_T3_VD <- fit_spikeslab_prior(data = T3_VD, bigp_smalln = TRUE, bigp_smalln_factor = 1, screen = TRUE)

# T4 data
ssp_T4_LD <- fit_spikeslab_prior(data = T4_LD, bigp_smalln = FALSE)
ssp_T4_ED <- fit_spikeslab_prior(data = T4_ED, bigp_smalln = FALSE)
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
fit_sslasso <- function(data, lambda1 = 1, lambda0 = seq(1, nrow(data),), 
                                           theta = 0.5, eps = 0.001,
                                           plot_width = 6, plot_height = 4) {
  # Input checks
  # Check that 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  # Check that 'lambda1' is a positive numeric value
  if (!is.numeric(lambda1) || lambda1 <= 0) {
    stop("'lambda1' must be a positive numeric value.")
  }
  
  # Check that 'lambda0' is a numeric sequence
  if (!is.numeric(lambda0)) {
    stop("'lambda0' must be a numeric sequence.")
  }
  
  # Check that 'theta' is a numeric value between 0 and 1
  if (!is.numeric(theta) || theta < 0 || theta > 1) {
    stop("'theta' must be a numeric value between 0 and 1.")
  }
  
  # Check that 'eps' is a small positive numeric value
  if (!is.numeric(eps) || eps <= 0 || eps >= 1) {
    stop("'eps' must be a small positive numeric value (0 < eps < 1).")
  }
  
  # Check that 'plot_width' is a positive numeric value
  if (!is.numeric(plot_width) || plot_width <= 0) {
    stop("'plot_width' must be a positive numeric value.")
  }
  
  # Check that 'plot_height' is a positive numeric value
  if (!is.numeric(plot_height) || plot_height <= 0) {
    stop("'plot_height' must be a positive numeric value.")
  }
  
  
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

# Call the function with the simulated data
# T1 Data
ssl_T1_LD <- fit_sslasso(T1_LD)
ssl_T1_ED <- fit_sslasso(T1_ED)
ssl_T1_HD <- fit_sslasso(T1_VD)
ssl_T1_VD <- fit_sslasso(T1_HD)

# T2 Data
ssl_T2_LD <- fit_sslasso(T2_LD)
ssl_T2_ED <- fit_sslasso(T2_ED)
ssl_T2_HD <- fit_sslasso(T2_VD)
ssl_T2_VD <- fit_sslasso(T2_HD)

# T3 Data
ssl_T3_LD <- fit_sslasso(T3_LD)
ssl_T3_ED <- fit_sslasso(T3_ED)
ssl_T3_HD <- fit_sslasso(T3_VD)
ssl_T3_VD <- fit_sslasso(T3_HD)

# T4 Data
ssl_T4_LD <- fit_sslasso(T4_LD)
ssl_T4_ED <- fit_sslasso(T4_ED)
ssl_T4_HD <- fit_sslasso(T4_VD)
ssl_T4_VD <- fit_sslasso(T4_HD)

# The output contains coefficients, ever_selected, and plot
ssl_T1_LD$coefficients
ssl_T1_LD$ever_selected




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
fit_hs_horseshoe_model <- function(data, method.tau, tau = 1, method.sigma = "Jeffreys", 
                                   burn = 1000, nmc = 5000, thin = 1, alpha = 0.05){
  
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
hshst_T3_LD <- fit_hs_horseshoe_model(data = T3_LD, method.tau = "truncatedCauchy")
hshst_T3_ED <- fit_hs_horseshoe_model(data = T3_ED, method.tau = "truncatedCauchy")
hshst_T3_HD <- fit_hs_horseshoe_model(data = T3_HD, method.tau = "truncatedCauchy")
hshst_T3_VD <- fit_hs_horseshoe_model(data = T3_VD, method.tau = "truncatedCauchy")

# T4 Data
hshst_T4_LD <- fit_hs_horseshoe_model(data = T4_LD, method.tau = "truncatedCauchy")
hshst_T4_ED <- fit_hs_horseshoe_model(data = T4_ED, method.tau = "truncatedCauchy")
hshst_T4_HD <- fit_hs_horseshoe_model(data = T4_HD, method.tau = "truncatedCauchy")
hshst_T4_VD <- fit_hs_horseshoe_model(data = T4_VD, method.tau = "truncatedCauchy")



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
hshsh_T3_LD <- fit_hs_horseshoe_model(data = T3_LD, method.tau = "halfCauchy")
hshsh_T3_ED <- fit_hs_horseshoe_model(data = T3_ED, method.tau = "halfCauchy")
hshsh_T3_HD <- fit_hs_horseshoe_model(data = T3_HD, method.tau = "halfCauchy")
hshsh_T3_VD <- fit_hs_horseshoe_model(data = T3_VD, method.tau = "halfCauchy")

# T4 Data
hshsh_T4_LD <- fit_hs_horseshoe_model(data = T4_LD, method.tau = "halfCauchy")
hshsh_T4_ED <- fit_hs_horseshoe_model(data = T4_ED, method.tau = "halfCauchy")
hshsh_T4_HD <- fit_hs_horseshoe_model(data = T4_HD, method.tau = "halfCauchy")
hshsh_T4_VD <- fit_hs_horseshoe_model(data = T4_VD, method.tau = "halfCauchy")




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
fit_horseshoe_bs_model <- function(data,
                                   n.samples = 1000, 
                                   burnin = 1000, 
                                   thin = 5, 
                                   coef_threshold = 1,
                                   prior = "hs") {
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'n.samples' is a positive integer
  if (!is.numeric(n.samples) || n.samples <= 0 || round(n.samples) != n.samples) {
    stop("'n.samples' must be a positive integer.")
  }
  
  # Ensure 'burnin' is a positive integer
  if (!is.numeric(burnin) || burnin <= 0 || round(burnin) != burnin) {
    stop("'burnin' must be a positive integer.")
  }
  
  # Ensure 'thin' is a positive integer
  if (!is.numeric(thin) || thin <= 0 || round(thin) != thin) {
    stop("'thin' must be a positive integer.")
  }
  
  # Ensure 'coef_threshold' is a positive numeric
  if (!is.numeric(coef_threshold) || coef_threshold <= 0) {
    stop("'coef_threshold' must be a positive numeric.")
  }
  
  # Ensure 'prior' is a character and contains valid value
  if (!is.character(prior) || !(prior %in% c("hs", "other_valid_prior"))) {
    stop("'prior' must be a character and contain a valid value.")
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
                                          prior = prior,
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
hs_bs_T3_LD <- fit_horseshoe_bs_model(data = T3_LD)
hs_bs_T3_ED <- fit_horseshoe_bs_model(data = T3_ED)
hs_bs_T3_HD <- fit_horseshoe_bs_model(data = T3_HD)
hs_bs_T3_VD <- fit_horseshoe_bs_model(data = T3_VD)

# T4 Data
hs_bs_T4_LD <- fit_horseshoe_bs_model(data = T4_LD)
hs_bs_T4_ED <- fit_horseshoe_bs_model(data = T4_ED)
hs_bs_T4_HD <- fit_horseshoe_bs_model(data = T4_HD)
hs_bs_T4_VD <- fit_horseshoe_bs_model(data = T4_VD)




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
hsp_bs_T3_LD <- fit_horseshoe_bs_model(data = T3_LD, prior = "hs+")
hsp_bs_T3_ED <- fit_horseshoe_bs_model(data = T3_ED, prior = "hs+")
hsp_bs_T3_HD <- fit_horseshoe_bs_model(data = T3_HD, prior = "hs+")
hsp_bs_T3_VD <- fit_horseshoe_bs_model(data = T3_VD, prior = "hs+")

# T4 Data
hsp_bs_T4_LD <- fit_horseshoe_bs_model(data = T4_LD, prior = "hs+")
hsp_bs_T4_ED <- fit_horseshoe_bs_model(data = T4_ED, prior = "hs+")
hsp_bs_T4_HD <- fit_horseshoe_bs_model(data = T4_HD, prior = "hs+")
hsp_bs_T4_VD <- fit_horseshoe_bs_model(data = T4_VD, prior = "hs+")




#### SIM DATA. SSS WITH SCREENING 'BayesS5' ####

# Function to fit a sparse Bayesian linear regression model using the BayesS5 
#   package. The S5 function is used to fit a model where sparsity is promoted 
#   in the regression coefficients. 
#
# INPUTS:
#     data - Data frame where the first column is the response variable, 
#            and the remaining columns are predictors.
#     ind_fun - A function to define the inclusion indicators of the model.
#     model - An object of class Model defining the prior distribution.
#     tuning - Tuning parameter for the S5 function.
#     C0 - Normalisation constant for the S5 function.
#
# OUTPUTS:
#     fit_S5 - An S5 object, which is the fitted model.
#
fit_S5_model <- function(data, ind_fun = ind_fun_pemom,
                                   model = Uniform, tuning = 100,
                                   C0 = 2) {
  # Input checks
  
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'ind_fun' is a function
  if (!is.function(ind_fun)) {
    stop("'ind_fun' must be a function.")
  }
  
  # Ensure 'model' is a valid model object or a string representing a model
  # This check will depend on your specific use case, 
  # add a more specific check if needed
  #if (!is.character(model) && !is.list(model)) {
  #  stop("'model' must be a model object or a character string representing a model.")
  #}
  
  # Ensure 'tuning' is a positive numeric
  if (!is.numeric(tuning) || tuning <= 0) {
    stop("'tuning' must be a positive numeric value.")
  }
  
  # Ensure 'C0' is a positive numeric
  if (!is.numeric(C0) || C0 <= 0) {
    stop("'C0' must be a positive numeric value.")
  }
  
  # Separate data into X and y
  X <- as.matrix(data[, -1])  # Design matrix (excluding the y column)
  y <- data[[1]]              # Response vector (first column)
  
  # Fit the model using the S5 function from the BayesS5 package
  fit_S5 <- BayesS5::S5(X = X, y = y, ind_fun = ind_fun, model = model,
                        tuning = tuning, C0 = C0)
  
  # Return the fitted model
  return(fit_S5)
}



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
S5_T3_LD <- fit_S5_model(data = T3_LD)# %>% results()
S5_T3_ED <- fit_S5_model(data = T3_ED)# %>% results()
S5_T3_HD <- fit_S5_model(data = T3_HD)# %>% results()
S5_T3_VD <- fit_S5_model(data = T3_VD)# %>% results()

# T4 Data
S5_T4_LD <- fit_S5_model(data = T4_LD)# %>% results()
S5_T4_ED <- fit_S5_model(data = T4_ED)# %>% results()
S5_T4_HD <- fit_S5_model(data = T4_HD)# %>% results()
S5_T4_VD <- fit_S5_model(data = T4_VD)# %>% results()


#### SIM DATA. LAPLACE APPROXIMATION 'LaplacesDemon' ####



#### SIM DATA. BAYESIAN LASSO 'monomvn' ####

# Function to fit a Bayesian LASSO regression model using the 'monomvn' package.
# The function implements cross-validation for hyperparameter tuning and 
# variable selection in the regression coefficients.
#
# INPUTS:
#   data - Data frame where the first column is the response variable, 
#         and the remaining columns are predictors.
#   T - Number of iterations in the MCMC chain.
#   RJ - Logical flag indicating whether to perform Reversible Jump MCMC.
#   verb - Verbosity level of the function's output.
#   cv_folds - Number of cross validations.
#   lambda_seq - Sequence of lambda2 values to loop over for tuning.
#
# OUTPUTS:
#   A list containing the following components:
#   model - A blasso object, which is the fitted model.
#   best_lambda2 - The lambda2 value that minimizes the cross-validation error.
#
fit_blasso_model <- function(data, T = 5000, RJ = FALSE, verb = 1, 
                             cv_folds = 5, lambda_seq = seq(0.1, 1, by = 0.1)) {
  
  # Input validation
  # Check if the input data is of the correct format: a data frame
  if (!is.data.frame(data)) {
    # If the input is not a data frame, throw an error and stop execution
    stop("'data' must be a data frame.")
  }
  
  # Check if the number of iterations 'T' is a positive numeric value
  if (!is.numeric(T) || T <= 0) {
    # If 'T' is not a positive number, throw an error and stop execution
    stop("'T' must be a positive numeric value.")
  }
  
  # Check if the flag 'RJ' is a logical value
  if (!is.logical(RJ)) {
    # If 'RJ' is not a logical value (TRUE/FALSE), throw an error and stop execution
    stop("'RJ' must be a logical value.")
  }
  
  # Check if the verbosity level 'verb' is a non-negative numeric value
  if (!is.numeric(verb) || verb < 0) {
    # If 'verb' is not a non-negative number, throw an error and stop execution
    stop("'verb' must be a non-negative numeric value.")
  }
  
  # Check if the number of cross-validation folds 'cv_folds' is a positive numeric value
  if (!is.numeric(cv_folds) || cv_folds <= 0) {
    # If 'cv_folds' is not a positive number, throw an error and stop execution
    stop("'cv_folds' must be a positive numeric value.")
  }
  
  # Check if the sequence of lambda values 'lambda_seq' is a numeric vector
  if (!is.numeric(lambda_seq)) {
    # If 'lambda_seq' is not a numeric vector, throw an error and stop execution
    stop("'lambda_seq' must be a numeric vector.")
  }
  
  # Separate data into X and y
  X <- as.matrix(data[, -1])  # Design matrix (excluding the response column)
  y <- data[[1]]              # Response vector (first column)
  
  # Initialize variables for cross-validation
  cv_errors <- rep(0, length(lambda_seq))
  fold_size <- round(nrow(data) / cv_folds)
  
  # Loop over lambda values
  for (i in 1:length(lambda_seq)) {
    lambda2 <- lambda_seq[i]
    
    # Cross-validation loop
    for (fold in 1:cv_folds) {
      # Index for validation set
      val_idx <- ((fold-1)*fold_size+1):(fold*fold_size)
      
      # Split the data into training and validation sets
      X_train <- X[-val_idx, ]
      y_train <- y[-val_idx]
      X_val <- X[val_idx, ]
      y_val <- y[val_idx]
      
      # Fit the model on the training set
      fit <- monomvn::blasso(X = X_train, y = y_train, T = T, 
                             RJ = RJ, lambda2 = lambda2, verb = verb)
      
      # Make predictions on the validation set
      y_pred <- X_val %*% fit$beta
      
      # Compute and store the mean squared error
      cv_errors[i] <- cv_errors[i] + mean((y_val - y_pred)^2) / cv_folds
    }
  }
  
  # Choose the lambda2 value that minimizes the cross-validation error
  best_lambda2 <- lambda_seq[which.min(cv_errors)]
  
  # Refit the model on the full dataset with the selected lambda2 value
  fit_blasso <- monomvn::blasso(X = X, y = y, T = T, RJ = RJ, 
                                lambda2 = best_lambda2, verb = verb)
  
  # Return the fitted model and the selected lambda2 value
  return(list("model" = fit_blasso, "best_lambda2" = best_lambda2))
}

# Fit the model
# T1 Data
blasso_T1_LD <- fit_blasso_model(data = T1_LD)
blasso_T1_LD %>% plot()

## summarize the beta (regression coefficients) estimates
plot(blasso_T1_LD, burnin=100)
points(drop(blasso_T1_LD$b), col=2, pch=20)
points(drop(blasso_T1_LD$b), col=3, pch=18)
legend("topleft", c("blasso-map", "lasso", "lsr"),
       col=c(2,2,3), pch=c(21,20,18))

# Plot the density of the coefficient values
dev.new(width=18, height=10)
plot(colMeans(blasso_T1_LD$beta))


# T1 Data
blasso_T1_ED <- fit_blasso_model(data = T1_ED)
blasso_T1_HD <- fit_blasso_model(data = T1_HD)
blasso_T1_VD <- fit_blasso_model(data = T1_VD)

# T2 Data
blasso_T2_LD <- fit_blasso_model(data = T2_LD)
blasso_T2_ED <- fit_blasso_model(data = T2_ED)
blasso_T2_HD <- fit_blasso_model(data = T2_VD)
blasso_T2_VD <- fit_blasso_model(data = T2_HD)

# T3 Data
blasso_T3_LD <- fit_blasso_model(data = T3_LD)
blasso_T3_ED <- fit_blasso_model(data = T3_ED)
blasso_T3_HD <- fit_blasso_model(data = T3_VD)
blasso_T3_VD <- fit_blasso_model(data = T3_HD)

# T4 Data
blasso_T4_LD <- fit_blasso_model(data = T4_LD)
blasso_T4_ED <- fit_blasso_model(data = T4_ED)
blasso_T4_HD <- fit_blasso_model(data = T4_VD)
blasso_T4_VD <- fit_blasso_model(data = T4_HD)


# View the summary of the fit
print(blasso_T1_LD)
# Extract the coefficients
coefficients <- blasso_T1_LD$beta
summary(blasso_T1_LD)

plot(blasso_T1_LD)

#### SIM DATA. RJMCMC 'rjmcmc' ####


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
check_data(df)

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
outlier_indices <- detect_outliers_iqr(df, names(df))

# Print the number of outliers and their indices
cat(paste0("Number of outliers detected: ", length(outlier_indices)), "\n")
cat(paste0("Outlier indices: ", outlier_indices), "\n")

# Remove the outliers from the data frame by subsetting the data frame to exclude these rows.
# The negative sign before outlier_indices means "all rows EXCEPT these indices".
df_no_outliers <- df[-outlier_indices, ]




# Check for Multicollinearity
# Fit the regression model
model <- lm(ViolentCrimesPerPop ~ ., data = df)
car::vif(model)

library(caret)
# Find predictors with near zero variance
nzv <- nearZeroVar(df, saveMetrics= TRUE)
print(nzv)


#### CRIME. LASSO PENALISED REGRESSION 'glmnet' ####

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
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'nfolds' is a function
  if (!is.numeric(nfolds) || nfolds <= 0 || round(nfolds) != nfolds) {
    stop("'nfolds' must be a positive integer.")
  }
  
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
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'nfolds' is a function
  if (!is.numeric(nfolds) || nfolds <= 0 || round(nfolds) != nfolds) {
    stop("'nfolds' must be a positive integer.")
  }
  
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
#     seed               - A NEGATIVE integer used for setting the seed for reproducibility.
#
# OUTPUT:
#     A list containing:
#         result - The fitted Spike and Slab model.
#
fit_crime_spikeslab_prior <- function(data, bigp_smalln, bigp_smalln_factor, y, seed = -42) {
  
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

# Function to fit the Spike-and-Slab LASSO model, plot the coefficients,
# and extract selected variables from a given data frame.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, and the rest are predictors.
#     y - prediction variable.
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
fit_crime_sslasso <- function(data, y, lambda1 = 1, lambda0 = seq(1, nrow(data),), 
                        theta = 0.5, eps = 0.001, plot_width = 6, plot_height = 4) {
  
  # Input checks
  # Check that 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  # Check that 'lambda1' is a positive numeric value
  if (!is.numeric(lambda1) || lambda1 <= 0) {
    stop("'lambda1' must be a positive numeric value.")
  }
  
  # Check that 'lambda0' is a numeric sequence
  if (!is.numeric(lambda0)) {
    stop("'lambda0' must be a numeric sequence.")
  }
  
  # Check that 'theta' is a numeric value between 0 and 1
  if (!is.numeric(theta) || theta < 0 || theta > 1) {
    stop("'theta' must be a numeric value between 0 and 1.")
  }
  
  # Check that 'eps' is a small positive numeric value
  if (!is.numeric(eps) || eps <= 0 || eps >= 1) {
    stop("'eps' must be a small positive numeric value (0 < eps < 1).")
  }
  
  # Check that 'plot_width' is a positive numeric value
  if (!is.numeric(plot_width) || plot_width <= 0) {
    stop("'plot_width' must be a positive numeric value.")
  }
  
  # Check that 'plot_height' is a positive numeric value
  if (!is.numeric(plot_height) || plot_height <= 0) {
    stop("'plot_height' must be a positive numeric value.")
  }
  
  
  # Separate data into X and y
  X <- model.matrix(~ ., data = data[, -ncol(data)])  # Design matrix (excluding the y column)
  
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

# Call the function with the Crimes data
ssl_crime <- fit_crime_sslasso(data = df, y = df$ViolentCrimesPerPop)
#sslasso_crime

# The output contains coefficients, ever_selected, and plot
ssl_crime$coefficients
ssl_crime$ever_selected





#### CRIME. HORSESHOE PRIOR 'horseshoe' ####

# Function to fit the Horseshoe prior model, plot predicted values against observed values,
#   and plot credible intervals for coefficients.
#
# INPUTS:
#     data - Crime dataset.
#     y - prediction variable.
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
fit_crime_horseshoe_model <- function(data, y, method.tau, tau, method.sigma, 
                                      burn, nmc, thin, alpha) {
  
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
  X <- model.matrix(~ ., data = data[, -ncol(data)])  # Design matrix (excluding the y column)
  X <- cbind(X[, 1], scale(X[, -1], center = TRUE, scale = TRUE))
  
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
  #cat("Posterior mean of tau:", fit_horseshoe$TauHat, "\n")
  
  # Load the Hmisc package for plotting credible intervals
  #library(Hmisc)
  
  # Plot the credible intervals for coefficients
  xYplot(Cbind(fit_horseshoe$BetaHat, fit_horseshoe$LeftCI, fit_horseshoe$RightCI) ~ 1:ncol(X),
         type = c("p", "g", "g"), ylab = "Coefficients", xlab = "Variables",
         main = "Credible Intervals for Coefficients")
  
  # Return the fitted horseshoe model
  return(fit_horseshoe)
}


# Run the functions an extract the results
# Truncated Cauchy prior
crime_horseshoe_t_model <- fit_crime_horseshoe_model(data = df,
                                                     y = df$ViolentCrimesPerPop,
                                                   method.tau = "truncatedCauchy",
                                                   tau = 10,
                                                   method.sigma = "Jeffreys", 
                                                   burn = 1000, 
                                                   nmc = 5000, 
                                                   thin = 10, 
                                                   alpha = 0.05)
#### CRIME. HORSESHOE PRIOR 'bayesreg' ####

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
fit_horseshoe_bs_crime <- function(data,
                                   y,
                                   n.samples = 1000, 
                                   burnin = 1000, 
                                   thin = 5, 
                                   coef_threshold = 1,
                                   prior = "hs") {
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'n.samples' is a positive integer
  if (!is.numeric(n.samples) || n.samples <= 0 || round(n.samples) != n.samples) {
    stop("'n.samples' must be a positive integer.")
  }
  
  # Ensure 'burnin' is a positive integer
  if (!is.numeric(burnin) || burnin <= 0 || round(burnin) != burnin) {
    stop("'burnin' must be a positive integer.")
  }
  
  # Ensure 'thin' is a positive integer
  if (!is.numeric(thin) || thin <= 0 || round(thin) != thin) {
    stop("'thin' must be a positive integer.")
  }
  
  # Ensure 'coef_threshold' is a positive numeric
  if (!is.numeric(coef_threshold) || coef_threshold <= 0) {
    stop("'coef_threshold' must be a positive numeric.")
  }
  
  # Separate data into X and y
  X <- model.matrix(~ ., data = data[, -ncol(data)])  # Design matrix (excluding the y column)
  X <- cbind(Intercept = X[, 1], scale(X[, -1], center = TRUE, scale = TRUE))
  
  # Combine standardized predictors and response variable into a new data frame
  data_std <- cbind(y, as.data.frame(X))
  
  # Formula to fit the model (assuming that y is the name of your response variable)
  formula <- as.formula(paste("y ~", paste(colnames(X)[-1], collapse = " + ")))
  
  # Fit the initial horseshoe model using the bayesreg package
  fit_horseshoe_b <- bayesreg::bayesreg(formula, data = data_std, 
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
                                          prior = prior,
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

# Fit the model
hs_bs_crime <- fit_horseshoe_bs_crime(data = df, y = df$ViolentCrimesPerPop,
                                      prior = "hs")



#### CRIME. HORSESHOE + PRIOR 'bayesreg' ####

# Fit the model
hsp_bs_crime <- fit_horseshoe_bs_crime(data = df, y = df$ViolentCrimesPerPop,
                                       prior = "hs+")



#### CRIME. SSS 'BayesS5' ####

# Function to fit a sparse Bayesian linear regression model using the BayesS5 
#   package. The S5 function is used to fit a model where sparsity is promoted 
#   in the regression coefficients. 
#
# INPUTS:
#     data - Data frame where the first column is the response variable, 
#            and the remaining columns are predictors.
#     y - Target variable.
#     ind_fun - A function to define the inclusion indicators of the model.
#     model - An object of class Model defining the prior distribution.
#     tuning - Tuning parameter for the S5 function.
#     C0 - Normalisation constant for the S5 function.
#
# OUTPUTS:
#     fit_S5 - An S5 object, which is the fitted model.
#
fit_S5_crime <- function(data, y, ind_fun = ind_fun_pemom,
                         model = Bernoulli_Uniform, tuning = 20,
                         C0 = 5) {
  # Input checks
  
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure y is a numeric vector 
  if (!is.vector(y) && !is.numeric(y)) {
    stop("Input 'y' must be a numeric vector.")
  }
  
  # Ensure 'ind_fun' is a function
  if (!is.function(ind_fun)) {
    stop("'ind_fun' must be a function.")
  }
  
  # Ensure 'model' is a valid model object or a string representing a model
  # This check will depend on your specific use case, 
  # add a more specific check if needed
  #if (!is.character(model) && !is.list(model)) {
  #  stop("'model' must be a model object or a character string representing a model.")
  #}
  
  # Ensure 'tuning' is a positive numeric
  if (!is.numeric(tuning) || tuning <= 0) {
    stop("'tuning' must be a positive numeric value.")
  }
  
  # Ensure 'C0' is a positive numeric
  if (!is.numeric(C0) || C0 <= 0) {
    stop("'C0' must be a positive numeric value.")
  }
  
  # Scale data and create a matrix
  #X <- scale(model.matrix(~ . - 1, data = data[, -ncol(data)]), 
  #           center = TRUE, scale = TRUE)
  
  # Separate data into X and y
  X <- model.matrix(~ . - 1, data = data[, -ncol(data)])  # Design matrix (excluding the y column)
  
  # Rename the columns
  colnames(X) <- paste0("X", 1:ncol(X))
  
  # Fit the model using the S5 function from the BayesS5 package
  fit_S5 <- BayesS5::S5(X = X, y = y, ind_fun = ind_fun, model = model,
                        tuning = tuning, C0 = C0)
  
  # Return the fitted model
  return(fit_S5)
}

# Fit the model
S5_crime <- fit_S5_crime(data = df, y = df$ViolentCrimesPerPop)# %>% results()

# Print the MAP model
print(result(S5_crime)$hppm) 
print(result(S5_crime)$hppm.prob) # the posterior probability of the hppm
plot(result(S5_crime)$marg.prob, ylim = c(0, 1), ylab = "marginal inclusion probability")


#### CRIME. LAPLACE APPROXIMATION 'LaplacesDemon' ####
#### CRIME. BAYESIAN LASSO 'monomvn' ####

# Function to fit a Bayesian LASSO regression model using the 'monomvn' package.
# The function implements cross-validation for hyperparameter tuning and 
# variable selection in the regression coefficients.
#
# INPUTS:
#   data - Data frame where the first column is the response variable, 
#         and the remaining columns are predictors.
#   y - Target variable.
#   T - Number of iterations in the MCMC chain.
#   RJ - Logical flag indicating whether to perform Reversible Jump MCMC.
#   verb - Verbosity level of the function's output.
#   cv_folds - Number of cross validations.
#   lambda_seq - Sequence of lambda2 values to loop over for tuning.
#
# OUTPUTS:
#   A list containing the following components:
#   model - A blasso object, which is the fitted model.
#   best_lambda2 - The lambda2 value that minimizes the cross-validation error.
#
fit_blasso_crime <- function(data, y, T = 1000, RJ = FALSE, verb = 1, 
                             cv_folds = 1, lambda_seq = 0.8) {
  
  # Input validation
  # Check if the input data is of the correct format: a data frame
  if (!is.data.frame(data)) {
    # If the input is not a data frame, throw an error and stop execution
    stop("'data' must be a data frame.")
  }
  
  # Check if the number of iterations 'T' is a positive numeric value
  if (!is.numeric(T) || T <= 0) {
    # If 'T' is not a positive number, throw an error and stop execution
    stop("'T' must be a positive numeric value.")
  }
  
  # Check if the flag 'RJ' is a logical value
  if (!is.logical(RJ)) {
    # If 'RJ' is not a logical value (TRUE/FALSE), throw an error and stop execution
    stop("'RJ' must be a logical value.")
  }
  
  # Check if the verbosity level 'verb' is a non-negative numeric value
  if (!is.numeric(verb) || verb < 0) {
    # If 'verb' is not a non-negative number, throw an error and stop execution
    stop("'verb' must be a non-negative numeric value.")
  }
  
  # Check if the number of cross-validation folds 'cv_folds' is a positive numeric value
  if (!is.numeric(cv_folds) || cv_folds <= 0) {
    # If 'cv_folds' is not a positive number, throw an error and stop execution
    stop("'cv_folds' must be a positive numeric value.")
  }
  
  # Check if the sequence of lambda values 'lambda_seq' is a numeric vector
  #if (!is.numeric(lambda_seq)) {
  #  # If 'lambda_seq' is not a numeric vector, throw an error and stop execution
  #  stop("'lambda_seq' must be a numeric vector.")
  #}
  
  # Scale data and create a matrix
  X <- scale(model.matrix(~ . - 1, data = data[, -ncol(data)]), 
             center = TRUE, scale = TRUE)
  
  # Initialize variables for cross-validation
  cv_errors <- rep(0, length(lambda_seq))
  fold_size <- round(nrow(data) / cv_folds)
  
  # Loop over lambda values
  for (i in 1:length(lambda_seq)) {
    lambda2 <- lambda_seq[i]
    
    # Cross-validation loop
    for (fold in 1:cv_folds) {
      # Index for validation set
      val_idx <- ((fold-1)*fold_size+1):(fold*fold_size)
      
      # Split the data into training and validation sets
      X_train <- X[-val_idx, ]
      y_train <- y[-val_idx]
      X_val <- X[val_idx, ]
      y_val <- y[val_idx]
      
      
      # Fit the model on the training set
      fit <- monomvn::blasso(X = X_train, y = y_train, T = T, thin = 10, M = ncol(X), 
                             RJ = FALSE, lambda2 = 0.8, verb = verb)
      
      # Make predictions on the validation set
      y_pred <- X_val %*% fit$beta
      
      # Compute and store the mean squared error
      cv_errors[i] <- cv_errors[i] + mean((y_val - y_pred)^2) / cv_folds
    }
  }
  
  # Choose the lambda2 value that minimizes the cross-validation error
  best_lambda2 <- lambda_seq[which.min(cv_errors)]
  
  # Refit the model on the full dataset with the selected lambda2 value
  fit_blasso <- monomvn::blasso(X = X, y = y, T = T, RJ = RJ, 
                                lambda2 = best_lambda2, verb = verb)
  
  # Return the fitted model and the selected lambda2 value
  return(list("model" = fit_blasso, "best_lambda2" = best_lambda2))
}

# Fit the model
blasso_crime <- fit_blasso_crime(data = df, y = df$ViolentCrimesPerPop)
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

# Could we maybe fit models with the most important variables that 
#   were identified in previous methods?




