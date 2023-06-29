#### SET UP ####

# Combine the list of libraries from both scripts
library_list <- c("tidyverse", "corrplot", "betareg", "R.matlab", "glmnet", "dplyr", 
                  "cowplot", "coda", "igraph", "R6", "nimble", "MASS", "xgboost",
                  "caret", "spikeslab")

# Uncomment the following lines if you need to install the packages
# for (i in library_list) {
#   install.packages(i, character.only = TRUE)
# }

# Load the libraries
for (i in library_list) {
  library(i, character.only = TRUE)
}

# Set working directory (assuming you want to set it to the 'main' directory)
setwd("~/Documents/Dissertation/main")

# Remove unwanted objects
rm(library_list, i)




#### SOURCE SIMULATED DATA ####

# Source the file that contains the simulation functions
source("simulate_data.R")

#### SOURCE DATA FRAMES AND VECTORS OF CRIME DATA ####
# Source the file that contains the crime data
#source("data_crime_raw.R")





#### SIM DATA. LASSO PENALISED REGRESSION ####

# Function to fit Lasso regression on different datasets and extract the best-fitted model.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
# OUTPUT:
#         lasso_model: The fitted Lasso model with the optimal lambda value.
#
fit_lasso <- function(data, nfolds = 10) {
  
  # Create the matrix of predictors by excluding the intercept term, 
  # and standardize the predictors. Add an intercept column with all values equal to 1.
  xmatrix <- cbind(Intercept = 1, scale(model.matrix(~ . - 1, data = data[, -1])))
  
  # Fit the Lasso model on the design matrix with alpha = 1 (Lasso penalty).
  # Note: alpha = 0 corresponds to Ridge and alpha between 0 and 1 corresponds to ElasticNet.
  initial_lasso <- glmnet(x = xmatrix, y = data$y, alpha = 1)
  
  # Perform k-fold cross-validation to find the optimal value of the regularization 
  # parameter lambda that minimizes the cross-validation error.
  lasso_cv <- cv.glmnet(x = xmatrix, y = data$y, alpha = 1, nfolds = nfolds)
  
  # Plot the regularization path which is the coefficient profiles of the Lasso model
  # as a function of lambda. The vertical line represents the optimal lambda value 
  # that minimizes the cross-validation error.
  #par(mfrow = c(1, 2))
  #plot(initial_lasso, xvar = "lambda")
  #abline(v = log(initial_lasso$lambda.min), lwd = 4, lty = 2)
  
  # Plot the cross-validation errors as a function of lambda.
  #plot(lasso_cv)
  #abline(v = log(lasso_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Lasso model using the optimal lambda value obtained from cross-validation.
  lasso_model <- glmnet(x = xmatrix, y = data$y, alpha = 1, lambda = lasso_cv$lambda.min)
  
  # Optionally, you can print a summary of the model with the coefficients 
  # corresponding to the optimal lambda value.
  # coef(T1_LD_lasso, s = T1_LD_lasso_cv$lambda.min)
  
  # Return the fitted Lasso model
  return(lasso_model)
}




#### SIM DATA. ELASTIC-NET PENALISED REGRESSION ####

# Function to fit Elastic net regression on different datasets and extract the best-fitted model.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#               The response variable should be named "y".
#         nfolds - number of folds for cross-validation (default is 10).
# OUTPUT:
#         elnet_model: The fitted Elastic net model with the optimal lambda value.
#
fit_elnet <- function(data, nfolds = 10) {
  
  # Create the matrix of predictors by excluding the intercept term, 
  # and standardise the predictors. Add an intercept column with all values equal to 1.
  xmatrix <- cbind(Intercept = 1, scale(model.matrix(~ . - 1, data = data[, -1])))
  
  # Fit the Lasso model on the design matrix with alpha = 1 (Lasso penalty).
  # Note: alpha = 0 corresponds to Ridge and alpha between 0 and 1 corresponds to ElasticNet.
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
  #plot(elnet_cv)
  #abline(v = log(elnet_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the Elastic net model using the optimal lambda value obtained from cross-validation.
  elnet_model <- glmnet(x = xmatrix, y = data$y, alpha = 1, lambda = elnet_cv$lambda.min)
  
  # Optionally, you can print a summary of the model with the coefficients 
  # corresponding to the optimal lambda value.
  # coef(T1_LD_elnet, s = T1_LD_elnet_cv$lambda.min)
  
  # Return the fitted Lasso model
  return(elnet_model)
}




#### SIM DATA. LASSO AND ELASTIC NET MODEL EXTRACTION ####

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




#### SIM DATA. AIC ####

# Fit a full model with all predictors
full_model <- lm(y ~ ., data = T1_LD)

# Perform stepwise selection
selected_model <- step(full_model, direction = "both")

# Display the selected model
summary(selected_model)











#### SIM DATA. XGBOOST ####

# Set the initial parameters for XGBoost
# These are general parameters to define the boosting algorithm
params <- list(
  eta = 0.1,                   # Learning rate
  max_depth = 10,              # Max depth of the trees
  subsample = 0.8,             # Fraction of observations to be randomly sampled for each tree
  colsample_bytree = 0.8,      # Fraction of features to be randomly sampled for each tree
  min_child_weight = 1,        # Minimum sum of instance weight needed in a leaf
  gamma = 0,                   # Minimum loss reduction required to make a further partition
  objective = "reg:linear"     # Specify the learning task and the corresponding learning objective
)

# Define an extensive grid for hyperparameter tuning
# This grid consists of multiple values for each parameter, allowing for more refined tuning
grid <- expand.grid(
  nrounds = c(50, 100, 150),                  # Number of boosting rounds
  max_depth = c(3, 5, 7, 9),                  # Maximum depth of the trees
  eta = c(0.01, 0.1, 0.3),                    # Learning rate
  gamma = c(0, 0.1, 1),                       # Minimum loss reduction required
  colsample_bytree = c(0.6, 0.8, 1),          # Fraction of features to be randomly sampled for each tree
  min_child_weight = c(1, 3, 5),              # Minimum sum of instance weight needed in a leaf
  subsample = c(0.8, 1)                       # Fraction of observations to be randomly sampled for each tree
)

# Define cross-validation strategy
# This helps in assessing the model's performance in an unbiased way using a subset of the data
cv <- trainControl(
  method = "repeatedcv",     # Repeated cross-validation
  number = 5,                # Number of folds
  repeats = 3,               # Number of complete sets of folds to compute
  verboseIter = TRUE,        # Display training progress
  returnData = FALSE,        # Do not return the training data
  returnResamp = "all",      # Save all resampling scores
  allowParallel = TRUE,      # Allow parallel processing
)


# Function to train and evaluate an XGBoost model on different datasets and plot feature importance.
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#                The response variable should be named "y".
#         cv   - a trainControl object defining the cross-validation strategy.
#         grid - a data frame defining the grid of hyperparameters to search over.
# OUTPUT:
#         A list containing:
#               model - The trained XGBoost model.
#               rmse  - The root mean squared error (RMSE) of the model on the test set.
#
train_evaluate_xgb <- function(data, cv, grid) {
  # Separate features and target from the dataset
  # Features (exclude the target variable 'y')
  X <- as.matrix(T1_LD[, -1])
  # Target variable
  y <- T1_LD$y
  
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



#### SIM DATA. SPIKE AND SLAB PRIOR #### 

# Function to fit a Spike and Slab model using spikeslab package and evaluate it on different datasets.
# It also plots the path of the estimates for the Spike and Slab model.
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
#     A list containing:
#         result - The fitted Spike and Slab model.
#
fit_spikeslab_prior <- function(data, bigp_smalln, bigp_smalln_factor, seed = 42) {
  
  # Define the formula for the model
  # y ~ . indicates that y is the response variable and . represents all other variables as predictors
  formula <- as.formula("y ~ .")
  
  # Run the spikeslab model
  # Various parameters are passed to the spikeslab function:
  # n.iter1 and n.iter2 control the number of iterations,
  # bigp.smalln and bigp.smalln.factor control high-dimensional low sample size adjustments,
  # other parameters control the model settings
  result <- spikeslab(formula, data = data,
                      n.iter1 = 1000, n.iter2 = 1000, mse = TRUE,
                      bigp.smalln = bigp_smalln, bigp.smalln.factor = bigp_smalln_factor,
                      r.effects = NULL, max.var = 500, center = TRUE, intercept = TRUE,
                      fast = TRUE, beta.blocks = 5, verbose = FALSE, ntree = 300,
                      seed = seed)
  
  # Print the result summary
  print(result)
  
  # Plot the path of the estimates for the Spike and Slab model
  plot(result, plot.type = "path")
  
  # Return the result
  return(list(result = result))
}

# Define a list to store the results
results_list <- list()

# List of all datasets by dimensionality as some adjustments need to be made 
#   when fitting in the models
data_LD_ED <- list(T1_LD, T2_LD, T3_LD, T4_LD)#, T1_ED, T2_ED, T3_ED, T4_ED) # bigger n
data_HD_VD <- list(T1_HD, T2_HD, T3_HD, T4_HD, T1_VD, T2_VD, T3_VD, T4_VD) # big p

# Iterate through each dataset
for (i in 1:length(data_LD_ED)) {
  # Fit the spikeslab model and store the results
  results_list[[i]] <- fit_spikeslab_prior(data = data_LD_ED[[i]], 
                                           bigp_smalln = FALSE)
}

# Iterate through each dataset
for (i in 1:length(data_HD_VD)) {
  # Fit the spikeslab model and store the results
  results_list[[i]] <- fit_spikeslab_prior(data = data_HD_VD[[i]], 
                                           bigp_smalln = TRUE, 
                                           bigp_smalln_factor = 1)
}




























#### CRIME ANALYSIS ####

# Fit in a model
#mdl_fr_lasso <- glmnet(x = df_predictors, y = vector_target, alpha = 1)


#cvlasso <- cv.glmnet(x = as.matrix(df_predictors), y = vector_target, alpha = 1, 
#                     nfolds = 10)


#par(mfrow = c(1, 2))
#plot(mdl_fr_lasso, xvar="lambda")
#abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)
#plot(cvlasso)
#abline(v = log(cvlasso$lambda.min), lwd = 4, lty = 2)

# Extract the best lambda
#best_lambda <- cvlasso$lambda.min

# Refit the model with the best lasso
#mdl_fr_lasso_1 <- glmnet(x = as.matrix(df_predictors), y = vector_target, 
#                         alpha = 1, lambda = best_lambda)