#### PENALISED REGRESSION 'glmnet' ####

# Function to fit penalised regression on different datasets and extract the selected predictors
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#                The response variable should be named "y".
#         nfolds - number of folds for cross-validation, default = 10.
#         alpha - numeric entry 1 for Lasso, 0.5 for Elastic Net.
# OUTPUT:
#         A list containing:
#               selected_predictors - a data frame with the predictors selected by the model 
#                                     with their respective coefficients.
#               model_fit - the fitted glmnet model.
#
fit_glmnet <- function(data, nfolds = 10, alpha = 0.5) {
  
  # Input checks
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'y' is in the data
  if (!"y" %in% names(data)) {
    stop("The response variable 'y' is not present in the input data.")
  }
  
  # Ensure 'nfolds' is a positive integer
  if (!is.numeric(nfolds) || nfolds <= 0 || round(nfolds) != nfolds) {
    stop("'nfolds' must be a positive integer.")
  }
  
  # Ensure only Lasso or Elnet alpha values are fit
  if (!is.numeric(alpha) || !(alpha %in% c(0.5, 1))) {
    stop("Alpha should be a numeric value of either 0.5 (Elastic Net) or 1 (Lasso).")
  }
    
  # Extract the target
  y <- data$y
    
  # Remove the original categorical variables
  data <- data.frame(subset(data, select = -y))
    
  # Combine intercept, scaled continuous variables
  X <- model.matrix(~ . - 1, data = data)
  
  # Perform k-fold cross-validation to find the optimal value of the regularisation 
  #   parameter lambda that minimises the cross-validation error
  set.seed(7)
  model_cv <- cv.glmnet(x = X, y = y, alpha = alpha, nfolds = nfolds)
  
  # Plot the cross-validation errors as a function of lambda.
  plot(model_cv)
  abline(v = log(model_cv$lambda.min), lwd = 4, lty = 2)
  
  # Refit the model using the optimal lambda value obtained from cross-validation
  set.seed(7)
  model_fit <- glmnet(x = X, y = y, alpha = alpha, lambda = model_cv$lambda.min)
  
  # Extract the coefficients from the  model
  coefficients <- coef(model_fit, s = model_fit$lambda.min)
  
  # Find the names of the variables with non-zero coefficients
  selected_variable_names <- rownames(coefficients)[coefficients[, 1] != 0]
  
  # Extract the non-zero coefficients
  selected_predictors <- coefficients[selected_variable_names, 1] %>% data.frame()
  
  # Sort the selected_predictors in descending order by the absolute value of its column
  selected_predictors <- selected_predictors %>% arrange(desc(abs(selected_predictors[,1])))
  
  # Return the list of selected predictors and model itself
  return(list(selected_predictors = selected_predictors, model_fit = model_fit))
}




#### XGBOOST 'caret' ####

# Function to train and evaluate an XGBoost model from 'caret' package on different 
#   datasets and plot feature importance
# INPUTS:
#         data - a data frame containing the predictors and the response variable.
#                The response variable should be named "y".
#         cat_var - logical factor if there are categorical variables.
#         xgb_cv   - a trainControl object defining the cross-validation strategy.
#         xgb_grid - a data frame defining the grid of hyperparameters to search over.
# OUTPUT:
#         A list containing:
#               model - the trained XGBoost model.
#               rmse  - the root mean squared error (RMSE) of the model on the test set.
#               feature_importance - a data frame showing the importance of each feature.
#               importance_plot - a plot object showing the feature importance.
#
fit_xgb <- function(data, cat_var = FALSE, xgb_cv, xgb_grid) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data should be a data frame.")
  }
  
  # Check if data contains 'y' target
  if (!"y" %in% names(data)) {
    stop("data should contain a column named 'y' as the response variable.")
  }
  
  # Check if xgb_grid is a valid data frame with required columns
  if (!is.data.frame(xgb_grid) || 
      !all(c("nrounds", "max_depth", "eta", "gamma", "colsample_bytree", "min_child_weight", "subsample") %in% names(xgb_grid))) {
    stop("xgb_grid should be a data frame with hyperparameters to be tuned.")
  }
  
  # Check if xgb_cv is a valid object with required parameters
  if (!inherits(xgb_cv, "trainControl") || 
      !all(c("method", "number", "repeats", "verboseIter", "returnData", "returnResamp", "allowParallel") %in% names(xgb_cv))) {
    stop("xgb_cv should be a trainControl object with appropriate parameters.")
  }
 
  # Extract the target
  y <- data$y
    
  # Remove the target
  data <- data.frame(subset(data, select = -y))
    
  # Combine scaled continuous variables, intercept
  X <- model.matrix(~ . - 1, data = data)
  
  # Split the dataset into training and testing sets
  # createDataPartition helps in creating stratified random samples
  set.seed(42)
  index <- createDataPartition(y, p = 0.8, list = FALSE)
  # Extract training features
  X_train <- X[index, ]   
  # Extract training target
  y_train <- y[index]    
  # Extract testing features
  X_test <- X[-index, ]  
  # Extract testing target
  y_test <- y[-index]            
  
  # Train the XGBoost model with cross-validation and parameter tuning
  xgb_model <- train(
    # Feature matrix
    x = X_train,   
    # Target vector
    y = y_train,     
    # Cross-validation strategy
    trControl = xgb_cv,   
    # Grid of hyperparameters to tune
    tuneGrid = xgb_grid,   
    # XGBoost model
    method = "xgbTree",         
    metric = "RMSE",
    maximize = FALSE,
    # Specify the learning task and the corresponding learning objective
    objective = "reg:linear"                    
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
  
  # Save the plot to an object so it can be returned
  importance_plot <- recordPlot(xgb.plot.importance(importance_matrix))
  
  # Return the model, feature importance dataframe, RMSE, and plot
  return(list(
    "model" = xgb_model,
    "feature_importance" = importance_matrix,
    "rmse" = rmse,
    "importance_plot" = importance_plot
  ))
}




#### SPIKE AND SLAB PRIOR 'spikeslab' #### 

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
fit_spikeslab_prior <- function(data, bigp_smalln = FALSE, bigp_smalln_factor = 0, 
                                screen = FALSE, K = 10, seed = -42) {
  
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
    
  # Extract the target
  y <- scale(data$y)
    
  # Remove the 'y' column from the data frame and convert the remaining data into a model matrix
  X <- model.matrix(~ . - 1, data = data[, -which(names(data) == "y")])
    
  # Scale the predictor variables
  X <- scale(X)
  
  # Run the spikeslab model
  ss_results <- spikeslab::cv.spikeslab(
    # Formula representing the relationship between predictors and response
    #formula,  
    # The matrix containing the variables in the formula
    x = X,
    y = y,
    K = K,
    # The number of iterations in the two MCMC chains used in spikeslab.
    # n.iter1 Number of burn-in Gibbs sampled values (i.e., discarded values)
    #    and n.iter2 is for the number of Gibbs sampled values, following burn-in.
    n.iter1 = 1000,        
    n.iter2 = 5000,        
    # Calculate the mean squared error as part of the model evaluation
    #mse = TRUE,           
    # High-dimensional low sample size adjustments.
    # bigp.smalln - logical flag, if TRUE adjustments for high-dimensional low sample size are made.
    # bigp.smalln.factor - controls the magnitude of the adjustments.
    bigp.smalln = bigp_smalln,                 
    bigp.smalln.factor = bigp_smalln_factor,   
    # To screen the variables when p is big
    screen = screen,
    # If TRUE, an intercept term is included in the model
    intercept = TRUE,      
    # If TRUE, outputs progress and additional information while fitting the model
    verbose = TRUE,       
    # Seed for random number generator, for reproducibility of results
    seed = seed
  )
  
  # Plot the path of the estimates for the Spike and Slab model
  #plot(ss_results, plot.type = "path")
  
  # Return the result
  return(ss_results)
}




#### SPIKE-AND-SLAB LASSO 'SSLASSO' ####

# Function to fit the Spike-and-Slab LASSO model, plot the coefficients,
#   and extract selected variables from a given data frame.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, and the rest are predictors.
#     lambda1 - Slab variance parameter.
#     lambda0 - Vector of spike penalty parameters.
#     var - variance of error, unknown of fixed.
# OUTPUTS:
#     A list containing:
#         coefficients - The fitted matrix of coefficients.
#         ever_selected - A binary vector indicating which variables were
#                        ever selected along the regularization path.
#         plot - A plot of the coefficient paths for the fitted model.
fit_sslasso <- function(data, lambda1 = 1, lambda0 = seq(1, nrow(data), length.out = 100), 
                        var = "unknown") {
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

  # Check that 'plot_width' is a positive numeric value
  if (!is.numeric(plot_width) || plot_width <= 0) {
    stop("'plot_width' must be a positive numeric value.")
  }
  
  # Check that 'plot_height' is a positive numeric value
  if (!is.numeric(plot_height) || plot_height <= 0) {
    stop("'plot_height' must be a positive numeric value.")
  }
  
  # Extract the target
  y <- data$y
    
  # Remove the original categorical variables
  data <- data.frame(subset(data, select = -y))
    
  # Combine scaled continuous variables, intercept
  X <- model.matrix(~ . - 1, data = data)
  
  # Fit the SSLASSO model
  set.seed(42)
  result <- SSLASSO(X = X, y = y, penalty = "adaptive", variance = var,
                    lambda1 = lambda1, lambda0 = lambda0, warn = TRUE)
  
  # Extract selection indicators and determine which variables were ever selected
  selected_variables <- result$select
  ever_selected <- apply(selected_variables, 1, max)
  
  # Assuming X is a data.frame or matrix
  variable_names <- colnames(X)
  
  # selected_variable_indices gives us the indices of the selected variables
  selected_variable_indices <- which(ever_selected == 1)
  
  # Get the names of the selected variables
  selected_variable_names <- variable_names[selected_variable_indices]
  
  # Return the results as a list
  return(list(coefficients = result$beta, ever_selected = ever_selected, 
              selected_variable_names = selected_variable_names, plot = result))
}




#### HORSESHOE PRIOR. 'horseshoe' ####

# Function to fit the Horseshoe model, plot predicted values against observed values,
# and plot credible intervals for coefficients.
#
# INPUTS:
#     data - Data frame where the first column is the response variable, and the rest are predictors.
#     method.tau - Method for handling tau (truncatedCauchy, halfCauchy, or fixed).
#     method.sigma - Method for handling sigma (Jeffreys or fixed).
#     burn - Number of burn-in MCMC samples.
#     nmc - Number of posterior draws to be saved.
#     thin - Thinning parameter of the chain.
#     alpha - Level for the credible intervals.
# OUTPUTS:
#     A list containing:
#       - model: The fitted horseshoe model.
#       - sel_var: The names of the selected variables in the model.
#
fit_hs_horseshoe <- function(data, method.tau, method.sigma = "Jeffreys", 
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
  
  # Extract the target
  y <- data$y
    
  # Remove the original categorical variables
  data <- data.frame(subset(data, select = -y))
    
  # Combine scaled continuous variables, intercept
  X <- model.matrix(~ . - 1, data = data)
  
  # Fit the horseshoe model using the horseshoe package
  set.seed(42)
  fit_horseshoe <- horseshoe::horseshoe(y = y, X = X, 
                                        method.tau = method.tau,
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
  
  # Plot the credible intervals for coefficients
  xYplot(Cbind(fit_horseshoe$BetaHat, fit_horseshoe$LeftCI, fit_horseshoe$RightCI) ~ 1:ncol(X),
         type = c("p", "g", "g"), ylab = "Coefficients", xlab = "Variables",
         main = "Credible Intervals for Coefficients")
  
  # Use HS.var.select to get the selected variables
  selected <- HS.var.select(fit_horseshoe, y = y, 
                            # Threshold left as default
                            method = "intervals", threshold = 0.5)
  
  # The variable names for the selected variables
  variable_names <- colnames(X)
  
  # Get the indices of the selected variables
  selected_indices <- which(selected == 1)
  
  # Get the names of the selected variables
  sel_var <- variable_names[selected_indices]
  
  # Return the fitted horseshoe model
  return(list(model = fit_horseshoe, sel_var = sel_var))
}




#### HORSESHOE PRIOR 'bayesreg' ####

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
# OUTPUTS:
#     A list containing:
#       - model: Summary of the initial fitted horseshoe model.
#       - conf_intervals: Confidence intervals of the coefficients of the initial model.
#       - selected_variables: Names of the selected variables based on non-zero 95% confidence intervals.
#
fit_horseshoe_bs <- function(data, n.samples = 1000, burnin = 200, 
                             thin = 1, prior = "hs") {
  
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
  
  # Ensure 'prior' is a character and contains valid value
  if (!is.character(prior) || !(prior %in% c("hs", "hs+"))) {
    stop("'prior' must be a character and contain a valid value.")
  }

  # Fit the initial horseshoe model using the bayesreg package
  set.seed(42)
  fit_bayesreg <- bayesreg::bayesreg(y ~ ., data = data, 
                                     # Distribution of the target
                                     model = "gaussian",
                                     prior = prior,
                                     n.samples = n.samples,
                                     burnin = burnin,
                                     thin = thin)
  
  # Generate the summary of the bayesreg model fit
  bayesreg_summary <- summary(fit_bayesreg)
  
  # Extract the confidence interval (CI) of the coefficients
  ci <- bayesreg_summary$CI.coef
  
  # Identify the coefficients whose 95% CI does not contain zero 
  non_zero_ci_indicator <-  ifelse(ci[, 1] < 0 & ci [, 2] > 0, 0, 1)
  
  # Extract the variables (coefficients) whose 95% CI does not contain zero
  selected_variables <- names(non_zero_ci_indicator[non_zero_ci_indicator == 1])
  
  # Return the summary of the refitted model and selected variables
  return(list(model = bayesreg_summary, conf_intervals = ci,
              selected_variables = selected_variables))
}







#### SSS WITH SCREENING 'BayesS5' NOT finished ####

# This function fits a sparse Bayesian linear regression model using the BayesS5 package. 
#   The S5 function promotes sparsity in the regression coefficients.
#
# Inputs:
#     data - Data frame where the first column is the response variable, 
#            and the remaining columns are predictors.
#     ind_fun - A function to define the inclusion indicators of the model.
#               Default: ind_fun_pimom.
#     model - An object of class Model defining the prior distribution.
#             Bernoulli_Uniform or Uniform. Default is Bernoulli_Uniform.
#     C0 - Normalisation constant for the S5 function.
#     type - a type of nonlocal priors; ’pimom’ or ’pemom’.
#     has_binary - Logical, TRUE when data includes a binary variable 
#                 (first 2 columns).
#
# Outputs:
#     list containing the fitted model, model summary, and selected variables.
#
fit_S5 <- function(data, ind_fun = ind_fun_pimom, model = Bernoulli_Uniform, 
                  C0 = 5, type = "pimom", has_binary = FALSE) {
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  # Ensure 'ind_fun' is a function
  if (!is.function(ind_fun)) {
    stop("'ind_fun' must be a function.")
  }
  
  # Check if 'type' is either 'pimom' or 'pemom'
  if (!(type %in% c("pimom", "pemom"))) {
    stop("'type' must be either 'pimom' or 'pemom'.")
  }
  
  # Check 'C0' is a positive numeric value
  if (!is.numeric(C0) || C0 <= 0) {
    stop("'C0' must be a positive numeric value.")
  }
  
  # Check if the flag 'has_binary' is a logical value
  if (!is.logical(has_binary)) {
    stop("'has_binary' must be a logical value.")
  }
  
  # Separate response variable (y) and predictors (X)
  y <- scale(data$y)             
  
  # Remove target from data and convert into a matrix
  X <- model.matrix(~ . - 1, data = data[, -which(names(data) == "y")])
  
  # Check if data has binary variables
  if (has_binary) {
    # If it does, remove the first binary variable and scale the rest
    X <- scale(X[, -1])
  } else {
    # If it doesn't, scale the whole matrix
    X <- scale(X)
  }
  
  # Tuning parameters before fitting the model
  # Set seed for reproducibility
  set.seed(42)
  tuning <- hyper_par(type = type, X = X, y = y)
  
  # Fit the model using the S5 function from the BayesS5 package
  set.seed(42)
  fit_S5 <- BayesS5::S5(X = X, y = y, ind_fun = ind_fun, model = model,
                        tuning = tuning, C0 = C0)
  
  # Save the results of the model
  fit_S5_res <- result(fit_S5)
  
  # Extract the marginal probabilities
  marg_probs <- fit_S5_res$marg.prob %>% round(0)
  
  # Get the variable names
  var_names <- colnames(X)
  
  # Create a data frame with variable names and inclusion probabilities
  selected_variables <- data.frame(Variable = var_names, Included = marg_probs)
  
  # Return the fitted model, results summary and selected variables
  return(list(model = fit_S5, summary = fit_S5_res, selected_variables = selected_variables))
}


#### BAYESIAN LASSO 'monomvn' ####

# Function to fit a Bayesian LASSO regression model using the 'monomvn' package.
#   This function performs hyperparameter tuning and variable selection in a 
#   Bayesian LASSO regression model. Includes the ability to perform RJMCMC.
#
# INPUTS:
#   data - A data frame where the first column is the response variable, 
#          and the remaining columns are predictors.
#   T - Number of iterations in the MCMC chain.
#   RJ - Logical flag indicating whether to perform Reversible Jump MCMC.
#   verb - Verbosity level of the function's output.
#   lambda2 - Value for penalty.
#   threshold - Threshold for variable selection based on the posterior 
#               inclusion probabilities.
#   burnin - Burnin value.
#
# OUTPUTS:
#   A list containing:
#     model: The fitted Bayesian LASSO regression model.
#     var_prob: The posterior inclusion probabilities of each predictor.
#     sel_var_df: The variables that were selected by the model
#               based on the threshold with probabilities.
#
fit_blasso <- function(data,  T = 5000, RJ = TRUE, verb = 1, lambda2 = 1, 
                       threshold = 0.5, burnin = 1000) { 
  
  # Input validation
  # Check if the input data is of the correct format: a data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  # Check if the number of iterations 'T' is a positive numeric value
  if (!is.numeric(T) || T <= 0) {
    stop("'T' must be a positive numeric value.")
  }
  
  # Check if the lambda2 is a positive numeric value
  if (!is.numeric(lambda2) || lambda2 <= 0) {
    stop("'lambda2' must be a positive numeric value.")
  }
  
  # Check if the burnin is a positive numeric value
  if (!is.numeric(burnin) || burnin <= 0) {
    stop("'burnin' must be a positive numeric value.")
  }
  
  # Check if the flag 'RJ' is a logical value
  if (!is.logical(RJ)) {
    stop("'RJ' must be a logical value.")
  }
  
  # Check if the verbosity level 'verb' is a non-negative numeric value
  if (!is.numeric(verb) || verb < 0) {
    stop("'verb' must be a non-negative numeric value.")
  }
  
  # Check if the threshold is a numeric value between 0 and 1
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("'threshold' must be a numeric value between 0 and 1.")
  }
  
  # Extract the target
  y <- data$y
    
  # Remove the original categorical variables
  data <- data.frame(subset(data, select = -y))
  
  # Set seed for reproducibility outside the loop
  set.seed(42)
  
  # Fit the model on the full dataset
  blasso_model <- monomvn::blasso(X = data, y = y, lambda2 = 1, RJ = RJ, 
                                T = T, verb = verb)
  
  # Get the summary with burn-in
  blasso_summary <- summary(blasso_model, burnin = burnin)
  
  # Get the probabilities of each variable
  var_prob <- blasso_summary$bn0
  
  # Extract the variable names selected based on the threshold
  sel_var <- colnames(data)[blasso_summary$bn0 > threshold]
  
  # Extract the variable probabilities selected based on the threshold
  sel_var_prob <- var_prob[blasso_summary$bn0 > threshold]
  
  # Create a data frame with variable names and respective probabilities
  sel_var_df <- data.frame(Variable = sel_var, Probability = sel_var_prob)
  
  # Order data frame by probability in descending order
  sel_var_df <- sel_var_df[order(-sel_var_df$Probability), ]
  
  # Return the fitted model, variable inclusion probabilities ,
  #   selected variables above a threshold
  return(list(model = blasso_model, var_prob = var_prob, 
              sel_var_df = sel_var_df))
}

