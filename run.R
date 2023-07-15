#### SIMULATE T1 UNCORRELATED CONTINUOUS DATA ####

# Function to simulate a T1 type data set
# INPUT: 
#       p - number of covariates
#       n - number of data points to simulate
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T1 <- function(p, n, sigma_e = sqrt(15), seed = 42) {
  
  # Input validation
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || floor(seed) != seed)) {
    stop("seed must be a non-negative integer or NULL")
  }
  
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    stop("n must be a positive integer")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Set the mean vector
  u_x <- rep(5, p)
  
  # Set the correlation matrix as identity matrix
  sigma_x <- diag(p)
  
  # Generate the covariates X
  X <- MASS::mvrnorm(n, mu = u_x, Sigma = sigma_x)
  
  # Generate the true regression coefficients beta
  beta <- c(rep(3, 10), rep(5, 10), rep(0, p - 20))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + epsilon
  
  # Combine X and y into a data frame
  sim_data <- as.data.frame(cbind(y, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", paste0("X", 1:p))
  
  # Return the dataset
  return(sim_data)
}

# Extract the simulated data
# Simulate where p < n
T1_LD <- simulate_T1(p = 50, n = 200)
# Simulate where p = n
T1_ED <- simulate_T1(p = 100, n = 100)
# Simulate where p > n
T1_HD <- simulate_T1(p = 200, n = 150)
# Simulate where p >> n
T1_VD <- simulate_T1(p = 200, n = 50)
# Simulate for XGBoost p << n
T1_XD <- simulate_T1(p = 50, n = 500)




#### SIMULATE T2 TEMPORAL CORRELATED CONTINUOUS DATA ####

# Function to simulate a T2 type data set
# INPUT: 
#       p - number of covariates
#       n - number of data points to simulate
#       rho - AR(1) correlation coefficient
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T2 <- function(p, n, rho = 0.8, sigma_e = sqrt(10), seed = 42) {
  
  # Input validation
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    stop("rho must be a numeric value between -1 and 1")
  }
  
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || floor(seed) != seed)) {
    stop("seed must be a non-negative integer or NULL")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Set the mean vector
  u_x <- c(rep(3, 30), rep(7, p-30))
  
  # Set the covariance matrix with AR(1) structure
  sigma_x <- matrix(rho^abs(outer(1:p, 1:p, "-")), p, p)
  
  # Generate the covariates X
  X <- MASS::mvrnorm(n, mu = u_x, Sigma = sigma_x)
  
  # Generate the true regression coefficients beta
  beta <- c(rep(5, 20), rep(0, p - 20))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + epsilon
  
  # Combine X and y into a data frame
  sim_data <- as.data.frame(cbind(y, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", paste0("X", 1:p))
  
  # Return the dataset
  return(sim_data)
}

# Simulate where p < n
T2_LD <- simulate_T2(p = 50, n = 200)
# Simulate where p = n
T2_ED <- simulate_T2(p = 100, n = 100)
# Simulate where p > n
T2_HD <- simulate_T2(p = 200, n = 150)
# Simulate where p >> n
T2_VD <- simulate_T2(p = 200, n = 50)
# Simulate for XGBoost p << n
T2_XD <- simulate_T2(p = 50, n = 500)




#### SIMULATE T3 MIXED CONTINUOUS AND CATEGORICAL DATA ####

# Function to simulate a T3 type data set with mixed continuous and 
#     categorical variables, and some polynomials, interaction terms.
# INPUT: 
#       p - number of continuous covariates (minimum of 10)
#       n - number of data points to simulate
#       rho - AR(1) correlation coefficient
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T3 <- function(p, n, rho = 0.6, sigma_e = sqrt(12), seed = 42) {
  
  # Input validation
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    stop("rho must be a numeric value between -1 and 1")
  }
  
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || floor(seed) != seed)) {
    stop("seed must be a non-negative integer or NULL")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Calculate the number of continuous covariates needed
  # 10 for 2 categorical, 4 interactions, and 4 polynomials
  p <- p - 10  
  
  # Set the mean vector for continuous covariates
  u_x <- c(rep(2, 10), rep(5, 30), rep(8, p - 40))
  
  # Set the covariance matrix with AR(1) structure
  sigma_x <- matrix(rho^abs(outer(1:p, 1:p, "-")), p, p)
  
  # Generate the continuous covariates X
  X <- MASS::mvrnorm(n, mu = u_x, Sigma = sigma_x)
  
  # Generate binary categorical variable
  cat_var1 <- sample(c(0, 1), n, replace = TRUE) %>% 
    as.factor()
  # Treat as ordinal categorical variable
  cat_var2 <- sample(1:5, n, replace = TRUE) %>% 
    as.factor()
  
  # Generate interaction terms (multiplying first and second continuous covariate)
  interaction_term_1_2 <- X[, 1] * X[, 2]
  interaction_term_3_4 <- X[, 3] * X[, 4]
  interaction_term_21_22 <- X[, 21] * X[, 22]
  interaction_term_c1_22 <- interaction(cat_var1, X[, 22])
  
  # Generate polynomial feature (squared third continuous covariate)
  polynomial_feature_5_2 <- X[, 5]^2
  polynomial_feature_6_3 <- X[, 6]^3
  polynomial_feature_23_2 <- X[, 23]^2
  polynomial_feature_23_3 <- X[, 23]^3
  
  # Generate the true regression coefficients beta
  beta <- c(rep(6, 5), rep(4, 5), rep(3, 5), rep(0, p - 15))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Add the intercept too
  intercept <- 2
  
  # Define the betas
  beta_cat_var1 <- 4
  beta_it_1_2 <- 3
  beta_p_23_2 <- 6
  
  # Use zero for all other betas
  beta_cat_var2 <- 0
  beta_it_3_4 <- 0
  beta_it_21_22 <- 0
  beta_it_c1_22 <- 0
  beta_p_5 <- 0
  beta_p_6 <- 0
  beta_p_23_3 <- 0
  
  # Generate the response variable y
  y <- intercept + X %*% beta + 
    beta_cat_var1 * as.numeric(cat_var1) + 
    beta_cat_var2 * as.numeric(cat_var2) + 
    beta_it_1_2 * interaction_term_1_2 + 
    beta_it_3_4 * interaction_term_3_4 + 
    beta_it_21_22 * interaction_term_21_22 + 
    beta_it_c1_22 * as.numeric(interaction_term_c1_22) +
    beta_p_5 * polynomial_feature_5_2 + 
    beta_p_6 * polynomial_feature_6_3 + 
    beta_p_23_2 * polynomial_feature_23_2 +
    beta_p_23_3 * polynomial_feature_23_3 +
    epsilon
  
  # Combine continuous covariates, categorical vars, interaction terms, 
  # polynomial features and y into a data frame
  sim_data <- as.data.frame(cbind(y, 
                                  cat_var1, 
                                  cat_var2, 
                                  interaction_term_1_2, 
                                  interaction_term_3_4,
                                  interaction_term_21_22,
                                  interaction_term_c1_22,
                                  polynomial_feature_5_2,
                                  polynomial_feature_6_3, 
                                  polynomial_feature_23_2,
                                  polynomial_feature_23_3, X))
  
  # Make sure the categorical variables are factors
  sim_data$cat_var1 <- as.factor(sim_data$cat_var1)
  sim_data$cat_var2 <- as.factor(sim_data$cat_var2)
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", "cat_var1", "cat_var2", 
                          "interaction_1_2", "interaction_3_4",
                          "interaction_21_22", "interaction_term_c1_22", 
                          "poly_5_2", "poly_6_3", "poly_23_2", "poly_23_3",
                          paste0("X", 1:p))
  
  # Return the dataset
  return(sim_data)
}

# Simulate where p < n
T3_LD <- simulate_T3(p = 50, n = 200)
# Simulate where p = n
T3_ED <- simulate_T3(p = 100, n = 100)
# Simulate where p > n
T3_HD <- simulate_T3(p = 200, n = 150)
# Simulate where p >> n
T3_VD <- simulate_T3(p = 200, n = 50)
# Simulate for XGBoost p << n
T3_XD <- simulate_T3(p = 50, n = 500)




#### SIMULATE T4 GROUPED CONTINUOUS DATA WITH CATEGORICAL VARIABLES AND INTERACTIONS ####

# Function to simulate a T4 type data set
# INPUT: 
#       p - number of continuous covariates
#       n - number of data points to simulate
#       rho - within-group correlation coefficient
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T4 <- function(p, n, rho = 0.6, sigma_e = sqrt(10), seed = 42) {
  
  # Input validation
  if (!is.numeric(p) || p <= 0 || floor(p) != p || p %% 5 != 0) {
    stop("p must be a positive integer that is divisible by 5")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    stop("rho must be a numeric value between -1 and 1")
  }
  
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || floor(seed) != seed)) {
    stop("seed must be a non-negative integer or NULL")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Adjust p to generate the correct number of continuous variables
  p <- p - 5
  
  # Group sizes
  group_sizes <- rep(p / 5, 5)
  
  # Covariance matrices for each group
  cov_matrices <- lapply(1:5, function(i) {
    matrix(rho, nrow = group_sizes[i], ncol = group_sizes[i]) +
      diag(rep(1 - rho, group_sizes[i]))
  })
  
  # Set the means for each group
  u_x <- rep(seq(2, 10, by = 2), times = group_sizes)
  
  # Generating continuous covariates X
  X <- do.call(cbind, lapply(1:length(cov_matrices), function(i) {
    MASS::mvrnorm(n, mu = rep(u_x[i], ncol(cov_matrices[[i]])), Sigma = cov_matrices[[i]])
  }))
  
  # Generate binary categorical variable
  cat_var1 <- sample(c(0, 1), n, replace = TRUE) %>% 
    as.factor()
  # Treat as ordinal categorical variable
  cat_var2 <- sample(1:5, n, replace = TRUE) %>% 
    as.factor()
  
  # Inclute 2 interaction terms
  # Generate interaction terms
  interaction_term_1_2_3 <- X[, 1] * X[, 2] * X[, 3]
  interaction_term_4_5 <- X[, 4] * X[, 5]
  interaction_term_16_17 <- X[, 16] * X[, 17]
  
  # Generating true regression coefficients beta
  beta <- c(rep(6, 5), rep(4, 5), rep(3, 5), rep(0, p - 15))
  
  # Define the betas
  beta_cat_var1 <- 4
  beta_cat_var2 <- 0
  beta_it_1_2_3 <- 3
  beta_it_4_5 <- 0
  beta_it_16_17 <- 0
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + 
    beta_cat_var1 * as.numeric(cat_var1) + 
    beta_cat_var2 * as.numeric(cat_var2) + 
    beta_it_1_2_3 * interaction_term_1_2_3 + 
    beta_it_4_5 * interaction_term_4_5 + 
    beta_it_16_17 * interaction_term_16_17 +
    epsilon
  
  
  # Combine continuous covariates and y into a data frame
  # Combine continuous covariates, categorical vars, interaction terms, 
  # polynomial features and y into a data frame
  sim_data <- as.data.frame(cbind(y, 
                                  cat_var1, 
                                  cat_var2, 
                                  interaction_term_1_2_3, 
                                  interaction_term_4_5, 
                                  interaction_term_16_17,
                                  X))
  
  # Make sure the categorical variables are factors
  sim_data$cat_var1 <- as.factor(sim_data$cat_var1)
  sim_data$cat_var2 <- as.factor(sim_data$cat_var2)
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", "cat_var1", "cat_var2", 
                          "interaction_term_1_2_3", "interaction_term_4_5",
                          "interaction_term_16_17",
                          paste0("X", 1:p))
  
  # Return the dataset
  return(sim_data)
}

# Simulate where p < n
T4_LD <- simulate_T4(p = 50, n = 200)
# Simulate where p = n
T4_ED <- simulate_T4(p = 100, n = 100)
# Simulate where p > n
T4_HD <- simulate_T4(p = 200, n = 150)
# Simulate where p >> n
T4_VD <- simulate_T4(p = 200, n = 50)
# Simulate for XGBoost p << n
T4_XD <- simulate_T4(p = 50, n = 500)


# Remove functions
rm(simulate_T1, simulate_T2, simulate_T3, simulate_T4)



# Function to fit a Bayesian LASSO regression model using the 'monomvn' package.
# The function implements cross-validation for hyperparameter tuning and 
# variable selection in the regression coefficients.
#
# INPUTS:
#   data - Data frame where the first column is the response variable, 
#         and the remaining columns are predictors.
#   cat_var - logical factor if there are categorical variables.
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
fit_blasso_model <- function(data, cat_var = FALSE, T = 1000, RJ = TRUE, verb = 1, 
                             lambda_seq = c(seq(0.1, 1, by = 0.2), seq(1, 5, 0.5)), threshold = 0.5,
                             cv_folds = 5) { #lambda2 = 1, 
  
  # Input validation
  # Check if the input data is of the correct format: a data frame
  if (!is.data.frame(data)) {
    # If the input is not a data frame, throw an error and stop execution
    stop("'data' must be a data frame.")
  }
  
  # Ensure cat_var is a logical
  if (!is.logical(cat_var)) {
    stop("Input 'cat_var' must be a logical (TRUE or FALSE).")
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
  
  # Check if the number of cross-validation folds 'cv_folds' is a positive numeric value
  if (!is.numeric(threshold) || threshold <= 0) {
    # If 'threshold' is not a positive number, throw an error and stop execution
    stop("'threshold' must be a positive numeric value.")
  }
  
  # Check if the sequence of lambda values 'lambda_seq' is a numeric vector
  if (!is.numeric(lambda_seq)) {
    # If 'lambda_seq' is not a numeric vector, throw an error and stop execution
    stop("'lambda_seq' must be a numeric vector.")
  }
  
  # If categorical variables are provided, convert them to factors
  if (cat_var == TRUE) {
    
    # Extract the target
    y <- data$y
    
    # Scale only the numeric variables (i.e., exclude the first three columns)
    numeric_vars <- data.frame(scale(data[, -(1:3)])) # All columns except first three
    
    # Combine the non-scaled y, non-scaled categorical variables, and scaled numeric variables
    X <- data.frame(cbind(as.factor(data$cat_var1), 
                          as.factor(data$cat_var2),
                          numeric_vars))
    
    # Rename columns 1 and 2
    colnames(X)[1] <- "cat_var1" 
    colnames(X)[2] <- "cat_var2"
    
    # Combine scaled continuous variables, intercept, include categorical as 
    #   dummy variables
    #X <- model.matrix(~ . - 1, data = data)
  }
  
  # If no categorical variables
  else if (cat_var == FALSE) {
    
    # Extract the target
    y <- data$y
    
    # Remove the original categorical variables
    X <- data.frame(scale(subset(data, select = -y)))
    
    # Combine scaled continuous variables, intercept
    #X <- model.matrix(~ . - 1, data = data)
  }
  
  # Set a seed for reproducibility
  set.seed(42)
  
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
      fit <- monomvn::blasso(X = X_train, y = y_train, T = T, RJ = TRUE, 
                             lambda2 = lambda2, verb = verb)
      
      # Initialize a matrix to hold the predictions for each draw from the posterior
      y_pred <- matrix(nrow = nrow(X_val), ncol = nrow(fit$beta))
      
      # For each draw from the posterior...
      for (i in 1:nrow(fit$beta)) {
        # Replicate the coefficients for each observation
        beta_rep <- matrix(rep(fit$beta[i, ], nrow(X_val)), nrow = nrow(X_val), byrow = TRUE)
        
        # Make predictions using the coefficients from this draw
        y_pred[, i] <- rowSums(X_val * beta_rep)
        # Make predictions using the coefficients from this draw
        #y_pred[, i] <- X_val %*% matrix(fit$beta[i, ], nrow = 1)
      }
      
      # Compute and store the mean squared error
      cv_errors[i] <- cv_errors[i] + mean((y_val - y_pred)^2) / cv_folds
    }
  }
  
  # Choose the lambda2 value that minimizes the cross-validation error
  best_lambda2 <- lambda_seq[which.min(cv_errors)]
  
  # Refit the model on the full dataset with the selected lambda2 value
  set.seed(42)
  fit_blasso <- monomvn::blasso(X = X, y = y,# T = T, #RJ = RJ, beta = beta,
                                lambda2 = best_lambda2, verb = verb)
  
  # Return the fitted model and the selected lambda2 value
  return(list("model" = fit_blasso, "best_lambda2" = best_lambda2, 
              "sel_var" = colMeans(abs(fit_blasso$beta)) > threshold))
}



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