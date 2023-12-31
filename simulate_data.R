#### SIMULATE T1 UNCORRELATED CONTINUOUS DATA ####

# Function to simulate a T1 type data set
# INPUT: 
#       p - number of covariates.
#       n - number of data points to simulate.
#       sigma_e - variance of the error term.
#       seed - seed for random number generation.
#       standardise - whether to standardise the variables.
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T1 <- function(p, n, sigma_e = sqrt(15), 
                        seed = 42, standardise = TRUE) {
  
  # Input validation
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || seed %% 1 != 0)) {
    # Check if the seed is a non-negative integer.
    stop("seed must be a non-negative integer")
  }
  
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    # Check if p is a positive integer.
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    # Check if n is a positive integer.
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(sigma_e) || sigma_e <= 0) {
    # Check if sigma_e is a positive number.
    stop("sigma_e must be a positive number")
  }
  
  # Set seed if not provided
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
  
  if (standardise) {
    # Standardise only X variables, not y
    sim_data[ , -1] <- scale(sim_data[ , -1])
  }
  
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
#       standardise - whether to standardise the variables.
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T2 <- function(p, n, rho = 0.8, sigma_e = sqrt(10), 
                        seed = 42, standardise = TRUE) {
  
  # Input validation
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || seed %% 1 != 0)) {
    # Check if the seed is a non-negative integer.
    stop("seed must be a non-negative integer")
  }
  
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    # Check if p is a positive integer.
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    # Check if n is a positive integer.
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(sigma_e) || sigma_e <= 0) {
    # Check if sigma_e is a positive number.
    stop("sigma_e must be a positive number")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    # Check if rho is a number between -1 and 1.
    stop("rho must be a number between -1 and 1")
  }
  
  # Set seed if not provided
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
  beta <- c(seq(1, 20, 1), rep(0, p - 20))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + epsilon
  
  # Combine X and y into a data frame
  sim_data <- as.data.frame(cbind(y, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", paste0("X", 1:p))
  
  if (standardise) {
    # Standardise only X variables, not y
    sim_data[ , -1] <- scale(sim_data[ , -1])
  }
  
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
#       p - number of continuous covariates (minimum of 10).
#       n - number of data points to simulate.
#       rho - AR(1) correlation coefficient.
#       sigma_e - variance of the error term.
#       seed - seed for random number generation.
#       standardise - whether to standardise the variables.
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T3 <- function(p, n, rho = 0.6, sigma_e = sqrt(12), 
                        seed = 42, standardise = TRUE) {
  
  # Input validation
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || seed %% 1 != 0)) {
    # Check if the seed is a non-negative integer.
    stop("seed must be a non-negative integer")
  }
  
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    # Check if p is a positive integer.
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    # Check if n is a positive integer.
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(sigma_e) || sigma_e <= 0) {
    # Check if sigma_e is a positive number.
    stop("sigma_e must be a positive number")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    # Check if rho is a number between -1 and 1.
    stop("rho must be a number between -1 and 1")
  }
  
  # Set seed if not provided
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
  
  if (standardise) {
    # Select all variables except y, cat_var1, cat_var2
    variables_to_scale <- setdiff(colnames(sim_data), c("y", "cat_var1", "cat_var2"))
    
    # Apply scale() to these variables
    sim_data[variables_to_scale] <- scale(sim_data[variables_to_scale])
  }
  
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
#       p - number of continuous covariates.
#       n - number of data points to simulate.
#       rho - within-group correlation coefficient.
#       sigma_e - variance of the error term.
#       seed - seed for random number generation.
#       standardise - whether to standardise the variables.
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T4 <- function(p, n, rho = 0.6, sigma_e = sqrt(10), 
                        seed = 42, standardise = TRUE) {
  
  # Input validation
  if (!is.null(seed) && (!is.numeric(seed) || seed < 0 || seed %% 1 != 0)) {
    # Check if the seed is a non-negative integer.
    stop("seed must be a non-negative integer")
  }
  
  if (!is.numeric(p) || p <= 0 || floor(p) != p) {
    # Check if p is a positive integer.
    stop("p must be a positive integer")
  }
  
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    # Check if n is a positive integer.
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(sigma_e) || sigma_e <= 0) {
    # Check if sigma_e is a positive number.
    stop("sigma_e must be a positive number")
  }
  
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    # Check if rho is a number between -1 and 1.
    stop("rho must be a number between -1 and 1")
  }
  
  # Set seed if not provided
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
  
  if (standardise) {
    # Select all variables except y, cat_var1, cat_var2
    variables_to_scale <- setdiff(colnames(sim_data), c("y", "cat_var1", "cat_var2"))
    
    # Apply scale() to these variables
    sim_data[variables_to_scale] <- scale(sim_data[variables_to_scale])
  }
  
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
