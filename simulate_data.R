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
  
  # Generate binary categorical variables
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
  polynomial_feature_5 <- X[, 5]^2
  polynomial_feature_6 <- X[, 6]^3
  polynomial_feature_23_2 <- X[, 23]^2
  polynomial_feature_23_3 <- X[, 23]^3
  
  # Generate the true regression coefficients beta
  beta <- c(rep(6, 5), rep(4, 5), rep(3, 5), rep(0, p - 20))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Add the intercept too
  intercept <- 2
  
  # Generate the response variable y
  y <- intercept + X %*% beta + 
    cat_var1 + 
    cat_var2 + 
    interaction_term_1_2 + 
    interaction_term_3_4 + 
    interaction_term_21_22 + 
    as.numeric(interaction_term_c1_22) +
    polynomial_feature_5 + 
    polynomial_feature_6 + 
    polynomial_feature_23_2 +
    polynomial_feature_23_3 + 
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
                                  polynomial_feature_5,
                                  polynomial_feature_6, 
                                  polynomial_feature_23_2,
                                  polynomial_feature_23_3, X))
  
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", "cat_var1", "cat_var2", 
                          "interaction_1_2", "interaction_3_4",
                          "interaction_21_22", "interaction_term_c1_22", 
                          "poly_5", "poly_6", "poly_23_2", "poly_23_3",
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
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generating true regression coefficients beta (I am using random beta here)
  beta <- c(rep(6, 5), rep(4, 5), rep(3, 5), rep(0, p - 20))
  
  # Generate the response variable y
  y <- X %*% beta + epsilon
  
  # Combine continuous covariates and y into a data frame
  sim_data <- as.data.frame(cbind(y, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", paste0("X", 1:p))
  
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
