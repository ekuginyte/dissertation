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

# Function to simulate a T3 type data set
# INPUT: 
#       p - number of continuous covariates
#       n - number of data points to simulate
#       rho - AR(1) correlation coefficient
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T3 <- function(p, n, rho = 0.6, sigma_e = sqrt(12), seed = 42) {
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Set the mean vector for continuous covariates
  u_x <- c(rep(2, 20), rep(5, 30), rep(8, p-50))
  
  # Set the covariance matrix with AR(1) structure
  sigma_x <- matrix(rho^abs(outer(1:p, 1:p, "-")), p, p)
  
  # Generate the continuous covariates X
  X <- MASS::mvrnorm(n, mu = u_x, Sigma = sigma_x)
  
  # Generate interaction terms (multiplying first and second continuous covariate)
  interaction_term <- X[,1]*X[,2]
  
  # Generate polynomial feature (squared third continuous covariate)
  polynomial_feature <- X[,3]^2
  
  # Generate binary categorical variables
  cat_var1 <- sample(c(0, 1), n, replace = TRUE)
  cat_var2 <- sample(c(0, 1), n, replace = TRUE)
  
  # Generate the true regression coefficients beta
  beta <- c(rep(6, 10), rep(4, 5), rep(3, 5), rep(0, p - 20))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + interaction_term + polynomial_feature + epsilon
  
  # Combine continuous covariates, categorical vars, interaction term, 
  # polynomial feature and y into a data frame
  sim_data <- as.data.frame(cbind(y, interaction_term, polynomial_feature, cat_var1, cat_var2, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", "interaction", "polynomial", "cat_var1", "cat_var2", paste0("X", 1:p))
  
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




#### SIMULATE T4 GROUPED CONTINUOUS DATA WITH CATEGORICAL VARIABLES ####

# Function to simulate a T4 type data set
# INPUT: 
#       p - number of continuous covariates
#       n - number of data points to simulate
#       rho - within-group correlation coefficient
#       sigma_e - variance of the error term
#       seed - seed for random number generation
# OUTPUT:
#       sim_data - data frame with simulated data
simulate_T4 <- function(p, n, rho = 0.9, sigma_e = sqrt(10), seed = 42) {
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Set the mean vector for continuous covariates
  u_x <- c(rep(3, 30), rep(7, p-30))
  
  # Set the covariance matrix as a block-diagonal matrix with AR(1) structure within each block
  block1 <- matrix(rho^abs(outer(1:30, 1:30, "-")), 30, 30)
  block2 <- matrix(rho^abs(outer(1:(p-30), 1:(p-30), "-")), p-30, p-30)
  sigma_x <- bdiag(block1, block2)
  
  # Generate the continuous covariates X
  X <- MASS::mvrnorm(n, mu = u_x, Sigma = sigma_x)
  
  # Generate binary categorical variables
  cat_var1 <- sample(c(0, 1), n, replace = TRUE)
  cat_var2 <- sample(c(0, 1), n, replace = TRUE)
  
  # Generate the true regression coefficients beta
  # (Assuming non-zero values for the first 5 entries in each group)
  beta <- c(rep(5, 5), rep(0, 25), rep(5, 5), rep(0, p - 35))
  
  # Generate the error terms
  epsilon <- rnorm(n, mean = 0, sd = sigma_e)
  
  # Generate the response variable y
  y <- X %*% beta + epsilon
  
  # Combine continuous covariates, categorical vars, and y into a data frame
  sim_data <- as.data.frame(cbind(y, cat_var1, cat_var2, X))
  
  # Name the columns of the data frame
  colnames(sim_data) <- c("y", "cat_var1", "cat_var2", paste0("X", 1:p))
  
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
