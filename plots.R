#### HORSESHOE PRIORS 4 VARIATIONS

# Number of samples
n <- 10000
# tau specifications
taus <-  c(0.25, 1, 4)

#Probability density function of the horseshoe distribution with scale tau.
dhorseshoe <- function(x, tau = 1) {
  theta2 <- x^2 / (2 * tau^2)
  expint::expint_E1(theta2, scale = TRUE) / sqrt(2 * pi^3 * tau^2)
}

# Function to generate samples from horseshoe distribution
generate_horseshoe <- function(tau, n) {
  lambda <- abs(rcauchy(n, scale = tau))
  x <- rnorm(n, sd = lambda)
  data.frame(x = x, tau = tau)
}

# Generate data
set.seed(123)
df <- bind_rows(lapply(taus, generate_horseshoe, n = n))

# Create a new data frame for the density curves
x_seq <- seq(-10, 10, length.out = 200)
df_density <- expand.grid(x = x_seq, tau = taus)
df_density$y <- with(df_density, mapply(dhorseshoe, x, tau))

# Specify colors and linetypes
my_colors <- c("deeppink4", "royalblue4", "seagreen")
my_linetypes <- c("solid", "dashed", "dotdash")

# Plot the densities
hs_prior_plot <- ggplot() +
  geom_line(data = df_density, aes(x = x, y = y, color = factor(tau), 
                                   linetype = factor(tau)), size = 0.6) +
  scale_color_manual(name = expression(tau), values = my_colors) +
  scale_linetype_manual(name = expression(tau), values = my_linetypes) +
  labs(x = expression(beta), y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.95),  # Position of legend
    legend.justification = c("right", "top"),  # Anchor point of the legend
    legend.background = element_rect(fill = "white", colour = "lightgrey"),  # Background of the legend
    legend.box.margin = margin(6, 6, 6, 6),  # Margin around the legend
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 10))


#rm(df, df_density, n, dhorseshoe, x_seq, generate_horseshoe, taus)




#### HORSESHOE PLUS ####

# Number of samples
n <- 10000
# tau specifications
taus <-  c(0.25, 1, 4)

# Probability density function of the horseshoe+ distribution with scales tau and eta.
dhorseshoe_plus <- function(x, tau = 1) {
  lambda2 <- x^2 / (2 * tau^2 * eta^2)
  expint::expint_E1(theta2, scale = TRUE) / sqrt(2 * pi^3 * tau^2)
  4 * log(x / tau) / ((pi^2 * tau) * (x / tau)^2) - 1
}

# Function to generate samples from horseshoe+ distribution
generate_horseshoe_plus <- function(tau, n) {
  eta <- abs(rcauchy(n))
  lambda <- abs(rcauchy(n, scale = tau * eta))
  x <- rnorm(n, sd = lambda)
  data.frame(x = x, tau = tau, eta = eta)
}

# Generate data
set.seed(123)
df <- bind_rows(lapply(taus, function(tau) generate_horseshoe_plus(tau, n = n)))

# Create a new data frame for the density curves
x_seq <- seq(-10, 10, length.out = 200)
df_density <- expand.grid(x = x_seq, tau = taus)
df_density$y <- with(df_density, mapply(function(x, tau) {
  mean(sapply(df$eta[df$tau == tau], dhorseshoe_plus, x = x, tau = tau))
}, x = x_seq, tau = taus))

# Specify colors and linetypes
my_colors <- c("deeppink4", "royalblue4", "seagreen")
my_linetypes <- c("solid", "dashed", "dotdash")

# Plot the densities
hs_plus_prior_plot <- ggplot() +
  geom_line(data = df_density, aes(x = x, y = y, color = factor(tau), 
                                   linetype = factor(tau)), size = 0.6) +
  scale_color_manual(name = expression(tau), values = my_colors) +
  scale_linetype_manual(name = expression(tau), values = my_linetypes) +
  labs(x = expression(beta), y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.95),  # Position of legend
    legend.justification = c("right", "top"),  # Anchor point of the legend
    legend.background = element_rect(fill = "white", colour = "lightgrey"),  # Background of the legend
    legend.box.margin = margin(6, 6, 6, 6),  # Margin around the legend
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 10))

