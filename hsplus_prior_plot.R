# Number of samples
n <- 900000
# tau specification
tau <- 1

# Probability density function of the horseshoe distribution
dhorseshoe <- function(x, tau = 1) {
  theta2 <- x^2 / (2 * tau^2)
  expint::expint_E1(theta2, scale = TRUE) / sqrt(2 * pi^3 * tau^2)
}

# Generate horseshoe samples
generate_horseshoe <- function(tau, n) {
  lambda <- abs(rcauchy(n, scale = tau)) # Half-Cauchy distribution for lambda
  x <- rnorm(n, sd = lambda)
  data.frame(x = x, model = "Horseshoe")
}

# Generate Horseshoe+ samples
generate_horseshoe_plus <- function(tau, n) {
  eta <- abs(rcauchy(n)) # Half-Cauchy distribution for eta
  lambda <- abs(rcauchy(n, scale = tau * eta)) # Half-Cauchy distribution for lambda
  x <- rnorm(n, sd = lambda)
  data.frame(x = x, model = "Horseshoe+")
}

set.seed(123)
df <- rbind(generate_horseshoe(tau, n), generate_horseshoe_plus(tau, n))

# Create a sequence for x values to calculate densities
x_seq <- seq(-10, 10, length.out = 200)

# Calculate densities for Horseshoe distribution
df_density_hs <- data.frame(x = x_seq, y = dhorseshoe(x_seq, tau), model = "Horseshoe")

# Since we don't have the exact form of the Horseshoe+ PDF, we approximate it
# by using a histogram based density estimation on the generated samples
hs_plus_dens <- density(df$x[df$model == "Horseshoe+"], from = -10, to = 10, n = length(x_seq))
df_density_hs_plus <- data.frame(x = hs_plus_dens$x, y = hs_plus_dens$y, model = "Horseshoe+")

# Combine densities
df_density <- rbind(df_density_hs, df_density_hs_plus)

# Plot

# Specify colors and linetypes
my_colors <- c("Horseshoe" = "deeppink4", "Horseshoe+" = "seagreen")
my_linetypes <- c("Horseshoe" = "solid", "Horseshoe+" = "dashed")

# Plot the densities
hsplus_prior_plot <- ggplot(df_density, aes(x = x, y = y, color = model, linetype = model)) +
  geom_line(size = 0.6) +
  scale_color_manual(name = "Model", values = my_colors) +
  scale_linetype_manual(name = "Model", values = my_linetypes) +
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


rm(df, df_density, n, dhorseshoe, x_seq, generate_horseshoe, 
   my_colors, my_linetypes, tau, df_density_hs, df_density_hs_plus,
   hs_plus_dens, generate_horseshoe_plus) 


