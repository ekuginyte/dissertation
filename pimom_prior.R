# Function to calculate the piMoM prior density
piMoM_density <- function(beta, sigma_sq, tau, r = 1) {
  # Calculate the constant C*
  C_star <- tau^(-r + 1/2) * gamma(r - 1/2)
  
  # Calculate the product term
  product_term <- prod(beta^(-2*r) * exp(-tau / beta^2))
  
  # Return the prior density
  return(C_star * product_term)
}

# Define a range of beta values
beta_values <- seq(-10, 10, by = 0.01)

# Define a range of tau values
tau_values <- c(0.75, 1, 2)

# Calculate the piMoM density for each beta value and each tau value
df <- expand.grid(beta = beta_values, tau = tau_values)
df$density <- mapply(function(beta, tau) piMoM_density(beta, sigma_sq = 1, tau = tau), df$beta, df$tau)

# Specify colors and linetypes
my_colors <- c("deeppink4", "royalblue4", "seagreen")
my_linetypes <- c("solid", "dashed", "dotdash")

# Plot the densities
piMoM_prior_plot <- ggplot(df, aes(x = beta, y = density, color = factor(tau), linetype = factor(tau))) +
  geom_line(size = 0.6) +
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

rm(df, piMoM_density, beta_values, tau_values, my_linetypes, my_colors)

