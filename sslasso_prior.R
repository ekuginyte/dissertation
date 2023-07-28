# Number of samples
n <- 10000

# Generate samples from Laplace distribution
generate_laplace <- function(n, lambda) {
  rexp(n, rate = lambda) - rexp(n, rate = lambda)
}

# Generate samples for lambda = 1 and lambda = 20
samples1 <- generate_laplace(n, lambda = 1)
samples20 <- generate_laplace(n, lambda = 20)

# Combine into a data frame
df <- data.frame(
  value = c(samples1, samples20),
  lambda = factor(rep(c("1", "20"), each = n))
)

# Specify colors and linetypes
my_colors <- c("1" = "deeppink4", "20" = "royalblue4")
my_linetypes <- c("1" = "solid", "20" = "dashed")

# Plot
sslassso_prior <- ggplot(df, aes(x = value, color = lambda, fill = lambda, linetype = lambda)) +
  geom_density(size = 0.6, alpha = 0.3) +
  scale_fill_manual(name = expression(lambda), values = my_colors) +
  scale_color_manual(name = expression(lambda), values = my_colors) +
  scale_linetype_manual(name = expression(lambda), values = my_linetypes) +
  xlim(c(-5, 5)) + 
  labs(x = expression(beta), y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", colour = "lightgrey"),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key = element_blank())

rm(df, my_colors, my_linetypes, n, samples1, samples20, generate_laplace)
