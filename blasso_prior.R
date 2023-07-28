# Define Laplace distribution
dLaplace <- function(x, mu = 0, lambda = 1) {
  (1/(2*lambda)) * exp(-abs(x - mu)/lambda)
}

# Generate a sequence of x-values
x <- seq(-10, 10, length.out = 1000)

# First let's create a data frame of values to plot
lambda_vals <- c(0.5, 1, 2)

x_vals <- seq(-2, 2, length.out = 1000)

df_list <- lapply(lambda_vals, function(lambda) {
  y_vals <- dLaplace(x_vals, 0, 1/lambda)
  data.frame(x = x_vals, y = y_vals, lambda = lambda)
})

df <- do.call(rbind, df_list)

# Specify colors and linetypes
my_colors <- c("deeppink4", "royalblue4", "seagreen")
my_linetypes <- c("solid", "dashed", "dotdash")

# Plot the densities
blasso_prior_plot <- ggplot(df, aes(x = x, y = y, color = factor(lambda), 
                                    linetype = factor(lambda))) +
    geom_line(size = 0.6) +
    scale_color_manual(name = expression(lambda), values = my_colors) +
    scale_linetype_manual(name = expression(lambda), values = my_linetypes) +
    labs(x = expression(beta), y = "Density") +
    theme_minimal() +
    theme(
      # Position of legend
      legend.position = c(0.95, 0.95),  
      # Anchor point of the legend
      legend.justification = c("right", "top"),  
      # Background of the legend
      legend.background = element_rect(fill = "white", colour = "lightgrey"),  
      # Margin around the legend
      legend.box.margin = margin(6, 6, 6, 6),  
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 10)
)

# Remove information
rm(dLaplace, x, lambda_vals, x_vals, df_list, my_colors, my_linetypes, df)

