library(ggplot2)
library(dplyr)

# Generate a sample from the normal distribution for the spike
# small variance
spike <- rnorm(10000, mean = 0, sd = 0.1)  

# Generate a sample from the normal distribution for the slab
# large variance
slab <- rnorm(10000, mean = 0, sd = 10) 

# Combine these samples into a single dataset
df <- data.frame(value = c(spike, slab),
                 Sigma = c(rep("Small (0.01)", 10000), rep("Large (10)", 10000)))

# Specify colors and linetypes
my_colors <- c("Small (0.01)" = "deeppink4", "Large (10)" = "royalblue4")
my_linetypes <- c("Small (0.01)" = "solid", "Large (10)" = "dashed")

# Plot the spike-and-slab distribution
spike_slab_prior <- ggplot(df, aes(value, color = Sigma, fill = Sigma, linetype = Sigma)) +
  geom_density(size = 0.6, alpha = 0.3) +
  scale_fill_manual(name = expression(sigma), values = my_colors) +
  scale_color_manual(name = expression(sigma), values = my_colors) +
  scale_linetype_manual(name = expression(sigma), values = my_linetypes) +
  xlim(c(-7.5, 7.5)) + 
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

# Remove information
rm(df, my_colors, my_linetypes, slab, spike)

