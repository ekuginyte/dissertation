# Crime Results into data frames
#### SOURCE THE RESULTS ####
crime_lasso <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_lasso.rds")
crime_elnet <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_elnet.rds")
crime_S5 <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_S5.rds")
crime_hs_bs_results <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_hs_bs.rds")
crime_hsp_bs_results <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_hsp_bs.rds")
crime_ssl <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_ssl.rds")
crime_spikeslab_prior <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_spikeslab_prior.rds")
crime_blasso <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_blasso.rds")
crime_xgboost <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_xgboost.rds")
crime_hs_hs_tC <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_horseshoe_tC.rds")
crime_hs_hs_hC <- readRDS("~/Documents/Dissertation/main/dissertation/results/crime/crime_horseshoe_hC.rds")

source("~/Documents/Dissertation/main/dissertation/data_crime_raw.R")

#### PENALISED REG ####

# Select the first 14 variables for lasso_df
lasso_df <- data.frame(
  Variables = rownames(crime_lasso$selected_predictors),
  Coefficients = round(crime_lasso$selected_predictors[, 1], 2)
) |> subset(abs(Coefficients) > 0.2)

# Select the first 8 variables for elnet_df
elnet_df <- data.frame(
  Variables = rownames(crime_elnet$selected_predictors),
  Coefficients = round(crime_elnet$selected_predictors[, 1], 2)
) |> subset(abs(Coefficients) > 0.2)

# Add additional rows with "" to elnet_df
#elnet_df <- rbind(elnet_df, data.frame(Variables = rep("", 3), Coefficients = rep("", 3)))

# Create a combined data frame
pen_reg_df <- data.frame(
  Lasso_Variables = lasso_df$Variables,
  Lasso_Coefficients = lasso_df$Coefficients,
  EMPTY = rep("", 26),
  ElasticNet_Variables = elnet_df$Variables,
  ElasticNet_Coefficients = elnet_df$Coefficients
)

pen_reg_df$Lasso_Variables <- gsub("_", ".", pen_reg_df$Lasso_Variables)
pen_reg_df$ElasticNet_Variables <- gsub("_", ".", pen_reg_df$ElasticNet_Variables)

## Plot
crime_lasso_cv_p <- crime_lasso$model_cv

## Plot elnet
crime_elnet_cv_p <- crime_elnet$model_cv

# Remove
rm(elnet_df, lasso_df)

#### SPIKESLAB ####

# Extract BMA and GNET coefficients
crime_gnet <- crime_spikeslab_prior[["results"]]["gnet"] %>% data.frame()
crime_bma <- crime_spikeslab_prior[["results"]][["bma"]] %>% data.frame()
crime_variables <- crime_spikeslab_prior[["results"]]["Variable"] %>% data.frame()

# Combine everything into a data frame with rounded values
crime_spikeslab <- data.frame(
  Variables = crime_variables,
  BMA = crime_bma,
  GNET = crime_gnet
)

# Adjust the column names
colnames(crime_spikeslab) <- c("Variables", "BMA", "gnet")

# Filter rows where gnet is not 0
crime_spikeslab <- crime_spikeslab %>% filter(gnet != 0)

# Round to 2 decimals
crime_spikeslab <- crime_spikeslab %>%
  mutate(BMA = round(BMA, 2),
         gnet = round(gnet, 2))

# Adjust variable names
crime_spikeslab$Variables <- gsub("_", ".", crime_spikeslab$Variables)

## Spikeslab plot
crime_spikeslab_plot <- crime_spikeslab_prior$model

#### SSLASSO ####

crime_ssl_df <- crime_ssl$selected_variables
crime_ssl_df <- subset(crime_ssl_df, Coefficient > 0.0001)
rownames(crime_ssl_df) <- NULL
crime_ssl_df$Coefficient <- round(crime_ssl_df$Coefficient, 2)
crime_ssl_df$Variable <- gsub("_", ".", crime_ssl_df$Variable)

## Plot SSLASSO
#crime_ssl_plot <- crime_ssl$plot

#### BLASSO ####

crime_blasso_df <- crime_blasso$sel_var_df
rownames(crime_blasso_df) <- NULL
crime_blasso_df$Probability <- round(crime_blasso_df$Probability, 2)
crime_blasso_df$Variable <- gsub("_", ".", crime_blasso_df$Variable)

# Convert to data frame
crime_blasso_p <- data.frame(
  Variable = 1:110,
  Probability = crime_blasso[["var_prob"]]
)

# Determine groups
crime_blasso_p$Group <- ifelse(crime_blasso_p$Variable <= 55, "Group1", "Group2")

crime_blasso_p <- ggplot(crime_blasso_p, aes(x = Variable, y = Probability)) +
  geom_point(aes(color = ifelse(Probability > 0.499999, "violetred", "lightblue3")), size = 2) +
  scale_color_identity() +  # This ensures that the colors are used as-is
  theme_minimal() +
  labs(x = "Variable",
       y = "Inclusion Probabilities") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~Group, scales = "free_x", ncol = 1)

#### S5 ####

# DF #
crime_S5_var <- crime_S5[["selected_variables"]][["Variable"]]
crime_S5_prob <- crime_S5[["summary"]][["marg.prob"]]
crime_S5_df <- data.frame(
  Variables = crime_S5_var,
  Probabilities = round(crime_S5_prob, 2)
)
crime_S5_df <- dplyr::arrange(crime_S5_df, by = desc(Probabilities))[1:6, ]
crime_S5_df$Variables <- gsub("_", ".", crime_S5_df$Variables)



# Convert to data frame
crime_S5_p <- data.frame(
  Variable = 1:110,
  Probability = crime_S5[["summary"]][["marg.prob"]]
)

# Determine groups
crime_S5_p$Group <- ifelse(crime_S5_p$Variable <= 55, "Group1", "Group2")

crime_S5_p <- ggplot(crime_S5_p, aes(x = Variable, y = Probability)) +
  geom_point(aes(color = ifelse(Probability > 0.5, "violetred", "lightblue3")), size = 2) +
  scale_color_identity() +  # This ensures that the colors are used as-is
  theme_minimal() +
  labs(x = "Variable",
       y = "Marginal Inclusion Probabilities") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~Group, scales = "free_x", ncol = 1)


#### XGBOOST ####



df_t <- data.frame(subset(df_t, select = -y))
X <- model.matrix(~ . + 0, data = df_t)
importance_matrix <- xgboost::xgb.importance(feature_names = colnames(X), model = crime_xgboost$finalModel)

# Data frame
xgboost_df <- data.frame(
  Feature = c(importance_matrix$Feature[1:12]),
  Gain = c(round(importance_matrix$Gain[1:12], 2))
)

xgboost_df$Feature <- gsub("_", ".", xgboost_df$Feature)  

# Create a bar plot
xgboost_df_plot <- ggplot(xgboost_df, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_gradient(low = "violetred", high = "royalblue4") +
  coord_flip() +
  labs(
    x = "Features",
    y = "Gain",
    fill = "Gain"
  ) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5))




# Remove the rds files
rm(crime_lasso, crime_elnet, crime_S5, crime_hs_bs, crime_hsp_bs, crime_ssl, 
   crime_spikeslab_prior, crime_horseshoe_tc, crime_horseshoe_hc, crime_blasso,
   df_t, X, importance_matrix)


#### HS BS ####
# Extract CIs
crime_hs_bs_ci <- crime_hs_bs_results$model
crime_hs_bs_ci <- crime_hs_bs_ci$CI.coef[1:110, ]
crime_hs_bs_ci <- crime_hs_bs_ci %>% 
  data.frame()

# Modify the data
crime_hs_bs_ci <- crime_hs_bs_ci %>% 
  rownames_to_column(var = "Variable") %>% 
  # Change variable names to 1, 2,
  mutate(Variable = paste0("X", row_number())) %>%  
  rename(Lower = X1, Upper = X2) %>% 
  # Calculate point estimate as the average of lower and upper CI
  mutate(Estimate = (Lower + Upper) / 2)  


# Add a grouping variable for faceting
crime_hs_bs_ci$facet_group <- ifelse(row_number(crime_hs_bs_ci$Variable) <= 55, "Group 1", "Group 2")

# Add a column to indicate if CI includes zero
crime_hs_bs_ci$CI_includes_zero <- with(crime_hs_bs_ci, Lower * Upper <= 0)

# Create the plot
crime_hs_bs_p <- ggplot(crime_hs_bs_ci, aes(x = reorder(Variable, -Estimate), y = Estimate)) +
  geom_point(aes(color = CI_includes_zero), size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = CI_includes_zero), width = 0.2) +
  scale_color_manual(values = c("TRUE" = "lightblue3", "FALSE" = "violetred")) +
  theme_minimal() +
  labs(y = "Credible Intervals", x = "Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~facet_group, scales = "free_x", ncol = 1)

#### HS+ BS ####
# Extract CIs
crime_hsp_bs_ci <- crime_hsp_bs_results$model
crime_hsp_bs_ci <- crime_hsp_bs_ci$CI.coef[1:110, ]
crime_hsp_bs_ci <- crime_hsp_bs_ci %>% 
  data.frame()

# Modify the data
crime_hsp_bs_ci <- crime_hsp_bs_ci %>% 
  rownames_to_column(var = "Variable") %>% 
  # Change variable names to 1, 2,
  mutate(Variable = paste0("X", row_number())) %>%  
  rename(Lower = X1, Upper = X2) %>% 
  # Calculate point estimate as the average of lower and upper CI
  mutate(Estimate = (Lower + Upper) / 2)  


# Add a grouping variable for faceting
crime_hsp_bs_ci$facet_group <- ifelse(row_number(crime_hsp_bs_ci$Variable) <= 55, "Group 1", "Group 2")

# Add a column to indicate if CI includes zero
crime_hsp_bs_ci$CI_includes_zero <- with(crime_hsp_bs_ci, Lower * Upper <= 0)

# Create the plot
crime_hsp_bs_p <- ggplot(crime_hsp_bs_ci, aes(x = reorder(Variable, -Estimate), y = Estimate)) +
  geom_point(aes(color = CI_includes_zero), size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = CI_includes_zero), width = 0.2) +
  scale_color_manual(values = c("TRUE" = "lightblue3", "FALSE" = "violetred")) +
  theme_minimal() +
  labs(y = "Credible Intervals", x = "Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~facet_group, scales = "free_x", ncol = 1)

#### HS tC ####

# Create a data frame
crime_hs_hs_tC_df <- data.frame(
  Variable = paste0("X", 1:111),
  LeftCI = crime_hs_hs_tC$model$LeftCI,
  RightCI = crime_hs_hs_tC$model$RightCI
)

# Calculate the estimate (midpoint) for plotting
crime_hs_hs_tC_df$Estimate <- (crime_hs_hs_tC_df$LeftCI + crime_hs_hs_tC_df$RightCI) / 2

# Add a grouping variable for faceting
crime_hs_hs_tC_df$facet_group <- ifelse(as.numeric(gsub("X", "", crime_hs_hs_tC_df$Variable)) <= 55, 
                                        "Group 1", "Group 2")

# Add a column to indicate if CI includes zero
crime_hs_hs_tC_df$CI_includes_zero <- with(crime_hs_hs_tC_df, LeftCI * RightCI <= 0)

# Plot
crime_hs_hs_tC_p <- ggplot(crime_hs_hs_tC_df, aes(x = Variable, y = Estimate)) +
  geom_point(aes(color = CI_includes_zero), size = 2) +
  geom_errorbar(aes(ymin = LeftCI, ymax = RightCI, color = CI_includes_zero), width = 0.2) +
  scale_color_manual(values = c("TRUE" = "lightblue3", "FALSE" = "violetred")) +
  theme_minimal() +
  labs(y = "Credible Intervals", x = "Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~facet_group, scales = "free_x", ncol = 1)

#### HS hC ####

# Create a data frame
crime_hs_hs_hC_df <- data.frame(
  Variable = paste0("X", 1:111),
  LeftCI = crime_hs_hs_hC$model$LeftCI,
  RightCI = crime_hs_hs_hC$model$RightCI
)

# Calculate the estimate (midpoint) for plotting
crime_hs_hs_hC_df$Estimate <- (crime_hs_hs_hC_df$LeftCI + crime_hs_hs_hC_df$RightCI) / 2

# Add a grouping variable for faceting
crime_hs_hs_hC_df$facet_group <- ifelse(as.numeric(gsub("X", "", crime_hs_hs_hC_df$Variable)) <= 55, 
                                        "Group 1", "Group 2")

# Add a column to indicate if CI includes zero
crime_hs_hs_hC_df$CI_includes_zero <- with(crime_hs_hs_hC_df, LeftCI * RightCI <= 0)

# Plot
crime_hs_hs_hC_p <- ggplot(crime_hs_hs_hC_df, aes(x = Variable, y = Estimate)) +
  geom_point(aes(color = CI_includes_zero), size = 2) +
  geom_errorbar(aes(ymin = LeftCI, ymax = RightCI, color = CI_includes_zero), width = 0.2) +
  scale_color_manual(values = c("TRUE" = "lightblue3", "FALSE" = "violetred")) +
  theme_minimal() +
  labs(y = "Credible Intervals", x = "Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +  # Hide legend
  facet_wrap(~facet_group, scales = "free_x", ncol = 1)
