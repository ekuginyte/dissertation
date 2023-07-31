################################################################################
#### READ IN AND WRANGLE DATA ####

# Read the text file into a dataframe
df <- read.csv('~/Documents/Dissertation/data/CommViolPredUnnormalizedData.txt')

# Rename the columns
# Function to rename columns of the data frame
# INPUT:
#       df - data frame with old column names.
# OUTPUT:
#       df - data frame with new column names.
rename_columns <- function(df) {
  new_column_names <- c("communityname", "state", "countyCode", "communityCode", "fold", "population", 
                        "householdsize", "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", 
                        "agePct12t21", "agePct12t29", "agePct16t24", "agePct65up", "numbUrban", "pctUrban", 
                        "medIncome", "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec", "pctWPubAsst", 
                        "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", 
                        "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov", "PctPopUnderPov", 
                        "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", 
                        "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce", 
                        "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", 
                        "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", 
                        "NumKidsBornNeverMar", "PctKidsBornNeverMar", "NumImmig", "PctImmigRecent", "PctImmigRec5", 
                        "PctImmigRec8", "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", 
                        "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", 
                        "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous", 
                        "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", 
                        "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", 
                        "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", 
                        "OwnOccQrange", "RentLowQ", "RentMedian", "RentHighQ", "RentQrange", "MedRent", 
                        "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters", 
                        "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85", 
                        "PctSameState85", "LemasSwornFT",  "LemasSwFTPerPop", "LemasSwFTFieldOps",
                        "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic",
                        "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack",
                        "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits",
                        "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "PopDens", "PctUsePubTrans",
                        "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr",
                        "LemasGangUnitDeploy", "LemasPctOfficDrugUn", "PolicBudgPerPop", "murders",
                        "murdPerPop", "rapes", "rapesPerPop", "robberies", "robbbPerPop", "assaults",
                        "assaultPerPop", "burglaries", "burglPerPop", "larcenies", "larcPerPop",
                        "autoTheft", "autoTheftPerPop", "arsons", "arsonsPerPop", "ViolentCrimesPerPop",
                        "nonViolPerPop")
  # Rename columns
  colnames(df) <- new_column_names
  return(df)
}

# Renaming columns
df <- rename_columns(df)

# Replace "?" with NA in the entire data frame
df[df == "?"] <- NA

# Calculate number of NA's in each column
columns_with_na <- colSums(is.na(df))

# There are many variables that have a great deal of missing values,
  # they are also not very relevant to the analysis here,
  # hence, shall delete them altogether
columns_with_na <- names(columns_with_na[columns_with_na > 221])

# Delete columns with more than 13 NA from data frame
df <- df[, !(names(df) %in% columns_with_na)]

# As there are still missing data (0 to 13 values in some columns) 
  # find which specific rows have missing data
rows_with_na <- rowSums(is.na(df)) > 0 
# Save these rows in a data frame
rows_with_na_indices <- df[rows_with_na, ]

# Check which columns have missing data and save them
cols_with_na <- colSums(is.na(rows_with_na_indices))

# Variables that hold information on specific convictions should be deleted
  # as the overall Violent Crimes number will be the target variable
# List of column names to be removed
cols_to_remove <- c("countyCode", "communityCode", "rapes", "rapesPerPop", "robberies", "robbbPerPop", 
                    "assaults", "assaultPerPop", "burglaries", "burglPerPop", "larcenies", "larcPerPop", 
                    "autoTheft", "autoTheftPerPop", "arsons", "arsonsPerPop", "nonViolPerPop", "fold",
                    "murders", "murdPerPop", "OwnOccQrange", "RentQrange", "state")

# Remove columns
df <- df[, !(names(df) %in% cols_to_remove)]

# Make sure that each column that has been left is numeric
df[, -1] <- sapply(df[, -1], as.numeric)

# Create a logical vector indicating which columns have NA's
  # Also keep the 'communityname' column regardless as it indicates the row
cols_with_na_logical <- (cols_with_na > 0) | 
  (colnames(rows_with_na_indices) == "communityname")

# Subset the data frame to retain only the columns with NA's and 'communityname'
  # as to figure out what is missing in the data set and if these variables
  # or rows can be deleted altogether
df_with_na_cols_with_na <- rows_with_na_indices[, cols_with_na_logical]

# Remove the rows with missing data
df <- df[complete.cases(df), ]

# Remove community names from the main df
df <- df[, -1]

# Rename the target
colnames(df)[99] <- "y"

# Remove unwanted objects
rm(df_with_na_cols_with_na, rows_with_na_indices, cols_to_remove, cols_with_na,
     cols_with_na_logical, columns_with_na, rows_with_na, rename_columns)



################################################################################
#### TRANSFORMING DATA ####

# Transform all data
df_t <- log(df + 1)




################################################################################
#### ADDING INTERACTION TERMS ####
# As from the linear model and correlation matrix, parents who have not married,
#   is an important predictor. Interesting to see a similar relationship 
#   for the divorce rates too.
# Kids born to parents who are not married against demographic groups
df_t$Black_KidsBornNeverMar_Int <- df_t$racepctblack * df_t$PctKidsBornNeverMar
df_t$White_KidsBornNeverMar_Int <- df_t$racePctWhite * df_t$PctKidsBornNeverMar
df_t$Asian_KidsBornNeverMar_Int <- df_t$racePctAsian * df_t$PctKidsBornNeverMar
df_t$Hispanic_KidsBornNeverMar_Int <- df_t$racePctHisp * df_t$PctKidsBornNeverMar
df_t$ForeignBorn_KidsBornNeverMar_Int <- df_t$PctForeignBorn * df_t$PctKidsBornNeverMar

# Divorced against demographic groups
df_t$Black_DivorcePct_Int <- df_t$racepctblack * df_t$TotalPctDiv
df_t$White_DivorcePct_Int <- df_t$racePctWhite * df_t$TotalPctDiv
df_t$Asian_DivorcePct_Int <- df_t$racePctAsian * df_t$TotalPctDiv
df_t$Hispanic_DivorcePct_Int <- df_t$racePctHisp * df_t$TotalPctDiv
df_t$ForeignBorn_DivorcePct_Int <- df_t$PctForeignBorn * df_t$TotalPctDiv

# Save a standardised version to check if different variables are selected
df_st <- scale(df_t) %>% data.frame()


################################################################################
#### EXPLORATORY ANALYSIS ####



## PLOTTING HISTOGRAMS ##

# Create a list to hold the plots
hist_list <- list()

# Loop over the columns of the dataframe
for (i in 1:(ncol(df))) {
  # Create the histogram for column i
  hist_list[[i]] <- ggplot(df, aes_string(names(df)[i])) + 
    geom_histogram(fill = "gray99", color = "lightblue3", size = 0.4, alpha = 0.5) + 
    ylab("Count") +
    # Choose theme of plot
    theme_minimal()  
}


## PLOTTING HISTOGRAMS TRANSFORMED DATA ##

# Create a list to hold the plots
hist_t_list <- list()

# Loop over the columns of the dataframe
for (i in 1:(ncol(df_t))) {
  # Create the histogram for column i
  hist_t_list[[i]] <- ggplot(df_t, aes_string(names(df_t)[i])) + 
    geom_histogram(fill = "gray99", color = "lightblue3", size = 0.4, alpha = 0.5) + 
    ylab("Count") +
    # Choose theme of plot
    theme_minimal()  
}


## PLOT INTERACTIONS WITH THE PREDICTION ##

# Create a list to hold the plots
scatter_t_list <- list()

# Loop over the columns of the dataframe
for (i in 1:(ncol(df_t)-1)) {
  # Create the scatter plot for column i vs prediction variable
  scatter_t_list[[i]] <- ggplot(df_t, aes_string(x = names(df_t)[i], y = "y")) +
    geom_point(colour = "plum3", alpha = 0.2) +
    # Minimal theme of plopt
    theme_minimal() 
}


## PLOT BOXPLOTS ##

# Create a list to hold the plots
boxplot_t_list <- list()

# Loop over the columns of the dataframe
for (i in 1:(ncol(df_t))) {
  # Create the boxplot for column i
  boxplot_t_list[[i]] <- ggplot(df_t, aes_string(x = 1, y = names(df_t)[i])) +
    geom_boxplot(fill = "lightblue3", colour = "gray20", size = 0.1, alpha = 0.4) +
    xlab("") +  # Removing the x-label as it's not meaningful here
    theme_minimal() +
    theme(axis.text.x = element_blank())  # Remove x-axis text
}


## PLOT CORRELATION MATRIX ##

# Calculate the correlation matrix
cor_matrix <- cor(df_t)


## MULTICOLLINEARITY CHECK ##

# Fit a linear model
model <- lm(y ~ ., data = df)

# Calculate VIF
vif_values <- vif(model)


## NORMALITY CHECKS ##

# Apply Shapiro-Wilk test to each column
p_values <- apply(df_t, 2, function(x) shapiro.test(x)$p.value)

# Create a data frame of the results
results <- data.frame(Variable = names(p_values), P_Value = p_values)
#any(results$P_Value > 0.05)


## MISSING VALUES ##

check_data <- function(data) {
  # Loop over columns
  for (col in names(data)) {
    # Check if the column is numeric
    if (!is.numeric(data[[col]])) {
      stop(paste("Column", col, "is not numeric."))
    }
    # Check for missing values
    if (anyNA(data[[col]])) {
      stop(paste("Column", col, "contains missing values."))
    }
  }
  # If no problems found, print a success message
  print("All columns are numeric and contain no missing values.")
}

# Does the data have any missing values?
#df_t_missing <- check_data(df_t)




################################################################################
#### OUTLIERS ####

# Define a function to detect outliers based on the IQR
# INPUTS:
#         data - the data frame containing the data
#         columns - the columns in which to look for outliers
#         factor - the factor to multiply the IQR by to find the bounds (default is 2)
# OUTPUT:
#         outlier_indices - indices of outliers.
# 
detect_outliers_iqr <- function(data, columns, factor = 2){
  
  # Initialize a vector to hold the indices of outlier rows
  outlier_indices <- c()
  
  # Loop over each specified column
  for(col in columns){
    
    # Calculate the first quartile (25th percentile)
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    
    # Calculate the third quartile (75th percentile)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    
    # Calculate the interquartile range (IQR)
    IQR <- Q3 - Q1
    
    # Calculate the lower bound for what will be considered an outlier
    lower_bound <- Q1 - factor * IQR
    
    # Calculate the upper bound for what will be considered an outlier
    upper_bound <- Q3 + factor * IQR
    
    # Identify the indices of rows where the column value is an outlier
    outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    
    # Add the indices of the outliers to the list of outlier indices
    outlier_indices <- c(outlier_indices, outliers)
  }
  
  # Return the unique outlier indices
  return(unique(outlier_indices))
}


# Call the detect_outliers_iqr function, passing the data frame and column names of the numeric columns.
outlier_indices <- detect_outliers_iqr(df_t, names(df_t))

# Print the number of outliers and their indices
#cat(paste0("Number of outliers detected: ", length(outlier_indices)), "\n")
#cat(paste0("Outlier indices: ", outlier_indices), "\n")

# Remove the outliers from the data frame by subsetting the data frame to exclude these rows.
# The negative sign before outlier_indices means "all rows EXCEPT these indices".
df_no_outliers <- df_t[-outlier_indices, ]

# Remove the unwanted data
rm(df_no_outliers, p_values, results, vif_values, i, check_data, 
   detect_outliers_iqr, scatter_list, model, outlier_indices, 
   df, model, df_t_missing)

