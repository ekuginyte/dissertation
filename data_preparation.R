### SET UP ###
# Packages/libraries required to install
library_list <- c("tidyverse", "corrplot", "betareg", "R.matlab")

# Install the packages
#for (i in library_list) {
#  install.packages(i, character.only = TRUE)
#}

# Load the libraries
for (i in library_list) {
  library(i, character.only = TRUE)
}

# Set working directory
setwd("~/Documents/Dissertation/data")

### LOAD DATA ###
# List all files to upload together
files <- list.files(path = "~/Documents/Dissertation/data/", pattern = "*.csv", full.names = TRUE)

## Read and merge the files
# Start with the first file
merged_df <- read_csv(files[1])  

# Loop all files to merge them into one data frame by reference "geography"
for (file in files[-1]) {  # Loop over the rest of the files
  df <- read_csv(file)
  merged_df <- merge(merged_df, df, by = "geography")
}

# Remove the duplicate columns
# Find column names that contain "date" or "geography code"
cols_to_remove <- grep("date|geography code", names(merged_df), ignore.case = TRUE, value = TRUE)

# Remove these columns
merged_df <- merged_df[ , !(names(merged_df) %in% cols_to_remove)]

# Remove the unwanted variables
rm(cols_to_remove, file, files, i, library_list, df)

# Add the divorce rate per area column
#merged_df$"Divorce_Rate" <- merged_df$`Marital and civil partnership status: Divorced or civil partnership dissolved; measures: Value` / 
#  merged_df$`Age: Total`

# Save the total number of people per area variable as a vector
count_per_area <- as.vector(merged_df$`Age: Total`)

# Create a new dataframe containing only columns with "Total" in their names
total_df <- merged_df[ , grepl("Total", names(merged_df))]

# Remove the columns that have "Total" values as they are unnecessary for
#   predictions
merged_df <- merged_df[, !grepl("Total", names(merged_df))]

# Add the variable with amount of people in the area
merged_df$Count_per_Area <- count_per_area

# Save geography code as a vector
geography_code <- as.vector(merged_df$geography)

# Remove the geography code from the data frame as it's not a meaningful
#   predictor
merged_df <- dplyr::select(merged_df, -geography)



### FORMAT DATA ###
# Make all variables proportions of people per area, except from the 
#   population density
# Get the names of all columns that you want to convert to proportions
#col_names <- names(merged_df)[!(names(merged_df) %in% c("Count_per_Area", 
#                                                        "Divorce_Rate",
#                                                        "Population Density: Persons per square kilometre; measures: Value"))]

# Convert all these columns to proportions
#merged_df[col_names] <- merged_df[col_names] / merged_df$Count_per_Area



### CHECK IF ANY VALUES IN THE COLUMNS HAVE MORE THAN 1 ###
# Exclude the density column
#cols_to_check <- names(merged_df)[!(names(merged_df) %in% "Population Density: Persons per square kilometre; measures: Value")]

# Initialize a vector to store column names with values greater than 1
#cols_with_values_greater_than_one <- c()

# Check each column
#for (col in cols_to_check) {
#  if (any(merged_df[[col]] > 1)) {
#    cols_with_values_greater_than_one <- c(cols_with_values_greater_than_one, col)
#  }
#}

# Print columns with values greater than one
#print(cols_with_values_greater_than_one)



### CHECK IF ALL COLUMNS ARE NUMERIC ###
# Check if all variables are numeric
all(sapply(merged_df, is.numeric))




### CHECK CORRELATIONS ###
correlation_matrix <- cor(merged_df)
#corrplot(correlation_matrix, method = "color")
#heatmap(correlation_matrix)

high_corr <- correlation_matrix
high_corr[abs(high_corr) < 0.8] <- NA
#corrplot(high_corr, method = "color", na.label = " ")











### FIT A LINEAR MODEL ###
# First get a small sample to test regression models on for easy computation
df_sample <- merged_df %>% 
  sample_n(100, replace = FALSE)

# Fit a logistic model
log_mdl1 <- glm(Divorce_Rate ~ ., data = df_sample, family = binomial)

summary(log_mdl1)

# Fit a beta model
beta_mdl1 <- betareg(Divorce_Rate ~ ., data = df_sample)

summary(beta_mdl1)














# Crime data
df <- read.csv('~/Downloads/communities.data')

# Replace "?" with NA in the entire data frame
df[df == "?"] <- NA

# Rename the columns
# Function to rename columns of the data frame
rename_columns <- function(df) {
  new_column_names <- c("state", "county", "community", "communityname", "fold",
                        "population", "householdsize", "racepctblack", "racePctWhite",
                        "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29",
                        "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "medIncome",
                        "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec", "pctWPubAsst",
                        "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap",
                        "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov",
                        "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed",
                        "PctEmploy", "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf",
                        "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PersPerFam",
                        "PctFam2Par", "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids",
                        "PctWorkMom", "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5",
                        "PctImmigRec8", "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8",
                        "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam",
                        "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous",
                        "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant",
                        "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt",
                        "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart",
                        "RentLowQ", "RentMedian", "RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc",
                        "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet", "PctForeignBorn", "PctBornSameState",
                        "PctSameHouse85", "PctSameCity85", "PctSameState85", "LemasSwornFT", "LemasSwFTPerPop",
                        "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop",
                        "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack",
                        "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz",
                        "PolicAveOTWorked", "LandArea", "PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg",
                        "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "LemasPctOfficDrugUn", "PolicBudgPerPop",
                        "ViolentCrimesPerPop")
  # Rename columns
  colnames(df) <- new_column_names
  return(df)
}

# Renaming columns
df <- rename_columns(df)

# Calculate number of NA's in each column
columns_with_na <- colSums(is.na(df))
columns_with_na <- names(columns_with_na[columns_with_na > 1])

# Delete columns with more than 1 NA from data frame
df <- df[, !(names(df) %in% columns_with_na)]



# Find which columns have missing data
columns_with_na <- colSums(is.na(df))
columns_with_na <- columns_with_na[columns_with_na > 0]
na_columns_info <- data.frame(column_name = names(columns_with_na), na_count = columns_with_na)

# Find which rows have missing data
rows_with_na <- rowSums(is.na(df))
rows_with_na <- rows_with_na[rows_with_na > 0]
na_rows_info <- data.frame(row_number = which(rows_with_na > 0), na_count = rows_with_na[rows_with_na > 0])

# Display the results
if (nrow(na_columns_info) > 0) {
    cat("Columns with missing data:\n")
    print(na_columns_info)
  } else {
    cat("No columns with missing data.\n")
}

if (length(na_rows_info$row_number) > 0) {
    cat("Rows with missing data:\n")
    print(na_rows_info)
  } else {
    cat("No rows with missing data.\n")
}

# Row 130, OtherPerCap has a missing value












### Unnormalised raw data
# Read the text file into a dataframe
df <- read.csv('~/Downloads/CommViolPredUnnormalizedData.txt')

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
columns_with_na <- names(columns_with_na[columns_with_na > 0])
#na_columns_info <- data.frame(column_name = names(columns_with_na), na_count = columns_with_na)

# Delete columns with more than 1 NA from data frame
df <- df[, !(names(df) %in% columns_with_na)]


# Find which rows have missing data
rows_with_na <- rowSums(is.na(df))
rows_with_na <- rows_with_na[rows_with_na > 0]
na_rows_info <- data.frame(row_number = which(rows_with_na > 0), na_count = rows_with_na[rows_with_na > 0])

# Display the results
if (nrow(na_columns_info) > 0) {
  cat("Columns with missing data:\n")
  print(na_columns_info)
} else {
  cat("No columns with missing data.\n")
}

if (length(na_rows_info$row_number) > 0) {
  cat("Rows with missing data:\n")
  print(na_rows_info)
} else {
  cat("No rows with missing data.\n")
}


# Reorder the data frame so the column names are in alphabetical order
df_ordered <- df[, order(names(df))]


