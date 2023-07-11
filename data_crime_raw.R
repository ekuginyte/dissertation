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
                    "state", "murders", "murdPerPop")

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

# df_with_na_cols_with_na shows that the variables that have only some 
  # missing data will not be relevant to the analysis, as these cannot be used 
  # as predictors or targets.
#irrelevant_variables <- colnames(df_with_na_cols_with_na)[-1]

# Delete these irrelevant variables
#df <- df[, !(names(df) %in% irrelevant_variables)]

# Remove the rows with missing data
df <- df[complete.cases(df), ]

# Extract the community names seperately as it won't play a part in the 
  # analysis
df_communities <- df[, 1] %>% 
  data.frame()

# Remove community names from the main df
df <- df[, -1]

# Remove unwanted objects
rm(df_with_na_cols_with_na, rows_with_na_indices, cols_to_remove, cols_with_na,
     cols_with_na_logical, columns_with_na, rows_with_na, rename_columns)




################################################################################
#### VARIABLES CONTINUOUS VS DISCRETE ####
# Identify numeric columns
numeric_cols <- sapply(df, is.numeric)

# Extract numeric columns
df_numeric <- df[, numeric_cols]

# Identify continuous and discrete columns
continuous_cols <- sapply(df_numeric, function(x) any(x != round(x)))
discrete_cols <- !continuous_cols

# Extract continuous and discrete columns
df_continuous <- df_numeric[, continuous_cols]
df_discrete <- df_numeric[, discrete_cols]

# Remove unwanted objects
rm(df_numeric, continuous_cols, discrete_cols, numeric_cols)


# Separate the target
df_target <- data.frame(df[, "ViolentCrimesPerPop"])

# Predictors
df_predictors <- data.frame(df[, -103])

# Convert df_target to numeric
vector_target <- as.numeric(df_target[, 1])

