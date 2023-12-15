### POL 501 Final Project Script ### 
# First we import data and prepare 

# Import Census Data 
census_data

# Change headers of census_data
### Current headers are B19013_001E B15003_022E B02001_002E B01001_026E B01002_001E B23025_005E B25064_001E state county
# Assigning new, more descriptive column names to the census_data dataframe
colnames(census_data) <- c("Median_Household_Income", "People_with_Bachelors", 
                           "People_Identifying_as_White", "Total_Female_Population", 
                           "Median_Age", "Number_of_Unemployed", "Median_Gross_Rent", 
                           "State_Code", "County_Code")
# Next, we will clean the census_data
head(census_data)
summary(census_data)
str(census_data)
# Check for missing values
sum(is.na(census_data))
# Options to handle missing values:
# Remove rows with missing values
census_data <- na.omit(census_data)
# Or, impute missing values (example with median)
# census_data$Median_Household_Income <- ifelse(is.na(census_data$Median_Household_Income), 
#                                               median(census_data$Median_Household_Income, na.rm = TRUE), 
#                                               census_data$Median_Household_Income)
# Example: Convert columns to numeric, if they are not already
numeric_cols <- c("Median_Household_Income", "People_with_Bachelors", 
                  "People_Identifying_as_White", "Total_Female_Population", 
                  "Median_Age", "Number_of_Unemployed", "Median_Gross_Rent")
census_data[numeric_cols] <- lapply(census_data[numeric_cols], as.numeric)
# Convert State_Code and County_Code to factors
census_data$State_Code <- as.factor(census_data$State_Code)
census_data$County_Code <- as.factor(census_data$County_Code)
# Example: Boxplot to spot outliers
boxplot(census_data$Median_Household_Income)
# Handling outliers depends on your analysis needs. 
# You might choose to remove them or cap them.
# List of all your dataframes excluding CensusCodes1
census_dfs <- list(CensusCodes2, CensusCodes3, CensusCodes4, 
                   CensusCodes5, CensusCodes6, CensusCodes7, CensusCodes8, 
                   CensusCodes9, CensusCodes10, CensusCodes13, 
                   CensusCodes14, CensusCodes15)
# Standardize column names for each dataframe
headers <- c('state code', 'county code', 'county name', 'state abbr')
# Setting column names and ensuring all dataframes have the same structure
census_dfs <- lapply(census_dfs, function(df) {
  colnames(df) <- headers
  return(df)
})
# Combining all dataframes into one master dataframe
master_df <- do.call(rbind, census_dfs)
# Check the combined dataframe
head(master_df)
# Load the dplyr package for easier data manipulation
library(dplyr)
# Updating the column names
colnames(master_df) <- c("State_Code", "County_Code", "County_Name", "State_Abbr")

# Formatting the County_Code column to have leading zeroes, making each code 3 characters long
master_df$County_Code <- sprintf("%03d", as.integer(master_df$County_Code))

# Checking the changes
head(master_df)

# Assigning new column names to the states dataframe
colnames(states) <- c("State_Name", "State_Abbr")

# Check the changes
head(states)

# We are now ready to merge the dataframes 
# Merging the master_df dataframe with the states dataframe based on 'State_Abbr'
combined_df <- merge(master_df, states, by = "State_Abbr")

# Check the merged dataframe
head(combined_df)

# Merging combined_df with census_data on 'State_Code' and 'County_Code'
final_df <- merge(combined_df, census_data, by = c("State_Code", "County_Code"))

# Check the merged dataframe
head(final_df)

# Creating a new dataframe with data from the year 2020
election_data <- subset(countypres_2000_2020, year == 2020)

# Check the resulting dataframe
head(election_data)

# Function to standardize values in a specific column
standardize_values <- function(df, column_name) {
  df[[column_name]] <- tolower(gsub(" |-", "", df[[column_name]]))
  return(df)
}

# Apply the function to the 'County_Name' column of both dataframes
final_df <- standardize_values(final_df, "County_Name")
election_data <- standardize_values(election_data, "county_name")
final_df
election_data

# Renaming 'county_name' to 'County_Name' in election_data
colnames(election_data)[colnames(election_data) == "county_name"] <- "County_Name"

# Check the change
head(election_data)

# Merging final_df with election_data on 'County_Name'
merged_df <- merge(final_df, election_data, by = "County_Name")

# Check the merged dataframe
head(merged_df)

merged_df

