# Load packages
library(readxl)
library(dplyr)
library(tidyr) # manipulate data
library(janitor) # for data cleaning ie: cleaning col. names
library(lubridate) # Parsing the dates format

# Load data and clean column names
gas_data <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx") %>%

# Remove unnecessary rows and columns early [-(rows), -(cols)]
gas_data_cleaned <- gas_data[-(36:99), -(76:115)]

# Extract all column names
all_cols <- names(gas_data_cleaned)

print(all_cols)

# Define fixed metadata columns
fixed_cols <- c(
  "unique_identifier", "location", "floor", "tank_no", "element_name", "matter_type", 
  "material_type", "tank_type", "avg_weight_lbs", "quanity"
)

# Filter out fixed columns
raw_date_cols <- setdiff(all_cols, fixed_cols) # The date cols are now spliced from the fixed cols. as there own vector.

head(raw_date_cols)

# Clean the column names: remove "x" or "x_" and replace "_" with "/"
cleaned_date_names <- gsub("^x_?", "", raw_date_cols)  # Remove ("x" or "x_") from the dates. #Dates are slightly distorted in the dates columns, ie: x 5_20_25 instead of 5/20/25 or 2025-05-20
cleaned_date_names <- gsub("_", "/", cleaned_date_names)  # Replace ("_" with "/") to the dates

# Attempt to convert them to actual dates
parsed_dates <- suppressWarnings(mdy(cleaned_date_names))

# Keep only those that successfully converted to a Date
valid_date_cols <- raw_date_cols[!is.na(parsed_dates)]
parsed_date_values <- parsed_dates[!is.na(parsed_dates)]

# Rename those columns to real date strings (optional but cleaner)
names(gas_data_cleaned)[names(gas_data_cleaned) %in% valid_date_cols] <- as.character(parsed_date_values)

# Now assign the updated date_cols
date_cols <- as.character(parsed_date_values)

# Check that they look good
print(date_cols)

# Identify potential date columns
all_cols <- names(gas_data_cleaned)
date_cols <- setdiff(all_cols, fixed_cols)

# View and verify
print(date_cols) # Looks good Dates are parsed. "yyyy-mm-dd"
sapply(gas_data_cleaned[date_cols], class) # Dates classes are "numeric" which is positive sign. 

# Keep only fixed + date columns
gas_data_cleaned <- gas_data_cleaned %>%
  select(all_of(fixed_cols), all_of(date_cols))

# Convert date columns to numeric values
gas_data_cleaned[date_cols] <- lapply(gas_data_cleaned[date_cols], function(col) as.numeric(as.character(col)))

# Pivot Longer transpose the dates columns
df_long <- gas_data_cleaned %>%
  pivot_longer(
    cols = all_of(date_cols),
    names_to = "Date",
    values_to = "Level"
  ) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Level))  # Optional: remove NA rows

# Preview result
head(df_long)

View(df_long)

