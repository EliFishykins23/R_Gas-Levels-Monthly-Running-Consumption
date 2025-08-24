# Load packages

library(readxl)
library(dplyr)
library(tidyr) # Manipulate data
library(janitor) # For data cleaning ie: cleaning col. names
library(lubridate) # Parsing the dates format
library(scales) # Transforming numerical data by scaling or adjusting

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

# QC the date columns
print(date_cols)

# Identify potential date columns
all_cols <- names(gas_data_cleaned)
date_cols <- setdiff(all_cols, fixed_cols) # calculates the difference between (2) vectors. Leaving me with the remainder

# View and verify
print(date_cols) # Dates are now parsed. "yyyy-mm-dd"
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

# Excel Logic - If the value in ie: column A (row 3) equals the value in ie: column A (row 2) AND
# The value in column ie: L (row 3) is greater than the value in ie: column L (row 2)
# Then return 1, otherwise return 0.

# Code for - (Running Total) 

 df_running_total <- df_long %>%
  mutate(
    gas_turns = if_else(
      unique_identifier == lag(unique_identifier) & Level > lag(Level), 
      1, 
      0,
      missing = 0 # This is related to the n/a, missing value in the first row.
    )
  )
 
 View(df_running_total)
 
# Format the "Levels" column into percentage instead of decimals
 
 df_percentage <- df_running_total %>%
   mutate(Level = percent(Level, accuracy = 0.1))  # 0.1 = one decimal place
 
View(df_percentage)

# Bar Chart - Yearly Average Gas Turns by Element Name and Floor

df_running_total %>%
  group_by(floor, element_name) %>%
  summarise(avg_turns = mean(gas_turns), .groups = "drop") %>%
  ggplot(aes(x = reorder(element_name, -avg_turns), y = avg_turns, fill = floor)) +
  geom_col(position = "dodge") +  # side-by-side bars per floor
    labs(
    title = "Yearly Average Gas Turns by Element Name and Floor",
    x = "Element Name",
    y = "Average Gas Turns",
    fill = "Floors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5))  # tilt labels for readability

#  Bar Chart - "Yearly Sum of Gas Turns by Element Name and Floor"

df_running_total %>%
  mutate(floor = factor(floor, levels = c("2nd Floor", "3rd Floor", "4th Floor"))) %>% # Customize the order by floor 
  group_by(floor, element_name) %>%
  summarise(Total_Gas_turns = sum(gas_turns), .groups = "drop") %>%
  ggplot(aes(x = floor, y = Total_Gas_turns, fill = element_name)) + # aes() is overriding the factor levels.
  geom_col(position = "dodge", width = 0.8) + # (width) adjustment will allow the bar in the chart to become more uniform
  geom_text(aes(label = round(Total_Gas_turns, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = 4.5,  # Vertical and horizontal adjustment for data labels within the bar.
            hjust = 0.5,  # " "
            color = "white", # " "
            size = 3) +  # adjust size as needed
  labs(
    title = "Yearly Sum of Gas Turns by Element Name and Floor",
    x = "Floor",
    y = "Total Gas Turns",
    fill = "Element Name"
  ) +
  theme_linedraw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9), # adjustment to the title
    axis.text.x = element_text(angle = 360, hjust = 0.5)) # adjustments to the x-axis "Floor" labels


# Line Graph - Total Gas Turns per Month by Element

df_monthly_turns <- df_running_total %>%
  mutate(month = floor_date(Date, "month")) %>%  # Get month from Dates
  group_by(element_name, month) %>%
  summarise(
    total_turns = sum(gas_turns, na.rm = TRUE),
    count_turns = n(),
    .groups = "drop"
  )

# Plot total turns per month by element_name

ggplot(df_monthly_turns, aes(x = month, y = total_turns, color = element_name)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Total Gas Turns per Month by Element Name",
    x = "Month",
    y = "Total Gas Turns",
    color = "Element Name"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

