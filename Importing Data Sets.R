
# -----------------------------------
file.exists("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx") # Verifying that the file exist and is the correct path name on the macos. If returns true the file does in fact exist.

library(readxl)

excel_sheets("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx") #This will display the list of available sheets within the excel file "Gas Levels.xlsx"

# Load the data 
gas_data <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx", # name df in R as "gas_data"
                       sheet = "Gas Levels",
                       col_names = TRUE)

View(gas_data)
head(gas_data)
str(gas_data)

# -----------------------------

gas_data <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx", # name df in R as "gas_data"
                      sheet = "Gas Levels",
                      col_names = TRUE)

transposed_data <- t(gas_data) # transpose with the (t function) - ["t(place your df name inside)] Remember t-function is df is a matrix

transposed_df <- as.data.frame(t(gas_data)) #convert the matrix back to df

t_data <- as.data.frame(t(gas_data))

colnames(t_data) <- t_data[1, ]
t_data <- t_data[-10, ]

# -------------------------

# Step 1: Split the data
fixed_cols <- gas_data[, 1:10]         # First 10 columns (leave untransposed)
transpose_cols <- gas_data[, -c(1:10)] # Everything else (to be transposed)

# Step 2: Transpose the rest
transposed_part <- as.data.frame(t(transpose_cols))

# View results
View(fixed_cols)
View(transposed_part)

library(tidyr)
library(dplyr)

# Assuming: 
# gas_data is your full original dataframe
# Columns 1 = ID, 2:10 = metadata, 11+ = date columns

long_gas <- gas_data %>%
  pivot_longer(
    cols = 11:ncol(.),               # all date columns
    names_to = "Date",               # new column for date
    values_to = "Gas_Percentage"     # new column for gas % values
  ) %>%
  arrange(ID, Date)                  # optional: sort by ID and date
View(gas_data)


#----- Fixing and testing formula


library(readxl)
library(tidyr)
library(dplyr)

gas_data <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned_Rev.xlsx", 
                       sheet = "Gas Levels")


# Identify the date columns
date_cols1 <- 11:ncol(gas_data)

# Convert them all to character
gas_data[date_cols1] <- lapply(gas_data[date_cols1], as.character) # Change data columns into character (chr)

# Tidy it up
long_gas <- gas_data %>%
  pivot_longer(cols = 11:ncol(.), names_to = "Date", values_to = "Gas_Percentage") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))  # or "%m/%d/%Y" depending on your header

# Step 1: Convert column names to character (if they came in as numbers)
colnames(gas_data) <- as.character(colnames(gas_data))

library(dplyr)

arranged_data <- gas_data %>%
  arrange(.[[1]])  # This is incorrect if [[1]] returns a short vector

arranged_data <- gas_data %>%
  arrange(Unique Identifier)  # If you know the name of the column

str(gas_data)

# Step 2: Pivot longer from column 11 onward
long_gas$Date <- as.Date(long_gas$Date, format = "%m/%d/%y")
long_gas <- gas_data %>%
  pivot_longer(
    cols = 11:ncol(.),
    names_to = "Date",
    values_to = "Gas_Percentage"
  ) %>%
  arrange(gas_data[[1]], Date)  # optional: sort by ID and Date



# -----------------

library(dplyr)
library(tidyr)

# Step 1: Convert column names to character (force all to consistent format)
names(gas_data) <- as.character(names(gas_data))

# Step 2: Identify where the date columns begin (e.g., column 11)
# Transpose only from that point on
long_gas <- gas_data %>%
  pivot_longer(
    cols = 11:ncol(.),
    names_to = "Date",
    values_to = "Gas_Percentage"
  )

# Try interpreting it as a real date
long_gas$Date <- as.Date(long_gas$Date, format = "%m/%d/%y")

# If that gives NAs, try:
# long_gas$Date <- as.Date(long_gas$Date, format = "%m/%d/%Y")

str(names(gas_data)[11:ncol(gas_data)])

# --------------------

library(readxl)
library(dplyr)
library(tidyr)

# Step 1: Read the Excel file

gas_data <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels.xlsx", 
                       sheet = "Insitro, Inc. - Gas Levels_Rev",
                       col_names = TRUE)

# Step 2: Coerce all column names to character (this is key!)

names(gas_data) <- as.character(names(gas_data))

str(names(gas_data)[11:ncol(gas_data)])
# 🔍 Optional: Print column classes to confirm issue
# str(names(gas_data)[11:ncol(gas_data)])

# Step 3: Now safely pivot
long_gas <- gas_data %>%
  pivot_longer(
    cols = 11:ncol(.),
    names_to = "Date",
    values_to = "Gas_Percentage"
  )

library(readxl)
library(dplyr)
library(tidyr)

# Load the data
gas_data_ <- read_excel("/Users/elijajuanfisher/Documents/Gas Levels/Gas Levels_Cleaned.xlsx")

colnames(gas_data_cleaned) # View column names to identify fixed vs date columns

gas_data_cleaned <- gas_data[-(36:99), -(76:115)] # This will remove rows in which you do not need; ie: eliminate rows 36 to 99

# -(36:99) will eliminate rows 36 to 99
# -(76:115) will eliminate 76 to 115

ncol(gas_data_cleaned) # To view the number of columns
nrow(gas_data_cleaned) # To view the number of rows
View(gas_data_cleaned) # To view the df



# Reshape the data
gas_long <- gas_data_cleaned %>%
  pivot_longer(
    cols = c(`Unique Identifier`,`Date`),  # Keep ID and Timestamp fixed
    names_to = "Gas",           # New column for gas name
    values_to = "Level"         # New column for measurement value
  )

# Optional: view or save
head(gas_long)
writexl::write_xlsx(gas_long, "Reshaped_Gas_Data.xlsx")

