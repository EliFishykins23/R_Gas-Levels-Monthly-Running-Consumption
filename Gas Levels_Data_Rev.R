# Load packages

install.packages(dplyr)
install.packages(readr)
install.packages(scales)


install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("janitor", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("readr", dependencies = TRUE)

library(readxl) # Allows to read xlsx format
library(dplyr) #
library(tidyr) # Manipulate data
library(janitor) # For data cleaning ie: cleaning col. names
library(lubridate) # Parsing the dates format
library(scales) # Transforming numerical data by scaling or adjusting
library(ggplot2) # Graph plotting
library(readr) # Allows you to read factors and date times directly from disk

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

View(gas_data_cleaned)

# Check what data is missing

# Keep only fixed + date columns cont'd.

setdiff(fixed_cols, colnames(gas_data_cleaned))
setdiff(date_cols, colnames(gas_data_cleaned))

gas_data_cleaned <- gas_data_cleaned %>%
  select(any_of(fixed_cols), any_of(date_cols))

# Convert date columns to numeric values
gas_data_cleaned[date_cols] <- lapply(gas_data_cleaned[date_cols], function(col) as.numeric(as.character(col)))

# Pivot Longer transpose the dates columns 

df_long <- gas_data_cleaned %>%
  pivot_longer(
    cols = all_of(date_cols),
    names_to = "Date",
    values_to = "Level"
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d")  # adjust format if needed
  ) %>%
  filter(!is.na(Date))   # Optional: remove NA rows

# Preview result
head(df_long)
View(df_long)


# Excel Logic - If the value in ie: column A (row 3) equals the value in ie: column A (row 2) AND
# The value in column ie: L (row 3) is greater than the value in ie: column L (row 2)
# Then return 1, otherwise return 0.

# Code for - (Running Total) - For each "unique identifier" 

View(long_gas)

 df_running_total <- long_gas %>%
  group_by(`Unique Identifier`) %>% 
   mutate(
    Gas_Turns = if_else(`Unique Identifier` == lag(`Unique Identifier`) & Gas_Percentage > lag(Gas_Percentage), 
      1, 
      0, 
      missing = 0    # if triggered a missing value = (na) return back value 0
    )
  )
 
 View(df_running_total)
 
# Format the "Levels" column into percentage instead of decimals # This requires using "scales" - install library(scales)
 
 
 df_percentage <- df_running_total %>%
   mutate(
     Level_num = suppressWarnings(parse_number(as.character(Gas_Percentage), na = c("", "NA", "N/A"))),
     Gas_Percentage = scales::percent(Level_num / 1.0, accuracy = 1.0)    # "Level_num" represents Gas_Percentage 
   ) %>%
   select(-Level_num) # Remove Unique Helper Column
 
 df_percentage %>% 
   select(Gas_Percentage) %>%  # choose the column
   head(20)                    # show first 20 rows
 
 
View(df_percentage)

# Bar Chart - Yearly Average Gas Turns by Element Name and Floor Revise * 

df_percentage %>%
  group_by(Floor, `Element Name`) %>%
  summarise(avg_turns = mean(Gas_Turns), .groups = "drop") %>%
  ggplot(aes(x = reorder(`Element Name`, -avg_turns), y = avg_turns, fill = Floor)) +
  geom_col(position = "dodge") +  # side-by-side bars per floor
    labs(
    title = "Yearly Average Gas Turns by Element Name and Floor",
    x = "Element Name",
    y = "Average Gas Turns",
    fill = "Floors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5))  # tilt labels for readability


#

df_percentage %>%
  mutate(Floor = factor(Floor, levels = c("2nd Floor", "3rd Floor", "4th Floor"))) %>% 
  group_by(Floor, `Element Name`) %>%
  summarise(Total_Gas_Turns = sum(Gas_Turns), .groups = "drop") %>%
  ggplot(aes(x = Floor, y = Total_Gas_Turns, fill = `Element Name`)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(aes(label = round(Total_Gas_Turns, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = 4.5,
            hjust = 0.5,
            color = "white",
            size = 3) +
  labs(
    title = "Yearly Sum of Gas Turns by Element Name and Floor",
    x = "Floor",
    y = "Total Gas Turns",
    fill = "Element Name"
  ) +
  theme_linedraw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9),
    axis.text.x = element_text(angle = 360, hjust = 0.5)
  )

# Yearly Total Sum of Gas Turns by Element Name and Floor - [Bar Chart]

# Prepare data
df_total <- df_percentage %>%
  mutate(
    Floor = factor(Floor, levels = c("2nd Floor", "3rd Floor", "4th Floor")),
    `Element Name` = factor(`Element Name`)
  ) %>%
  group_by(Floor, `Element Name`) %>%
  summarise(Total_Gas_Turns = sum(Gas_Turns, na.rm = TRUE), .groups = "drop") %>%
  complete(Floor, `Element Name`, fill = list(Total_Gas_Turns = 0))

View(df_total)

# Define Element Name colors 

element_name_colors <- c(
  "Liquid-Nitrogen" = "#1f77b4",          # blue
  "Carbon Dioxide" = "#ff7f0e",            # orange
  "Compressed Gas-Nitrogen" = "#2ca02c",  # green
  "Helium" = "#ffd700"                     #yellow 
)

# Plot
ggplot(df_total, aes(x = Floor, y = Total_Gas_Turns, fill = `Element Name`)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = round(Total_Gas_Turns, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = element_name_colors, guide = guide_legend(title.position = "top", title.hjust = 0.5)  # <-- centers the legend title
) +     # Legend title above items
  labs(title = "Yearly Total Gas Turns by Element Name and Floor",
       x = "Floor", y = "Total Gas Turns", fill = "Element Name") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_rect(fill = "grey90"),      # background color
        strip.text = element_text(face = "bold", color = "white"),
    panel.border = element_rect(color = "white", fill = NA, size = 1),
    panel.grid.major = element_line(color = "white", size = 0.4),  # x-axis grid.major
    panel.grid.minor = element_line(color = "white", size = 0.4),   # y-axis grid.major
    legend.position = "bottom",            # move legend to bottom
    legend.direction = "horizontal")       # make legend horizontal
)       

# Uniformity Correction - "Yearly Average Gas Turns by Element Name and Floor" (Numeric Value with Decimals)

# Prepare data
df_avg <- df_percentage %>%
  mutate(
    Floor = factor(Floor, levels = c("2nd Floor", "3rd Floor", "4th Floor")),
    `Element Name` = factor(`Element Name`)
  ) %>%
  group_by(Floor, `Element Name`) %>%
  summarise(avg_turns = mean(Gas_Turns, na.rm = TRUE), .groups = "drop") %>%
  complete(Floor, `Element Name`, fill = list(avg_turns = 0))  # fill missing combos

# Plot
ggplot(df_avg, aes(x = reorder(`Element Name`, -avg_turns), y = avg_turns, fill = Floor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = round(avg_turns, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  labs(title = "Yearly Average Gas Turns by Element Name and Floor",
       x = "Element Name", y = "Average Gas Turns", fill = "Floor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Customize Color Palette Modification - "Yearly Average Gas Turns by Element Name and Floor" 

# Prepare data
df_avg <- df_percentage %>%
  mutate(
    Floor = factor(Floor, levels = c("2nd Floor", "3rd Floor", "4th Floor")),
    `Element Name` = factor(`Element Name`)
  ) %>%
  group_by(Floor, `Element Name`) %>%
  summarise(avg_turns = mean(Gas_Turns, na.rm = TRUE), .groups = "drop") %>%
  complete(Floor, `Element Name`, fill = list(avg_turns = 0))

# Define floor colors
floor_colors <- c(
  "2nd Floor" = "#1f77b4",  # blue
  "3rd Floor" = "#ff7f0e",  # orange
  "4th Floor" = "#2ca02c"   # green
)

# Plot
ggplot(df_avg, aes(x = reorder(`Element Name`, -avg_turns), y = avg_turns, fill = Floor)) + # Rename the title in the bar otherwise R will insert "reorder(`Element Name`, -avg_turns)" as the label name". Use the (labs) function
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = round(avg_turns, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = floor_colors) +
  labs(title = "Yearly Average Gas Turns by Element Name and Floor",
       x = "Element Name", y = "Average Gas Turns", fill = "Floor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Percentages - "Yearly Average Gas Turns by Element Name and Floor" 

 # You'll need scales for (%) formatting

# Prepare data
df_avg <- df_percentage %>%
  mutate(
    Floor = factor(Floor, levels = c("2nd Floor", "3rd Floor", "4th Floor")),
    `Element Name` = factor(`Element Name`)
  ) %>%
  group_by(Floor, `Element Name`) %>%
  summarise(avg_turns = mean(Gas_Turns, na.rm = TRUE), .groups = "drop") %>%
  complete(Floor, `Element Name`, fill = list(avg_turns = 0))

# Define floor colors
floor_colors <- c(
  "2nd Floor" = "#1f77b4",
  "3rd Floor" = "#ff7f0e",
  "4th Floor" = "#2ca02c"
)

# Plot with percentages
ggplot(df_avg, aes(x = reorder(`Element Name`, -avg_turns), y = avg_turns, fill = Floor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = scales::percent(avg_turns, accuracy = 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # y-axis as percentage
  scale_fill_manual(values = floor_colors) +
  labs(title = "Yearly Average Gas Turns by Element Name and Floor", x = "Element Names",
       y = "Average Gas Turns")


# Line Graph - Total Gas Turns per Month by Element - Revise _ (Checkpoint)

df_percentage <- df_running_total %>%
  mutate(month = floor_date(Date, "month")) %>%  # Get month from Dates
  group_by(Element Name, month) %>%
  summarise(
    total_turns = sum(GasTurns, na.rm = TRUE),
    count_turns = n(),
    .groups = "drop"
  )

# Plot total turns per month by element_name - Linear Line Graph

# Summarize by month + element
df_monthly <- df_percentage %>%
  mutate(Month = floor_date(Date, "month")) %>%  # round dates down to the first day of the month
  group_by(Month, `Element Name`) %>%
  summarise(Total_Gas_Turns = sum(Gas_Turns, na.rm = TRUE), .groups = "drop")

# Plot monthly totals
ggplot(df_monthly, aes(x = Month, y = Total_Gas_Turns, color = `Element Name`)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Monthly Total Gas Turns by Element Name",
    x = "Month",
    y = "Total Gas Turns",
    color = "Element Name"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot total turns per Year by Element Name [Stacked Bar Chart]

# Applying (dplyr) function to group my data by year and count the occurrences

df_yearly <- df_percentage %>%
  mutate(Year = format(as.Date(Date), "%Y")) %>%
  group_by(Year, 'Element Name') %>%
  summarise(turns = n(), .groups = "drop")

# To group my data by "Years" and "Count" the repeated occurrences -  [Final Results] - This will produce a stacked bar chart showing the total number of turns per year, broken down by element_name.

# Summarize yearly totals
df_yearly <- df_percentage %>%
  mutate(Year = format(as.Date(Date), "%Y")) %>%
  group_by(Year, `Element Name`) %>%
  summarise(turns = sum(Gas_Turns, na.rm = TRUE), .groups = "drop")

# Plot yearly totals
ggplot(df_yearly, aes(x = Year, y = turns, fill = `Element Name`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Number of Turns",
       title = "Total Turns per Year by Element Name",
       fill = "Element Name") +
  theme_minimal()

# Or different [Stacked Bar Chart] - (fix)

ggplot(df_yearly, aes(x = Gas_Turns, y = Year, fill = 'Element Name')) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Number of Turns", y = "Year", title = "Histogram of Turns per Year by Element Name") +
  theme_minimal()

# Stacked Bar Chart by [Month] Horizontal *

df_monthly <- df_running_total %>%
  mutate(Month = format(as.Date(Date), "%Y-%m")) %>%
  group_by(Month, 'Element Name') %>%
  summarise(turns = n(), .groups = "drop")

ggplot(df_monthly, aes(x = turns, y = Month, fill = "Elements")) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Total Number of Turns", y = "Month", title = "Stacked Bar Chart of Turns per Month by Element Name") +
  theme_minimal()

# Linear Plot Chart by Year * ----

# Note "df_yearly" was already previously created (fix)

ggplot(df_yearly, aes(x = Year, y = turns, color = "Element Name", group = "Element Name")) +
  geom_line() +
  labs(y = "Number of Turns", title = "Total Turns per Year by Element Name") +
  theme_minimal()

# Stacked Bar Chart - Monthly Turns of Element Names

df_monthly <- df_running_total %>%
  mutate(
    Month = format(as.Date(Date), "%Y-%m"),
    rank = factor(
      format(as.Date(Date), "%m"),       # take numeric month
      levels = sprintf("%02d", 1:12),    # order 01–12
      labels = month.name,               # map to month names
      ordered = TRUE
    )
  ) %>%
  group_by(Month, `Element Name`, rank) %>%
  summarise(turns = n(), .groups = "drop")

ggplot(df_percentage, aes(x = Date, group = `Element Name`, fill = `Element Name`)) +
  geom_histogram(color="black", binwidth=3, alpha=0.8,
                 position="identity") +
  theme_bw() +
  xlab("Element Turns By Month") +
  ylab("Number of Turns") +
  scale_x_date(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b %y")
  )

# Histogram Chart - aiming to create a multi-faceted histogram

df_percentage <- df_percentage %>%
  dplyr::rename(Element_Name = `Element Name`)

ggplot(df_percentage, aes(x = Date, fill = Element_Name)) +
  geom_histogram(color = "darkgrey", binwidth = 8, alpha = 0.8, position = "identity") +
  facet_wrap(~Element_Name) +
  theme_bw() +
  xlab("Date of Gas Turns") +
  ylab("Number of Turns") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
##  Checkpoint

    
    
    ggplot(df_monthly_turns, aes(x = month, fill = element_name)) +
    geom_histogram(
      color = "black",
      bins = 20,
      alpha = 0.8,
      position = "identity"
    ) +
    facet_grid(. ~ element_name) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
    theme_bw() +
    xlab("Date of Gas Turns") +
    ylab("Number of Turns") +
    

#  Histogram [Works]
    
    ggplot(df_monthly_turns, aes(x = Date, fill = element_name)) +
      geom_histogram(color = "black", bins = 30, binwidth = 10, alpha = 0.8, position = "stack") + # No need to use "stat_bin" function with geom_histogram. It would be redundant in the code 
      facet_grid(~ element_name) +                                                                 # binwidth widens the bars
      theme_bw() +
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
      
    
    # Histogram Facets - [Side-By-Side] 
    
    ggplot(df_running_total, aes(x = Date, fill = element_name)) +
      geom_histogram(color = "black", binwidth = 30, alpha = 0.8, position = "stack") +
      facet_grid(~ element_name) +
      theme_bw() +
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 6, face = "bold"),     # bigger text inside box
        strip.background = element_rect(fill = "grey",      # background color
                                        color = "black", 
                                        size = 1.0)    
      )
    
#  Histogram Facets -[Works] Contd. [Vertical Charts] *
    
    ggplot(df_running_total, aes(x = Date, fill = element_name)) +
      geom_histogram(color = "black", binwidth = 10, alpha = 0.8, position = "stack") +
      facet_grid(~ element_name) +
      coord_flip() +         # This will turn the vertical bar chart into a horizontal chart.
      theme_bw() +
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 6, face = "bold"),     # bigger text inside box [For the four facet title boxes]
        strip.background = element_rect(fill = "lightgrey",      # background color
                                        size = 1.0)              # thickness of box border
      )
      
    
#   Histogram Facets - [4 Point Quadrant]
    
    ggplot(df_running_total, aes(x = Date, fill = element_name)) +
      geom_histogram(color = "black", binwidth = 10, alpha = 0.8, position = "stack") +
      facet_wrap(~ element_name) +
      coord_flip() +         # This will turn the vertical bar chart into a horizontal chart.
      theme_bw() +
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 6, face = "bold"),     # bigger text inside box [For the four facet title boxes]
        strip.background = element_rect(fill = "lightgrey",      # background color
                                        size = 1.0)              # thickness of box border
      )
    
    
#   Histogram Facet Horizontal

    ggplot(df_running_total, aes(x = Date, fill = element_name)) +
      geom_histogram(color = "black", binwidth = 10, alpha = 0.8, position = "stack") +
      facet_grid(element_name~., scales = "free" ) + # You can add [scales = "free"] into the facet_grid function to change from vert to horiz. Remember, element name on the left of (~) is horizontal, element name on the right is vertical
      coord_flip() +         # This will turn the vertical bar chart into a horizontal chart.
      theme_bw() +          # "face_wrap is four point quadrants diagrams
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 5, face = "bold"),     # bigger text inside box [For the four facet title boxes]
        strip.background = element_rect(fill = "lightgrey",      # background color
                                        size = 1.0),             # thickness of box border
        strip.text.y = element_text(angle = -90)                   # Rotate facet labels to horizontal
      )
    
    # Histogram - Elements separated into 4 different panels cascading down [Done]
    
    ggplot(df_percentage, aes(x = Date, fill = Element_Name)) +
      geom_histogram(color = "grey", binwidth = 9, alpha = 0.8, position = "stack") +
      facet_grid(Element_Name ~ ., scales = "free") +
      theme_bw() +
      xlab("Month") +
      ylab("Number of Turns") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 5, face = "bold"),
        strip.background = element_rect(fill = "lightgrey", size = 0.1),
        strip.text.y = element_text(angle = -90),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
      
      # Histogram Stacked bar chart (All 4 Elements) - [Done]
      
    df_running_total_Rev <- df_running_total %>%
      dplyr::rename(Element_Name = `Element Name`)
    
    ggplot(df_running_total, aes(x = Date, fill = Element_Name)) +
      geom_histogram(color = "grey", binwidth = 9, alpha = 0.8, position = "stack") +
      labs(x = "Month", y = "Running Turn Count", fill = "Element Names") +
      facet_grid(Element_Name ~ ., scales = "free") +
      scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
      theme(
        strip.text = element_text(size = 5, face = "bold"),
        strip.background = element_rect(fill = "lightgrey", size = 0.1),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
     
      # Histogram Stacked bar chart (All 4 Elements) - Code cleaned and polished [Done]
      
    ggplot(df_running_total, aes(x = Date, fill = Element_Name)) +
      geom_histogram(color = "grey", binwidth = 9, position = "stack") +
      labs(title = "Monthly Turn Counts By Element Names",
           x = "Month",
           y = "Turn Count",
           fill = "Element Names") +
      scale_x_date(date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))
      