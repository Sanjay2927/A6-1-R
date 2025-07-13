---
title: "A6_V01151304"
author: "sanjay"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{1}
#-------------------------------#
# Step 1: Load and inspect data ----
#-------------------------------#

# Set working directory to the location of your dataset
setwd('E:\\A6 ASSIGNMENT VCU') 


# Read the NSSO68 dataset
df <- read.csv('NSSO68 (2).csv')

# Inspect column names
names(df)

# Check the 'foodtotal_v' column
head(df$foodtotal_v)


#----------------------------------------#
# Step 2: Filter the data for Karnataka ----
#----------------------------------------#

# Check how many Tripura rows are there
sum(df$state_1 == 'TRPR')

# Filter rows where state_1 is 'TRPR'
TRPR <- df[df$state_1 == 'TRPR', ]

dim(TRPR)

# Histogram of food consumption in Tripura
hist(TRPR$foodtotal_v, 
     main = "Distribution of Food Consumption in Tripura",
     xlab = "Food Consumption (foodtotal_v)", 
     col = "lightblue", 
     border = "white")


#----------------------------------------------#
# Step 3: Group-wise summary at District level ----
#----------------------------------------------#

# Convert District column to factor (if not already)
TRPR$District <- as.factor(TRPR$District)

# Load dplyr for grouping
library(dplyr)

# Add district-wise average food consumption column
TRPR <- TRPR %>%
  group_by(District) %>%
  mutate(DWCons = mean(foodtotal_v, na.rm = TRUE)) %>%
  ungroup()


#--------------------------------------------------------#
# Step 4: Create mapping of District Codes to Names ----
#--------------------------------------------------------#

district_map <- data.frame(
  DistrictCode = sprintf("%02d", 1:4), # Format as 01, 02, ..., 29
  DistrictName = c("West Tripura","South Tripura","Dhalai","North Tripura"),
  stringsAsFactors = FALSE
)

#---------------------------------------------------------#
# Step 5: Merge mapping into main data using District code ----
#---------------------------------------------------------#

# Create a DistrictCode column from District number
TRPR <- TRPR %>%
  mutate(DistrictCode = sprintf("%02d", as.numeric(District)))  # Converts 1 to '01', etc.

# Merge to get District names
TRPR <- TRPR %>%
  left_join(district_map, by = "DistrictCode")


# Step 6: Summarize and Plot Bar Chart ----
# Create summary table: average food consumption by district
district_avg <- TRPR %>%
  group_by(DistrictName) %>%
  summarise(avg_food = mean(foodtotal_v, na.rm = TRUE)) %>%
  arrange(desc(avg_food))  # Sort by consumption

# Barplot: average food consumption by district
barplot(height = district_avg$avg_food,
        names.arg = district_avg$DistrictName,
        las = 2,                 # Rotate x-axis labels vertically
        col = "skyblue",        
        main = "Average Food Consumption by District (Tripura)",
        ylab = "Avgerage Food Consumption (Rs.)",
        cex.names = 0.7)        # Adjust label size if too crowded

```
```{2}
# Choropleth Maps
# Plot data on the map itself

# a variable of our choice
# geojson file or the shapefile 
install.packages("sf")

library(ggplot2) 
library(sf) # mapping
library(dplyr) 

#Sys.setenv("SHAPE_RESTORE_SHX" = "YES")

data_map <- st_read("Tripura.geojson") 
View(data_map)


# Step 1: Ensure district name column matches in both datasets
data_map <- data_map %>%
  rename(DistrictName = Dist_Name)

# Step 2: Left join spatial data with data values
data_map_data <- data_map %>%
  left_join(district_avg, by = "DistrictName")  # Keeps all districts

# Step 3: Replace NA with 0 for missing data
data_map_data$avg_food[is.na(data_map_data$avg_food)] <- 0

# Step 4: Plot using ggplot2
ggplot(data_map_data) + 
  geom_sf(aes(fill = avg_food, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Average Food Consumption by District") +
  theme_minimal() +
  geom_sf_text(aes(label = DistrictName), size = 3, color = "black")
```
