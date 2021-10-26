# Set working directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets/Agriculture Data Analysis")

# View items in the directory
dir()

# Load in the necessary libraries
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(data.table)
library(stringr)
library(ggplot2)
library(gganimate)

# Read the datafile into the workspace
data_file <- read.table("ObservationData_lueqin.csv", sep = ",", skip = 13, 
                        col.names = c("State", "Indicators", "Unit", "Year", "Value"))

# Drop the Unit Column
data_file$Unit <- NULL

# Subset the required rows
new_data <- data_file %>% filter(Indicators %in% c(str_subset(data_file$Indicators, pattern = c("Rice", "Millet", 
                                                                                                "Cassava", "Employment", 
                                                                                                "Ground Nut", "Guinea Corn", 
                                                                                                "fertilizer"))))

# Categorize the states into the political regions
new_data$Region <- ifelse(new_data$State %in% c("Lagos", "Osun", "Ogun", "Ondo", "Ekiti", "Oyo"), "South-West", 
                          ifelse(new_data$State %in% c("Imo", "Enugu", "Anambra", "Abia", "Ebonyi"), "South-East", 
                                 ifelse(new_data$State %in% c("Abuja", "Benue", "Nassarawa", "Kogi", "Kwara", "Niger"), "North-Central", 
                                        ifelse(new_data$State %in% c("Katsina", "Kano", "Kaduna", "Sokoto", "Kebbi", "Jigawa", "Zamfara"), "North-West", 
                                               ifelse(new_data$State %in% c("Akwa Ibom", "Delta", "Cross River", "Rivers", "Edo"), "South-South","North-East")))))

# Insert a new column bearing the country name
new_data$Country <- "Nigeria"

# Drop the State Column
new_data$State <- NULL

# Rearrange the table to include only the variables we need
new_data <- new_data %>% select(Indicators, Year, Value, Region) %>% group_by(Indicators, Year, Region) %>% summarise_at(vars(Value), sum)

# Collapse the table into a smaller size
new_concise_data <- new_data %>% select(Indicators, Year, Value) %>% group_by(Indicators, Year) %>% summarise_at(vars(Value), sum)

# Visualize the relationship in our data
  # Trend of the production of each crop over time
  new_concise_data %>% filter(Indicators %in% str_subset(new_data$Indicators, pattern = "Production")) %>% group_by(Indicators) %>% 
    ggplot(aes(x = Year, y = Value, color = Indicators, group = Indicators)) + geom_point() + geom_line() + 
    labs(y = "Production of Crops, 000'Metric Tons", title = "Trend of Crop Production from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15) + scale_color_manual(values = c("Red", "Blue", "Green", "Yellow", "Purple"), 
                       labels = c("Cassava", "ground Nut", "Guinea Corn", "Millet", "Rice"))
  
  # Trend of the increase/decrease in area of land cultivated for each crop
  new_concise_data %>% filter(Indicators %in% str_subset(new_concise_data$Indicators, 
                                                         pattern = "Area")) %>% 
    group_by(Indicators) %>% ggplot(aes(x = Year, y = Value, color = Indicators, group = Indicators)) + 
    geom_point() + geom_line() + labs(y = "Area of Land Cultivated, 000'hectare", title = "Trend of the Area of Land Cultivated for each Crop from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15) + scale_color_manual(values = c("Red", "Blue", "Green", "Yellow", "Purple"), 
                                                           labels = c("Cassava", "ground Nut", "Guinea Corn", "Millet", "Rice"))

  # Trend of the increase/decrease of costs of seeds for each crop over time
  new_concise_data %>% filter(Indicators %in% str_subset(new_concise_data$Indicators, pattern = "Cost")) %>% 
    group_by(Indicators) %>% ggplot(aes(x = Year, y = Value, color = Indicators, group = Indicators)) + 
    geom_line() + geom_point() + labs(y = "Cost of Seeds, Million Naira", title = "Trend of the Cost of Seeds for each Crop from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15) + scale_color_manual(values = c("Red", "Blue", "Green", "Yellow", "Purple"), 
                                                           labels = c("Cassava", "ground Nut", "Guinea Corn", "Millet", "Rice"))
  # Trend of the increase/decrease of costs of fertilizer over time
  new_concise_data %>% filter(Indicators %in% str_subset(new_concise_data$Indicators, pattern = "Cost of fertilizer")) %>% 
    group_by(Indicators) %>% ggplot(aes(x = Year, y = Value)) + 
    geom_line() + geom_point() + labs(y = "Cost of Fertilizer, Million Naira", title = "Trend of the Cost of the Cost of Fertilizer from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15)
  
  # Trend of Employment across the sector
  new_concise_data %>% filter(Indicators %in% str_subset(new_concise_data$Indicators, pattern = "Employment")) %>% 
    group_by(Indicators) %>% ggplot(aes(x = Year, y = Value, color = Indicators, group = Indicators)) + 
    geom_point() + geom_line() + labs(y = "Number of People Employed, 000", title = "Trend of the Employment Rates for each Crop from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15) + scale_color_manual(values = c("Red", "Blue", "Green", "Yellow", "Purple"), 
                                                           labels = c("Paid Farmers:Female", "Paid Farmers:Male", "Paid Employee:Male", "Unpaid Family:Female", "Unpaid Farmers:Male"))

  # Trend of farmgate prices for all crops
  new_concise_data %>% filter(Indicators %in% str_subset(new_concise_data$Indicators, pattern = "Farmgate")) %>% 
    group_by(Indicators) %>% ggplot(aes(x = Year, y = Value, color = Indicators, group = Indicators)) + 
    geom_point() + geom_line() + labs(y = "Farmgate Prices, Naira Per Kg", title = "Trend of the Farmgage Prices for each Crop from 1995 through 2006", color = "Crop") + 
    scale_x_continuous(n.breaks = 15) + scale_color_manual(values = c("Red", "Blue", "Green", "Yellow", "Purple"), 
                                                           labels = c("Cassava", "ground Nut", "Guinea Corn", "Millet", "Rice"))

  # Comparison of the annual production output of each region  
  new_data %>% filter(Indicators %in% str_subset(new_data$Indicators, pattern = "Production")) %>% group_by(Indicators) %>% 
    ggplot(aes(x = Year, y = Value, fill = Region)) + geom_col(position = "dodge") + 
    scale_x_continuous(n.breaks = 15) + labs(y = "Production Output, 000'Metric Tons", title = "Production Output Per Region")
  
  # Comparison of the annual land cultivation of each region
  new_data %>% filter(Indicators %in% str_subset(new_data$Indicators, pattern = "Area")) %>% group_by(Indicators) %>% 
    ggplot(aes(x = Year, y = Value, fill = Region)) + geom_col(position = "dodge") + 
    scale_x_continuous(n.breaks = 15) + labs(y = "Area of Land Cultivated, 000'hectares", title = "Area of Land Cultivated Per Region")
  