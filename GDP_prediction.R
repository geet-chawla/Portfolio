# Author: Geet Chawla
# Date: 02-24-2025
# Email: geetchawla94@gmail.com

#Housekeeping
rm(list = ls())

#Setting working directory
getwd()
setwd("/Users/geetchawla/Desktop/Project/Quarterly")

#Loading librarires
library(ggplot2) 
library(tidyr)    
library(lmtest)   
library(tidyverse) 
library(car)
library(dplyr)
library(lubridate)
library(scales)

############ Preparing function to convert yearly Tariff data to Quarterly ##################

# Function to interpolate quarterly values between consecutive years
interpolate_quarters <- function(start_year, start_value, end_value) {
  # Calculate quarterly increments
  increments <- seq(start_value, end_value, length.out = 5)[1:4]  # 5 points: Q1 to next year's Q1
  
  data.frame(
    year = rep(start_year,4),
    quarter = paste0("Q", 1:4),
    value = increments
  )
}

#Read Tariff data
tariff_data = read_csv("****/tariff_normal.csv") %>% 
  select(1,3) %>% 
  rename(year = 1, value = 2) %>% 
  filter(year>= 1960 & year < 2025) 

# Apply interpolation for all years except the last one
quarterly_data <- purrr::map2_df(
  tariff_data$year[-length(tariff_data$year)],
  seq_along(tariff_data$value[-length(tariff_data$value)]),
  ~ interpolate_quarters(.x, tariff_data$value[.y], tariff_data$value[.y + 1])
)

last_year_data <- data.frame(
  year = tail(tariff_data$year, 1),
  quarter = "Q1",
  value = tail(tariff_data$value, 1)
)

quarterly_tariff_data <- bind_rows(quarterly_data, last_year_data) %>%
  arrange(year, quarter) %>% 
  mutate(
    date = as.Date(paste0(
      year, "-", 
      case_when(
        quarter == "Q1" ~ "01-01",
        quarter == "Q2" ~ "04-01",
        quarter == "Q3" ~ "07-01",
        quarter == "Q4" ~ "10-01"
      )
    ))
  ) %>% 
  select(observation_date = date, tariff = value) %>% 
  rbind(.,data.frame(
    observation_date = c(as.Date("2024-4-01"), as.Date("2024-7-01"), as.Date("2024-10-01")),
    tariff = c(2.4,2.4,2.4)
    )) %>% 
  #Export clean tariff data to folder with all other clean files
  write_csv(., "****clean_folder/tariff.csv")


############ Importing, Merging, and Preparing data for analyses ##################


# List all CSV files in a clean directory
files <- list.files(path = "****", pattern = "\\.csv$", full.names = TRUE)

# Import all CSV files and rename the second column to the file name
data_list <- lapply(files, function(file) {
  data <- read_csv(file)  # Read the CSV file
  file_name <- tools::file_path_sans_ext(basename(file))  # Get file name without extension
  colnames(data)[2] <- file_name  # Rename second column to file name
  return(data)
})

# Merge all data frames by a common key 
clean_data <- Reduce(function(x, y) merge(x, y, by = "observation_date", all = TRUE), data_list) %>% 
  rename(date = 1) %>%
  rename_with(~ gsub("_absolute", "", .)) %>% 
  filter(date >= "1975-01-01" & date < "2025-01-01")

prediction_data = read_csv("****/prediction.csv")

merged_data = bind_rows(clean_data, prediction_data)  %>% 
  filter(date >= "1975-01-01")

############ Running the GDP model on date from 1975 - 2020 ##################

#Regression trial
gdp_model <- lm(log(gdp) ~ log(govt_spend) + log(investment) + log(tariff), data = merged_data %>% filter(date < "2020-01-01"))

# # View regression summary
summary(gdp_model)


############ Checking the GDP model on date from 2020 - 2024 ##################
predicted_gdp <- exp(
   predict(gdp_model, newdata = merged_data %>%
                                filter(date >= "2020-01-01" & date < "2025-01-01") %>%
                               select(investment, govt_spend, tariff))
 )

actual_gdp = clean_data$gdp[clean_data$date >= "2020-01-01"]

corr = lm(actual_gdp ~ predicted_gdp
          )
summary(corr)

# Finding: R squared on 0.9 indicates model explains 90% ov variation (good result)

############ Running the GDP model on date from 1975 - 2024 ##################

gdp_model <- lm(log(gdp) ~ log(govt_spend) + log(investment) + log(tariff), data = merged_data %>% filter(date < "2025-01-01"))

# View regression summary
summary(gdp_model)

############ Using model coefficients to predict GDP for 2025 - 2026  with increased tariffs ##################

predicted_gdp <- exp(
  predict(gdp_model, newdata = merged_data %>% 
            filter(date >= "2025-01-01") %>% 
            select(investment, govt_spend, tariff))
)

start_index <- which(merged_data$date == "2025-01-01")

merged_data$gdp[start_index:(start_index + length(predicted_gdp) - 1)] <- predicted_gdp

############ Importing data prepared under business-as-usual for 2025 - 2026 ##################

bau_data = read_csv("****bau.csv")
bau_data = bind_rows(clean_data, bau_data) %>% 
  rename(gdp_bau = gdp)

############ Using model coefficients to predict GDP for 2025 - 2026 with normal tariffs ##################

predicted_gdp_normal <- exp(
  predict(gdp_model, newdata = bau_data %>% 
            filter(date >= "2025-01-01") %>% 
            select(investment, govt_spend, tariff))
)

start_index <- which(bau_data$date == "2025-01-01")

bau_data$gdp_bau[start_index:(start_index + length(predicted_gdp_normal) - 1)] <- predicted_gdp_normal

############ Plotting the estimated GDO under both scenarios ##################

plot_data_filtered = merge(merged_data %>% 
                    select(date,gdp),
                  bau_data %>% 
                    select(date, gdp_bau), all=TRUE) %>% 
  mutate(quarter = paste0(year(date), " Q", quarter(date))) %>% 
  select(date, quarter, gdp, gdp_bau) %>%
  filter(date >= as.Date("2022-01-01")) %>%
  mutate(quarter = factor(quarter, levels = unique(quarter), ordered = TRUE))


label_data <- plot_data_filtered %>%
  filter(date %in% as.Date(c("2024-10-01", "2025-10-01", "2026-10-01"))) %>%
  pivot_longer(cols = c(gdp_bau, gdp), names_to = "scenario", values_to = "value") %>%
  mutate(
    scenario = case_when(
      scenario == "gdp_bau" ~ "GDP (Business as usual)",
      scenario == "gdp" ~ "GDP (under Tariffs in 2025)",
      TRUE ~ scenario
    )
  )

ggplot(plot_data_filtered, aes(x = quarter)) +
  # Business as usual line (blue)
  geom_line(aes(y = gdp_bau / 1000, color = "GDP (Business as usual)", group = 1), size = 1) +
  # Tariffs scenario line (red)
  geom_line(aes(y = gdp / 1000, color = "GDP (under Tariffs in 2025)", group = 1), size = 1) +
  # Data labels
  geom_text(
    data = label_data,
    aes(y = value / 1000, label = round(value / 1000, 2), color = scenario),
    vjust = -0.5, size = 3.5, fontface = "bold"
  ) +
  # Vertical dotted line at 2025-01-01
  geom_vline(xintercept = which(plot_data_filtered$quarter == "2025 Q1"),
             linetype = "dotted", color = "black", size = 1) +
  # Labels and title
  labs(
    title = "Estimated GDP with Tariffs in 2025",
    x = "Year / Quarter",
    y = "GDP (in $Tn)",
    color = "Scenario"
  ) +
  # Styling
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(
    "GDP (Business as usual)" = "blue",
    "GDP (under Tariffs in 2025)" = "red"
  ))

############ End ##################
