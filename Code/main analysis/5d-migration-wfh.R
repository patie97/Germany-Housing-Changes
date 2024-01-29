rm(list = ls())
setwd("")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
df4 <- read.csv("datasets/Intermediate/price-indexes-v11.csv")

top10 <- c("München", "Frankfurt", "Berlin", "Freiburg", "Heidelberg",
           "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")

top10c <- c("München", "Frankfurt am Main", "Berlin", "Freiburg im Breisgau", "Heidelberg",
           "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")

# immigrated, migrated, migration_balance

df5 <- df4[df4$erg_amd %in% top10, ]


df5$ejahr <- as.factor(df5$ejahr)
df5$migration_balance <- as.numeric(df5$migration_balance)
df5$immigrated <- as.numeric(df5$immigrated)
df5$log_mc_distance <- log(1 + df5$mc_distance)

df7_subset <- df5[df5$ejahr %in% c(2019, 2020, 2021), ]

# Fit linear regression models for each year
model_2019 <- lm(migration_balance ~ wfh_tot, data = df7_subset[df7_subset$ejahr == 2019, ])
model_2020 <- lm(migration_balance ~ wfh_tot, data = df7_subset[df7_subset$ejahr == 2020, ])
model_2021 <- lm(migration_balance ~ wfh_tot, data = df7_subset[df7_subset$ejahr == 2021, ])

ggplot(df7_subset, aes(x = wfh_tot, y = migration_balance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue", formula = y ~ x) +
  facet_wrap(~ejahr) +
  labs(title = "Impact of WFH on Migration Balance",
       x = "WFH Percentage",
       y = "Migration Balance")


selected_years <- c(2019, 2020, 2021)
df7_subset <- df5[df5$ejahr %in% selected_years, ]

# Fit linear regression model for each year
models <- lapply(selected_years, function(year) {
  subset_data <- df7_subset[df7_subset$ejahr == year, ]
  
  # Check for missing or non-numeric values
  if(all(!is.na(subset_data$log_mc_distance) & !is.na(subset_data$migration_balance))) {
    lm_model <- lm(migration_balance ~ log_mc_distance, data = subset_data)
    return(list(year = year, model = lm_model))
  } else {
    return(NULL)
  }
})

# Remove NULL entries from the list
models <- models[!sapply(models, is.null)]

# Create a list of data frames containing predicted values and confidence intervals
predictions <- lapply(models, function(model_info) {
  year <- model_info$year
  lm_model <- model_info$model
  new_data <- data.frame(log_mc_distance = seq(min(df7_subset$log_mc_distance), max(df7_subset$log_mc_distance), length.out = 100))
  predicted_values <- predict(lm_model, newdata = new_data, interval = "confidence", level = 0.95)
  predictions_df <- data.frame(log_mc_distance = new_data$log_mc_distance, 
                               migration_balance = predicted_values[, 1], 
                               ymin = predicted_values[, 2],
                               ymax = predicted_values[, 3],
                               year = year)
  return(predictions_df)
})

# Combine predictions into a single data frame
predictions_df <- do.call(rbind, predictions)

# Plot the results
plot1 <- ggplot(predictions_df, aes(x = log_mc_distance, y = migration_balance, color = factor(year))) +
  geom_line() +
  labs(title = "All age groups",
       x = "Log (1 + Distance to center)",
       y = "Migration",
       color = "Year") +  # Rename the legend title
  theme_minimal() + 
  guides(color = FALSE)


#### Movements w.r.t. age

df10 <- read.csv("datasets/Source/migration-age-2019.csv")
df10 <- df10 %>%
  select(kid2019, age_group, Immigrated, Migrated, ejahr)
df11 <- read.csv("datasets/Source/migration-age-2020.csv")
df11 <- df11 %>%
  select(kid2019, age_group, Immigrated, Migrated, ejahr)
df12 <- read.csv("datasets/Source/migration-age-2021.csv")
df12 <- df12 %>%
  select(kid2019, age_group, Immigrated, Migrated, ejahr)


df10 <- rbind(df10, df11, df12)

df10 <- df10 %>%
  mutate(kid2019 = str_sub(kid2019, start = ifelse(str_sub(kid2019, 1, 1) == "0", 2, 1)))

df10 <- df10 %>%
  filter(nchar(kid2019) %in% c(4,5))

df11 <- df5 %>%
  select(log_mc_distance, kid2019, wfh_tot)
df12 <- merge(df10, df11, by = "kid2019", all.y = TRUE)

df12$Immigrated <- as.numeric(df12$Immigrated)
df12$Migrated <- as.numeric(df12$Migrated)
df12$Migration_balance_age <- df12$Immigrated - df12$Migrated

### young age group

unique(df12$age_group)
young <- c("18 bis unter 25 Jahre")
young_df <- df12[df12$age_group %in% young, ]

selected_years <- c(2019, 2020, 2021)
df7_subset <- young_df[young_df$ejahr %in% selected_years, ]

# Fit linear regression model for each year
models <- lapply(selected_years, function(year) {
  subset_data <- df7_subset[df7_subset$ejahr == year, ]
  
  # Check for missing or non-numeric values
  if(all(!is.na(subset_data$mc_distance) & !is.na(subset_data$Migration_balance_age))) {
    lm_model <- lm(Migration_balance_age ~ log_mc_distance, data = subset_data)
    return(list(year = year, model = lm_model))
  } else {
    return(NULL)
  }
})

# Remove NULL entries from the list
models <- models[!sapply(models, is.null)]

# Create a list of data frames containing predicted values and confidence intervals
predictions <- lapply(models, function(model_info) {
  year <- model_info$year
  lm_model <- model_info$model
  new_data <- data.frame(log_mc_distance = seq(min(df7_subset$log_mc_distance), max(df7_subset$log_mc_distance), length.out = 100))
  predicted_values <- predict(lm_model, newdata = new_data, interval = "confidence", level = 0.95)
  predictions_df <- data.frame(log_mc_distance = new_data$log_mc_distance, 
                               Migration_balance_age = predicted_values[, 1], 
                               ymin = predicted_values[, 2],
                               ymax = predicted_values[, 3],
                               year = year)
  return(predictions_df)
})

# Combine predictions into a single data frame
predictions_df <- do.call(rbind, predictions)

# Plot the results
plot2 <- ggplot(predictions_df, aes(x = log_mc_distance, y = Migration_balance_age, color = factor(year))) +
  geom_line() +
  labs(title = "Age 18 - 25",
       x = "Log (1 + Distance to center)",
       y = "Migration Balance",
       color = "Year") +  # Rename the legend title
  theme_minimal() + 
  guides(color = FALSE)


# middle age group
mid <- c("30 bis unter 50 Jahre")
mid_df <- df12[df12$age_group %in% mid, ]



selected_years <- c(2019, 2020, 2021)
df7_subset <- mid_df[mid_df$ejahr %in% selected_years, ]

# Fit linear regression model for each year
models <- lapply(selected_years, function(year) {
  subset_data <- df7_subset[df7_subset$ejahr == year, ]
  
  # Check for missing or non-numeric values
  if(all(!is.na(subset_data$mc_distance) & !is.na(subset_data$Migration_balance_age))) {
    lm_model <- lm(Migration_balance_age ~ log_mc_distance, data = subset_data)
    return(list(year = year, model = lm_model))
  } else {
    return(NULL)
  }
})

# Remove NULL entries from the list
models <- models[!sapply(models, is.null)]

# Create a list of data frames containing predicted values and confidence intervals
predictions <- lapply(models, function(model_info) {
  year <- model_info$year
  lm_model <- model_info$model
  new_data <- data.frame(log_mc_distance = seq(min(df7_subset$log_mc_distance), max(df7_subset$log_mc_distance), length.out = 100))
  predicted_values <- predict(lm_model, newdata = new_data, interval = "confidence", level = 0.95)
  predictions_df <- data.frame(log_mc_distance = new_data$log_mc_distance, 
                               Migration_balance_age = predicted_values[, 1], 
                               ymin = predicted_values[, 2],
                               ymax = predicted_values[, 3],
                               year = year)
  return(predictions_df)
})

# Combine predictions into a single data frame
predictions_df <- do.call(rbind, predictions)

# Plot the results
plot3 <- ggplot(predictions_df, aes(x = log_mc_distance, y = Migration_balance_age, color = factor(year))) +
  geom_line() +
  labs(title = "Age 30 - 50",
       x = "Log (1 + Distance to center)",
       y = "Migration Balance",
       color = "Year") +  # Rename the legend title
  theme_minimal() + 
  guides(color = FALSE)

# older

old <- c("50 bis unter 65 Jahre", "65 Jahre und mehr")
old_df <- df12[df12$age_group %in% old, ]



selected_years <- c(2019, 2020, 2021)
df7_subset <- old_df[old_df$ejahr %in% selected_years, ]

# Fit linear regression model for each year
models <- lapply(selected_years, function(year) {
  subset_data <- df7_subset[df7_subset$ejahr == year, ]
  
  # Check for missing or non-numeric values
  if(all(!is.na(subset_data$mc_distance) & !is.na(subset_data$Migration_balance_age))) {
    lm_model <- lm(Migration_balance_age ~ log_mc_distance, data = subset_data)
    return(list(year = year, model = lm_model))
  } else {
    return(NULL)
  }
})

# Remove NULL entries from the list
models <- models[!sapply(models, is.null)]

# Create a list of data frames containing predicted values and confidence intervals
predictions <- lapply(models, function(model_info) {
  year <- model_info$year
  lm_model <- model_info$model
  new_data <- data.frame(log_mc_distance = seq(min(df7_subset$log_mc_distance), max(df7_subset$log_mc_distance), length.out = 100))
  predicted_values <- predict(lm_model, newdata = new_data, interval = "confidence", level = 0.95)
  predictions_df <- data.frame(log_mc_distance = new_data$log_mc_distance, 
                               Migration_balance_age = predicted_values[, 1], 
                               ymin = predicted_values[, 2],
                               ymax = predicted_values[, 3],
                               year = year)
  return(predictions_df)
})

# Combine predictions into a single data frame
predictions_df <- do.call(rbind, predictions)

# Plot the results
plot4 <- ggplot(predictions_df, aes(x = log_mc_distance, y = Migration_balance_age, color = factor(year))) +
  geom_line() +
  labs(title = "Age > 50",
       x = "Log (1 + Distance to center)",
       y = "Migration Balance",
       color = "Year") +  # Rename the legend title
  theme_minimal() + 
  guides(color = guide_legend(override.aes = list(fill = "white")))

a <- plot1 + plot2 + plot3 + plot4
a






