# Together distance
rm(list=ls())
setwd("")

library(dplyr)
library(ggplot2)
library(plm)
library(lubridate)
library(tidyr)
library(stargazer)

df <- read.csv("datasets/Intermediate/price-indexes-v11.csv")

xdesc <- c("2018-02-01", "2018-06-01","2018-09-01", "2018-12-01","2019-03-01", "2019-06-01",
           "2019-09-01", "2019-12-01", "2020-03-01", "2020-06-01", "2020-09-01",
           "2020-12-01", "2021-03-01", "2021-06-01", "2021-09-01", "2021-12-01",
           "2022-03-01", "2022-06-01")
graph_dist <- function(abc){
  # Extract coefficients from plm result
  coefficients <- coef(abc)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    variable = names(coefficients),
    coefficient = coefficients
  )
  plot_data <- plot_data %>%
    filter(variable != "log_mc_distance")
  
  # Extract the 'edate' variables only
  plot_data <- plot_data[grep("^log_mc_distance", plot_data$variable), ]
  
  # Extract the time information
  plot_data$year_month <- gsub("^log_mc_distance:edate", "", plot_data$variable)
  
  # Plotting the effects using ggplot2
  ggplot(plot_data, aes(x = year_month, y = coefficient)) +
    geom_line(aes(group = 1), color = "brown3", size = 1) +  # Set color and line thickness
    labs(title = "Houses for sale",
         x = "Year/Month",
         y = "Elasticity w.r.t distance from center") +
    theme_minimal() +
    scale_x_discrete(breaks = xdesc) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
}

graph_dist2 <- function(abc1, abc2){
  # Extract coefficients from plm results
  coefficients1 <- coef(abc1)
  coefficients2 <- coef(abc2)
  
  # Create a data frame for plotting for model 1
  plot_data1 <- data.frame(
    variable = names(coefficients1),
    coefficient = coefficients1
  )
  
  # Create a data frame for plotting for model 2
  plot_data2 <- data.frame(
    variable = names(coefficients2),
    coefficient = coefficients2
  )
  
  # Extract the 'edate' variables only for model 1
  plot_data1 <- plot_data1[grep("^log_mc_distance", plot_data1$variable), ]
  
  # Extract the time information for model 1
  plot_data1$year_month <- gsub("^log_mc_distance", "", plot_data1$variable)
  
  # Extract the 'edate' variables only for model 2
  plot_data2 <- plot_data2[grep("^log_mc_distance", plot_data2$variable), ]
  
  # Extract the time information for model 2
  plot_data2$year_month <- gsub("^log_mc_distance", "", plot_data2$variable)
  
  # Plotting the effects using ggplot2 with different colors for each model
  ggplot() +
    geom_point(data = plot_data1, aes(x = year_month, y = coefficient, color = "Houses offered"), size = 1) +
    geom_point(data = plot_data2, aes(x = year_month, y = coefficient, color = "Apartments offered"), size = 1) +
    geom_line(data = plot_data1, aes(x = year_month, y = coefficient, group = 1, color = "Houses offered")) +
    geom_line(data = plot_data2, aes(x = year_month, y = coefficient, group = 1, color = "Apartments offered")) +
    labs(title = "House prices vs. apartment rents < 20km ", x = "Year/Month", y = "Elasticity w.r.t distance from city center") +
    theme_minimal() +
    geom_smooth() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(name = "Model", values = c("Houses offered" = "blue", "Apartments offered" = "red"))
}

graph_dist3 <- function(abc){
  # Extract coefficients from plm result
  coefficients <- coef(abc)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    variable = names(coefficients),
    coefficient = coefficients
  )
  plot_data <- plot_data %>%
    filter(variable != "log_mc_distance")
  
  # Extract the 'edate' variables only
  plot_data <- plot_data[grep("^log_mc_distance", plot_data$variable), ]
  
  # Extract the time information
  plot_data$year_month <- gsub("^log_mc_distance:edate", "", plot_data$variable)
  
  # Plotting the effects using ggplot2
  ggplot(plot_data, aes(x = year_month, y = coefficient)) +
    geom_line(aes(group = 1), color = "brown3", size = 1) +  # Set color and line thickness
    labs(title = "Apartments for rent",
         x = "Year/Month",
         y = "Elasticity w.r.t distance from center") +
    theme_minimal() +
    scale_x_discrete(breaks = xdesc) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
}


#*******************************
# Visualization functions
#*******************************


top10 <- c("München", "Frankfurt am Main", "Berlin", "Freiburg", "Heidelberg",
           "Stuttgart", "Potsdam", "Mainz", "Köln", "Hamburg")

df <- df[df$erg_amd %in% top10, ]

df$log_mc_distance <- log(df$mc_distance + 1)
df$edate <- as.factor(df$edate)
df <- df[!is.na(df$erg_amd), ]

df$fin_houses <- as.numeric(df$fin_houses)
df$fin_apps <- as.numeric(df$fin_apps)

df$edate <- as.Date(df$edate)


df$log_mc_distance <- log(df$mc_distance + 1)
plm1 <- plm(log_bauer_pi ~ log_mc_distance * edate + income_hh_19 +
              mean_age + foreigner_quota_19 + wfh_tot,
              data = df, index = c("erg_amd"), model = "within")

graph_dist(plm1)

df$log_mc_distance_app <- log(df$mc_distance_app_rents + 1)

plm2 <- plm(log_pi_app_rent ~ log_mc_distance_app * edate + income_hh_19 +
              mean_age + foreigner_quota_19 + wfh_tot, data = df,
            index = c("erg_amd"), model = "within")

graph_dist3(plm2)

stargazer(plm1, plm2)


