rm(list =ls())
setwd("")


library(dplyr)
library(readxl)
library(zoo)
library(rio)
library(stargazer)
library(plm)
library(ggplot2)
library(lmtest)
library(tidyr)
library(broom)
library(lme4)

# load the previously created dataset
df <- read.csv("datasets/Intermediate/ger-1-apartment-rents.csv")
main <- df

# eliminate explicit identified duplicates and keep the newest entry
vars <- c(1,6)
main1 <- main[main$dupID_gen_num %in% vars, ]

vars2 <- c(2,3,4,5)
main2 <- main[main$dupID_gen_num %in% vars2, ]

main22 <- main2[main2$duplicateid == "Other missing", ]


main2 <- main2 %>%
  group_by(duplicateid) %>%
  slice(which.max(ejahr[emonat == max(emonat)])) %>%
  ungroup()

main <- rbind(main1, main2, main22)


write.csv(main,"datasets/Intermediate/ger-1.5-apartment-rents.csv")

main <- read.csv("datasets/Intermediate/ger-1.5-apartment-rents.csv")

############################################################

# change the year and month format to a format that R can work with

main$edate <- paste(main$ejahr, main$emonat, sep = "-")
main$edate <- as.Date(paste(main$edate, "01", sep = "-"), format = "%Y-%m-%d")

#*****************************************************************************
#***************** estimating the hedonic price index **********
#*****************        municipal level            ************
#*****************.        Bauer(2013).             *************
#*****************************************************************************

df2 <- main

#pre-selection of data in accordance with Bauer paper
df2$log_mk <- log(df2$mietekalt)
df2$log_mk <- as.numeric(df2$log_mk)
df2$log_wohnflaeche <- log(df2$wohnflaeche)
df2$zimmeranzahl <- as.integer(df2$zimmeranzahl)


# creation of condition and luxury dummy condition
condition <- c("Like new", "First occupancy", "Well kempt", "Modernised",
"First occupancy after reconstruction", "Completly renovated")
lux <- c("Maisonette", "Attic flat", "Penthouse")


df2$good_condition_appartment <- ifelse(df2$objektzustand %in% condition, 1, 0)
df2$constr <- ifelse(df2$baujahr >2022, 1, 0)
df2$luxury <- ifelse(df2$kategorie_Wohnung %in% lux, 1, 0)

df2$baujahr <- as.numeric(df2$baujahr)
df2$age_building <- 2022 - df2$baujahr
df2 <- df2[!is.na(df2$age_building), ]
df2 <- df2[df2$age_building < 199, ]

# creation of all dummy variables
df2$keller_miss <- ifelse(df2$keller == "Other missing", 1, 0)
df2$keller <- ifelse(df2$keller == "Yes", 1, 0)
df2$garten_miss <- ifelse(df2$garten == "Other missing", 1, 0)
df2$garten <- ifelse(df2$garten == "Yes", 1, 0)
df2$aufzug_miss <- ifelse(df2$aufzug == "Other missing", 1, 0)
df2$aufzug <- ifelse(df2$aufzug == "Yes", 1, 0)
df2$balkon_miss <- ifelse(df2$balkon == "Other missing", 1, 0)
df2$balkon <- ifelse(df2$balkon == "Yes", 1, 0)
df2$einbaukueche_miss <- ifelse(df2$einbaukueche == "Other missing", 1, 0)
df2$einbaukueche <- ifelse(df2$einbaukueche == "Yes", 1, 0)

df3 <- df2

df3$edate <- as.Date(df3$edate)

#model
bauer_model <- log_mk ~ age_building + I(log(wohnflaeche)) + zimmeranzahl +
  keller + keller_miss + good_condition_appartment + garten + garten_miss + aufzug +
  aufzug_miss + balkon + balkon_miss + einbaukueche_miss + 
  einbaukueche + luxury  + edate

#output of overall regression
b <- plm(bauer_model, model = "within", index = c("kid2019"), data = df3)
summary(b)


#******************************************
#****** Creation of district-level price indexes
#*****************************************
df3$edate <- as.factor(df3$edate)

result_list <- list()

fit_lm_and_extract_coefficients <- function(kid_value) {
  # Subset the data for the specific kid2019 value
  subset_data <- df3[df3$kid2019 == kid_value, ]
  
  # Fit the linear regression model
  lm_model <- plm(bauer_model, data = subset_data, index = c("gid2019"), model = "within")
  
  # Extract the coefficients starting with "edate"
  coefficients <- coef(lm_model)
  adate_coefficients <- coefficients[grep("^edate", names(coefficients))]
  
  return(adate_coefficients)
}


# Apply the function to each unique kid2019 value and store results in the list
unique_kid2019_values <- unique(df3$kid2019)
for (kid_value in unique_kid2019_values) {
  result_list[[as.character(kid_value)]] <- fit_lm_and_extract_coefficients(kid_value)
}


# Convert the list of results to a data frame
result_df <- as.data.frame(do.call(rbind, result_list))
# Add the kid2019 values as a column in the result data frame
result_df$kid2019 <- unique_kid2019_values


names(result_df) <- sub("^.{5}", "", names(result_df)) #change variable name
df_cleaned <- result_df[complete.cases(result_df), ] 
df_cleaned <- df_cleaned[, !(names(df_cleaned) == "19")]

# to long format for ggplot
df_cleaned$kid2019 <- rownames(df_cleaned)
# Reshape the data to long format

long_format <- pivot_longer(df_cleaned, cols = -kid2019, names_to = "edate", values_to = "bauer_pi_app_rent")
# Remove the rownames
rownames(long_format) <- NULL

a <- long_format[]

long_format$bauer_pi_app_rent <- long_format$bauer_pi_app_rent + 100


# merge with original dataframe
merged_df <- merge(df3, long_format, by = c("kid2019","edate"), all.y = TRUE)


write.csv(merged_df, "datasets/Intermediate/ger-3-apartment-rents.csv")

# include the distances calculated in Python-script
df1 <- read.csv("datasets/Intermediate/ger-3-apartment-rent-coord.csv")

df1$edate <- paste(df1$ejahr, df1$emonat, sep = "-")
df1$edate <- as.Date(paste(df1$edate, "01", sep = "-"), format = "%Y-%m-%d")

dis <- df1 %>%
  select(distance, kid2019)

dis <- dis %>%
  group_by(kid2019) %>%
  summarize(mc_distance_app_rents = mean(distance, na.rm = TRUE))

df1 <- merge(df1, dis, by = "kid2019", all = TRUE)


df1 <- df1 %>% 
  group_by(kid2019) %>% 
  distinct(edate, .keep_all = TRUE) %>%  # Select the first occurrence of each unique value in edate
  ungroup()

write.csv(df1, "datasets/Intermediate/apartment-rents-hpi.csv")
