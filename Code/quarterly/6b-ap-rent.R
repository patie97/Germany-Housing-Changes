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
library(lubridate)

df <- read.csv("datasets/Intermediate/ger-1-apartment-rents.csv")
main <- df

vars <- c(1,6)
main1 <- main[main$dupID_gen_num %in% vars, ]

vars2 <- c(2,3,4,5)
main2 <- main[main$dupID_gen_num %in% vars2, ]

main2 <- main2 %>%
  filter(ejahr != "Other missing") %>%
  group_by(duplicateid) %>%
  slice(which.max(ejahr[emonat == max(emonat)])) %>%
  ungroup()

main <- rbind(main1, main2)


write.csv(main,"datasets/Intermediate/ger-1.5-apartment-rents.csv")

main <- read.csv("datasets/Intermediate/ger-1.5-apartment-rents.csv")

############################################################

# change the year and month format to a format that R can work with

main$edate <- paste(main$ejahr, main$emonat, sep = "-")
main$edate <- as.Date(paste(main$edate, "01", sep = "-"), format = "%Y-%m-%d")
#main <- main[complete.cases(main$kid2019), ]

# creating quarterly variable

main <- main %>%
  mutate(edate = as.Date(edate),
         time_quarter = paste0( year(edate), "q", quarter(edate)))

#*****************************************************************************
#***************** Analysis begins**********
#*****************************************************************************

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
df2 <- df2 %>%
  mutate(aufzug = ifelse(aufzug == "Yes", 1, 0)) %>%
  mutate(garten = ifelse(garten == "Yes", 1, 0)) %>%
  mutate(balkon = ifelse(balkon == "Yes", 1, 0)) %>%
  mutate(einbaukueche = ifelse(einbaukueche == "Yes", 1, 0))%>%
  mutate(keller = ifelse(keller == "Yes", 1, 0))

condition <- c("Like new", "First occupancy", "Well kempt", "Modernised",
"First occupancy after reconstruction", "Completly renovated")

df2$good_condition_appartment <- ifelse(df2$objektzustand %in% condition, 1, 0)

# only as old as 199 years
# losing 248214 observations because no baujahr
df2$baujahr <- as.numeric(df2$baujahr)
df2$age_building <- 2022 - df2$baujahr
df2 <- df2[!is.na(df2$age_building), ]
df2 <- df2[df2$age_building >= 0, ] #only buildings not under construction
df2 <- df2[df2$age_building < 199, ]


df3 <- df2

df3$edate <- as.factor(df3$edate)



#model
bauer_model <- log_mk ~ age_building + I(log(wohnflaeche)) + zimmeranzahl + keller +
  + good_condition_appartment + garten + aufzug + balkon + einbaukueche + time_quarter



#******************************************
#****** Run for all kid's price index
#*****************************************
#*

result_list <- list()

fit_lm_and_extract_coefficients <- function(kid_value) {
  # Subset the data for the specific gid2019 value
  subset_data <- df3[df3$kid2019 == kid_value, ]
  
  # Fit the linear regression model
  lm_model <- plm(bauer_model, data = subset_data, index = c("gid2019"), model = "within")
  
  # Extract the coefficients starting with "adate"
  coefficients <- coef(lm_model)
  adate_coefficients <- coefficients[grep("^time_quarter", names(coefficients))]
  
  return(adate_coefficients)
}


# Apply the function to each unique kid2019 value and store results in the list
unique_kid2019_values <- unique(df3$kid2019)
for (kid_value in unique_kid2019_values) {
  result_list[[as.character(kid_value)]] <- fit_lm_and_extract_coefficients(kid_value)
}


# Convert the list of results to a data frame
result_df <- as.data.frame(do.call(rbind, result_list))

result_df$kid2019 <- unique_kid2019_values


names(result_df) <- sub("^.{5}", "", names(result_df))
df_cleaned <- result_df[complete.cases(result_df), ]
df_cleaned <- df_cleaned[, !(names(df_cleaned) == "19")]

# to long format for ggplot
df_cleaned$kid2019 <- rownames(df_cleaned)
# Reshape the data to long format

long_format <- pivot_longer(df_cleaned, cols = -kid2019, names_to = "time_quarter", values_to = "bauer_pi_app_rent")
# Remove the rownames
rownames(long_format) <- NULL

a <- long_format[]

long_format$bauer_pi_app_rent <- long_format$bauer_pi_app_rent + 100

df3 <- df3 %>%
  mutate(time_quarter = paste0("quarter", time_quarter))


# merge with original dataframe
merged_df <- merge(df3, long_format, by = c("kid2019","time_quarter"), all.y = TRUE)


write.csv(merged_df, "datasets/Intermediate/ger-3-apartment-rents-quart.csv")


df1 <- read.csv("datasets/Intermediate/ger-3-apartment-rents-quart.csv")


df1 <- merged_df


df1 <- df1 %>% 
  group_by(kid2019) %>% 
  distinct(time_quarter, .keep_all = TRUE) %>%  # Select the first occurrence of each unique value in edate
  ungroup()


write.csv(df1, "datasets/Intermediate/apartment-rents-hpi-quart.csv")
