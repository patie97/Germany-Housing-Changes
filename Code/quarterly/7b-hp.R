rm(list=ls())

# loading the necessary librarys and the corresponding dataset
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

main <- read.csv("datasets/Intermediate/ger-1-house-sales.csv")

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

#*****************************************************************************
#***************** pre- analysis **********
#*****************************************************************************


############################################################

# change the year and month format to a format that R can work with

main$edate <- paste(main$ejahr, main$emonat, sep = "-")
main$edate <- as.Date(paste(main$edate, "01", sep = "-"), format = "%Y-%m-%d")

main <- main %>%
  mutate(edate = as.Date(edate),
         time_quarter = paste0(year(edate), "q", quarter(edate)))


main$time_quarter <- as.factor(main$time_quarter)

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
df2$log_kp <- log(df2$kaufpreis)
df2$log_kp <- as.numeric(df2$log_kp)


df2$log_wohnflaeche <- log(df2$wohnflaeche)
df2$grundstuecksflaeche <- as.numeric(df2$grundstuecksflaeche)
df2 <- df2[!is.na(df2$grundstuecksflaeche), ]
df2$log_grundstuecksflaeche <- log(df2$grundstuecksflaeche)
df2 <- df2[is.finite(df2$log_grundstuecksflaeche), ]


df2 <- df2 %>%
  mutate(keller = ifelse(keller == "1", 1, 0)) %>%
  mutate(f_occupancy = ifelse(objektzustand == "First occupancy", 1, 0))


# only as old as 199 years
# losing 248214 observations because no baujahr
df2$baujahr <- as.numeric(df2$baujahr)
df2$age_building <- 2022 - df2$baujahr
df2 <- df2[!is.na(df2$age_building), ]

# detached house versus not detached

df2$detached_house_dummy <- ifelse(df2$kategorie_Haus == "Single-family house (detached)", 1, 0)

# Create a dummy variable for all other categories

condition <- c("Like new", "First occupancy", "Well kempt", "Modernised", "First occupancy after reconstruction")

df2$good_condition_house <- ifelse(df2$objektzustand %in% condition, 1, 0)

df3 <- df2


bauer_model <- log_kp ~ age_building + I(log(wohnflaeche)) + I(log(grundstuecksflaeche)) + 
  zimmeranzahl + keller + detached_house_dummy + good_condition_house + time_quarter

#*****************************************
#****** Run for all kid's price index
#*****************************************
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


# Apply the function to each unique gid2019 value and store results in the list
unique_kid2019_values <- unique(df3$kid2019)
for (kid_value in unique_kid2019_values) {
  result_list[[as.character(kid_value)]] <- fit_lm_and_extract_coefficients(kid_value)
}


# Convert the list of results to a data frame
result_df <- as.data.frame(do.call(rbind, result_list))
# Add the gid2019 values as a column in the result data frame
result_df$kid2019 <- unique_kid2019_values


names(result_df) <- sub("^.{5}", "", names(result_df)) #change variable name
df_cleaned <- result_df[complete.cases(result_df), ]
df_cleaned <- df_cleaned[, !(names(df_cleaned) == "19")]

# to long format for ggplot
df_cleaned$kid2019 <- rownames(df_cleaned)
# Reshape the data to long format

long_format <- pivot_longer(df_cleaned, cols = -kid2019, names_to = "time_quarter", values_to = "bauer_pi")
# Remove the rownames
rownames(long_format) <- NULL

a <- long_format[]

long_format$bauer_pi <- long_format$bauer_pi + 100

df3 <- df3 %>%
  mutate(time_quarter = paste0("quarter", time_quarter))


# merge with original dataframe
merged_df <- merge(df3, long_format, by = c("kid2019","time_quarter"), all = TRUE)


write.csv(merged_df, "datasets/Intermediate/ger-3-house-sales-quart.csv")

df <- read.csv("datasets/Intermediate/ger-3-house-sales-quart.csv")

df1 <- df %>% 
  group_by(kid2019) %>% 
  distinct(time_quarter, .keep_all = TRUE) %>%  # Select the first occurrence of each unique value in edate
  ungroup()



write.csv(df1, "datasets/Intermediate/house-sales-hpi-quart.csv")
