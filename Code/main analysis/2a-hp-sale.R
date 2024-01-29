rm(list =ls())
setwd("")

library(readxl)
library(dplyr)
library(tidyverse)

df <- read.csv("/datasets/Intermediate/ger-1-house-sales.csv")


df$edate <- paste(df$ejahr, df$emonat, sep = "-")
df$edate <- as.Date(paste(df$edate, "01", sep = "-"), format = "%Y-%m-%d")


# merge with distance parameters
df1 <- read.csv("datasets/Intermediate/ger-3-house-sales-coord.csv")

dis <- df1 %>%
  select(distance, kid2019)

dis <- dis %>%
  group_by(kid2019) %>%
  summarize(mc_distance = mean(distance, na.rm = TRUE))

df <- merge(df, dis, by = "kid2019", all = TRUE)

# This script pulls socioeconomic and other data for german districts and puts them 
# in a dataframe that can be used with the data from IS24 for analysis 

#****************
# mean age of 2019
# Regionalstatistik - Datensatz 12411-07-01-4
#****************

m_age <- read_xlsx("datasets/Source/whole-germany/12411-07-01-4.xlsx")
ind <- c("kid2019", "mean_age")
m_age <- m_age[, ind]


m_age <- m_age %>%
  mutate(kid2019 = str_sub(kid2019, start = ifelse(str_sub(kid2019, 1, 1) == "0", 2, 1)))

m_age <- m_age %>%
  filter(nchar(kid2019) %in% c(4,5))

df <- merge(df, m_age, by = "kid2019", all.x = TRUE)

#****************
# Population and share of foreigners 2019
# Regionalstatistik - Datensatz 12411-03-03-4
#****************

pop <- read_xlsx("datasets/Source/whole-germany/12411-03-03-4-2019.xlsx")
pop <- pop[pop$...3 == "Insgesamt", ]
pop1 <- c("kid2019", "total", "foreigners")
pop <- pop[, pop1]
pop <- na.omit(pop)


pop <- pop %>%
  mutate(kid2019 = str_sub(kid2019, start = ifelse(str_sub(kid2019, 1, 1) == "0", 2, 1)))

pop <- pop %>%
  filter(nchar(kid2019) %in% c(4,5))



df <- merge(df, pop, by = "kid2019", all.x = TRUE)

df$total.x <- as.numeric(df$total.x)

df$foreigners.x <- as.numeric(df$foreigners.x)

df$foreigner_quota_19 <- df$total.x / df$foreigners.x


#****************
# available income per household 2019
# Volkswirtschaftliche Gesamtrechnung der Länder
#****************

inc <- read_xlsx("datasets/Source/whole-germany/vgrdl_r2b3_bs2019.xlsx", sheet = "2.4")
colnames(inc)

inc <- inc %>%
  mutate(kid2019 = str_sub(kid2019, start = ifelse(str_sub(kid2019, 1, 1) == "0", 2, 1)))

inc <- inc %>%
  filter(nchar(kid2019) %in% c(4,5))
#a <- df[is.na(df$income_hh_19), ]
df <- merge(df, inc, all.x = TRUE)

#****************
# ability to work from home 19/20
# Alipour, J.-V., Falck, O., & Schüller (2020) Germany’s Capacity to Work from Home
#****************

wfh <- read.csv("datasets/Source/wfh_nuts3.csv")

df <- merge(df, wfh, all.x = TRUE)

#****************
# Migration data 
# 12711-05-02-4 Regionalstatistik
#****************

mig <- read.csv("datasets/Source/migration.csv")
mig <- na.omit(mig)


mig <- mig %>%
  mutate(kid2019 = str_sub(kid2019, start = ifelse(str_sub(kid2019, 1, 1) == "0", 2, 1)))

mig <- mig %>%
  filter(nchar(kid2019) %in% c(4,5))


df <- merge(df, mig, by = c("kid2019", "ejahr"), all.x = TRUE)


write.csv(df, "/datasets/Intermediate/ger-2-house-sales-2.csv")
