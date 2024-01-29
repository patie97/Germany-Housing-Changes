rm(list=ls())
setwd("")

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


df1 <- read.csv("datasets/Intermediate/house-sales-hpi-quart.csv")
df2 <- read.csv("datasets/Intermediate/apartment-rents-hpi-quart.csv")


df2 <- df2 %>%
  select(kid2019, bauer_pi_app_rent, time_quarter)

df4 <- merge(df1, df2, by = c("kid2019", "time_quarter"), all = TRUE)

df4$log_bauer_pi <- log(df4$bauer_pi + 1)

df4$log_pi_app_rent <- log(df4$bauer_pi_app_rent + 1)


write.csv(df4, "datasets/Intermediate/price-indexes-v1-quart.csv") 

df4 <- read.csv("datasets/Intermediate/price-indexes-v1-quart.csv")



result <- df4 %>%
  group_by(erg_amd) %>%
  summarise(count_unique_kid2019 = n_distinct(kid2019))

df4 <- df4 %>%
  filter(erg_amd %in% result$erg_amd[result$count_unique_kid2019 > 1])

df4$log_mc_distance <- log(df4$mc_distance + 1)


top10 <- c("München", "Frankfurt", "Berlin", "Freiburg", "Heidelberg",
           "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")



df4 <- df4[df4$erg_amd %in% top10, ]


'
total_population <- df4 %>%
  group_by(erg_amd) %>%
  summarize(total_population = sum(unique(total)))


top_10_regions <- total_population %>%
  top_n(10, wt = total_population)

df4 <- df4 %>%
  filter(erg_amd %in% top_10_regions$erg_amd)
'

######
# The bid-rent curve
#####
unique_colors <- c("cyan3", "lavenderblush3", "lavenderblush4", "brown3")

df9 <- df4[df4$time_quarter == "quarter2018q4", ]
df10 <- df4[df4$time_quarter == "quarter2019q4", ]
df11 <- df4[df4$time_quarter == "quarter2020q4", ]
df12 <- df4[df4$time_quarter == "quarter2021q4", ]

ggplot(df4[df4$time_quarter %in% c("quarter2018q4", "quarter2019q4", "quarter2020q4", "quarter2021q4"), ], 
       aes(x = log_mc_distance, y = log_bauer_pi)) +
  geom_point(aes(color = time_quarter)) +
  geom_smooth(data = df9, method = "lm", se = FALSE, aes(color = df9$time_quarter)) +
  geom_smooth(data = df10, method = "lm", se = FALSE, aes(color = df10$time_quarter)) +
  geom_smooth(data = df11, method = "lm", se = FALSE, aes(color = df11$time_quarter)) +
  geom_smooth(data = df12, method = "lm", se = FALSE, aes(color = df12$time_quarter)) +
  scale_color_manual(
    values = unique_colors,
    breaks = c("quarter2018q4", "quarter2019q4", "quarter2020q4", "quarter2021q4"),
    labels = c("Q4 2018", "Q4 2019", "Q4 2020", "Q4 2021")) +
  labs(title = "Bid-rent house prices",
       x = "Log Distance",
       y = "Log Price-index") +
  theme_minimal()

### 
# apartment rents
###


ggplot(df4[df4$time_quarter %in% c("quarter2018q4", "quarter2019q4", "quarter2020q4", "quarter2021q4"), ], 
       aes(x = log_mc_distance, y = log_pi_app_rent)) +
  geom_point(aes(color = time_quarter)) +
  geom_smooth(data = df9, method = "lm", se = FALSE, aes(color = df9$time_quarter)) +
  geom_smooth(data = df10, method = "lm", se = FALSE, aes(color = df10$time_quarter)) +
  geom_smooth(data = df11, method = "lm", se = FALSE, aes(color = df11$time_quarter)) +
  geom_smooth(data = df12, method = "lm", se = FALSE, aes(color = df12$time_quarter)) +
  labs(title = "Bid-rent apartment rents",
       x = "Log Distance",
       y = "Log Price-index") +
  scale_color_manual(
    values = unique_colors,
    breaks = c("quarter2018q4", "quarter2019q4", "quarter2020q4", "quarter2021q4"),
    labels = c("Q4 2018", "Q4 2019", "Q4 2020", "Q4 2021")) +
  theme_minimal()

