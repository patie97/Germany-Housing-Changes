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
library(stringr)


df1 <- read.csv("datasets/Intermediate/house-sales-hpi-1.csv")
df2 <- read.csv("datasets/Intermediate/apartment-rents-hpi.csv")
df3 <- read.csv("datasets/Intermediate/apartment-sales-hpi.csv")

df2 <- df2 %>%
  select(kid2019, bauer_pi_app_rent, edate, mc_distance_app_rents)

df3 <- df3 %>%
  select(kid2019, bauer_pi_app_sales, edate)


df4 <- merge(df1, df2, by = c("kid2019", "edate"), all = TRUE)
df4 <- merge(df4, df3, by = c("kid2019", "edate"), all = TRUE)



df4$log_bauer_pi <- log(df4$bauer_pi + 1)

df4$log_pi_app_rent <- log(df4$bauer_pi_app_rent + 1)

df4$log_bauer_pi_app_sales <- log(df4$bauer_pi_app_sales + 1)


#create houses versus rents

df4 <- df4 %>%
  group_by(kid2015) %>%
  filter(n() >= 52)




write.csv(df4, "datasets/Intermediate/price-indexes-v11.csv") 



df4 <- read.csv("datasets/Intermediate/price-indexes-v11.csv")



df4 <- df4 %>%
  filter(!is.na(bauer_pi) & !is.na(bauer_pi_app_rent) & !is.na(bauer_pi_app_sales))


# immigrated, migrated, migration_balance


result <- df4 %>%
  group_by(erg_amd) %>%
  summarise(count_unique_kid2019 = n_distinct(kid2019))

df4 <- df4 %>%
  filter(erg_amd %in% result$erg_amd[result$count_unique_kid2019 > 1])


xdesc <- c("2018-02-01", "2018-06-01","2018-09-01", "2018-12-01","2019-03-01", "2019-06-01",
           "2019-09-01", "2019-12-01", "2020-03-01", "2020-06-01", "2020-09-01",
           "2020-12-01", "2021-03-01", "2021-06-01", "2021-09-01", "2021-12-01",
           "2022-03-01", "2022-06-01")

top10 <- c("München", "Frankfurt", "Berlin", "Freiburg", "Heidelberg",
           "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")

top10c <- c("München", "Frankfurt am Main", "Berlin", "Freiburg im Breisgau", "Heidelberg",
            "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")


df5 <- df4[df4$erg_amd %in% top10, ]
df5 <- df5[df5$gid2015 %in% top10c, ]

df6 <- df4[df4$erg_amd %in% top10, ]
df6 <- df6[!df6$gid2015 %in% top10c, ]


#******************************************
#****** Price analysis houses suburbs versus centers
#*****************************************



lsC <- df5 %>%
  group_by(edate) %>%
  summarise(housepC = mean(log_bauer_pi))%>%
  ungroup()


lsS <- df6 %>%
  group_by(edate) %>%
  summarise(housepS = mean(log_bauer_pi)) %>%
  ungroup()


combined_data <- merge(lsC, lsS, by = "edate")


ggplot(combined_data, aes(x = edate)) +
  geom_point(aes(y = housepC, color = "Center"), size = 1) +
  geom_point(aes(y = housepS, color = "Suburbs"), size = 1) +
  geom_line(aes(y = housepC, group = 1, color = "Center"), size = 1) +
  geom_line(aes(y = housepS, group = 1, color = "Suburbs"), size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks = xdesc) + 
  labs(title = "Houseprices", x = "Year/Month", y = "relative price-index") +
  scale_color_manual(name = "Location", values = c("Center" = "brown3", "Suburbs" = "cyan3"))


#******************************************
#****** Price analysis apartment sales suburbs versus centers
#****************

lsC <- df5 %>%
  group_by(edate) %>%
  summarise(apartmentC = mean(log_bauer_pi_app_sales))%>%
  ungroup()


lsS <- df6 %>%
  group_by(edate) %>%
  summarise(apartmentS = mean(log_bauer_pi_app_sales)) %>%
  ungroup()

combined_data <- merge(lsC, lsS, by = "edate")


ggplot(combined_data, aes(x = edate)) +
  geom_point(aes(y = apartmentC, color = "Center"), size = 1) +
  geom_point(aes(y = apartmentS, color = "Suburbs"), size = 1) +
  geom_line(aes(y = apartmentC, group = 1, color = "Center"), size = 1) +
  geom_line(aes(y = apartmentS, group = 1, color = "Suburbs"), size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks = xdesc) + 
  labs(title = "Apartment sales", x = "Year/Month", y = "relative price-index") +
  scale_color_manual(name = "Location", values = c("Center" = "brown3", "Suburbs" = "cyan3"))


#******************************************
#****** Price analysis apartment rents suburbs versus centers
# Plot 3 rent prices
#*****************************************

df5 <- df4[df4$gid2015 %in% top10, ]
df6 <- df4[!df4$gid2015 %in% top10, ]

lsC <- df5 %>%
  group_by(edate) %>%
  summarise(apartmentC = mean(log_pi_app_rent))%>%
  ungroup()


lsS <- df6 %>%
  group_by(edate) %>%
  summarise(apartmentS = mean(log_pi_app_rent)) %>%
  ungroup()

combined_data <- merge(lsC, lsS, by = "edate")



ggplot(combined_data, aes(x = edate)) +
  geom_point(aes(y = apartmentC, color = "Center"), size = 1) +
  geom_point(aes(y = apartmentS, color = "Suburbs"), size = 1) +
  geom_line(aes(y = apartmentC, group = 1, color = "Center"), size = 1) +
  geom_line(aes(y = apartmentS, group = 1, color = "Suburbs"), size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks = xdesc) + 
  labs(title = "Rentprices center vs. suburbs", x = "Year/Month", y = "relative price-index") +
  scale_color_manual(name = "Location", values = c("Center" = "brown3", "Suburbs" = "cyan3"))

#******************************************
#****** price-rent ratio
#*****************************************

 
#### price-to-rent ratio

df33 <- df4
df33$log_mc_distance <- log(df33$mc_distance + 1)
df33$price_rent <- df4$bauer_pi / df4$bauer_pi_app_rent
summary(df33$price_rent)

df33$year_group <- ifelse(df33$ejahr %in% c(2018, 2019), "Pre-Pandemic", "During/after pandemic")

# Plot for both groups
ggplot(df33, aes(x = log_mc_distance, y = price_rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, aes(col = year_group)) +
  labs(title = "Price-Rent Ratio vs Log Distance",
       x = "Log Distance",
       y = "Price-Rent Ratio") +
  theme_minimal() +
  scale_color_manual(values = c("cyan2", "brown2")) +
  guides(color = guide_legend(title = ""))
