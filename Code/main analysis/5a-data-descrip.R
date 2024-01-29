rm(list = ls())
setwd("")
library(dplyr)
library(xtable)
library(tidyverse)
library(stargazer)

df <- read.csv("datasets/Intermediate/ger-3-house-sales.csv")


df33 <- df %>%
  select(kaufpreis, baujahr, wohnflaeche, zimmeranzahl, keller, detached_house_dummy,
         zustand, constr, neubau, keller_miss)

stargazer(df33, summary.stat = c("n", "mean", "sd", "p25", "p75"), digits = 2)


df1 <- read.csv("datasets/Intermediate/ger-3-apartment-rents.csv")

df11 <- df1 %>%
  select(mietekalt, baujahr, wohnflaeche, zimmeranzahl, keller, good_condition_appartment,
         garten, aufzug, balkon, einbaukueche, luxury, einbaukueche_miss, keller_miss,
         aufzug_miss, garten_miss)


df11 <- df11[!is.na(df11$mietekalt), ]

stargazer(df11, summary.stat = c("n", "mean", "sd", "p25", "p75"), digits = 2)

stargazer(df11, summary.stat = c("n", "mean", "sd", "p25", "p75"), digits = 2)


