rm(list=ls())
setwd("/datasets/Source/RedX/apartment-rents")
library(data.table)
library(dplyr)
library(foreign)
library(ggplot2)
library(stringr)
library(tidyr)

di1 <- fread("WMSUF6.csv")  # Import columns
di2 <- fread("WMSUF7.csv")  # Import columns
di3<- fread("WMSUF8.csv")  # Import columns
di4 <- fread("WMSUF9.csv")  # Import columns
di5 <- fread("WMSUF10.csv")
di6 <- fread("WMSUF11.csv")
di7 <- fread("WMSUF12.csv")

df <- rbind(di1, di2, di3, di4, di5, di6, di7, fill = TRUE)
rm(di1, di2, di3, di4, di5, di6, di7)


df <- as.data.frame(df)
df <- df[df$ajahr > 2017,] #filter for relevant years'

df33 <- df



########################################
#data cleaning 
#########################################


remove_vars <- c("bef1", "bef2", "bef3", "bef4", "bef5", "bef6", "bef7", 
                 "bef8", "bef9", "bef10", "courtage", "wohngeld", "click_customer",
                 "kaufpreis", "miteinnahmenpromonat", "grundstuecksflaeche", 
                 "nebenraeume", "wohngeld", "hits", "click_schnellkontakte", 
                 "click_customer", "click_weitersagen", "click_url", "betreut",
                 "denkmalobjekt", "einliegerwohnung", "ferienhaus", "kaufvermietet",
                 "bauphase", "kategorie_Haus")
vars_to_keep <- colnames(df)[!colnames(df) %in% remove_vars]

df <- df[, vars_to_keep]


# set non-numeric variables to NA in relevant columns
for (col in c("mietekalt","wohnflaeche", "baujahr", "etage" 
              )) {
  df[, col] <- gsub("[^[:digit:].]", NA, df[, col])
  df[, col] <- as.integer(df[, col])
}


#eliminate all row entries where important values are marked as invalid by the authors
impl <- c("baujahr", "mietekalt",
          "wohnflaeche", "zimmeranzahl", "etage")

df <- df %>% 
  mutate_at(vars(impl), ~ replace(., . < 1, NA))


df <- na.omit(df)


df <- df[df$kategorie_Wohnung != "Penthouse", ]
df <- subset(df, wohnflaeche > 5 & wohnflaeche < 500)

df <- df[df$mietekalt <= 10000, ]
df <- df[df$mietekalt > 0, ]
df$zimmeranzahl <- as.numeric(df$zimmeranzahl)
df <- df[df$zimmeranzahl <= 15,]



##########################################################
#data preparation so that binary variables are like RWI price index
#########################################################


# recoding dupid_gen to numbers as in the data description
df$dupID_gen_num <- factor(df$dupID_gen, levels = c("ID occurs once only, or first occurence of ID",
                                                  "Probably one spell",
                                                  "Like (1) but time gap >6 months",
                                                  "Ads close in time (<= 6 months), but some dicrepancies in features",
                                                  "Like (4) but time gape >6 months",
                                                  "Large differences in important features"))
df$dupID_gen_num <- as.integer(df$dupID_gen_num)




df <- df %>%
  separate(col = ergg_1km, into = c("xvar", "yvar"), sep = "_")%>%
  mutate(xvar = as.numeric(xvar) * 1000,
         yvar = as.numeric(yvar) * 1000)

# supplying missing cells with the corresponding labor market value. 
# each object within a municipal will correspond to the same labor market


# fill up the corresponding erg_amd value if it is missing
df1 <- df %>%
  group_by(kid2019, erg_amd) %>%
  summarise(n = n()) %>%
  filter(erg_amd != "Other missing") %>%
  slice(which.max(n)) %>%
  ungroup() %>%
  select(kid2019, most_frequent_erg_amd = erg_amd)


df <- left_join(df, df1, by = "kid2019") %>%
  mutate(erg_amd = coalesce(most_frequent_erg_amd, erg_amd)) %>%
  select(-most_frequent_erg_amd)

df <- df %>%
  filter(erg_amd != "Other missing")




write.csv(df, "/datasets/Intermediate/ger-1-apartment-rents.csv")
