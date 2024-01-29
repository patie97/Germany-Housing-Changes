rm(list=ls())
setwd("/datasets/Source/RedX/house-sales")
library(data.table)
library(dplyr)
library(foreign)
library(ggplot2)
library(stringr)
library(tidyr)

di3 <- fread("HKSUF3.csv")  # Import columns
di4 <- fread("HKSUF4.csv")  # Import columns
di5 <- fread("HKSUF5.csv")  # Import columns
di6 <- fread("HKSUF6.csv")  # Import columns

df1 <- rbind(di3, di4, di5, 
             di6, fill = TRUE)

di7 <- fread("HKSUF7.csv")  # Import columns
di8 <- fread("HKSUF8.csv")  # Import columns
di9 <- fread("HKSUF9.csv")  # Import columns
di10 <- fread("HKSUF10.csv")  # Import columns
di11 <- fread("HKSUF11.csv")  # Import columns
di12 <- fread("HKSUF12.csv")

df2 <- rbind(di7, di8, di9, di10, 
             di11, di12, fill = TRUE)

df <- rbind(df1, df2, fill = TRUE)
rm(di2, di3, di4, di5, di6, di7, di8, di9, di10, di11, di12, df1, df2)


df <- as.data.frame(df)
df <- df[df$ajahr > 2017,] #filter for relevant years'

df33 <- df



########################################
#data cleaning 
#########################################


remove_vars <- c("bef1", "bef2", "bef3", "bef4", "bef5", "bef6", "bef7", 
                 "bef8", "bef9", "bef10", "haustier_erlaubt"," betreut", "nebenraeume",
                 "freiab", "courtage", "mietekaution", "wohngeld", "etage", "click_customer",
                 "denkmalobjekt", "foerderung", "balkon", "bauphase",
                 "mieteinnahmenpromonat","mietekalt", "aufzug", "einbaukueche", 
                 "mietewarm", "nebenkosten", "heizkosten", "parkplatzpreis", "ev_kennwert",
                 "ev_wwenthalten", "heizkosten_in_wm_erhalten", "energieeffizienzklasse", 
                 "energieausweistyp", "heizungsart", "kategorie_wohnung", "gid_updated",
                 "kid_updated", "nutzflaeche", "heizkosten_in_wm_enthalten", 
                 "kategorie_wohnung", "clicks_url", "liste_show", "liste_match",
                 "click_schnellkontakte", "click_weitersagen", "click_url",
                 "garten", "kategorie_wohnung")
vars_to_keep <- colnames(df)[!colnames(df) %in% remove_vars]

df <- df[, vars_to_keep]


# set non-numeric variables to NA in relevant columns
for (col in c("kaufpreis","wohnflaeche", "zimmeranzahl", "baujahr", "grundstuecksflaeche"
              )) {
  df[, col] <- gsub("[^[:digit:].]", NA, df[, col])
  df[, col] <- as.integer(df[, col])
}


#eliminate all row entries where important values are marked as invalid by the authors
impl <- c("baujahr", "grundstuecksflaeche", "kaufpreis",
          "wohnflaeche", "zimmeranzahl")

df <- df %>% 
  mutate_at(vars(impl), ~ replace(., . < 0, NA))


df <- na.omit(df)

# some more transformations according to Bauer price index

df <- df[df$kategorie_Haus != "Block of flats", ]
df <- subset(df, wohnflaeche > 5 & wohnflaeche < 500)
df <- df[df$kaufpreis <= 5000000, ]
df <- df[df$kaufpreis > 0, ]
df$zimmeranzahl <- as.numeric(df$zimmeranzahl)
df <- df[df$zimmeranzahl <= 15,]
etagen <- c("Other missing", "1", "2", "3", "4", "5")
df <- df[df$anzahletagen %in% etagen,]
df <- df[df$kategorie_Haus != "Castle", ]



##########################################################
#data preparation
#########################################################

df$dupID_gen_num <- factor(df$dupID_gen, levels = c("ID occurs once only, or first occurence of ID",
                                                  "Probably one spell",
                                                  "Like (1) but time gap >6 months",
                                                  "Ads close in time (<= 6 months), but some dicrepancies in features",
                                                  "Like (4) but time gape >6 months",
                                                  "Large differences in important features"))
df$dupID_gen_num <- as.integer(df$dupID_gen_num)


df <- df %>%
  mutate(keller = ifelse(keller == "Yes", 1, ifelse(keller == "No", 0, keller)))



df <- df %>%
  separate(col = ergg_1km, into = c("xvar", "yvar"), sep = "_")%>%
  mutate(xvar = as.numeric(xvar) * 1000,
         yvar = as.numeric(yvar) * 1000)

# supplying missing cells with the corresponding labor market value. 
# each object within a municipal will correspond to the same labor market

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




write.csv(df, "/datasets/Intermediate/ger-1-house-sales.csv")
