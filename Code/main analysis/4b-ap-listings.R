rm(list=ls())
setwd("")
library(lubridate)
library(dplyr)
library(foreign)
library(ggplot2)
library(stringr)
library(tidyr)
df <- read.csv("datasets/Intermediate/ger-3-apartment-rents.csv")


df$adate <- paste(df$ajahr, df$amonat, sep = "-")
df$adate <- as.Date(paste(df$adate, "01", sep = "-"), format = "%Y-%m-%d")

df$edate <- paste(df$ejahr, df$emonat, sep = "-")
df$edate <- as.Date(paste(df$edate, "01", sep = "-"), format = "%Y-%m-%d")

a1 <- c("First occupancy", "Well kempt", "By arrangement", "Like new", 
        "First occupancy after reconstruction", "Complety renovated", "Modernised",
        "Reconstructed", "Not specified")
df <- df[df$objektzustand %in% a1,]




top10 <- c("München", "Frankfurt", "Berlin", "Freiburg", "Heidelberg",
           "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")

top10c <- c("München", "Frankfurt am Main", "Berlin", "Freiburg im Breisgau", "Heidelberg",
            "Stuttgart", "Düsseldorf", "Mainz", "Köln", "Hamburg")




#***************************************************
#******development of length of advertisement*****
#***************************************************

df2 <- df

df2$adate <- paste(df2$ajahr, df2$amonat, sep = "-")
df2$adate <- as.Date(paste(df2$adate, "01", sep = "-"), format = "%Y-%m-%d")


center <- df2[df2$erg_amd %in% top10, ]
center <- center[center$gid2015 %in% top10c, ]

suburbs <- df2[df2$erg_amd %in% top10, ]
suburbs <- suburbs[!suburbs$gid2015 %in% top10c, ]



ls1 <- center %>%
  group_by(edate) %>%
  summarise(lauf = mean(laufzeittage))%>%
  ungroup()

ls2 <- suburbs %>%
  group_by(edate) %>%
  summarise(lauf = mean(laufzeittage))%>%
  ungroup()

combined_data_laufzeit <- rbind(mutate(ls1, location = "Center"), mutate(ls2, location = "Suburb"))

# Plot the data
# comparison of new listings
ggplot(combined_data_laufzeit, aes(x = edate, y = lauf, color = location, group = location)) +
  #geom_line() +
  labs(title = "Development of validity period length",
       x = "Date",
       y = "Length of validity period") +
  theme_minimal() +
  geom_smooth(method = "loess") 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#***************************************************
#******count of active listings*****
#***************************************************
#####
df1 <- main
  
  
  center <- df1[df1$erg_amd %in% top10, ]
  center <- center[center$gid2015 %in% top10c, ]
  suburbs <- df1[df1$erg_amd %in% top10, ]
  suburbs <- suburbs[!suburbs$gid2015 %in% top10c, ]
  
  
  center <- center %>%
    mutate(across(c(adate, edate), as.Date)) %>%
    filter(!is.na(adate))  # Remove rows with missing adate
  
  # Create a sequence of months
  date_sequence <- seq(as.Date("2018-01-01"), max(center$edate, na.rm = TRUE), by = "months")
  
  # Create an empty data frame to store the results
  result_df <- data.frame(month = date_sequence, list = numeric(length(date_sequence)))
  
  # Loop through each month and calculate total observations
  for (i in seq_along(date_sequence)) {
    result_df$list[i] <- sum(
      (!is.na(center$adate) & floor_date(center$adate, "month") == date_sequence[i]) |
        (!is.na(center$edate) & floor_date(center$edate, "month") == date_sequence[i])
    )
  }
  
  
  suburbs <- suburbs %>%
    mutate(across(c(adate, edate), as.Date)) %>%
    filter(!is.na(adate))  # Remove rows with missing adate
  
  # Create a sequence of months
  date_sequence <- seq(as.Date("2018-01-01"), max(suburbs$edate, na.rm = TRUE), by = "months")
  
  # Create an empty data frame to store the results
  result_df2 <- data.frame(month = date_sequence, list = numeric(length(date_sequence)))
  
  # Loop through each month and calculate total observations
  for (i in seq_along(date_sequence)) {
    result_df2$list[i] <- sum(
      (!is.na(suburbs$adate) & floor_date(suburbs$adate, "month") == date_sequence[i]) |
        (!is.na(suburbs$edate) & floor_date(suburbs$edate, "month") == date_sequence[i])
    )
  }
  
  
  
combined_data <- rbind(mutate(result_df, location = "Center"), mutate(result_df2, location = "Suburb"))
  
  ggplot(combined_data, aes(x = month, y = list, color = location, group = location)) +
    geom_smooth() + 
    labs(title = "Active apartment listings",
         x = "Year",
         y = "Count of listings") +
    scale_color_manual(values = c("brown3","cyan3")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  