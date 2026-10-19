
rm(list=ls())


#Setting WD
setwd("C:/Users/adria/Documents/JOBS/Data-Driven EnviroLab/Adriana_JimenezWillis_DDL_test")

#libraries
library(tidyverse)
library(readxl)
library(stringi)

################################################################################
###################################################################################

#ASSIGNMENT A

#Sometimes, I edit exisiting objects to keep object storage low. In this case I decided to create new dataset objects with each iteration/merge to be able to reference more recent versions in the case of mistakes.

#Importing Datasets
Forbes2000_2026 <- read.csv("Forbes_2000_Companies_2026.csv") 

#Was losing data the first time I imported or tried to convert to csv before importing. Hence converting all data to text to make sure the data types are compatible to shwo up in R.
NetZero_Current <- read_excel(
  "current_current-snapshot_2026-10-12_06-39-50.xlsx",
  sheet = "Current Snapshot",
  col_types = "text")

#Making sure the correct columns are numeric
NetZero_Current <- NetZero_Current %>%
  mutate(across(
    c(10, 19, 36, 47, 49, 51, 53, 54, 58, 59, 60, 63, 64, 65),
    as.numeric
  ))


#Renaming to make things easier/more legible and make sure we can merge properly
NetZero_Current <- NetZero_Current %>% 
  rename(Company = Name)

Forbes2000_2026 <- Forbes2000_2026 %>% 
  rename(Sales = Sales...B., Profit = Profit...B., Assets = Assets...B., Market.Value = Market.Value...B.)


#####################################################################################

#Iteration 1. Initial Merge

NetZero_Updated <- NetZero_Current %>%
  left_join(
    Forbes2000_2026 %>%
      select(Company, Sales, Profit),
    by = "Company"
  )


#Some Analysis of non-merged values
sum(is.na(Forbes2000_2026$Sales)) #All rows have information for the Sales variable, so using the sales column is a good way to keep track of how many observations have been matched and merged successfully.
4180 - sum(is.na(NetZero_Updated$Sales))

#1121 out of 2000 entries from the Forbes 2000 dataset had matches in the NetZero dataset, or 879 observations didn't have matches. Some of these are due to differences in company names between datsets. For example, the NetZero dataset describes a company with the name "3i", while the Forbes 2000 dataset describes the same company by adding the suffix "group". Thus, R does not recognize 3i and 3i group as the same company because their values are not identical.

#It's also possible that the top 2000 companies of the year changed positions, so they aren't the same 2000 companies each year (meaning that some of them are missing a match).

#Which values didn't get matched? Making an unmatched set for inspection
Unmatched_Forbes <- Forbes2000_2026 %>%
  anti_join(NetZero_Current, by = "Company")


#####################################################################################

#Iteration 2. A few errors I'm noticing: inconsistent spaces, parentheses, periods, etc. in company names. Let's standardize that & try again. 
NetZero_Current2 <- NetZero_Current %>%
  mutate(Company = str_remove_all(Company, "[ ().']"))

Forbes2000_2026_2 <- Forbes2000_2026 %>%
  mutate(Company = str_remove_all(Company, "[ ().']"))

#And trying again. 
NetZero_Updated2 <- NetZero_Current2 %>%
  left_join(
    Forbes2000_2026_2 %>%
      select(Company, Sales, Profit),
    by = "Company"
  )

#Now:
4180 - sum(is.na(NetZero_Updated2$Sales)) #At 1131 rows filled now. Added 10 new matches. 

Unmatched_Forbes2 <- Forbes2000_2026_2 %>%
  anti_join(NetZero_Current2, by = "Company")


#####################################################################################

#Iteration 3- there are words like "Group", "Ltd", "LTD", "Corp", "Corporation", "Holdings", "Holding", PLC", "Plc", "plc". Not going to use "Co" because a lot of words actually use this combination & I don't want to disturb the actual names of the companies.

NetZero_Current3 <- NetZero_Current2 %>%
  mutate(Company = str_remove_all(Company, "Group|Ltd|LTD|Corp|Corporation|Holdings|Holding|PLC|Plc|plc|The|Company"))

Forbes2000_2026_3 <- Forbes2000_2026_2 %>%
  mutate(Company = str_remove_all(Company, "Group|Ltd|LTD|Corp|Corporation|Holdings|Holding|PLC|Plc|plc|The|Company"))

#Now a couple companies have the same names. Fixing that manually so they match up correctly.

Forbes2000_2026_3$Company[Forbes2000_2026_3$Company == "Metro" & Forbes2000_2026_3$Sales == 38.12] <- "MetroGER"

Forbes2000_2026_3$Company[Forbes2000_2026_3$Company == "Metro" & Forbes2000_2026_3$Sales == 16.19] <- "MetroCAN"

NetZero_Current3$Company[NetZero_Current3$Company == "Metro" & NetZero_Current3$Country == "DEU"] <- "MetroGER"

NetZero_Current3$Company[NetZero_Current3$Company == "Metro" & NetZero_Current3$Country == "CAN"] <- "MetroCAN"

Forbes2000_2026_3$Company[Forbes2000_2026_3$Company == "ChinaMerchantsPort" & Forbes2000_2026_3$Sales == 2.46] <- "ChinaMerChantsPortHKG"

NetZero_Current3$Company[NetZero_Current3$Company == "Otsuka" & NetZero_Current3$Separate_removal_target == "Yes"] <- "OtsukaOLD"

#Merge 3
NetZero_Updated3 <- NetZero_Current3 %>%
  left_join(
    Forbes2000_2026_3 %>%
      select(Company, Sales, Profit),
    by = "Company"
  )

4180 - sum(is.na(NetZero_Updated3$Sales)) #Up to 1238 now. This fix added 107 new matches!

Unmatched_Forbes3 <- Forbes2000_2026_3 %>%
  anti_join(NetZero_Current3, by = "Company")


#####################################################################################

#Iteration 4. Now that I've removed things like "PLC" and "Ltd" (which are case sensitive), I'm going to capitalize the names of every company and try again.

NetZero_Current4 <- NetZero_Current3
Forbes2000_2026_4 <- Forbes2000_2026_3

NetZero_Current4$Company <- toupper(NetZero_Current4$Company)

Forbes2000_2026_4$Company <- toupper(Forbes2000_2026_4$Company)

#Merge 4 
NetZero_Updated4 <- NetZero_Current4 %>%
  left_join(
    Forbes2000_2026_4 %>%
      select(Company, Sales, Profit),
    by = "Company"
  )

4180 - sum(is.na(NetZero_Updated4$Sales)) #Added 40 more this way.

Unmatched_Forbes4 <- Forbes2000_2026_4 %>%
  anti_join(NetZero_Current4, by = "Company")


#####################################################################################

#Iteration 5. I notice that some names have accents in them. Let's get rid of those to make a few matches work.


NetZero_Current5 <- NetZero_Current4
Forbes2000_2026_5 <- Forbes2000_2026_4

NetZero_Current5$Company <- stri_trans_general(
  NetZero_Current5$Company, "Latin-ASCII"
)

Forbes2000_2026_5$Company <- stri_trans_general(
  Forbes2000_2026_5$Company, "Latin-ASCII"
)

# Merge 5
NetZero_Updated5 <- NetZero_Current5 %>%
  left_join(
    Forbes2000_2026_5 %>%
      select(Company, Sales, Profit),
    by = "Company"
  )

4180 - sum(is.na(NetZero_Updated5$Sales)) #+5 more matches.

Unmatched_Forbes5 <- Forbes2000_2026_5 %>%
  anti_join(NetZero_Current5, by = "Company")


#####################################################################################

#Iteration 6: Finally, I apply fuzzy matching. 

fuzzy_matches <- Unmatched_Forbes5 %>%
  mutate(
    match_index = stringdist::amatch(
      Company,
      NetZero_Current5$Company,
      method = "jw",
      maxDist = 0.10
    ),
    NetZero_Company = NetZero_Current5$Company[match_index]
  ) %>%
  filter(!is.na(match_index)) %>%
  select(Company, NetZero_Company, Sales, Profit)

Rejected <- c(
  "MARSH", "AEON", "TSFINANCIAL", "PHILIPS", "AKBANK", "ALDARPROPERTIES", "ECHOSTAR", "LPLFINANCIAL", "IVECO", "SHIMAOPROPERTY", "FIRSTHORIZON", "ASMINTERNATIONAL", "WTMICROELECTRONICS", "CFINDUSTRIES", "SOFI", "ARESMANAGEMENT", "NAURATECHNOLOGY", "UMBFINANCIAL", "BOROUGE", "ONSEMICONDUCTOR", "BGFINANCIAL", "ASIACOMMERCIALBANK", "IMFINANCIAL", "HERA", "AGCO", "ARCRESOURCES", "ALTAGAS", "BANKOFAFRICA")


Approved_fuzzy_matches <- fuzzy_matches %>%
  filter(!Company %in% Rejected)


Forbes2000_2026_5$Company[
  match(Approved_fuzzy_matches$Company, Forbes2000_2026_5$Company)
] <- Approved_fuzzy_matches$NetZero_Company


NetZero_Updated6 <- NetZero_Current5 %>%
  left_join(
    Forbes2000_2026_5 %>% select(Company, Sales, Profit),
    by = "Company"
  )

4180 - sum(is.na(NetZero_Updated6$Sales)) #Up to 1419 entries matched. This iteration added 136 matches! This is probably nearing the most matches that we are able to make at the moment. Like I noted earlier, it's not expected that all of the entries in the Forbes 2000 dataset even have matches in the NetZero Current dataset.

