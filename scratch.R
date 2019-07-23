# Scratch--
library(tidyverse)
library(DBI)
library(sf)
library(mapview)

detach("package:SETr", unload = TRUE)
library(SETr)
# Connect to DB and start pulling data:


#*************************************************************
# Function to calculate the standard error
stder <- function(x){ sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}



SET.DB.path <- "T:/Coastal/Marsh-Wetlands/SET-MH_project/SET_Data/SET_Monitoring_Database/Database_storage/SET_DB_BE_ver_2.94_TNC_Master.mdb"

DBconn <- set_get_DB(SET.DB.path)

tables <- dbListTables(DBconn)

Stations <- set_get_stations(DBconn)

Readers <- set_get_readers(DBconn)

Samplings <- set_get_samplingevents(DBconn)

SETyo <- set_get_sets(DBconn)

SA <- set_get_accretions(dbconn = DBconn)

# enhance SET and SA data with functions that summarize and/or append.


SET.position.Trends.summary <- SETyo %>%
  # filter(bigIssuePin == FALSE, !is.na(Raw)) %>% # filter out the pins that had issues (for changes to this, see 04_Data_munge.R)
  dplyr::group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID, pin_ID) %>% # Group by the full dataset by individual pin
  dplyr::do(broom::tidy(lm(Raw ~ DecYear, data = .))) %>% # apply a linear regression model of pin height against time (decimal year)
  filter(term == 'DecYear') %>%
  ungroup() %>%
  group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID) %>% # group by position
  dplyr::summarize(ElevationRate_mean = mean(estimate), ElevationRate_se = stder(estimate)) %>%
  `attr<-`("Date of analysis", Sys.Date()) %>%
  `attr<-`('Date of data retreival', attr(SETyo, which = 'Date of data retreival'))
