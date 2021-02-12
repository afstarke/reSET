# Scratch--
library(tidyverse)
library(DBI)
library(sf)
library(mapview)
library(shiny)
library(miniUI)



# detach("package:reSET", unload = TRUE)
library(reSET)
# Connect to DB and start pulling data:


#*************************************************************
# Function to calculate the standard error
stder <- function(x){ sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

dupfields <- function(df){df %>% map(~anyDuplicated(.x)) %>% unlist() %>% c(.,nrows = nrow(df))}
uniquefields <- function(df){df %>% map(~length(unique(.x))) %>% unlist() %>% c(.,nrows = nrow(df))}
wtfields <- function(df){list(no.duplicates = dupfields(df), no.uniques = uniquefields(df))}

SET.DB.path <- "T:/Coastal/Marsh-Wetlands/SET-MH_project/SET_Data/SET_Monitoring_Database/Database_storage/SET_DB_BE_ver_2.94_TNC_Master.mdb"

DBconn <- set_get_DB(SET.DB.path)

tables <- dbListTables(DBconn)

Stations <- set_get_stations(DBconn)

Readers <- set_get_readers(DBconn)

Samplings <- set_get_samplingevents(DBconn)

SETyo <- set_get_sets(DBconn)

SA <- set_get_accretions(dbconn = DBconn)

pinHts <- c(176, 176, 176, 175, 177, 176, 176, 176, 175)
names(pinHts) <- 1:9


SETyo %>%
  filter(issuePin == FALSE) %>%
  ggplot(aes(x = incrementalChange, y = Plot_Name)) +
  geom_jitter(aes(color = issuePin)) +
  theme_minimal()

SETyo %>%
  filter(issuePin == FALSE) %>%
  group_by(Plot_Name) %>% dplyr::group_split() %>%
  map(.x = .,
      .f = ~ggplot(data = .x, aes(x = Date, y = incrementalChange, group = pin_ID)) +
        geom_point(aes(color = SET_Reader), size = 1) +
        geom_line(alpha = .3) +
        geom_smooth(method = "glm", alpha = .05) +
        ylim(-20, 20) +
        facet_wrap("Plot_Name") +
        theme_minimal()
  )

# enhance SET and SA data with functions that summarize and/or append.



# Crux- How do we account for multiple SET readers across time.
# Recomendations are to use interval measures, change in t0 - t1, t1-t2, for EACH reader.
# Currently analysis is regressing the raw vales as per SOP.
#



  SET.position.Trends.summary.RAW <- SETyo %>%
    # filter(bigIssuePin == FALSE, !is.na(Raw)) %>% # filter out the pins that had issues (for changes to this, see 04_Data_munge.R)
    dplyr::group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID, pin_ID) %>% # Group by the full dataset by individual pin
    dplyr::do(broom::tidy(lm(Raw ~ DecYear, data = .))) %>% # apply a linear regression model of pin height against time (decimal year)
    filter(term == 'DecYear') %>%
    ungroup() %>%
    group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID) %>% # group by position
    dplyr::summarize(ElevationRate_mean = mean(estimate), ElevationRate_se = stder(estimate)) %>%
    `attr<-`("Date of analysis", Sys.Date()) %>%
    `attr<-`('Date of data retreival', attr(SETyo, which = 'Date of data retreival'))

  SET.position.Trends.summary.interval <- SETyo %>%
    # filter(bigIssuePin == FALSE, !is.na(Raw)) %>% # filter out the pins that had issues (for changes to this, see 04_Data_munge.R)
    dplyr::group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID, pin_ID) %>% # Group by the full dataset by individual pin
    dplyr::filter(!is.na(Change)) %>%
    dplyr::do(broom::tidy(lm(Change ~ DecYear, data = .))) %>% # apply a linear regression model of pin height against time (decimal year)
    filter(term == 'DecYear') %>%
    ungroup() %>%
    group_by(Site_Name, SET_Type, Location_ID, Plot_Name, Position_ID) %>% # group by position
    dplyr::summarize(ElevationRate_mean = mean(estimate), ElevationRate_se = stder(estimate)) %>%
    `attr<-`("Date of analysis", Sys.Date()) %>%
    `attr<-`('Date of data retreival', attr(SETyo, which = 'Date of data retreival'))

##
# adjusting RAW measures to be referenced to NAVD88
  # TODO: Create tibble of pins-lengths-SET arm used.
  # TODO: decide on function call to append that adjusted values or output a new dataset?

  set_append_elevations <- function(setData, pinHeights){
    if(is_tibble(pinHeights)){
      stop()
    }
  }


  transects <- sf::st_read("../../../../Downloads/transects.geojson")
transects <- transects %>% filter(field_5 == "vegetation_transect_point")

stationsedit <- mapedit::editFeatures(Stations)
mapview(transects, layer.name = "Veg. transects") + mapview(stationsedit, col.regions = 'darkgreen', layer.name = "SET stations")


