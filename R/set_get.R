#' SET_get_* functions that fetch data from provided database.
#'
# TODO: create set_get_xx functions to fetch the following
# DONE: set_get_DB make a data base connection.
# IDEA: database info, location, perhaps type of database? For future expansion to other platforms.
# DONE: set_get_stations return sf object with meta data on
# the station including elevation?
# set_get_readers returns basic dataframe of SET readers
# set_get_visits
# set_get_


#' Get connected to from Access DB from which
#'
#' This function connects and returns information relating to the SET-MH database
#' @param dbPath file url for backend data base.
#' @keywords SET-MH, SET, database
#' @return an odbc connection object to a database to be used in piped operations for extracting data from DB. Primarily used in other functions.
#' @export


set_get_DB <- function(dbPath) {
  if (is.null(dbPath)) {
    warning("Please provide database path.")
  }
  # TODO: Determine how to make connection 'Read-Only'. I have a fear
  # that with an open connection some hapless user could muggle up
  # the database with a call to DBI::dbRemove or something.
  # make sure the file exists before attempting to connect
  if (!file.exists(dbPath)) {
    warning("DB file does not exist at ", dbPath, "")
  }
  # Create connection strings
  dbq_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbPath)

  con <- DBI::dbConnect(odbc::odbc(),
    .connection_string = dbq_string
  )
  return(con)
}




#' set_get_stations
#'
#' Retrieve SET-MH stations from database as an sf object. conversion of coordinates on the fly
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return sf object containing SET-MH stations. Includes survey elevation data as well.
#' @export
#' @examples
#' # studyStations <- set_get_DB(path) %>% set_get_stations()

set_get_stations <- function(dbconn) {
  if (!dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get_* operations.")
  }
  # Connect to tables containing stations and site information. Munge here instead of bringing in to R env.
  Sites <- dbconn %>%
    dplyr::tbl("tbl_Sites") %>%
    dplyr::select(-GIS_Location_ID)
  Locations <- dbconn %>%
    dplyr::tbl("tbl_Locations") %>%
    dplyr::select(-Unit_Code, -Sub_Unit_Code)
  # TODO: Elevation surveys will be conducted repeatedly through time so need to account for multiple measures on
  # at the same stations. Determine survey summary method to use (mean, 'best', etc)
  # TODO: Add a filter/query method to return only sites or projects of interest.
  Elevations <- dbconn %>% tbl("tbl_Survey_Data")

  StudyStations <- Sites %>%
    dplyr::left_join(Locations, by = "Site_ID") %>%
    dplyr::left_join(Elevations, by = "Location_ID") %>%
    dplyr::collect() # execute query and get results.
  # Sites and Locations will be joined in munge by 'site_ID'
  # Use capwords function to standardize caps and revert Stratafication to as a factor
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "", collapse = " "
      )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

  StudyStations <- StudyStations %>% dplyr::mutate(Stratafication = as.factor(capwords(Stratafication)))


  # Convert to sf object.
  StudyStations <- StudyStations %>% sf::st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = 4326) # make sf


  return(StudyStations)
}

#' set_get_readers
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SET readers for projects.
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE

set_get_readers <- function(dbconn) {
  if (!dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get_* operations.")
  }
  # Events, Contacts and SET readers:
  Events <- dbconn %>% dplyr::tbl("tbl_Events")
  Contacts <- dbconn %>%
    dplyr::tbl("tlu_Contacts") %>%
    dplyr::select(Contact_ID, Last_Name, First_Name, Organization) %>%
    dplyr::mutate(FullName = paste(First_Name, Last_Name, sep = " "))
  EventContacts <- dbconn %>% dplyr::tbl("xref_Event_Contacts")
  SET_readers <- EventContacts %>%
    dplyr::left_join(Contacts, by = "Contact_ID") %>%
    dplyr::select(Event_ID, Contact_ID, Contact_Role, Last_Name, First_Name, Organization, FullName) %>%
    dplyr::collect() %>%
    dplyr::mutate(ID = paste(Event_ID, Contact_ID, sep = "_")) %>%
    tidyr::spread(key = Contact_Role, value = FullName) %>%
    dplyr::rename(SET_Reader = `SET Reader`) %>%
    dplyr::select(Contact_ID, Event_ID, Last_Name, First_Name, Organization, SET_Reader) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::group_by(Last_Name, First_Name, Organization, SET_Reader, Contact_ID) %>%
    dplyr::tally() %>% dplyr::arrange(desc(n)) %>% dplyr::rename(`SET measures` = n)

  return(SET_readers)
}

#' set_get_samplingevents
#'
#' Retrieve SET reading events or sampling information.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing sampling event information for SET readings
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE

set_get_samplingevents <- function(dbconn){
  if (!dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get_* operations.")
  }

  # Events, Contacts and SET readers:
  Events <- dbconn %>% dplyr::tbl("tbl_Events") %>% dplyr::collect()
  Contacts <- dbconn %>%
    dplyr::tbl("tlu_Contacts") %>%
    dplyr::select(Contact_ID, Last_Name, First_Name, Organization) %>%
    dplyr::mutate(FullName = paste(First_Name, Last_Name, sep = " "))
  EventContacts <- dbconn %>% dplyr::tbl("xref_Event_Contacts")
  SET_readers <- EventContacts %>%
    dplyr::left_join(Contacts, by = "Contact_ID") %>%
    dplyr::select(Event_ID, Contact_ID, Contact_Role, Last_Name, First_Name, Organization, FullName) %>%
    dplyr::collect() %>%
    dplyr::mutate(ID = paste(Event_ID, Contact_ID, sep = "_")) %>%
    tidyr::spread(key = Contact_Role, value = FullName) %>%
    dplyr::rename(SET_Reader = `SET Reader`) %>%
    dplyr::select(Contact_ID, Event_ID, Last_Name, First_Name, Organization, SET_Reader) %>%
    dplyr::filter(complete.cases(.))



  StudyStations <- set_get_stations(dbconn)
  Samplings <- dplyr::inner_join(StudyStations, Events, by= "Location_ID")
  Samplings <- dplyr::inner_join(Samplings, SET_readers, by = "Event_ID")

  return(Samplings)
}


#' set_get_sets
#'
#' return a tidy, long form, tibble of SET data.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SET data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'
set_get_sets <- function(dbconn) {
  if (!dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get operations.")
  }


  # Connect to tables containing set data. Munge here instead of bringing in to R env.
  SET_data <- dbconn %>% dplyr::tbl("tbl_SET_Data")
  SET_positions <- dbconn %>% dplyr::tbl("tbl_SET_Position")
  SET_readers <- set_get_readers(dbconn)
  SET_samplings <- set_get_samplingevents(dbconn)

  # Use capwords function to standardize caps and revert Stratafication to as a factor
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
      s <- substring(s, 2)
      if (strict) tolower(s) else s
    },
    sep = "", collapse = " "
    )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }


  # SET Rod data
  SET <- dplyr::inner_join(SET_data, SET_positions, by = "Position_ID") %>% dplyr::collect()
  # TODO: finish the munging steps in here to output the long format SET data with associated reader info.
  SET.data <- inner_join(SET, SET_samplings, by="Event_ID")
  # Munge
  SET.data <- SET.data %>%
    dplyr::select(
      Pin1:Pin9_Notes,
      Arm_Direction,
      Site_Name,
      SET_Type,
      Stratafication:Plot_Name,
      Location_ID.x,
      Position_ID,
      Start_Date,
      Organization,
      SET_Reader
    ) %>%
    dplyr::mutate(Stratafication = capwords(as.character(Stratafication)),
                  Start_Date = as.Date(Start_Date))     # Eventually add a filter that will filter out only 'clean' readings

  SET.data.long <- SET.data %>%
    dplyr::select(
      Site_Name,
      Stratafication,
      Plot_Name,
      SET_Type,
      Pin1:Pin9_Notes,
      Arm_Direction,
      Location_ID.x,
      Position_ID,
      Start_Date,
      SET_Reader
    ) %>%
    dplyr::group_by(Position_ID, Start_Date) %>%
    tidyr::gather(pin, measure, Pin1:Pin9_Notes) %>%
    dplyr::filter(!is.na(measure)) %>% # Remove NA from PinX_Notes
    tidyr::separate(pin, c('name', 'note'), "_", remove = TRUE) %>%
    tidyr::separate(name, c('name', 'Pin_number'), 3, remove = TRUE) %>%
    dplyr::mutate(key = ifelse(is.na(note), yes = "Raw", no = note)) %>%
    dplyr::select(-note,-name) %>%
    tidyr::spread(key, measure) %>%
    dplyr::mutate(pin_ID = paste(Position_ID, Pin_number, sep = "_")) %>% # Above all transposing and repositioning dataframe.
    dplyr::ungroup() %>% # Below- adding columns, renaming variables, and reordering rows.
    dplyr::rename(Date = Start_Date, Location_ID = Location_ID.x) %>%  # rename SET reading date
    dplyr::group_by(pin_ID) %>% # group by pinID to
    dplyr::mutate(EstDate = min(Date)) %>%  # create a column identifying the EstDate (date of the first SET-MH station reading)
    dplyr::ungroup() %>%
    dplyr::mutate(DecYear = round((((
      as.numeric(difftime(.$Date, .$EstDate, units = "days"))
    )) / 365), 3)) %>%
    dplyr::mutate(Raw = as.numeric(Raw)) %>%
    dplyr::filter(!is.na(Raw))

  SET.data.long <- SET.data.long %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pin_ID) %>% # reinforce that the grouping is based on pins
    dplyr::arrange(Date) %>%
    dplyr::mutate(Change = as.numeric(Raw) - as.numeric(Raw[1])) %>%
    dplyr::mutate(incrementalChange = c(NA, diff(Change)))

  attr(SET.data.long, 'Datainfo') <-"Full SET dataset including all measures in a LONG format" # give dataframe some metadata attributes
  attr(SET.data.long, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')


  return(SET.data.long)
}

#' set_get_accretions
#'
#' return a tidy, long form, tibble of Surface Accretion (SA) data.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SA data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'

set_get_accretions <- function(dbconn){
  if (!dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get operations.")
  }

  # Surface Accretion data
  SAccret <- dbconn %>% dplyr::tbl("tbl_Accretion_Data")
  SA_Layers <- dbconn %>% dplyr::tbl("tbl_Feldspar_Layers")
  SA <- inner_join(SA_Layers, SAccret, by="Layer_ID") %>% dplyr::collect()

  # Connect to tables containing set data. Munge here instead of bringing in to R env.

  # SET_readers <- set_get_readers(dbconn)
  SET_samplings <- set_get_samplingevents(dbconn)

  SA.data <- inner_join(SA, SET_samplings %>% dplyr::select(-Location_ID), by="Event_ID")



  SA.data.long <- SA.data %>%
    tidyr::gather('measure', 'Accretion', Measure_1:Measure_6) %>%
    dplyr::filter(!is.na(Accretion)) %>%
    dplyr::select(Layer_ID, Layer_Label, Location_ID, Estab_Date, Start_Date, Accretion, Core_Type, Site_ID, Site_Name, Plot_Name, Organization) %>%
    dplyr::mutate(DecYear = round((((as.numeric(difftime(Start_Date, Estab_Date, units = "days"))))/365),3)) %>%
    dplyr::rename(Date = Start_Date)

  attr(SA.data.long, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')

  return(SA.data.long)


}
