#' SET_get_* functions that fetch data from provided database.
#'
# TODO: create set_get_xx functions to fetch the following
# DONE: set_get_DB make a data base connection.
# IDEA: database info, location, perhaps type of database? For future expansion to other platforms.
# DONE: set_get_stations return sf object with meta data on
# the station including elevation?
# set_get_readers returns basic dataframe of SET readers
# DONE: set_get_samplingevents
# DONE: set_get_sets
# DONE: set_get_accretions
# set_get_pinlengths
# set_get_receiver_elevations
# set_get_absolute_heights


#' Get connected to from Access DB from which
#'
#' This function connects and returns information relating to the SET-MH database.
#' Note: There can (will) be compatibility issues with the bit-versions of R and Access.
#' The AccessDB that this function was designed around was built on a 32-bit version.
#' The R bit-version must match to successfully connect so can be an issue.
#' There is a work around available which allows both bit-versions to be installed side-by-side
#' more info here: https://knowledge.autodesk.com/support/autocad/learn-explore/caas/sfdcarticles/sfdcarticles/How-to-install-64-bit-Microsoft-Database-Drivers-alongside-32-bit-Microsoft-Office.html
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
  # TODO: Build this out to be flexible with database. Will need to create some
  # method to understand the DB and how to translate the wrangling methods
  dbq_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", dbPath)

  con <- DBI::dbConnect(drv = odbc::odbc(),
    .connection_string = dbq_string
  )
  attr(con, which = "File last updated") <- file.info(dbPath)$mtime
  return(con)
}




#' set_get_stations
#'
#' Retrieve SET-MH stations from database as an sf object. conversion of coordinates on the fly
#'
#' @param dbconn Connection to Database returned from set_get_db
#' @param epsg desired coordinate system of the output using epsg code.
#'
#' @return sf object containing SET-MH stations. Includes survey elevation data for sites collected.
#' @export
#' @examples
#' # studyStations <- set_get_DB(path) %>% set_get_stations()

set_get_stations <- function(dbconn, epsg = 4269) {
  if (!DBI::dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get_* operations.")
  }
  # Connect to tables containing stations and site information. Munge here instead of bringing in to R env.
  Sites <- dbconn %>%
    dplyr::tbl("tbl_Sites") %>%
    dplyr::select(-GIS_Location_ID)
  Locations <- dbconn %>%
    dplyr::tbl("tbl_Locations") %>%
    dplyr::select(-Unit_Code, -Sub_Unit_Code)
  # TODO: Elevation surveys will be conducted repeatedly through time so need to account for
  # multiple measures on/at the same stations. Determine survey summary method to use (mean, 'best', etc)
  # COMBAK: Fixed temporarily by entering the 'best available' survey data into the survey datasheet.
  # Avoided some complexities and nuances with survey method variations across sites.
  # NOTE: that multiple surveys will also provide multiple coordinate locations as well.

  # TODO: Determine how to handle elevation surveys. There's 2 locations for this data entry-
  # 1 in the survey form and 1 in the site location data.
  # Could pull all data from survey forms and average elevations and height- or do
  # that internally in the DB and then pull that value from station info.
  # Elevations <- dbconn %>% tbl("tbl_Survey_Data")

  StudyStations <- Sites %>%
    dplyr::left_join(Locations, by = "Site_ID") %>%
    # dplyr::left_join(Elevations, by = "Location_ID") %>%
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

  # Convert to sf object.
  StudyStations <- StudyStations %>%
    # make sf but note that the epsg isn't neccesarily correct for all stations since
    # some were collected in WGS and some in NAD88.
    sf::st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = epsg) %>%
    dplyr::mutate(Stratafication = as.factor(capwords(Stratafication)),
                  SET_Established_Date = as.Date(SET_Established_Date))

  reprojected_stations <- StudyStations %>%
    group_by(Coord_System, Datum, UTM_Zone, Coord_Units) %>%
    nest_by(.key = 'stations') %>%
    mutate(wkt = gdalraster::srs_to_wkt(Datum)) %>%
    rowwise() %>%
    dplyr::mutate(
      projected_stations = list(sf::st_set_crs(x = stations, sf::st_crs(wkt))),
      reprojected_stations = list(sf::st_transform(x = projected_stations, epsg))) %>%
    dplyr::select(reprojected_stations) %>%
    tidyr::unnest(reprojected_stations) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(epsg)




  attr(reprojected_stations, which = "File last updated") <- attr(dbconn, which = "File last updated")

  return(reprojected_stations)
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
  if (!DBI::dbIsValid(dbconn)) {
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

  attr(SET_readers, which = "File last updated") <- attr(dbconn, which = "File last updated")
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
  if (!DBI::dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get_* operations.")
  }

  # Events, Contacts and SET readers:
  Events <- dbconn %>% dplyr::tbl("tbl_Events") %>% dplyr::collect()
  Contacts <- dbconn %>%
    dplyr::tbl("tlu_Contacts") %>%
    dplyr::select(Contact_ID, Last_Name, First_Name, Organization) %>%
    dplyr::mutate(FullName = paste(First_Name, Last_Name, sep = " ")) %>% collect()
  EventContacts <- dbconn %>% dplyr::tbl("xref_Event_Contacts") %>% collect() # sampling events with contact id names and roles.
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
  Samplings <- dplyr::left_join(StudyStations, Events, by= "Location_ID")
  Samplings2 <- dplyr::left_join(Samplings, SET_readers, by = "Event_ID")

  attr(Samplings2, which = "File last updated") <- attr(dbconn, which = "File last updated")

  return(Samplings2)
}


#' set_get_sets
#'
#' return a tidy, long form, tibble of raw SET measurement data.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SET data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'
set_get_sets <- function(dbconn, ...) {
  if (!DBI::dbIsValid(dbconn)) {
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
  # Join the measured SET pin data to the positions to convert Position_ID to a arm direction
  SET <- dplyr::left_join(SET_data, SET_positions, by = "Position_ID") %>% dplyr::collect()
  # TODO: finish the munging steps in here to output the long format SET data with associated reader info.
  SET.data <- dplyr::left_join(SET, SET_samplings, by="Event_ID")
  # Munge
  # BUG: There's a set of duplicated values being introduced in here somewhere. Presumably by an indirect join with the Survey table
  SET.data1 <- SET.data %>%
    dplyr::select(
      Pin1:Pin9_Notes,
      Arm_Direction,
      Site_Name,
      SET_Type,
      Unit_Type,
      Stratafication:Plot_Name,
      Location_ID.x,
      Position_ID,
      Start_Date,
      Organization,
      SET_Reader
    ) %>%
    # reformat to clean up and set class appropriately
    dplyr::mutate(Stratafication = capwords(as.character(Stratafication)),
                  Start_Date = as.Date(Start_Date))

  SET.data.long <- SET.data1 %>%
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
    tidyr::separate(pin, c('name', 'note'), "_", remove = TRUE, fill = "right") %>%
    tidyr::separate(name, c('name', 'Pin_number'), 3, remove = TRUE) %>%
    dplyr::mutate(key = ifelse(is.na(note), yes = "Raw", no = note),
                  Pin_number = as.numeric(Pin_number)) %>%
    dplyr::select(-note,-name) %>%
    dplyr::group_by(Position_ID, Start_Date) %>% distinct() %>%
    tidyr::spread(key = key, value = measure) %>%
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

  pins <- set_check_pins(SET.data.long, issues = ...) # change the approach to give a message saying that there are issues with some pins as ided in set_check_pins
  SET.data.long <- SET.data.long %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pin_ID) %>% # reinforce that the grouping is based on pins
    dplyr::arrange(Date) %>%
    dplyr::mutate(Change = as.numeric(Raw) - as.numeric(Raw[1])) %>%
    dplyr::mutate(incrementalChange = c(NA, diff(Change))) %>%
    dplyr::mutate(incrementalTime = DecYear - dplyr::lag(DecYear, n = 1)) %>%
    dplyr::mutate(issuePin = pin_ID %in% pins$pin_ID)

  attr(SET.data.long, 'Datainfo') <-"Full SET dataset including all measures in a LONG format" # give dataframe some metadata attributes
  attr(SET.data.long, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')
  attr(SET.data.long, which = "File last updated") <- attr(dbconn, which = "File last updated")


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
  if (!DBI::dbIsValid(dbconn)) {
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
  attr(SA.data.long, which = "File last updated") <- attr(dbconn, which = "File last updated")

  return(SA.data.long)


}


#' set_get_pinlengths
#'
#' @param pin_numb numeric pin number most often passed in from an assigned
#'   column name.
#' @param pin_table named vector pin number and pin length combinations, name
#'   being numeric pin number.
#'
#' @return numeric pin length in mm
#' @export
#'
#' @examples


set_get_pinlengths <- function(pin_numb, pin_table){
  #TODO: add standard checks in R functions. Not sure this is the best method
  if(!is.numeric(pin_numb)){
    warning("Provide valid pin number")
  }
  # TODO: Need to find a good way to create named vector of pin lengths.
  return(pin_table[[pin_numb]])
}


#' set_get_receiver_elevations
#'
#' @description  Retrieve deep SET-rod receiver heights as measured at the top of
#'   the receiver. NAVD88.
#' @param plotID
#'
#' @return sf object of surveyed plots with Lat/Lon, NAVD88 elevations, date
#'   surveyed. Used for mapping or correcting raw SET measures to NAVD88
#'   elevations.
#' @export
#'
#' @examples
#'
set_get_receiver_elevations <- function(dbconn){
  if (!DBI::dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get operations.")
  }
  # TODO: determine how to incorporate the elevation survey data in this. Mean heights? Best measure? Where does that value get stored?
  surveys <- dbconn %>% dplyr::tbl("tbl_Survey_Data") %>%
    dplyr::left_join(tbl(dbconn, "tbl_Locations")) %>%
    dplyr::select(Survey_Date, Plot_Name, starts_with("Pipe"), Vertical_Datum, Plot_Name, "X_Coord", "Y_Coord") %>%
    dplyr::collect() %>%
    sf::st_as_sf(coords = c("Pipe_X", "Pipe_Y")) %>% sf::`st_crs<-`(6538)

  attr(surveys, which = "File last updated") <- attr(dbconn, which = "File last updated")

  return(surveys)

}

#' set_get_absolute_heights adjust the measured raw pin height to NAVD88
#' elevation using data collected using static or RTK methods.
#'
#' @param pin_height numeric raw pin height (in mm) as measured in the field on
#'   the SET arm
#' @param pin_numb Pin number associated with the measured height recorded
#'   (pin_height). Pins are uniquely identified to eliminate variation between
#'   readings of the SET
#' @param pin_table pin height table, created by user @seealso
#'   \code{\link{set_get_pinlengths}}
#' @param SETarmHt Distance (height in mm) from the receiver end to the top of
#'   the SET arm where pin heights are measured from.
#' @param receiverHt NAVD88 elevation of the SET receiver in meters
#'
#' @return numeric value representing the elevation of the marsh surface at the
#'   location of the SET pin measure.
#' @export
#'
#' @examples
set_get_absolute_heights <- function(pin_height, pin_numb, pin_table, SETarmHt, receiverHt){
  pinLgth <- set_get_pinlengths(pin_numb = pin_numb, pin_table = pin_table)
  absHt <- (receiverHt + (SETarmHt/1000) + (pin_height/1000)) - (pinLgth/1000)
  return(absHt)
}

#' TODO: Redefine the intent behind the set_get_doublereads. Alternative
#' approach would be to just correct all data. ' Check data for double reads. '
#' Investigate if and when a double read of a SET occurs. The result can be used
#' to ' make adjustments to data and provide some adjustments to the long-term
#' dataset as needed.
#' @param dataSET
#' @return tibble with data containing double reads.
#' @export
#' @examples
#'
set_get_doublereads <- function(dataSET){
  # urdid unique date read ID
  doubleids <- dataSET %>%
    dplyr::mutate(urdid = paste(pin_ID, Date, sep = "_")) %>%
    dplyr::group_by(urdid) %>% dplyr::tally() %>%
    dplyr::filter(n > 1) %>% dplyr::pull(urdid)

  dataSET %>%
    dplyr::mutate(urdid = paste(pin_ID, Date, sep = "_")) %>%
    dplyr::filter(urdid %in% doubleids)

}
