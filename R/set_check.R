# set_check_*
# Family of functions for QA of SET-MH data returns helpful messages on QA concerns.
#
#' set_check_measures = check for large changes in measures and flag
#' set_check_pins = checks for pins that landed on obstacles to the marsh surface (holes, mussels, etc)
#' set_check_notes =
#' set_check_readers = check SET reader consistency across events.
#'
#'
#'
#'
#' TODO: Need to figure out how to address this is cases when readers are inconsistent.
#'
#'

#' set_check_measures
#'
#' returns a list of pins that have issues indicated or suspicious incremental changes. Enhanced
#' further with set_check_change function.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SET data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'

set_check_measures <- function(dataSET){
  # create tibble of potential problem pins with date, site, pin info for QA checks.
  probs <- set_check_pins(dataSET)

  SET_data <- dataSET %>%
    dplyr::mutate(bigIssuePin = pin_ID %in% probs$pin_ID) %>% # Add in a column indicating if that pin is on the list of issues
    dplyr::filter(bigIssuePin == FALSE, Date != '2008-08-08') %>% # Remove initial readings from AH as they were throwing errors and erraneous rates.
    dplyr::group_by(pin_ID) %>% # reinforce that the grouping is based on pins
    dplyr::arrange(Date) %>%
    dplyr::mutate(Change = as.numeric(Raw) - as.numeric(Raw[1]),
           incrementalChange = c(NA, diff(Change))) %>%
    dplyr::select(Site_Name, Plot_Name, Arm_Direction, Pin_number, Date, SET_Reader, Raw, incrementalChange, issuePin, bigIssuePin)

  return(SET_data)

}

#' set_check_pins
#'
#' returns a tibble of measures that had recorded a note consisting of an
#' 'issue' i.e. mussel hole, or grass tuft. Allows the filtering of these pins
#' in analysis downstream in set_clean_pins().
#'
#' @param dataSET
#' @param issues Defaults to some common note flags that have been used, (hole, mussle, mussel hole)
#' @param ... additional character strings of notes to flag.
#'
#' @return tibble containing SET data in long format. This tibble must have a "Notes" column to operate properly.
#' @export
#' @examples
#'
#'
set_check_pins <- function(dataSET, issues = c("Hole", "hole", "mussel", "Holr", "Shell", "Mussel", "edge of hole", "hole next to mussel"), ...){
  issues <- c(issues, ...) # add new issue notes if needed.

  troublePins <- dataSET %>% dplyr::ungroup() %>%
    dplyr::select(Notes, pin_ID) %>% # TODO: make this more flexible to allow for other 'comment/note' fields.
    dplyr::filter(complete.cases(.)) %>% # remove all pins that don't have a note.
    `attr<-`("Datainfo", "List of pins that have reported issues (holes, etc)")

  pinlistClean <- unique(troublePins$pin_ID)

  issuePins <- troublePins %>% dplyr::filter(Notes %in% issues)
  issuePins

}


#' set_check_notes
#' Used to check what notes have been made which can indicate pins that may have measurement bias.
#' This information can be used to generate a list of flags that can act as a filter for analysis
#' further in the workflow. This allows the removal of certain data points from the analysis without
#' removing the datapoint from the dataset. Secondarily these flags can be used in QA/QC workflows
#'
#' @param dataSET  SET dataset from get_set_sets()
#'
#' @return character vector of unique notes made within data that is p
#' @export
#'
#' @examples
#'
set_check_notes <- function(dataSET){
  notes <- dataSET %>% dplyr::ungroup() %>%
    dplyr::select(Notes) %>% tidyr::drop_na() %>%
    unique() %>% dplyr::pull()
  notes
}



#' Check set data for potential biases in SET reader through a graphical and
#' optional tabular format
#' @description Used in conjunction with set_get_doublereads
#' @param dataSET  SET data as returned from set_get_sets
#'
#' @return `ggplotly` interactive plot
#' @export
#'
#' @examples
set_check_doublereads <- function(dataSET){
  dat <- set_get_doublereads(dataSET)

  dat <- dat %>%
    mutate(urdid = paste(pin_ID, Date, sep = "_")) %>%
    dplyr::select(urdid, SET_Reader, Raw, Date, Pin_number, Plot_Name, issuePin) %>%
    dplyr::group_by(urdid, Date) %>%
    # create a coded value for each SET reader for comparisons by
    # removing names which can be replicated over time
    # (i.e. staff stops reading part way and then resumes at a later date.)
    dplyr::mutate(reader = letters[seq_along(urdid)]) %>%
    dplyr::select(-SET_Reader) %>%
   tidyr::pivot_wider(# id_cols = optional vector of unaffected columns,
                names_from = c(reader),
                values_from = c(Raw),
                names_sep = "_"
   ) %>% dplyr::mutate(diff = a - b)

  plotly::ggplotly({
    dat %>% ggplot(aes(Plot_Name, diff, color = abs(diff), label = Date)) +
      geom_violin(alpha = .5) +
      geom_jitter(size = 2) +
      # facet_wrap( ~ Plot_Name) +
      scale_color_viridis_c() +
      theme_minimal() +
      labs(title = "Double Read SET measures",
           caption = "QA check for potential errors or misreads.",
          color = "Difference in readings")
  })

}


#' set_check_recorded_vals
#'
#' @param dbconn Connection to Database returned from set_get_db.
#'
#' @description This function requires a connection to the DB compared to the
#' other functions that take the SET data due to the methods used for converting
#' character values to numerics.
#' @return tibble with a flag column appended that is used in filtering out
#' potential errant values.
#' @export
#'
#' @examples
#'
set_check_recorded_vals <- function(dbconn) {
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

  SET.data <- dplyr::inner_join(SET, SET_samplings, by="Event_ID")
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
    dplyr::mutate(key = ifelse(is.na(note), yes = "Raw", no = note)) %>%
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
    )) / 365), 3))

  pins <- set_check_pins(SET.data.long) # change the approach to give a message saying that there are issues with some pins as ided in set_check_pins


  SET.data.long <- SET.data.long %>%
    dplyr::ungroup() %>%
    dplyr::mutate(raw_num = as.numeric(str_extract(Raw, "\\-*\\d+\\.*\\d*")),
                  Raw = as.numeric(Raw),
                  flag = dplyr::case_when(!is.numeric(raw_num) ~ "non-numeric",
                                          pin_ID %in% pins$pin_ID ~ Notes,
                                          raw_num != Raw ~ "non-numeric value entered",
                                          TRUE ~ NA_character_)) %>%  # everything else gets a NA
    dplyr::filter(!is.na(flag)) %>%
    dplyr::select(Site_Name:Arm_Direction, Date, SET_Reader, Pin_number, Notes, raw_num, Raw, flag)

  attr(SET.data.long, 'Datainfo') <-"Tibble of non-numeric recorded values" # give dataframe some metadata attributes
  attr(SET.data.long, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')


  return(SET.data.long)

  }

#' set_check_change
#' Calculates the rate of change from provided change in time and change in pin height.
#' Used for flagging and catching potential errors. Replaces the more simple incremental change
#' approach with one that allows for larger gaps in the data. Perhaps also catches more potential
#' issues for readings that occurred more closely together.
#'
#' @param duration character string of the time interval to use for threshold calculation; "1 year" default.
#' Leverages `lubridate` capability to parse duration so values such as "1 week" and "10 months" are acceptable.
#' @param mm_change Change in pin height over the duration provided. Values greater than this (within the specified
#' duration) will be given a flag.
#' @param dataSET SET data as returned by set_get_sets.
#' @param drop_rows TRUE, drops rows that don't meet the flag criteria. To return the full dataset, with
#' an appended column of flag set to FALSE. Defaults to FALSE to protect unwanted removal.
#'
#' @return tibble of SET data that's been trimmed down to show only measures that were made that fell above the
#' the treshold passed in the function call.
#' @export
#'
#' @examples set_check_change(duration = "3 months", mm_change = 5) # > 5 mm change over 3 months will
set_check_change <- function(dataSET, duration = "1 year", mm_change = 20, drop_rows = FALSE){

  dec_year <-  lubridate::duration(duration)/lubridate::dyears(1)
  threshold <- mm_change / dec_year

  SET_data <-
    dataSET %>% dplyr::mutate(
      chng_thresh = (incrementalChange / incrementalTime),
      flag_change = dplyr::case_when(
        chng_thresh == -Inf ~ FALSE,
        chng_thresh == Inf ~ FALSE,
        chng_thresh > threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% dplyr::select(Site_Name:Arm_Direction, Date:issuePin, flag_change)

  if(drop_rows) {
    SET_data <- SET_data %>% dplyr::filter(flag_change)}
  else{
    SET_data <- SET_data
  }


  attr(SET_data, 'Datainfo') <-"Tibble of non-numeric recorded values" # give dataframe some metadata attributes
  attr(SET_data, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')

  return(SET_data)

}
