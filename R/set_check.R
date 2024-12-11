# set_check_*
# Family of functions for QA of SET-MH data returns lists of measurements that require further investigation.
#
# set_check_notes = generate all the notes used in the dataaset. Use this to determine what to flag for removal
# set_check_measures = check for large changes in measures which may indicate data entry or misreading
# set_check_pins = checks for pins that landed on obstacles to the marsh surface (holes, mussels, etc)

# set_check_readers = check SET reader consistency across events.
#
#
#
#
#


#' check field notes for potential issues relating to pin readings
#'
#' Extract all the notes that have been made which can indicate pins that may have measurement bias.
#' This information can be used to generate a list of flags that can act as a filter for analysis
#' further in the workflow. This allows the removal of certain data points from the analysis without
#' removing the datapoint from the dataset. Secondarily these flags can be used in QA/QC workflows
#'
#' @param dataSET  SET dataset from get_set_sets()
#'
#' @return character vector of unique notes made within data set.
#' These notes can be edited and recycled for flagging measures [(set_check_measures)]
#' or flagging pins [(set_check_pins)]. Returned as a `dput` output for easy editing and
#' use elsewhere.
#' @export
#'
#' @examples pins_to_drop <- set_check_notes(set_data)
#'
set_check_notes <- function(dataSET){
  notes <- dataSET %>% dplyr::ungroup() %>%
    dplyr::select(Notes) %>% tidyr::drop_na() %>%
    unique() %>% dplyr::pull()
  dput(notes)
}


#' set_check_pins
#'
#' returns a vector of pin_ID's that had recorded a note consisting of an
#' 'issue' i.e. mussel hole, or grass tuft. These are pins that, at any
#' point in the data set, had an issue noted. This vector can be used in
#' filtering pins from inclusion in analysis.
#'
#' @param dataSET
#' @param issues Defaults to some common note flags that have been used, (hole, mussel, mussel hole)
#' @param ... additional character strings of notes to flag.
#'
#' @return tibble containing SET data in long format. This tibble must have a "Notes" column to operate properly.
#' @export
#' @examples
#'
#'
set_check_pins <- function(dataSET, issues = c("Hole", "hole", "mussel", "Holr", "Shell", "Mussel", "edge of hole", "hole next to mussel"), ...){
  issues <- c(issues, ...) # add new issue notes if needed.

  troublePins <- dataSET %>%
    dplyr::ungroup() %>%
    dplyr::select(Notes, pin_ID) %>%
    dplyr::filter(Notes %in% issues)  # remove all pins that don't have a note flagged in 'issues'.


  pinlistClean <- unique(troublePins$pin_ID) %>% `attr<-`("Datainfo", "List of pins that have reported issues (holes, etc)")

  issuePins <- troublePins %>% dplyr::filter(Notes %in% issues)
  issuePins

}



#' set_check_measures
#'
#' returns a list of pins that have issues indicated in the notes or has
#' suspicious incremental changes. These are individual measrements and differ from
#' set_check_pins which returns pin_id. Enhanced further with set_check_change function.
#'
#' @param dataSET SET data from set_get_sets
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
#' @examples set_check_change(dataSET = NULL, duration = "3 months", mm_change = 5) # > 5 mm change over 3 months will

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
    ) %>% dplyr::select(Site_Name:Arm_Direction, Date:issuePin, chng_thresh, flag_change)

  if(drop_rows) {
    SET_data <- SET_data %>% dplyr::filter(flag_change)}
  else{
    SET_data <- SET_data
  }


  attr(SET_data, 'Datainfo') <-"Tibble of non-numeric recorded values" # give dataframe some metadata attributes
  attr(SET_data, 'Date of data retreival') <- format(lubridate::today(), '%b %d %Y')

  return(SET_data)

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
#'
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

