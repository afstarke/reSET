# set_check_*
# Family of functions for QA of SET-MH data returns helpful messages on QA concerns.
# set_check_readers = check SET reader consistency across events.
#   Need to figure out how to address this is cases when readers are inconsistent.
# set_check_measures = check for large changes in measures and flag

#' set_check_measures
#'
#' returns a list of pins that have issues indicated or suspicious incremental changes.
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
    dplyr::select(Site_Name, Plot_Name, Arm_Direction, Pin_number, Date, SET_Reader, Raw, IncrementalChange, issuePin, bigIssuePin)

  return(SET_data)

}

#' set_check_pins
#'
#' returns a tibble of measures that had recorded 'issue' i.e.
#' mussel hole, or grass tuft. Allows the filtering of these pins
#' in analysis downstream in set_clean_pins().
#'
#' @param dataSET
#' @param issues Defaults to some common note flags that have been used, (hole, mussle, mussel hole)
#' @param ... additional character strings of notes to flag.
#'
#' @return tibble containing SA data in long format. This tibble must have a "Notes" column to operate proprerly.
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

  issuePins <- troublePins %>% filter(Notes %in% issues)
  # TODO: decide on returning a vector? or a tibble?
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
  notes <- dataSET %>% ungroup() %>%
    select(Notes) %>% drop_na() %>%
    unique() %>% pull()
  notes
}

#' Check set data for potential biases in SET reader through a
#' @description Used in conjunction with set_get_doublereads
#' @param dataSET  Double read data, typically output from set_get_doublereads
#'
#' @return
#' @export
#'
#' @examples
set_check_doublereads <- function(dataSET){
  # find where double reads occur.
  doubleids <- dataSET %>%
    mutate(urdid = paste(pin_ID, Date, sep = "_")) %>% # create an id unique to a pin and sample date combination
    group_by(urdid) %>% tally() %>% # count how many readings a specific pin received on a single date.
    filter(n > 1) %>% pull(urdid)

  dataSET %>%
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

}


