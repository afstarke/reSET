# set_check_*
# Family of functions for QA of SET-MH data.
# set_check_readers = check SET reader consistency across events.
#   Need to figure out how to address this is cases when readers are inconsistent.
# set_check_measures = check for large changes in measures

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
    dply::mutate(bigIssuePin = pin_ID %in% probs$pin_ID) %>% # Add in a column indicating if that pin is on the list of issues
    dply::filter(bigIssuePin == FALSE, Date != '2008-08-08') %>% # Remove initial readings from AH as they were throwing errors and erraneous rates.
    dply::group_by(pin_ID) %>% # reinforce that the grouping is based on pins
    dply::arrange(Date) %>%
    dply::mutate(Change = as.numeric(Raw) - as.numeric(Raw[1]),
           incrementalChange = c(NA, diff(Change))) %>%
    select()

}

#' set_check_pins
#'
#' returns a tibble of measures that had recorded 'issue' i.e.
#' mussel hole, or grass tuft. Allows the filtering of these pins
#' in analysis downstream in set_clean_pins().
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SA data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'
set_check_pins <- function(dataSET, issues = c("Hole", "hole", "mussel", "Holr", "Shell", "Mussel", "edge of hole", "hole next to mussel"), ...){
  issues <- c(issues, ...) # add new issue notes if needed.
  troublePins <- dataSET %>% dplyr::ungroup() %>%
    dplyr::select(Notes, pin_ID) %>%
    dplyr::filter(complete.cases(.)) %>% # remove all pins that don't have a note.
    `attr<-`("Datainfo", "List of pins that have reported issues (holes, etc)")

  pinlistClean <- unique(troublePins$pin_ID)

  issuePins <- troublePins %>% filter(Notes %in% issues)
  # TODO: decide on returning a vector? or a tibble?
  issuePins

}


#' set_check_notes
#' Used to check what notes have been made which can indicate pins that may have measurement bias
#'
#' @param dataSET  SET dataset from get_set_sets()
#'
#' @return
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
