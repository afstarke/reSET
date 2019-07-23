# set_check_*
# Family of functions for QA of SET-MH data.
# set_check_readers = check SET reader consistency across events.
# set_check_measures = check for large changes in measures

#' set_check_measures
#'
#' returns a list of .
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return tibble containing SA data in long format
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
#'

set_check_measures <- function(dataSET){
  # create tibble of potential problem pins.
  probs <- set_check_pins(dataSET)

  SET_data <- dataSET %>%
    mutate(bigIssuePin = pin_ID %in% probs$pin_ID) %>% # Add in a column indicating if that pin is on the list of issues
    filter(bigIssuePin == FALSE, Date != '2008-08-08') %>% # Remove initial readings from AH as they were throwing errors and erraneous rates.
    group_by(pin_ID) %>% # reinforce that the grouping is based on pins
    arrange(Date) %>%
    mutate(Change = as.numeric(Raw) - as.numeric(Raw[1]),
           incrementalChange = c(NA, diff(Change)))

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
set_check_pins <- function(dataSET){
  troublePins <- dataSET %>% dplyr::ungroup() %>%
    dplyr::select(Notes, pin_ID) %>%
    dplyr::filter(complete.cases(.)) %>% # remove all pins that don't have a note.
    `attr<-`("Datainfo", "List of pins that have reported issues (holes, etc)")

  pinlistClean <- unique(troublePins$pin_ID)

  bigIssues <- c("Hole", "hole", "mussel", "Holr", "Shell", "Mussel", "edge of hole", "hole next to mussel")
  bigIssuePins <- troublePins %>% filter(Notes %in% bigIssues)
  bigIssuePins

}

