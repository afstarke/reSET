#' set_calc.R
#'
#' set_calc family of functions intended use is to caluclate values using QAed
#' set and sa datasets from the set_get_ and set_check_ functions.
#' These functions take a tibble/dataset as input and return either a tibble/dataset, nested
#' tibble, or model objects that are used within the analysis.
#'

# TODO: Determine sequence and name of function
# Need to take in set data that's been filtered
# recalculate the change between succesive measures (incremental change)
# recalculate the cumulative change
# get marsh elevations (this may be an issue for site that don't have these measures)
# /\use set_get_absolute_heights
#
#

#
#
#
#' Calculate incremental and cumulative changes in pin height. Both within and across SET readers
#'
#' @param dataSET
#'
#' @returns
#' @export
#'
#' @examples
set_calc_changes <- function(dataSET){


  dataSET %>%
    # remove any groupings that carried over.
    ungroup() %>%
    # arrange by date to sequence measures and readers appropriately.
    arrange(Date) %>%
    # group by pin for calculations
    group_by(pin_ID) %>%
    mutate(
      # change from the first reading, unaccounting for SET reader changes but does account for QAed pins.
      cum_change = as.numeric(Raw) - as.numeric(Raw[1]),
      incrementalChange = c(NA, diff(cum_change)),
      incrementalTime = DecYear - dplyr::lag(DecYear, n = 1),
      # create a flag for when a new SET reader is introduced.
      newsetreader = if_else(
        SET_Reader == lag(SET_Reader), # tests if the SET reader is the same from the previous reading
        true = 0, # if the same than 0
        false = 1 # if different than 1
      ),
      # using a cumulative sum add up when ever a new reader takes over, creating a ID, that is unique only to that pinID!
      SET_Reader_uid = cumsum(replace_na(newsetreader, 1))

    ) %>% # create a 'new set reader' id
    ungroup() %>%
    group_by(pin_ID, SET_Reader_uid) %>%
    arrange(Date, SET_Reader_uid) %>%
    # calculate the incremental change within each SET readers data.
    mutate(inc_setreader = Raw - lag(Raw, n = 1, order_by = Date)) %>%
    ungroup() %>%

    group_by(pin_ID) %>%
    # calculate the cumulative change within each SET readers data.
    mutate(cum_change_setreader = cumsum(replace_na(inc_setreader, 0))) %>%

    `attr<-`(attr(x = dataSET, which = "File last updated", exact = F), which = "File last updated")
}

#
# set_calc_rates?
# set_calc_significance?
#
#
#
# set_calc_elevations - not all sites/stations will have elevation measures on the receivers.
# Solution to that is removed those from these steps by filtering through an earlier QA process
# and then pass that filtered dataset into these functions.
#
# #  Calculate the mean starting marsh surfave elevation (NAVD88) across each site, based
# on the first pins at each of the deep SETs
# group_by(Site_Name) %>%
#   mutate(mu_strt_mrsh_elev_site = mean(starting_elevation, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(Plot_Name) %>%
#   mutate(mu_strt_mrsh_elev_station = mean(starting_elevation, na.rm = TRUE),
#          mu_strt_mrsh_elev_station_se = stder(starting_elevation)) %>%
#   ungroup() %>%
# add SETreader grouping to calculate change within a individual SET reader
# calculate the mean marsh elevation at the start - used in one model
# inc_setreader_elev = marsh_elevation - lag(marsh_elevation, n = 1, order_by = Date)
# cum_change_setreader_elevation = cumsum(replace_na(inc_setreader_elev, 0))) %>%   # must use rolling or cumulative sum

#
