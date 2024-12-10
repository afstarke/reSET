# Utilities.

#' Pipe graphics from https://github.com/rstudio/ggvis/blob/master/R/pipe.R
#'
#' Like dplyr, ggvis also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of
#' layer_points(ggvis(mtcars, ~mpg, ~wt))
#' # you can write
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
NULL


# Utility to create pin height vector
#' Create pin height table (named vector) using prompts.
#'
#' @description This utility function creates a named vector representing the
#'   pin length with a name as the pin number. This table/vector is used in the
#' @seealso \code{\link{set_get_pinlengths} and
#' @seealso \code{\link{set_get_absolute_heights} functions which adjust the
#'   measured pin height taken from the SET arm to the NAVD88 datum. This
#'   utility is meant to be a helper function and would only need to be run once
#'   for creating the named vector and storing it within the project folder
#'   (e.g. /data). Users can also create a named vector directly using any
#'   number of approaches in R. One approach would be: pinHts <- c(176, 176,
#'   176, 175, 177, 176, 176, 176, 175); names(pinHts) <- 1:9
#' @param n_pins
#'
#' @return a named vector as well as a .rds object written to your current
#'   working directory
#' @export
#'
#' @examples
#' pinHts <- set_pinhts()
#' pinHts_18 <- set_pinhts(n_pins = 18)
#'
set_pinhts <- function(n_pins = 9){

  if(!is.numeric(n_pins)){
    stop("Must provide an integer value for the number of pins your SET arm contains (9 is most common)")
  }
  # create a function to create a named pin.
  name_pin_ht <- function(number){

    pin <- readline(prompt = glue::glue("Provide a length (mm) for pin  {number}:  "))
    pin <- as.numeric(pin)
    names(pin) <- number

    if(is.na(pin) | pin < 100){
      stop("Pin length must be numeric and greater than 100 mm \nRerun function and provide proper values.")
    }
    return(pin)
  }

  # make a vector of pins
  vec <- vector(length = n_pins) # start with a blank space.
  for(i in seq_len(n_pins)){
    pin <- name_pin_ht(i)
    vec[i] <- pin
    names(vec)[i] <-  names(pin)
  }

  readr::write_rds(x = vec, file = "pin_height_list.rds")
  return(vec)
}


#' pull in default pin_height_list.rds
#'
#' @return rds object read into session
#' @export
#'
#' @examples
#' pinhts <- get_pinhts()

get_pinhts <- function(path = "pin_height_list.rds"){
  pin_heights <- readr::read_rds(file = path)
  stopifnot(is.vector(pin_heights), is.character(names(pin_heights)))

  return(pin_heights)
}


#' set data to SETr data.
#'
#' @param dataSET tibble of SET data from reSET::set_get_sets
#'
#' @return renamed tibble for use in SETr packages
#' calc_change_cumu and calc_change_incr functions.
#' @export
#'
#' @examples
#' set_to_setr(SET_data) %>% SETr::calc_change_incr() # returns
#' list of dataframes of incremental change rates.
set_to_setr <- function(dataSET){

  dat <- dataSET %>% dplyr::mutate(pin_ht_cm = Raw * 100) %>%
    dplyr::rename(
      date = Date,
      set_id = Plot_Name,
      arm_position = Arm_Direction,
      pin_number = Pin_number,
      pin_height = pin_ht_cm
    )
  return(dat)
}


#' Get noted pin issues.
#'
#' @param dbconn Connection to Database returned from set_get_db
#'
#' @return a vector of text strings as written in the database.
#' @export
#'
#' @examples
get_pin_issues <- function(dbconn){
  if (!DBI::dbIsValid(dbconn)) {
    warning("Connect to database prior to running any set_get operations.")
  }
  # Connect to tables containing set data. Munge here instead of bringing in to R env.
  set_data <- set_get_sets(dbconn)
  issues <- set_data %>% filter(!is.na(Notes))
  dput(unique(issues$Notes))

}
