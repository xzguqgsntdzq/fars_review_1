#' fars_read data file reader
#'
#' This is a function that reads in a file with FARS data and informs user if the file does not exist as expected. Function can be used to
#' read a desired filename, but usually this function is used together with a year by function fars_read_years()
#'
#' @param filename Filename in csv. format., class: character
#'
#' @return This function returns a tibble dataframe of the loaded data with dplyr package.
#'
#' @note Stops execution if file does not exis e.g. no data for the given year exists.
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @export



fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename filename generator helper function
#'
#' make_filename generates a filename of format "accident_%d.csv.bz2NNNN", where NNNN=year that is read based on user input of the year parameter. Used as helper -function.
#'
#' @param year, class: integer
#'
#' @return generated filename with year to be read, class: character


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years read specific reads from a file
#'
#' This function creates data using above defined functions make_filename (for selected year) and fars_read (reads the selected filename with correct year). Selects the defined year from data.
#' Usually used as a helper -function.
#'
#' @param years 1 or several years to be selected, class: integer
#'
#' @return data from correct file as tibble dataframe
#'
#' @note Rises an error if given year does not exists in files.
#'
#' @importFrom dplyr mutate select
#'
#' @import magrittr
#'
#' @export


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years reats a summary of the data with selected years
#'
#' This function creates summary data of accidents based on selected years as form of a table.
#'
#' @param years selected years as vector, class: integer
#'
#' @return a summary table of the data
#'
#' @importFrom dplyr bind_rows group_by summarize
#'
#' @importFrom tidyr spread
#'
#' @export


fars_summarize_years <- function(years) {
  year<-NULL
  MONTH<-NULL
  n<-NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state Plot map of accidents in selected state
#'
#' This function exports the map of accidents during the given year. If no accients exist, function returns "no accidents to plot".
#'
#' @param state.num An integer representing state number, class: integer
#'
#' @param year An integer representing selected year, class: integer
#'
#' @return Returns "no accidents to plot" if there were no accidents. Otherwise returns a map containing the accident locations for selected data.
#'
#' @importFrom graphics points
#'
#' @importFrom maps map
#'
#' @note Stops execution if no state exists with the given state.num.
#'
#' @export


fars_map_state <- function(state.num, year) {
  STATE<-NULL
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
