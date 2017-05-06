#install.packages("roxygen2")

#' Reads a given file specified by the \code{filename}
#' ability to verify if the file exists
#'
#' @param filename the name of the file; provided as character string.
#'
#' @return dataframe representation of the file contents defined filename or
#' 'does not exist' message.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("filename.csv")
#' fars_read("path/filename.csv")
#' }
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

#' Create a filename for a given year with format "accident_YEAR.csv.bz2"
#'
#' This function print a character vector with the filename combining the default
#' file name and the \code{year} passed as argument to the function
#'
#' @param year of file creation as integer / string.
#'
#' @return This function returns a character vector  the format "accident_YEAR.csv.bz2"
#'
#' @examples
#' \dontrun{
#' make_filename(2017)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple .csv data file for a integer series of provided years
#'
#' This function take as input a vector of integers (\code{years}); returns a list collection of
#' DataFrames of Years and respesctive months.
#'
#' For each year with no data file, the function generates a warning message.
#'
#' @param years A vector of integers representing a series of years
#'
#' @return This function returns a dataframe with Year and Months columns for each year.
#'
#' @inherit fars_read_years
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' fars_read_years(2017)
#' fars_read_years(2015:2016)
#' }
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

#' Summarises data on month and given year from data format defined in "accident_YEAR.csv.bz2" files.
#'
#' This function takes a collection of years (\code{years}) as input and returns a Tibble(Data Frame)
#' containing the defined data summary as a Wide Format.
#'
#' @param years A list of years given as integers.
#'
#' @importFrom tidyr spread
#'
#' @inherit fars_read_years
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2017)
#' fars_summarize_years(2015:2016)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Creates a map visualising all accident events in a given State for a paricular year.
#'
#' This function takes a State number (\code{state.num}) indicator and a year (\code{year}).
#'
#' If the State number does not exhist in the data for the given year, the method exits (STOP)
#' with a message "invalid STATE number: ???".
#'
#' If no data for State Num / year combination a message "no accidents to plot" is returned.
#'
#' @param state.num a State Code Number as Integer
#' @param year a given Year as Integer
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @inherit make_filename
#' @inherit fars_read
#'
#' @examples
#' \dontrun{
#' fars_map_state(12, 2017)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
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
