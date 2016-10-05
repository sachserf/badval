#' Remove bad values from data
#'
#' @description This function will remove cells of a data frame, that have bad
#'   values.
#' @param data A data frame.
#' @param bad_values Character. A Vector of the same length as nrow(data).
#'   Generally it is meant to be a column of the data where you can specify the
#'   names of columns that has a bad value in this row. Therefore you do not need to
#'   delete the cells with bad values in a data frame but specify them in a
#'   column. If there are multiple bad values per row the column names should be
#'   separated by comma.
#' @author Frederik Sachser
#' @seealso \code{\link{add_text_to_vector}}
#' @export

badval_NA <- function(data, bad_values) {
    thenames <- names(data)
    for (i in seq_along(thenames)) {
        thebad <- grep(pattern = thenames[i], x = bad_values)
        if (length(thebad) > 0) {
          data[thebad, i] <- NA
        }
    }
    return(data)
}

