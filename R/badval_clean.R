#' Remove bad values from data
#'
#' @description This function will remove cells of a data frame, that have bad
#'   values.
#' @param data A data frame.
#' @param bad_vector Character. A Vector of the same length as nrow(data).
#'   Generally it is meant to be a column of the data where you can specify the
#'   names of columns that has a bad value in this row. Therefore you do not need to
#'   delete the cells with bad values in a data frame but specify them in a
#'   column. If there are multiple bad values per row the column names should be
#'   separated by comma.
#' @author Frederik Sachser
#' @seealso \code{\link{add_text_to_vector}}
#' @export


badval_clean <- function(data, bad_vector, single_badval = FALSE) {
  thenames <- names(data)
  if (single_badval != FALSE) {
    the_index <- grep(pattern = single_badval, x = names(data))
  } else {
    the_index <- seq_along(thenames)
  }

  for (i in the_index) {
    badrow <- grep(pattern = thenames[i], x = bad_vector)
    if (length(badrow) > 0) {
      data[badrow, i] <- NA
    }
  }
  return(data)
}
