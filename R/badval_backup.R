#' Remove bad values from data
#'
#' @description This function will remove cells of a data frame, that have bad
#'   values.
#' @param data A data frame.
#' @param badval_column Character. A Vector of the same length as nrow(data).
#'   Generally it is meant to be a column of the data where you can specify the
#'   names of columns that has a bad value in this row. Therefore you do not need to
#'   delete the cells with bad values in a data frame but specify them in a
#'   column. If there are multiple bad values per row the column names should be
#'   separated by comma.
#' @author Frederik Sachser
#' @seealso \code{\link{add_text_to_vector}}
#' @export


badval_backup <- function(data, badval_column, badval_pattern, badval_exactly) {
  thenames <- names(data)
  if (missing(badval_pattern)) {
    if (missing(badval_exactly)) {
      the_index <- seq_along(thenames)
    } else {
      the_index <- which(thenames == badval_exactly)
      if (length(the_index) != 1) {
        return(warning("badval_exactly not found. Check spelling."))
      }
    }
  } else {
    the_index <- grep(pattern = badval_pattern, x = thenames)
    if (length(the_index) == 0) {
      return(warning("badval_pattern not found. Check spelling."))
    }
  }

  # df same dimensions but all values NA
  df_backup <- data
  for (i in seq_along(names(df_backup))) {
    df_backup[1:length(df_backup[,i]), i] <- NA
  }

  for (i in the_index) {
    badrow <- grep(pattern = thenames[i], x = badval_column)
    if (length(badrow) > 0) {
      df_backup[badrow, i] <- data[badrow, i]
      cat("Replaced", length(badrow), "values in column", thenames[i], "\n")
    } else {
      cat("No matches in column", thenames[i], "\n")
    }
  }
  return(df_backup)
}
