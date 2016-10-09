#' Remove bad values from data
#'
#' @description This function will remove cells of a data frame, that have bad
#'   values.
#' @inheritParams badval_add_index
#' @param data A data frame containing a column with an index of bad values.
#' @param badval_pattern Character. Specify a pattern of bad values to clean
#'   multiple (but not all) columns of your data frame.
#' @param badval_exactly Character. Specify an exact string to clean only one
#'   single column of your data frame.
#' @return data frame
#' @author Frederik Sachser
#' @seealso \code{\link{badval_add_index}}, \code{\link{badval_rm_index}}
#' @export


badval_rm_data <- function(data, badval_column, badval_pattern, badval_exactly) {
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

  for (i in the_index) {
    badrow <- grep(pattern = thenames[i], x = badval_column)
    if (length(badrow) > 0) {
      data[badrow, i] <- NA
      cat("Replaced", length(badrow), "values in column", thenames[i], "\n")
    } else {
      cat("No matches in column", thenames[i], "\n")
    }
  }
  return(data)
}
