#' Remove bad values from data
#'
#' @description This function will remove cells of a data frame, that have bad
#'   values.
#' @inheritParams badindex_add
#' @param badpattern Character. Specify a pattern of bad values to clean
#'   multiple (but not all) columns of your data frame.
#' @param badstring Character. Specify an exact string to clean only one
#'   single column of your data frame.
#' @return data frame
#' @author Frederik Sachser
#' @seealso \code{\link{badindex_add}}, \code{\link{badindex_rm}}
#' @export

badvalue_rm <- function(data, badstring, badpattern, badindex = "BADVAL") {
  data <- as.data.frame(data)
  badval_column <- data[, badindex]
  thenames <- names(data)
  if (missing(badpattern)) {
    if (missing(badstring)) {
      the_index <- seq_along(thenames)
    } else {
      the_index <- which(thenames == badstring)
      if (length(the_index) != 1) {
        return(warning("badstring not found. Check spelling."))
      }
    }
  } else {
    the_index <- grep(pattern = badpattern, x = thenames)
    if (length(the_index) == 0) {
      return(warning("badpattern not found. Check spelling."))
    }
  }

  for (i in the_index) {
    badrow <- grep(pattern = thenames[i], x = badval_column)
    if (length(badrow) > 0) {
      data[badrow, i] <- NA
      message("Replaced ", length(badrow), " values in column ", thenames[i], "\n")
    } else {
      message("No matches in column ", thenames[i], "\n")
    }
  }
  return(data)
}
