#' Backup bad values
#'
#' @description This function will backup the specified bad values of a data
#'   frame.
#' @inheritParams badindex_add
#' @inheritParams badvalue_rm
#' @return data frame
#' @author Frederik Sachser
#' @seealso \code{\link{badvalue_restore}}
#' @export

badvalue_backup <- function(data, badstring, badpattern, badindex = "BADVAL") {
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
