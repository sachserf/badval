#' Backup bad values
#'
#' @description This function will backup the specified bad values of a data
#'   frame.
#' @inheritParams badval_clean_data
#' @inheritParams badval_add
#' @return data frame
#' @author Frederik Sachser
#' @seealso \code{\link{badval_restore}}
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
