#' Clean badindex - remove unnecessary separators
#'
#' @description This function will remove unnecessary separators from badindex.
#' @inheritParams badindex_add
#' @author Frederik Sachser
#' @seealso \code{\link{badstring_exist}}
#' @note The function won´t work with spaces in column names.
#' @export

badindex_rm_sep <- function(data, badindex, separator = ", ") {
  badval_column <- data[, badindex]
  separator_inout <- separator
  separator <- gsub(" ", "", separator)
  badval_column <- gsub(" ", "", badval_column)

  while (any(grep(paste0(separator, separator), badval_column, fixed = TRUE))) {
    badval_column <-
      gsub(paste0(separator, separator), separator, badval_column)
  }

  # preceeding ", "
  if (any(which(startsWith(badval_column, separator)))) {
    badval_column[which(startsWith(badval_column, separator))] <-
      substr(badval_column[which(startsWith(badval_column, separator))], 2, nchar(badval_column[which(startsWith(badval_column, separator))]))
  }

  # ending in ", "
  if (any(which(endsWith(badval_column, separator)))) {
    badval_column[which(endsWith(badval_column, separator))] <-
      substr(badval_column[which(endsWith(badval_column, separator))], 1, nchar(badval_column[which(endsWith(badval_column, separator))]) - 1)
  }

  badval_column <- gsub(separator, separator_inout, badval_column)

  if (any(which(badval_column == ""))) {
    badval_column[which(badval_column == "")] <- NA
  }

  return(badval_column)
}
