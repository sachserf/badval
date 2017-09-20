#' Remove badstring if is.na(badvalue)
#'
#' @description This function will remove corresponding badstrings from badindex if badvalues are missing (NA).
#' @inheritParams badindex_add
#' @author Frederik Sachser
#' @seealso \code{\link{badindex_rm_sep}}
#' @export

badindex_rm_na <-
  function(data,
           badstring = NULL,
           badindex = "BADVAL",
           separator = ",") {
    badval_column <- data[, badindex]
    if (missing(badstring)) {
      badstring <- badstring_exist(data, badindex, separator)$exist
      for (i in seq_along(badstring)) {
        badval_column[grep(badstring[i], badval_column)[grep(badstring[i], badval_column) %in% which(is.na(data[, badstring[i]]))]] <-
          gsub(badstring[i], "", badval_column[grep(badstring[i], badval_column)[grep(badstring[i], badval_column) %in% which(is.na(data[, badstring[i]]))]])
      }
    } else {
      badval_column[grep(badstring, badval_column)[grep(badstring, badval_column) %in% which(is.na(data[, badstring]))]] <-
        gsub(badstring, "", badval_column[grep(badstring, badval_column)[grep(badstring, badval_column) %in% which(is.na(data[, badstring]))]])
    }
    badval_column <- badindex_rm_sep(data, badindex, separator)

    badstring_exist(data, badindex, separator)

    return(badval_column)
  }

