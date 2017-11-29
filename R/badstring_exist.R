#' Check badindex - find misspelled badstrings.
#'
#' @description This function will check for entries not matched in colnames (badstrings).
#' @inheritParams badindex_add
#' @author Frederik Sachser
#' @seealso \code{\link{badindex_rm_sep}}
#' @note The function won´t work with spaces in column names.
#' @export

badstring_exist <-
  function(data, badindex = "BADVAL", separator = ", ") {
    data <- as.data.frame(data)
    badval_column <- data[, badindex]
    colnames <- names(data)
    separator <- gsub(" ", "", separator)

    bad_col_name <- unique(unlist(strsplit(
      gsub(
        pattern = " ",
        replacement = "",
        paste(unique(badval_column), collapse = separator)
      ), split = separator, fixed = TRUE
    )))

    if (any(bad_col_name == "")) {
      bad_col_name <- bad_col_name[-which(bad_col_name == "")]
    }

    if (any(bad_col_name == "NA")) {
      bad_col_name <- bad_col_name[-which(bad_col_name == "NA")]
    }

    outlist <- list()
    outlist[[1]] <- bad_col_name[which(bad_col_name %in% colnames == TRUE)] # exist
    outlist[[2]] <- bad_col_name[which(bad_col_name %in% colnames == FALSE)] # missing
    names(outlist) <- c("exist", "missing")

    if (any(bad_col_name %in% colnames == FALSE)) {
      message("Can not find the following bad values within column names of dataframe:\n ", paste0(outlist$missing, sep = "\n "))
      #      return(bad_col_name[which(bad_col_name %in% colnames == FALSE)])
    } else {
      message("All bad values are part of column names of data frame.")
    }
    invisible(outlist)
  }
