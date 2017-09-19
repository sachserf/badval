#' Add bad values
#'
#' @description This function will add predefined text to a specified vector.
#'   The special aspect is that it will not repeat the text if it exists.
#' @param data A data frame containing a column with an index of bad values.
#' @param badindex Character. Name of a column of the data frame that serves as an
#'   index for bad values.
#' @param subset Integer. A vector specifying the rows that contain bad
#'   values.
#' @param badstring Character. Specify the text for indexing (i.e. the names of columns that contain bad values).
#' @param override_NA Logical. Choose TRUE if you want to override NA values.
#' @param separator Character. Specify the separator between the text. It is not
#'   advisable to use '|'.
#' @return 'badindex' column (vector).
#' @author Frederik Sachser
#' @note The idea behind the function is that you can specify a subset of a data
#'   frame and add values to it, without overriding existing values. It is meant
#'   to prepare the data frame before calling badvalue_rm.
#' @seealso \code{\link{badvalue_rm}}, \code{\link{badindex_rm}}
#' @export

badindex_add <-
  function(data,
           subset,
           badindex = "BADVAL",
           badstring,
           override_NA = TRUE,
           separator = ", ") {
    badval_column <- data[, badindex]
    separator_inout <- separator
    # remove spaces
    separator <- gsub(pattern = " ", replacement = "", separator)
    # create bad_vector
    bad_vector <- badval_column[subset]
    # check if badstring already exists
    badval_split <-
      strsplit(gsub(pattern = " ", replacement =  "", bad_vector[!is.na(bad_vector)]), separator)
    df_matches <-
      data.frame(INDEX = rep(NA, length(badval_split)),
                 MATCH_BIN = rep(NA, length(badval_split)))
    for (i in seq_along(badval_split)) {
      df_matches$INDEX[i] <- i
      df_matches$MATCH_BIN[i] <-
        ifelse(length(which(
          unlist(badval_split[i]) == badstring
        )) > 0, TRUE, FALSE)
    }
    matches <-
      df_matches$INDEX[which(df_matches$MATCH_BIN == TRUE)]
    # add badstring
    if (length(matches) > 0) {
      bad_vector[!is.na(bad_vector)][-matches] <-
        paste0(bad_vector[!is.na(bad_vector)][-matches], paste0(separator, badstring))
    } else {
      bad_vector[!is.na(bad_vector)] <-
        paste0(bad_vector[!is.na(bad_vector)], paste0(separator_inout, badstring))
    }
    # override NA
    if (override_NA == TRUE & any(is.na(bad_vector))) {
      bad_vector[is.na(bad_vector)] <- badstring
    }

    badstring_exist(data, badindex, separator)

    # return output
    badval_column[subset] <- bad_vector
    return(badval_column)
  }
