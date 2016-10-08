#' add text to a vector
#'
#' @description This function will add predefined text to a specified vector.
#'   The special aspect is that it will not repeat the text if it exists.
#' @param the_vector Specify a vector that should be edited.
#' @param the_text Character. Specify the text you want to add to your vector.
#' @param override_NA Logical. Choose TRUE if you want to override NA values.
#' @param sep Character. Specify the separator between the text.
#' @author Frederik Sachser
#' @note The idea behind the function is that you can specify a subset of a data frame and add values to it, without overriding existing values. It is meant to prepare a data frame before calling badval_NA.
#' @seealso \code{\link{badval_NA}}
#' @export

badval_add <-
  function(badval_column,
           bad_row_index,
           bad_col_name,
           override_NA = TRUE,
           separator = ",") {
    separator_inout <- separator
    # remove spaces
    separator <- gsub(pattern = " ", replacement = "", separator)
    # create bad_vector
    bad_vector <- badval_column[bad_row_index]
    # check if bad_col_name already exists
    badval_split <-
      strsplit(gsub(pattern = " ", replacement =  "", bad_vector[!is.na(bad_vector)]), separator)
    df_matches <-
      data.frame(INDEX = rep(NA, length(badval_split)),
                 MATCH_BIN = rep(NA, length(badval_split)))
    for (i in seq_along(badval_split)) {
      df_matches$INDEX[i] <- i
      df_matches$MATCH_BIN[i] <-
        ifelse(length(which(
          unlist(badval_split[i]) == bad_col_name
        )) > 0, TRUE, FALSE)
    }
    matches <-
      df_matches$INDEX[which(df_matches$MATCH_BIN == TRUE)]
    # add bad_col_name
    if (length(matches) > 0) {
      bad_vector[!is.na(bad_vector)][-matches] <-
        paste0(bad_vector[!is.na(bad_vector)][-matches], paste0(separator, bad_col_name))
    } else {
      bad_vector[!is.na(bad_vector)] <-
        paste0(bad_vector[!is.na(bad_vector)], paste0(separator_inout, bad_col_name))
    }
    # override NA
    if (override_NA == TRUE & any(is.na(bad_vector))) {
      bad_vector[is.na(bad_vector)] <- bad_col_name
    }
    # return output
    badval_column[bad_row_index] <- bad_vector
    return(badval_column)
  }
