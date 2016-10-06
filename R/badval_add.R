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

# Problem: tausche which gegen grep, da sonst das wort vorkommen kann, sofern auch andere Dinge dort stehen
badval_add <-
  function(bad_vector,
           bad_value,
           override_NA = TRUE,
           separator = ", ") {
    if (length(grep(pattern = bad_value, x = bad_vector[!is.na(bad_vector)])) > 0) {
      bad_vector[!is.na(bad_vector)][-grep(pattern = bad_value, x = bad_vector[!is.na(bad_vector)])] <-
        paste0(bad_vector[!is.na(bad_vector)][-grep(pattern = bad_value, x = bad_vector[!is.na(bad_vector)])], paste0(separator, bad_value))
    } else {
      bad_vector[!is.na(bad_vector)] <-
        paste0(bad_vector[!is.na(bad_vector)], paste0(separator, bad_value))
    }

    if (override_NA == TRUE & any(is.na(bad_vector))) {
      bad_vector[is.na(bad_vector)] <- bad_value
    }
    return(bad_vector)
  }
