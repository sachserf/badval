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


add_text_to_vector <-
  function(the_vector,
           the_text,
           override_NA = TRUE,
           sep = ", ") {
    the_vector <- as.character(the_vector)
    the_vector[!is.na(the_vector)][-which(the_vector[!is.na(the_vector)] == the_text)] <-
      paste0(the_vector[!is.na(the_vector)][-which(the_vector[!is.na(the_vector)] == the_text)], paste0(sep, the_text))

    if (override_NA == TRUE & any(is.na(the_vector))) {
      the_vector[is.na(the_vector)] <- the_text
    }
    return(the_vector)
  }
