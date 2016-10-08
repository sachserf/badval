#' Restore bad values
#'
#' @description This function will backup the specified bad values of a data
#'   frame.
#' @param good_col Character. Name of a column that should be restored.
#' @param backup_data A data frame containing only bad values and NA.
#' @param cleaned_data A data frame containing data without bad values
#' @return data frame
#' @note The developmental status of this function is beta. Restoring your bad
#'   values will only work as intended if the order of the rows in your data
#'   frame has not been changed.
#' @author Frederik Sachser
#' @seealso \code{\link{badval_backup}}
#' @export

badval_restore <- function(good_col,
                           backup_data,
                           cleaned_data) {
    if (any(is.na(backup_data[good_col]) == FALSE) == TRUE) {
      vec <- which(is.na(backup_data[good_col]) == FALSE)
      cleaned_data[good_col][vec, 1] <- backup_data[good_col][vec, 1]
    }
  return(cleaned_data)
}



