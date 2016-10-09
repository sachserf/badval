#' Restore bad values
#'
#' @description This function will backup the specified bad values of a data
#'   frame.
#' @param good_col Character. Name of a column that should be restored.
#' @param backup_data A data frame containing only bad values and NA.
#' @param cleaned_data A data frame containing data without bad values
#' @return data frame
#' @note The Index will compare and use row.names of both datasets to restore
#'   the values. Therefore you should not use the function if you manually
#'   changed the row names of the cleaned data after creating the backup data.
#' @author Frederik Sachser
#' @seealso \code{\link{badval_backup}}
#' @export

badval_restore <- function(good_col,
                           backup_data,
                           cleaned_data) {
    if (any(is.na(backup_data[good_col]) == FALSE) == TRUE) {
      vec <- which(is.na(backup_data[good_col]) == FALSE)
      ID_backup <- row.names(backup_data[vec, ])
      ID_cleaned <- grep(pattern = paste(ID_backup, collapse = "|"), x = row.names(cleaned_data))
      ID_cleaned <- row.names(cleaned_data)[ID_cleaned]
      ID_missing <- ID_backup[!ID_backup %in% ID_cleaned]
      ID_backup <- ID_backup[ID_backup %in% ID_cleaned]
      cleaned_data[grep(pattern = paste(ID_cleaned, collapse = "|"), x = row.names(cleaned_data)), good_col] <- backup_data[grep(pattern = paste(ID_backup, collapse = "|"), x = row.names(backup_data)), good_col]
    }
  if (length(ID_missing > 0)) {
    cat("The following rows are missing: ", paste(ID_missing, collapse = ", "))
  }
  return(cleaned_data)
}



