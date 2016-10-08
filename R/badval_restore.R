
myiris_backup <- badval_backup(data = myiris, badval_column = myiris$Bad.Val)
myiris_clean <- badval_clean_data(data = myiris, badval_column = myiris$Bad.Val)


badval_restore <- function(good_col = "Petal.Width",
                           backup_data = myiris_backup,
                           cleaned_data = myiris_clean) {
    # nur wenn TRUE mach weiter:
    if (any(is.na(backup_data[good_col]) == FALSE) == TRUE) {
      vec <- which(is.na(backup_data[good_col]) == FALSE)
      cleaned_data[good_col][vec, 1] <- backup_data[good_col][vec, 1]
    }
  return(cleaned_data)
  # assign(deparse(substitute(cleaned_data)), cleaned_data)
}

badval_restore(good_col = "Species")

for (i in c("Petal.Width", "Species", "Petal.Length")) {
  myiris_clean <<- badval_restore(good_col = i)
}


