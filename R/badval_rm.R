badval_rm <- function(bad_vector, bad_value, separator = ",") {
  # convert to character
  bad_vector <- as.character(bad_vector)
  # store for output
  separator_inout <- separator
  # remove spaces
  separator <- gsub(pattern = " ", replacement = "", separator)
  # positive cases
  vector_string <- bad_vector[grep(pattern = bad_value, x = bad_vector)]
  # remove spaces
  vector_string <- gsub(pattern = " ",
                        replacement = "",
                        x = vector_string)
  # remove the bad_value
  vector_no_string <-
    trimws(gsub(paste0(
      '\\b(', paste(bad_value, collapse = '|'), ')\\b'
    ), '', vector_string))
  # remove potentially doubled separator
  beauty_output <-
    gsub(
      pattern = paste0(separator, separator),
      replacement = separator,
      x = vector_no_string
    )
  # remove last char separator
  last_char <-
    beauty_output[endsWith(x = beauty_output, suffix = separator)]
  beauty_output[endsWith(x = beauty_output, suffix = separator)] <-
    substr(last_char, 1, nchar(last_char) - 1)
  # remove first char separator
  firstchar <-
    beauty_output[startsWith(x = beauty_output, separator)]
  beauty_output[startsWith(x = beauty_output, separator)] <-
    substr(x = firstchar,
           start = 2,
           stop = nchar(firstchar))
  # Add separator as specified
  beauty_output <-
    gsub(pattern = separator, replacement = separator_inout, beauty_output)
  # edit the bad_vector
  bad_vector[grep(pattern = bad_value, x = bad_vector)] <- beauty_output
  # return bad_vector
  return(bad_vector)
}
