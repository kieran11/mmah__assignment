# ---- Helpers -----------------------------------------------------------------

find_one_column <- function(data_names, pattern) {
  matches <- data_names[
    str_detect(
      data_names,
      regex(pattern, ignore_case = TRUE)
    )
  ]
  
  if (length(matches) != 1) {
    stop(
      paste0(
        "Expected exactly one column matching '",
        pattern,
        "', but found: ",
        paste(matches, collapse = ", ")
      )
    )
  }
  
  matches[[1]]
}

first_or_na <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  x[[1]]
}

check_rate <- function(x, variable_name) {
  bad <- !is.na(x) & (x < 0 | x > 100)
  
  if (any(bad)) {
    stop(
      paste0(
        variable_name,
        " contains values outside 0-100."
      )
    )
  }
}

clean_municipality <- function(x) {
  
  x %>%
    # Convert accented characters to ASCII
    stringi::stri_trans_general("Latin-ASCII") %>%
    
    # Remove CMHC municipality-type abbreviations
    stringr::str_remove(
      "\\s+(CY|CV|T|TP|MU|VL|TV|DM|RM|RGM|CT|MD|P|NO)$"
    ) %>%
    
    # Remove StatsCan-style municipality descriptions if present
    stringr::str_remove(
      ",\\s*(City|Town|Township|Municipality|Village|County).*"
    ) %>%
    
    # Normalize punctuation and spacing
    stringr::str_replace_all("[^A-Za-z0-9]+", " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}
