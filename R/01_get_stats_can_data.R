# ==============================================================================
# 01_download_statscan.R
#
# Purpose:
#   Download Statistics Canada housing affordability data for Ontario
#   Census Subdivisions (municipalities), initially for 2016 and 2021.
#
# Sources:
#   2021 Census Table 98-10-0247-01
# ==============================================================================


# ---- Packages ----------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(readr)
library(tidyr)
library(purrr)
library(rvest)
library(xml2)
source("C:/Users/shahki/Downloads/helper_functions.R")

data_directory <- "C:/Users/shahki/Downloads/"


# ==============================================================================
# 4. 2021 AFFORDABILITY DATA
# ==============================================================================
#
# Statistics Canada Table:
# 98-10-0247-01
#
# Core housing need by tenure including presence of mortgage payments
# and subsidized housing.
#
# This table includes Census Subdivision geography and detailed
# shelter-cost-to-income categories.


statscan_2021_url <- paste0(
  "https://www150.statcan.gc.ca/n1/tbl/csv/",
  "98100247-eng.zip"
)

zip_file_2021 <- paste0(
  data_directory,
  "98100247-eng.zip"
  )

download.file(
  url = statscan_2021_url,
  destfile = zip_file_2021,
  mode = "wb"
)


# ---- Unzip -------------------------------------------------------------------

unzip_dir_2021 <- paste0(
  data_directory, 
  "/98100247")

dir.create(
  unzip_dir_2021,
  showWarnings = FALSE,
  recursive = TRUE
)

unzip(
  zip_file_2021,
  exdir = unzip_dir_2021
)


# Find CSV file automatically

csv_2021 <- list.files(
  unzip_dir_2021,
  pattern = "\\.csv$",
  full.names = TRUE
)

################################################################################

# ==============================================================================
# 2. LOCATE OR DOWNLOAD 2021 ONTARIO CSD CENSUS PROFILE
# ==============================================================================

find_profile_csv <- function(directory) {
  
  candidates <- list.files(
    directory,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  candidates <- candidates[
    !str_detect(
      basename(candidates),
      regex(
        "meta|metadata|starting_row|readme",
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  
  # Prefer Statistics Canada's English data file.
  preferred <- candidates[
    str_detect(
      basename(candidates),
      regex(
        "English_CSV_data|English.*data",
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(preferred) > 0) {
    return(preferred[[1]])
  }
  
  # Otherwise choose the largest CSV.
  sizes <- file.info(candidates)$size
  
  candidates[[which.max(sizes)]]
}


profile_csv <- find_profile_csv(data_directory)


download_page <- paste0(
    "https://www12.statcan.gc.ca/census-recensement/2021/",
    "dp-pd/prof/details/download-telecharger.cfm?Lang=E"
  )
  
  page <- rvest::read_html(download_page)
  
  rows <- rvest::html_elements(
    page,
    "tr"
  )
  
  row_text <- purrr::map_chr(
    rows,
    rvest::html_text2
  )
  
  ontario_row_index <- which(
    str_detect(
      row_text,
      regex(
        paste(
          "Census subdivisions? \\(CSDs?\\).*Ontario only",
          "Census subdivisions? \\(CSD\\) - Ontario only",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    )
  )
  
  ontario_row <- rows[[ontario_row_index[[1]]]]
  
  links <- rvest::html_elements(
    ontario_row,
    "a"
  )
  
  link_text <- rvest::html_text2(
    links
  )
  
  csv_link_index <- which(
    str_detect(
      link_text,
      regex(
        "CSV|comma-separated",
        ignore_case = TRUE
      )
    )
  )
  
  csv_href <- rvest::html_attr(
    links[[csv_link_index[[1]]]],
    "href"
  )
  
  csv_url <- xml2::url_absolute(
    csv_href,
    download_page
  )
  
  message("Downloading: ", csv_url)
  
  temp_download <- tempfile(
    pattern = "statscan_2021_profile_"
  )
  
  httr2::request(csv_url) |>
    httr2::req_user_agent(
      "R housing affordability analysis - public Statistics Canada data"
    ) |>
    httr2::req_perform(
      path = temp_download
    )
  
  magic_bytes <- readBin(
    temp_download,
    what = "raw",
    n = 4
  )
  
  is_zip <- (
    length(magic_bytes) >= 2 &&
      identical(
        magic_bytes[1:2],
        charToRaw("PK")
      )
  )
  
  if (is_zip) {
    
    unzip(
      temp_download,
      exdir = data_directory
    )
    
  } else {
    
    file.copy(
      temp_download,
      file.path(
        data_directory,
        "statscan_2021_ontario_csd_profile.csv"
      ),
      overwrite = TRUE
    )
  }
  