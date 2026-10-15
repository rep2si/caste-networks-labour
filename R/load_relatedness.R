require(tidyverse)

#' Function to load the relatedness data (calculated with make_kinship_objects)
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#'
#' @return A list object with two elements: rel (with columns i, j, r) and aff (with columns i, j, s)

load_relatedness <- function(site, sharepoint_path = Sys.getenv("REP2SI_DIR")) {
  # Load data (will be called 'kin_ties')
  load(
    paste0(
      sharepoint_path,
      "/rep2si-data/",
      tolower(site),
      "-data/processed/",
      site,
      "_kin.rdata"
    )
  )

  kin_ties$rel$i <- tolower(kin_ties$rel$i)
  kin_ties$rel$j <- tolower(kin_ties$rel$j)
  kin_ties$aff$i <- tolower(kin_ties$aff$i)
  kin_ties$aff$j <- tolower(kin_ties$aff$j)

  return(kin_ties)
}