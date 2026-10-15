require(tidyverse)

#' Function to load list set of residents
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#' @param year Year to be used to define residency (a few sites have multiple residency columns for different years: TN, TM, and UP)
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#'
#' @return The loaded kinship list object

load_reslist <- function(site, tn_site, year, sharepoint_path = Sys.getenv("REP2SI_DIR")) {

  indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path, year = year)

  if (site %in% c("TN", "TM")) {
    if (year <= 2022) {
      res_list <- unique(indiv_df$ID[indiv_df$is_resident_2022 == TRUE])
    } else { # treating 2023 and beyond as what was recorded in
      res_list <- unique(indiv_df$ID[indiv_df$is_resident == TRUE])
    }
  }

  # if (site %in% c("UP")) {
  #   if (year <= 2024) {
  #     res_list <- unique(indiv_df$ID[indiv_df$is_resident_2024 == TRUE])
  #   } else { # main is_resident is for 2025
  #     res_list <- unique(indiv_df$ID[indiv_df$is_resident == TRUE])
  #   }
  # }

  # other sites, only one is_resident column (check NP doesn't have two?)
  if (site %in% c("TZ", "NP", "WH", "WL", "UP")) {
    res_list <- unique(indiv_df$ID[indiv_df$is_resident == TRUE])
  }

  # for TN, limit to appropriate village (ten or ala)
  if (site == "TN") {
    if (tn_site == "ala") {
      res_list <- res_list[
        res_list %in% indiv_df$ID[indiv_df$AorT == "Alakapuram"]
      ]
    }
  }
  if (site == "TN") {
    if (tn_site == "ten") {
      res_list <- res_list[
        res_list %in% indiv_df$ID[indiv_df$AorT == "Tenpatti"]
      ]
    }
  }

  res_list <- res_list[!is.na(res_list)] # TODO: where is this coming from?

  return(res_list)
}