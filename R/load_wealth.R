require(tidyverse)

#' Function to load wealth data (note: this draws on wealth data pulled by Elly using the pull_wealth function)
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#' @param year Year to be used to define time of data points (used to identify missing SUs)
#'
#' @return A standardized dataframe with wealth data for resident SUs

load_wealth <- function(site, sharepoint_path = Sys.getenv("REP2SI_DIR"), tn_site = NA, year = 2024) {
  file_name <- paste0(site, "_SU_wealth.csv")
  if (!site %in% c("WL", "WH")) {
    # Load data
    df <- read.csv(file.path(sharepoint_path, paste0("rep2si-data/", tolower(site), "-data/de-identified/", file_name)))
    colnames(df)[which(colnames(df) == "su_id")] <- "SUID"
  } else { # if site %in% c("WL", "WH"), where we're currently drawing on Cody's own calculations of household wealth
    df <- read.csv(file.path(
      sharepoint_path,
      paste0(
        "rep2si-data/",
        tolower(site),
        "-data/de-identified/",
        site,
        "/",
        site,
        "_Household.csv"
      )
    ))
    baseline <- ifelse(site == "WL", 500, 700) # per W1, WL = 500, WH = 700
    df <- df %>%
      dplyr::rename(SUID = HHID, cash_value = GoodValues_Merged_by_W1_Fill) %>%
      mutate(
        SUID = tolower(SUID),
        cash_value = (cash_value + baseline) * .2) %>% # add base home value and convert to USD
      select(SUID, cash_value)
  }

  # add any missing SUs
  indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path, year = year)
  res_list <- load_reslist(site = site, tn_site = tn_site, year = year, sharepoint_path = sharepoint_path)
  res_su <- indiv_df %>%
    dplyr::filter(ID %in% res_list) %>%
    group_by(SUID) %>%
    summarise(size_su = n()) %>%
    left_join(df %>% select(SUID, cash_value), by = "SUID") %>%
    mutate(
      wealth_pc = cash_value / size_su,
      rank_wealth = dplyr::percent_rank(cash_value),
      logwealth = log10(cash_value),
      std_wealth = scale(cash_value)[,1],
      std_logwealth = scale(log10(cash_value))[,1]
    )

  res_su$wealth_pc[is.infinite(res_su$wealth_pc)] <- NA # if size_su is 0, recode to NA
  res_su$logwealth_pc = log10(res_su$wealth_pc)
  res_su$rank_wealth_pc <- dplyr::percent_rank(res_su$wealth_pc)
  mean_logwealth <- mean(res_su$logwealth, na.rm = TRUE)
  mean_logwealth_pc <- mean(res_su$logwealth_pc, na.rm = TRUE)
  res_su$demean_logwealth <- res_su$logwealth - mean_logwealth
  res_su$demean_logwealth_pc <- res_su$logwealth_pc - mean_logwealth_pc

  return(res_su)
}
