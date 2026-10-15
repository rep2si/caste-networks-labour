require(tidyverse)

#' Function to load and clean allocation data
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#' @param anonAlter Optional parameter for NP and UP to return anonymous alter information. Default set to FALSE.
#'
#' @return A cleaned data frame with additional columns for analysis.

load_alloc <- function(site, tn_site = NA, sharepoint_path = Sys.getenv("REP2SI_DIR"), anonAlter = FALSE) {

  # participants who did not finish the games
  dnf <- c("np12601", "up00901", "up12802", "up08902", "tm0134")

  # levels for 11-level ordinal category for perc given
  breaks <- c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  labels <- c("0", "(0, 10]", "(10, 20]", "(20, 30]", "(30, 40]", "(40, 50]", "(50, 60]", "(60, 70]", "(70, 80]", "(80, 90]", "(90, 100]")

  # Load data
  if (site == "TN") {
    if (tn_site == "ala") {
      df <- read.csv(file.path(
        sharepoint_path,
        "rep2si-data/tn-data/de-identified/games/ala/clean/SubsetContributions-SummaryTable-Clean.csv"
      ))
    } else { # implicitly "ten"
      df <- read.csv(file.path(
        sharepoint_path,
        "rep2si-data/tn-data/de-identified/games/ten/clean/SubsetContributions-SummaryTable-Clean.csv"
      ))
    }
  } else {
    df <- read.csv(
      switch(
        site,
        "WL" = file.path(
          sharepoint_path,
          "rep2si-data/wl-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        ),
        "WH" = file.path(
          sharepoint_path,
          "rep2si-data/wh-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        ),
        "TM" = file.path(
          sharepoint_path,
          "rep2si-data/tm-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        ),
        "TZ" = file.path(
          sharepoint_path,
          "rep2si-data/tz-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        ),
        "NP" = file.path(
          sharepoint_path,
          "rep2si-data/np-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        ),
        "UP" = file.path(
          sharepoint_path,
          "rep2si-data/up-data/de-identified/games/clean/SubsetContributions-SummaryTable-Clean.csv"
        )
      ),
      header = TRUE,
      stringsAsFactors = FALSE,
      as.is = TRUE
    )
  }

  df <- subset(df, df$loadTime != "")
  df$ID <- tolower(df$ID)
  df$AID <- tolower(df$AID)
  df$RID <- as.character(df$RID)
  df$ID_fct <- as.factor(paste0("id_", df$ID))
  df$AID_fct <- as.factor(paste0("aid_", df$AID))
  df$RID_fct <- as.factor(df$RID)
  df$Condition_fct = factor(df$Condition, c("anonymous", "revealed"))
  df$optedOut <- ifelse(df$optedOut == "true", TRUE, FALSE)
  df$optedOut[is.na(df$optedOut) == TRUE] <- FALSE
  df$amtGiven[df$optedOut == TRUE] <- NA
  df$percGiven <- df$amtGiven / df$Endowment * 100
  df$percGiven_std <- scale(df$percGiven)[,1]
  df$give50 <- df$percGiven == 50
  df$give50ormore <- df$percGiven >= 50
  df$give0 <- df$amtGiven == 0
  df$percGiven_ord = factor(
    cut(
      df$percGiven,
      breaks = breaks,
      include.lowest = TRUE,
      right = TRUE,
      labels = labels),
    ordered = TRUE,
    levels = labels)
  df$loadDateTime <- lubridate::as_datetime(df$loadTime / 1000)
  df$saveDateTime <- lubridate::as_datetime(df$saveTime / 1000)
  df$responseTime <- as.numeric(df$saveDateTime - df$loadDateTime, units = "secs")
  df$day_of_collection <- as.Date(lubridate::ymd_hms(df$loadDateTime, tz = "UTC")) -
      as.Date(min(lubridate::ymd_hms(df$loadDateTime, tz = "UTC"))) + 1

  df <- df %>%
    dplyr::group_by(ID) %>%
    dplyr::arrange(loadDateTime) %>%
    dplyr::mutate(q_order = dplyr::row_number()) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(anon_first = mean(q_order[Condition == "anonymous"]) < mean(q_order[Condition == "revealed"]))

  df$q_order_fct = factor(df$q_order, seq(1, max(df$q_order)))

  vars_to_retain <- c(
    "RID",
    "RID_fct",
    "ID",
    "ID_fct",
    "GID",
    "AID",
    "AID_fct",
    "Condition",
    "Condition_fct",
    "amtGiven",
    "percGiven",
    "percGiven_ord",
    "percGiven_std",
    "give50",
    "give50ormore",
    "give0",
    "optedOut",
    "responseTime",
    "q_order",
    "q_order_fct",
    "anon_first",
    "day_of_collection"
  )

  if (site %in% c("NP", "UP")) {
    if (anonAlter == TRUE) {
      df <- df %>%
        dplyr::filter(anonymousAlter == "true") %>%
        dplyr::select(any_of(vars_to_retain)) %>%
        dplyr::select(!c(AID, AID_fct, GID))

    } else {
      df <- df %>%
        dplyr::filter(anonymousAlter == "false") %>%
        dplyr::select(any_of(vars_to_retain))
    }
  }
  if (!site %in% c("NP", "UP")) {
    if (anonAlter == TRUE) {
      stop("anonAlter == TRUE is only applicable for NP and UP sites!")
    } else {
      df <- df %>%
        dplyr::select(any_of(vars_to_retain))
    }
  }

  df <- df %>% ungroup()

  # Remove allocations by participant who did not finish
  df <- subset(df, !ID %in% dnf)

  return(df)
}
