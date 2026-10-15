require(tidyverse)

#' Function to load and standardize sharing unit-level data
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#' @param year Year to be used to define time of data points (used to identify missing SUs)
#'
#' @return A cleaned data frame with standardized sharing unit-level variables.

load_su <- function(site, sharepoint_path = Sys.getenv("REP2SI_DIR"), tn_site = NA, year = 2024) {
  # Load data
    df <- read.csv(
      switch(
        site,
        "TN" = file.path(
          sharepoint_path,
          "rep2si-data/tn-data/de-identified/TN_SU_2022.csv"
        ),
        "WL" = file.path(
          sharepoint_path,
          "rep2si-data/wl-data/de-identified/WL/WL_Household.csv"
        ),
        "WH" = file.path(
          sharepoint_path,
          "rep2si-data/wh-data/de-identified/WH/WH_Household.csv"
        ),
        "TM" = file.path(
          sharepoint_path,
          "rep2si-data/tm-data/de-identified/TM_SharingUnit.csv"
        ),
        "TZ" = file.path(
          sharepoint_path,
          "rep2si-data/tz-data/de-identified/TZ_SharingUnit.csv"
        ),
        "NP" = file.path(
          sharepoint_path,
          "rep2si-data/np-data/de-identified/endow/NP_SharingUnit.csv"
        ),
        "UP" = file.path(
          sharepoint_path,
          "rep2si-data/up-data/de-identified/endow/UP_SharingUnit.csv"
        )
      )
    )

  # ## add WL extras
  # if (site == "WL") {
  # su_extra <- read.csv(paste0(
  #   sharepoint_path,
  #   "/rep2si-data/wl-data/restricted/WL_SharingUnit_Rep2SI_Extra.csv"
  # ))
  # df <- rbind(df, su_extra)
  # }

  ## standardize SUID variable name
  colnames(df)[which(
    colnames(df) %in% c("SUID", "suid", "SharingUnitID", "sharingunitid", "HHID")
  )] <- "SUID"

  df$SUID <- tolower(df$SUID)

  # add any missing SUs
  indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path, year = year)
  res_list <- load_reslist(site = site, tn_site = tn_site, year = year, sharepoint_path = sharepoint_path)
  res_su <- sort(unique(indiv_df$SUID[indiv_df$ID %in% res_list]))
  missing_su <- res_su[which(!res_su %in% df$SUID)]
  if (length(missing_su) > 0) {
    df <- tibble::add_row(df, SUID = missing_su)
  }

  # if TN, limit to just the one village
  if (site == "TN") {
    df <- subset(df, df$SUID %in% res_su)
  }

  # Standardize food security variables
  # already there for TN
  if (site %in% c("TM", "TZ")) {
    df <- df %>%
      dplyr::rename(
        smallermeal = SmallerMeal,
        fewermeals = FewerMeals,
        nofood = NoFood,
        sleephungry = SleepHungry,
        withouteating = WithoutEating
      )
  }
  if (site == "NP") {
    df <- df %>%
      dplyr::rename(
        smallermeal = food_smaller_meal,
        fewermeals = food_fewer_meals,
        nofood = food_nofood,
        sleephungry = food_sleep_hungry,
        withouteating = food_nofood_24
      )
  }
  if (site == "UP") {
    df <- df %>%
      dplyr::rename(
        smallermeal = fs_smaller_meal,
        fewermeals = fs_fewer_meals,
        nofood = fs_no_food,
        sleephungry = fs_sleep_hungry,
        withouteating = fs_day_night_no_eat
      )
  }
  if (site %in% c("NP", "UP")) {
    df <- df %>%
      dplyr::mutate(dplyr::across(
        c(smallermeal, fewermeals, nofood, sleephungry, withouteating),
        ~ dplyr::recode(
          .,
          "never" = 0,
          "rarely" = 1,
          "sometimes" = 2,
          "often" = 3,
          .default = NA_real_
        )
      ))
  }

  ## Colombia doesn't have standard ENDOW food security question, but rather a single question on 'days without sufficient food' and some others getting at poverty
  # Poor PoorRelativeCommunity PoorRelativeColombia DaysWithoutSufficientFood DaysWithoutElectricity DaysWithoutInternet DaysWithoutCleanWater
  # asked of each individual and stored in Indiv file
  # Taking average of all individuals in SU to create SU-level variable
  # Generally close agreement or only one respondent, but a few cases of bigger discrepancies
  if (site %in% c("WL", "WH")) {
    indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path, year = year, main_vars = FALSE)
    foodsec <- indiv_df %>%
      group_by(SUID) %>%
      summarize(DaysWithoutSufficientFood_W4 = mean(DaysWithoutSufficientFood_W4, na.rm = TRUE))
    df <- df %>%
      left_join(foodsec, by = "SUID")
    df$insec_aboveav <- df$DaysWithoutSufficientFood_W4 >
      mean(df$DaysWithoutSufficientFood_W4, na.rm = TRUE)
    df$insec_tally <- df$DaysWithoutSufficientFood_W4
  }

  df <- tibble::as_tibble(df)

  return(df)
}

# code to create new WL wealth values until in ENDOW database
# wl_wealth <- load_indiv("WL") %>%
#   count(SUID) %>%
#   right_join(load_su("WL"), by = "SUID") %>%
#   select(SUID, n, GoodValues_Merged_by_Mean)
# colnames(wl_wealth) <- c("su_id", "size_su", "cash_value")
# write.csv(wl_wealth, row.names = FALSE, file.path(
#           sharepoint_path,
#           "rep2si-data/wl-data/de-identified/WL/WL_SU_Wealth.csv"
#         ))
