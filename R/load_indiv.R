require(tidyverse)

#' Function to load and standardize individual-level data
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#' @param year The year for which the data is being standardized (default is 2024).
#' @param main_vars TRUE/FALSE with default as TRUE as to whether only the main variables should be returned. If FALSE, all variables in the original file are returned.
#' @param extra_vars If main_vars is TRUE, can additionally specify extra variables to also add, by name (e.g., c("occupation", "employment_status").)
#'
#' @return A cleaned data frame with standardized individual-level variables.

load_indiv <- function(
  site,
  sharepoint_path = Sys.getenv("REP2SI_DIR"),
  year = 2024,
  main_vars = TRUE,
  extra_vars = NULL
  ) {
  # Load data
  indiv <- read.csv(
    switch(
      site,
      "TN" = file.path(
        sharepoint_path,
        "/rep2si-data/tn-data/de-identified/TN_Indiv_2324.csv"
      ),
      "WL" = file.path(
        sharepoint_path,
        "/rep2si-data/wl-data/de-identified/WL/WL_Individual.csv"
      ),
      "WH" = file.path(
        sharepoint_path,
        "/rep2si-data/wh-data/de-identified/WH/WH_Individual.csv"
      ),
      "TM" = file.path(
        sharepoint_path,
        "/rep2si-data/tm-data/de-identified/TM_Indiv.csv"
      ),
      "TZ" = file.path(
        sharepoint_path,
        "/rep2si-data/tz-data/de-identified/TZ_Indiv.csv"
      ),
      "NP" = file.path(
        sharepoint_path,
        "/rep2si-data/np-data/de-identified/endow/NP_Indiv.csv"
      ),
      "UP" = file.path(
        sharepoint_path,
        "/rep2si-data/up-data/de-identified/endow/UP_Indiv.csv"
      )
    )
  )

  # ## add extra data provided by Cody
  # if (site == "WL") {
  #   indiv_extra <- read.csv(paste0(
  #     sharepoint_path,
  #     "/rep2si-data/wl-data/restricted/WL_Indiv_Rep2SI_Extra.csv"
  #   ))
  #   indiv <- rbind(indiv, indiv_extra)

  #   kinship_extra <- read.csv(paste0(
  #     sharepoint_path,
  #     "/rep2si-data/wl-data/restricted/WL_Kinship_Rep2SI_Extra.csv"
  #   ))

  #   kinship_extra <- subset(kinship_extra, !kinship_extra$Ego %in% indiv$PID)

  #   indiv <- dplyr::full_join(
  #     indiv,
  #     kinship_extra,
  #     by = c("PID" = "Ego", "FID" = "Father", "MID" = "Mother", "Sex" = "Sex")
  #   )
  # }

  ## add parents, where needed
  if (site %in% c("TZ", "NP", "WL", "WH")) {
    kinship <- read.csv(
      switch(
        site,
        "TZ" = file.path(
          sharepoint_path,
          "rep2si-data/tz-data/de-identified/TZ_Kinship.csv"
        ),
        "NP" = file.path(
          sharepoint_path,
          "rep2si-data/np-data/de-identified/endow/NP_Kinship.csv"
        ),
        "WL" = file.path(
          sharepoint_path,
          "rep2si-data/wl-data/de-identified/WL/WL_Kinship.csv"
        ),
        "WH" = file.path(
          sharepoint_path,
          "rep2si-data/wh-data/de-identified/WH/WH_Kinship.csv"
        )
      )
    )
    if (site == "TZ") {
      colnames(kinship) <- c("IndivID", "MotherID", "FatherID", "Gender")
    }
    if (site == "WL") {
      colnames(kinship) <- c("IndivID", "FatherID", "MotherID", "Gender")
      colnames(indiv)[which(colnames(indiv) == "PID")] <- "IndivID"
    }
    if (site == "WH") {
      colnames(kinship) <- c("", "IndivID", "FatherID", "MotherID", "Gender")
      colnames(indiv)[which(colnames(indiv) == "PID")] <- "IndivID"
    }
    kinship <- kinship[, c("IndivID", "MotherID", "FatherID")]
    indiv <- merge(indiv, kinship, by = "IndivID", all.x = TRUE, all.y = TRUE)
  }

  ## select and rename variables
  if (site == "TN") {
    indiv_df <- indiv %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID_2022 = tolower(SUID_2022),
        SUID = tolower(SUID_2324),
        hhead = Head,
        age = Age_2022,
        yob = BirthYear,
        gender = Gender,
        group = Caste,
        group2 = Religion,
        edu = EduYears_2022,
        high_status = EverCommMember_2022,
        is_resident_2022 = Residing_2022,
        is_resident = Residing_2324,
        location = Location_2324,
        ability_to_work = AbilityToWork_2022,
        is_living = Living_2324,
        AorT = as.factor(AorT),
        father_id = tolower(FatherID),
        mother_id = tolower(MotherID),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
        is_resident_2022 = ifelse(is_resident_2022 == "Yes", TRUE, FALSE)
      )
  }

  if (site %in% c("WL", "WH")) {
    indiv_df <- indiv %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID = tolower(HHID_W4),
        hhead = HH_Head,
        age = Age_W4,
        yob = NA,
        gender = Sex,
        group = Ethnicity,
        group2 = Religion,
        edu = Education_Years_W4,
        high_status = Status_Ethnographer_Opinion,
        respect_count = Status_Respect_Count,
        location = NA_character_,
        ability_to_work = Ability_To_Work,
        is_living = NA,
        father_id = tolower(FatherID),
        mother_id = tolower(MotherID),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
       is_resident = ifelse(is.na(SUID) | SUID == "", FALSE, TRUE),
       location = ifelse(is_resident == TRUE, "Community", NA_character_),
       yob = 2022 - age # W4 is gathered from Nov 2021 to Feb 2022
      )
  }

    if (site %in% c("TN", "WL", "WH")) {
    if (year == 2023) {
      ## bringing contemporary to 2023
      indiv_df <- indiv_df %>%
        dplyr::mutate(age = age + 1)
    }

    if (year == 2024) {
      ## bringing contemporary to 2024
      indiv_df <- indiv_df %>%
        dplyr::mutate(age = age + 2)
    }
  }

  if (site == "TZ") {
    indiv_df <- indiv %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID = tolower(SharingUnitID),
        hhead = Head,
        age = Age,
        yob = BirthYear,
        gender = Gender,
        group = Group_bone,
        edu = YearsofEdu,
        high_status = Status,
        location = Location_com,
        ability_to_work = AbilityToWork,
        is_living = NA,
        father_id = tolower(FatherID),
        mother_id = tolower(MotherID),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
       is_resident = ifelse(is.na(SUID), FALSE, TRUE)
      )
  }

  if (site == "TM") {
    indiv_df <- indiv %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID = tolower(SharingUnitID),
        hhead = Head,
        age = Age2024,
        gender = Gender,
        group = Religion,
        edu = YearsofEdu,
        high_status = Status_pasado,
        is_resident_2022 = is_resident_2022,
        is_resident = is_resident_2024,
        location = Location,
        ability_to_work = AbilityToWork,
        is_living = is_alive_2024,
        father_id = tolower(Father),
        mother_id = tolower(Mother),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
        yob = 2024 - age,
        is_resident_2022 = ifelse(
          is_resident_2022 == 1,
          TRUE,
          ifelse(is_resident_2022 == 0, FALSE, NA)
        )
      )
  }

  if (site == "NP") {
    indiv_df <- indiv %>%
      tibble::tibble() %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID = tolower(SharingUnitID),
        hhead = HoH,
        yob = BirthYear,
        gender = Gender,
        group = Group_jati,
        group2 = Group_caste_macro,
        edu = Education,
        high_status = Status,
        #is_resident_2024 = resident_2024,
        is_resident = Resident,
        location = Location,
        ability_to_work = Work_ability,
        is_living = Alive,
        father_id = tolower(FatherID),
        mother_id = tolower(MotherID),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
        age = year - yob
      )
  }

  if (site == "UP") {
    indiv_df <- indiv %>%
      dplyr::mutate(
        ID = tolower(IndivID),
        SUID = tolower(SharingUnitID),
        hhead = HoH,
        yob = BirthYear,
        gender = Gender,
        group = Caste,
        edu = years_education,
        high_status = status,
        #high_status_k = status_myassessment,
        #is_resident_2024 = is_resident_2024,
        is_resident = is_resident_2025,
        location = Location_2025,
        ability_to_work = Ability_to_work,
        is_living = is_living_2025,
        father_id = tolower(FatherID),
        mother_id = tolower(MotherID),
        .keep = "unused",
        .before = 1
      ) %>%
      dplyr::mutate(
        age = year - yob
      )
  }

  ## standardize variables
  indiv_df <- indiv_df %>%
    dplyr::mutate(
      father_id = dplyr::case_match(
        father_id,
        "999" ~ NA_character_,
        "" ~ NA_character_,
        .default = father_id
      ),
      mother_id = dplyr::case_match(
        mother_id,
        "999" ~ NA_character_,
        "" ~ NA_character_,
        .default = mother_id
      ),
      ability_to_work = dplyr::case_match(
        ability_to_work,
        c("Full", "full", "good", "Ful") ~ "full",
        c("Moderate", "moderate", "low", "partial") ~ "moderate",
        c("None", "none") ~ "none",
        "" ~ NA_character_,
        .default = ability_to_work
      ),
      hhead = ifelse(
        hhead %in% c("1", "yes", "Yes", "HEAD"),
        TRUE,
        ifelse(hhead %in% c("0", "no", "No", "NOT_HEAD"), FALSE, NA)
      ),
      is_resident = ifelse(
        is_resident %in% c("1", "yes", "Yes", "Internal", TRUE),
        TRUE,
        ifelse(is_resident %in% c("0", "no", "No", "External", FALSE), FALSE, NA)
      ),
      is_living = ifelse(
        is_living %in% c("1", "yes", "Yes", TRUE),
        TRUE,
        ifelse(is_living %in% c("0", "no", "No", FALSE), FALSE, NA)
      ),
      gender = ifelse(
        gender %in% c("female", "Female", "F"),
        "Female",
        ifelse(gender %in% c("male", "Male", "M"), "Male", NA)
      ),
      high_status = ifelse(
        high_status %in% c("1", "yes", "Yes"),
        TRUE,
        ifelse(high_status %in% c("0", "no", "No"), FALSE, NA)
      )
    ) #%>%
    # in cases of NA gender, check to see if listed as father or mother and populate accordingly
    #dplyr::mutate(
    #  gender = dplyr::case_when(
    #    !is.na(gender) ~ gender,
    #    is.na(gender) & ID %in% father_id ~ "Male",
    #    is.na(gender) & ID %in% mother_id ~ "Female")
    #)

  indiv_df$ability_to_work <- as.factor(indiv_df$ability_to_work)
  indiv_df$gender <- as.factor(indiv_df$gender)
  indiv_df$group <- as.factor(indiv_df$group)

  main_variables <- c("ID", "SUID", "hhead", "age", "yob", "gender", "group", "edu", "high_status", "is_resident", "location", "is_living", "ability_to_work", "father_id", "mother_id", "SUID_2022", "group2", "is_resident_2022", "AorT", "PID", "high_status_k", "is_resident_2024", "respect_count")

  indiv_df <- indiv_df %>%
    dplyr::relocate(all_of(main_variables[1:which(main_variables == "mother_id")]), .before = 1)

  if (main_vars) {
    indiv_df <- indiv_df %>%
      dplyr::select(
        any_of(main_variables),
        any_of(extra_vars)
      )
  }

  return(indiv_df)
}
