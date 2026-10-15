require(tidyverse)

#' Function to load the social support network data
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#'
#' @return For most sites, a standardized edge list data frame with columns for Ego, Alter, and Question. For WH and WL, a list containing multiple adjacency matrices for different network layers.

load_netdat <- function(site, sharepoint_path = Sys.getenv("REP2SI_DIR"), tn_site = NA) {
  net_dat <- read.csv(switch(
    site,
    "TN" = file.path(
      sharepoint_path,
      "rep2si-data/tn-data/de-identified/TN_Nodelist_2022.csv"
    ),
    "TM" = file.path(
      sharepoint_path,
      "rep2si-data/tm-data/de-identified/TM_Nodelist.csv"
    ),
    "TZ" = file.path(
      sharepoint_path,
      "rep2si-data/tz-data/de-identified/TZ_Nodelist.csv"
    ),
    "NP" = file.path(
      sharepoint_path,
      "rep2si-data/np-data/de-identified/endow/NP_Edgelist.csv"
    ),
    "UP" = file.path(
      sharepoint_path,
      "rep2si-data/up-data/de-identified/endow/UP_Edgelist.csv"
    ),
    "WL" = file.path(
      sharepoint_path,
      "rep2si-data/wl-data/de-identified/WL/WL_Networks_Wave_4.csv"
    ),
    "WH" = file.path(
      sharepoint_path,
      "rep2si-data/wh-data/de-identified/WH/WH_Networks_Wave_4.csv"
    )
  ))

  # limit columns for TM
  if (site == "TM") {
    net_dat <- dplyr::select(
      .data = net_dat,
      c(
        "Ego",
        "Question",
        colnames(net_dat)[which(grepl("Alter[1-9]", colnames(net_dat)))]
      )
    )
  }

  ## change node lists to edge lists
  if (site %in% c("TN", "TM", "TZ")) {
    net_dat <- reshape2::melt(net_dat, id.vars = c("Ego", "Question"))
    names(net_dat)[names(net_dat) == "value"] <- "Alter" # Rename the value column to "Alter"
    net_dat <- net_dat[, -3] # Drop the variable column
    net_dat <- net_dat[which(net_dat$Alter != ""), ] # Remove all rows with empty alter
  }

  ## align column names for WL
  ## and add extra RICH games data -- but note! different set of respondents!
  if (site %in% c("WL", "WH")) {
    pr <- read.csv(
      file.path(
        sharepoint_path,
        paste0(
           "rep2si-data/",
           tolower(site),
           "-data/de-identified/",
           site,
           "/",
           site,
           "_Peer_Ratings.csv"
         )
      ))
    # not doing anything with RICH games (giving, reducing, leaving) for now
    #rich <- read.csv(
    #  file.path(
    #    sharepoint_path,
    #    "rep2si-data/wl-data/de-identified/WL/WL_RICH.csv"
    #  ))

    pr <- subset(pr, pr$TokenPlaced == 1)
    pr <- pr[, c("PID", "AID", "Question")]
    pr$Question <- tolower(pr$Question)
    pr$Question[which(pr$Question == "friends")] <- "friends_pr" # aligning with other data

    net_dat <- rbind(net_dat, pr)

    colnames(net_dat)[which(colnames(net_dat) == "PID")] <- "Ego"
    colnames(net_dat)[which(colnames(net_dat) == "AID")] <- "Alter"
  }

  net_dat$Ego <- tolower(net_dat$Ego)
  net_dat$Alter <- tolower(net_dat$Alter)

  ## for TN, retaining respondents only from appropriate village
  if (site == "TN") {
    indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path)
    if (tn_site == "ala") {
      net_dat <- subset(
        net_dat,
        net_dat$Ego %in% indiv_df$ID[indiv_df$AorT == "Alakapuram"]
      )
    }
    if (tn_site == "ten") {
      net_dat <- subset(
        net_dat,
        net_dat$Ego %in% indiv_df$ID[indiv_df$AorT == "Tenpatti"]
      )
    }
  }

  ## define different network layers for each site
  if (site == "TN") {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "1" ~ "loan_req",
          "2" ~ "loan_giv",
          "3" ~ "exchange_req",
          "4" ~ "exchange_giv",
          "5" ~ "f_behav",
          "6" ~ "m_behav",
          "7" ~ "f_info",
          "8" ~ "m_info",
          "9" ~ "govt",
          "10" ~ "external",
          "11" ~ "impmat",
          "12" ~ "work",
          "13" ~ "generous",
          "14" ~ "influence",
          "15" ~ "devout",
          "16" ~ "sloan_req",
          "17" ~ "sloan_giv",
          .default = Question
        )
      )
  }
  if (site == "TM") {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "1" ~ "loan_req",
          "2" ~ "loan_giv",
          "3" ~ "exchange_req",
          "4" ~ "exchange_giv",
          "5" ~ "f_behav",
          "6" ~ "m_behav",
          "7" ~ "f_info",
          "8" ~ "m_info",
          "9" ~ "govt",
          "10" ~ "external",
          "11" ~ "godchil",
          "12" ~ "godpar",
          .default = Question
        )
      )
  }
  if (site == "TZ") {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "LoanIn" ~ "loan_req",
          "LoanOut" ~ "loan_giv",
          "FoodIn" ~ "exchange_req",
          "FoodOut" ~ "exchange_giv",
          "Labor" ~ "behav", # note: same question asked of both
          "SocialWomen" ~ "f_info",
          "SocialMen" ~ "m_info",
          "Organisations" ~ "govt",
          "External" ~ "external",
          "Market" ~ "external", ## DECISION: recoding 'market' to external as well
          .default = Question
        )
      )
  }
  if (site == "NP") {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "q1" ~ "loan_req",
          "q2" ~ "loan_giv",
          "q3" ~ "exchange_req",
          "q4" ~ "exchange_giv",
          "q5" ~ "f_behav",
          "q6" ~ "m_behav",
          "q7" ~ "f_info",
          "q8" ~ "m_info",
          "q9" ~ "govt",
          "q10" ~ "external",
          "q11" ~ "sloan_req",
          "q12" ~ "sloan_giv",
          "q13" ~ "impmat",
          "q14" ~ "parma_call",
          "q15" ~ "mela_call",
          "q16" ~ "mela_go",
          "q17" ~ "work",
          "q18" ~ "influence",
          "q19" ~ "generous",
          "q20" ~ "devout",
          .default = Question
        )
      )
  }
  if (site == "UP") {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "q1" ~ "loan_req",
          "q2" ~ "loan_giv",
          "q3" ~ "exchange_req",
          "q4" ~ "exchange_giv",
          "q5" ~ "f_behav",
          "q6" ~ "m_behav",
          "q7" ~ "f_info",
          "q8" ~ "m_info",
          "q9" ~ "govt",
          "q10" ~ "external",
          "q11" ~ "impmat",
          "q12" ~ "work",
          "q13" ~ "generous",
          "q14" ~ "influence",
          "q15" ~ "sloan_req",
          "q16" ~ "sloan_giv",
          .default = Question
        )
      )
  }
  if (site %in% c("WL", "WH")) {
    net_dat <- net_dat %>%
      dplyr::mutate(
        Question = dplyr::case_match(
          Question,
          "RecievedFrom" ~ "loan_req",
          "GivenTo" ~ "loan_giv",
          "WouldHelpYou" ~ "exchange_req",
          "WouldHelpThem" ~ "exchange_giv",
          "Work" ~ "behav", # not divided by gender
          "Friends" ~ "info", # not divided by gender
          "ONG" ~ "govt",
          .default = Question
        )
      )
  }

  return(net_dat)
}