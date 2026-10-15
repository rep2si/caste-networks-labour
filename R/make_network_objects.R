require(tidyverse)

#' Function to create igraph network objects
#'
#' @param site The site for which to create the network objects, one of "TN", "TM", "TZ", "NP", "UP", "WL", or "WH".
#' @param sharepoint_path The path to the SharePoint directory where the data files are stored.
#' @param node_set The set of nodes to include, one of "main" (all those in edge list), "surv", "res", "adres", "player", "survadres" (both surveyed and adult individuals), "playeradres" (both players and adult individuals), and "survplayeradres" (players, surveyed, and adult individuals).
#' @param tn_site If site is "TN", the specific village to consider, one of "ala" or "ten". Defaults to NA.
#' @param year The year for which to create the network objects, one of 2022, 2023, or 2024. Defaults to 2024.
#' @param adult_age What should be counted as 'adult' for the site. Defaults to 18.
#' @param edge_type The type of edges to include in the network, can be "sum" (the default) or a vector of name generators (e.g., c("exchange_giv", "exchange_req")). Options include: "loan_req", "loan_giv", "exchange_req", "exchange_giv", "f_behav", "m_behav", "f_info", "m_info", "govt", "external", Additional for some sites (mostly AZ/TE, NP, UP): "impmat", "work", "generous", "influence", "devout", "sloan_req", "sloan_giv", "work"; TM specific: "golchil", "godpar"; TZ specific "behav" (since no m/f diff), NP specific: "parma_call", "mela_go".
#' @param use_ds Whether to use double-sampled edges, can be "yes" or "no". Defaults to "no".
#' @param keep_extra Whether to keep extra name generators, can be "yes" or "no". Defaults to "no".
#' @param remove_mela Whether to remove mela name generator (specific to NP), can be "yes" or "no". Defaults to "no".
#' @param writeout Whether the resulting object should be written to Sharepoint, options "yes" or "no". Defaults to "no".
#'
#' @return A list containing the igraph network object and a data frame of node attributes.

make_network_objects <- function(
  site,
  sharepoint_path = Sys.getenv("REP2SI_DIR"),
  node_set,
  tn_site = NA,
  year = 2024,
  adult_age = 18,
  edge_type = "sum",
  use_ds = "no",
  keep_extra = "no",
  remove_mela = "no",
  writeout = "no"
) {

  # Stop is tn_site needed and not defined
  if(site == "TN" & !(tn_site %in% c("ala", "ten"))) stop("Invalid tn_site option passed. Must be 'ten' or 'ala'.")

  ## options for node_set = main, surv, res, adres, player, survadres
  ## options for tn_site = ala, ten
  ## options for year = 2022, 2023, 2024 (2022 for orig networks, 2023 for Tenpatti games, 2024 for Alakapuram games)
  ## note that el_df is NOT ALL NOMINATIONS: it currently excludes gender mis-aligned responses!

  ## load data
  el <- load_netdat(site = site, sharepoint_path = sharepoint_path, tn_site = tn_site)
  indiv_df <- load_indiv(site = site, sharepoint_path = sharepoint_path, year = year)

  respondents <- load_netsurvlist(site = site, tn_site = tn_site, edge_type = edge_type, sharepoint_path = sharepoint_path)

  ## define givers, receivers, and overall players (by loading allocations)
  allocations <- load_alloc(site = site, tn_site = tn_site, sharepoint_path = sharepoint_path)

  givers <- indiv_df$ID[which(indiv_df$ID %in% allocations$ID[is.na(allocations$responseTime) == FALSE])]
  receivers <- indiv_df$ID[which(indiv_df$ID %in% allocations$AID[is.na(allocations$responseTime) == FALSE])]
  players <- sort(unique(c(givers, receivers)))

  ## Add variable to note if person was dictator, was recipient, was surveyed for social support Qs
  indiv_df$was_giver <- indiv_df$ID %in% givers
  indiv_df$was_receiver <- indiv_df$ID %in% receivers
  indiv_df$was_networkresp <- indiv_df$ID %in% respondents

  ## defining list of residents
  res_list <- load_reslist(site = site, tn_site = tn_site, year = year, sharepoint_path = sharepoint_path)

  ## defining list of *adult* residents
  # note, in TM one female household head in TM was aged 15 and interviewed - would get excluded here.
  adres_list <- res_list[
    res_list %in% indiv_df$ID[which(indiv_df$yob <= (year - adult_age))]
  ]

  ## retaining only gender-aligned responses
  gender_misaligned <- c(
    which(
      el$Question %in%
        c("f_behav", "f_info") &
        indiv_df$gender[match(el$Ego, indiv_df$ID)] == "Male"
    ),
    which(
      el$Question %in%
        c("m_behav", "m_info") &
        indiv_df$gender[match(el$Ego, indiv_df$ID)] == "Female"
    )
  )
  if (length(gender_misaligned) > 0) {
    el <- el[-gender_misaligned, ]
  }
  el <- el %>%
    dplyr::mutate(
      Question = dplyr::case_match(
        Question,
        c("f_behav", "m_behav") ~ "behav",
        c("f_info", "m_info") ~ "info",
        .default = Question
      )
    )

  ## define name generator question types
  # universal:
  loan <- c("loan_req", "loan_giv")
  exchange <- c("exchange_req", "exchange_giv")
  double_sampled <- c("loan_giv", "exchange_giv")
  rep <- NULL
  extra <- NULL

  if (site %in% c("TN", "NP", "UP")) {
    small <- c("sloan_req", "sloan_giv")
  } else {
    small <- NULL
  }

  # site-specific
  if (site == "TN") {
    double_sampled <- c(double_sampled, "sloan_giv")
    rep <- c("generous", "influence", "devout")
    extra <- c("impmat", "work")
  }
  if (site == "TM") {
    extra <- c("godchil", "godpar")
  }
  if (site %in% c("TZ", "WL")) {
    extra <- NULL # 'External' or 'Market' for TZ could potentially be called as such
  }
  if (site == "NP") {
    double_sampled <- c(double_sampled, "sloan_giv", "mela_go") ## choosing to reverse 'who calls you for mela'
    rep <- c("generous", "influence", "devout")
    extra <- c(
      "impmat",
      "work",
      "parma_call",
      "mela_call",
      "mela_go",
      "mela_go_rev"
    )
  }
  if (site == "UP") {
    double_sampled <- c(double_sampled, "sloan_giv") ## choosing to reverse 'who calls you for mela'
    rep <- c("generous", "influence")
    extra <- c("impmat", "work")
  }

  ## def want to drop q15 for NP, as cut mid-way
  if (site == "NP") {
    el <- subset(el, el$Question != "mela_call")
  }

  if(site %in% c("WL", "WH")) { ## WL and WH special case
    peer_rate_types <- c("contempt","distrust","dominant","fear","friends_pr","generous","like","ostracized","passive","poor","popular","powerful","powerless","respect","rich","selfish","trust")
    supp_types <- c("behav","exchange_giv","exchange_req","govt","info","loan_giv","loan_req")
    if(edge_type == "sum") {
      el <- subset(el, !el$Question %in% peer_rate_types)
    } else { ## then what was given was single or multiple edge types
      if (substring(edge_type, 1, 2) == "c(") { ## if multiple
      all_edge_types <- eval(parse(text = edge_type))
        if (any(all_edge_types %in% peer_rate_types) & any(!all_edge_types %in% peer_rate_types)) {
          stop("Don't mix social support and peer rating edge types -- respondents differ!")
        }
        if (any(all_edge_types %in% peer_rate_types)) {
          el <- subset(el, el$Question %in% peer_rate_types)
        }
      } else { ## if single
        if(!edge_type %in% c(peer_rate_types, supp_types)) {
          stop("edge_type not recognized for WL!")
        }
        if(edge_type %in% peer_rate_types) {
          el <- subset(el, el$Question %in% peer_rate_types)
        } else {
          el <- subset(el, !el$Question %in% peer_rate_types)
        }
      }
    }
  }

  ## create el that flips double-sampled questions
  el_ds <- el

  el_ds$Reporter <- el_ds$Ego

  el_ds <- el_ds %>%
    dplyr::mutate(
      temp = ifelse(Question %in% double_sampled, Ego, Alter),
      Ego = ifelse(Question %in% double_sampled, Alter, Ego),
      Alter = temp,
      Question = dplyr::case_match(
        Question,
        "loan_giv" ~ "loan_giv_rev",
        "exchange_giv" ~ "exchange_giv_rev",
        "sloan_giv" ~ "sloan_giv_rev",
        "mela_go" ~ "mela_go_rev",
        .default = Question
      )
    ) %>%
    dplyr::select(-temp)

  ## removing double-sampled, relations, reputations, and extra site-specific questions, if present
  ## NOTE: for NP q16/q16rev gets dropped for use_ds == "no", but there is no 'single sampled' name gen of this, as q15 def dropped
  if (use_ds == "no") {
    if (keep_extra == "no") {
      el_main <- subset(
        el,
        !el$Question %in% c("Relations", "related", "unrelated", double_sampled, small, rep, extra)
      )
    }
    if (keep_extra == "yes") {
      el_main <- subset(
        el,
        !el$Question %in% c("Relations", "related", "unrelated", double_sampled, rep)
      )
    }
  }

  if (use_ds == "yes") {
    if (keep_extra == "no") {
      el_main <- subset(
        el_ds,
        !el_ds$Question %in% c("Relations", "related", "unrelated", small, rep, extra)
      )
    }
    if (keep_extra == "yes") {
      el_main <- subset(el_ds, !el_ds$Question %in% c("Relations", "related", "unrelated", rep))
    }
  }

  if (remove_mela == "yes") {
    el_main <- subset(
      el_main,
      !el_main$Question %in% c("mela_go", "mela_go_rev")
    )
  }

  ## change edge list to edge dataframe
  el_df <- reshape2::dcast(
    el_main,
    Ego + Alter ~ Question,
    value.var = "Alter",
    fill = 0,
    fun.aggregate = length
  )

  if (edge_type == "sum"){
    el_df$SupSum <- rowSums(el_df[, 3:ncol(el_df)])
    el_df <- subset(el_df, el_df$SupSum > 0)
  }

  ## All well and good, but if a particular edge type is desired, proceed with
  ## just that edge type(s) from the original edge list
  if (edge_type != "sum") {
    el <- rbind(
      el,
      el_ds[
        el_ds$Question %in% paste0(double_sampled, "_rev"),
        -which(colnames(el_ds) == "Reporter")
      ]
    )
    if (substring(edge_type, 1, 2) == "c(") {
      edge_type <- eval(parse(text = edge_type))
    }
    el_df <- subset(el, el$Question %in% edge_type)
    el_df <- reshape2::dcast(
      el_df,
      Ego + Alter ~ Question,
      value.var = "Alter",
      fill = 0,
      fun.aggregate = length
    )
  }

  ## edgelist of surveyed people
  el_surv <- subset(
    el_df,
    el_df$Ego %in% respondents & el_df$Alter %in% respondents
  )

  ## edgelist of residents
  el_res <- subset(el_df, el_df$Ego %in% res_list & el_df$Alter %in% res_list)

  ## edgelist of adult residents
  el_adres <- subset(
    el_df,
    el_df$Ego %in% adres_list & el_df$Alter %in% adres_list
  )

  ## edgelist of surveyed AND adult residents
  el_survadres <- subset(
    el_df,
    el_df$Ego %in%
      c(respondents, adres_list) &
      el_df$Alter %in% c(respondents, adres_list)
  )

  ## edgelist of players
  el_play <- subset(el_df, el_df$Ego %in% players & el_df$Alter %in% players)

  ## edgelist of players AND adult residents
  el_playeradres <- subset(
    el_df,
    el_df$Ego %in%
      c(players, adres_list) &
      el_df$Alter %in% c(players, adres_list)
  )

  ## edgelist of players AND surveyed AND adult residents
  el_survplayeradres <- subset(
    el_df,
    el_df$Ego %in%
      c(respondents, players, adres_list) &
      el_df$Alter %in% c(respondents, players, adres_list)
  )

  net <- NULL

  switch(
    node_set,
    "main" = {
      net <- igraph::graph_from_data_frame(el_df)
    },
    "surv" = {
      net <- igraph::graph_from_data_frame(el_surv)
      vertices_to_add <- respondents[which(
        !respondents %in% igraph::V(net)$name
      )]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "res" = {
      net <- igraph::graph_from_data_frame(el_res)
      vertices_to_add <- res_list[which(!res_list %in% igraph::V(net)$name)]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "adres" = {
      net <- igraph::graph_from_data_frame(el_adres)
      vertices_to_add <- adres_list[which(!adres_list %in% igraph::V(net)$name)]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "survadres" = {
      net <- igraph::graph_from_data_frame(el_survadres)
      vertices_to_add <- unique(c(respondents, adres_list))[which(
        !unique(c(respondents, adres_list)) %in% igraph::V(net)$name
      )]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "player" = {
      net <- igraph::graph_from_data_frame(el_play)
      vertices_to_add <- players[which(!players %in% igraph::V(net)$name)]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "playeradres" = {
      net <- igraph::graph_from_data_frame(el_playeradres)
      vertices_to_add <- unique(c(players, adres_list))[which(
        !unique(c(players, adres_list)) %in% igraph::V(net)$name
      )]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    "survplayeradres" = {
      net <- igraph::graph_from_data_frame(el_survplayeradres)
      vertices_to_add <- unique(c(respondents, players, adres_list))[which(
        !unique(c(respondents, players, adres_list)) %in% igraph::V(net)$name
      )]
      net <- igraph::add_vertices(
        net,
        length(vertices_to_add),
        name = vertices_to_add
      )
    },
    stop(
      "Invalid value for node_set. Must be one of 'main', 'surv', 'res', 'adres', 'survadres', 'player', 'playeradres', or 'survplayeradres'."
    )
  )

  df <- data.frame("ID" = igraph::vertex_attr(net, "name"))

  df <- merge(df, indiv_df, by = "ID", sort = FALSE, all.x = TRUE)

  net <- igraph::set_vertex_attr(net, name = "gender", value = df$gender)
  net <- igraph::set_vertex_attr(net, name = "age", value = df$age)
  net <- igraph::set_vertex_attr(net, name = "hhead", value = df$hhead)
  net <- igraph::set_vertex_attr(net, name = "group", value = df$group)
  net <- igraph::set_vertex_attr(net, name = "edu", value = df$edu)
  net <- igraph::set_vertex_attr(net, name = "status", value = df$high_status)
  net <- igraph::set_vertex_attr(net, name = "su", value = df$SUID)
  if (site == "TN") {
    if (year == 2022) {
      net <- igraph::set_vertex_attr(net, name = "su", value = df$SUID_2022)
    }
  }
  net <- igraph::set_vertex_attr(net, name = "networkresp", value = df$was_networkresp)
  net <- igraph::set_vertex_attr(net, name = "giver", value = df$was_giver)
  net <- igraph::set_vertex_attr(net, name = "receiver", value = df$was_receiver)
  net <- igraph::set_vertex_attr(net, name = "resident", value = df$is_resident)

  net <- list(net, df)
  names(net) <- c("net", "node_df")

  if (writeout == "no") {
    return(net)
  } else {
    if (site == "TN") if (tn_site %in% c("ala", "ten")) {
      path <- paste0(sharepoint_path, "/rep2si-data/", tolower(site), "-data/processed/", site, "_", node_set, "_", use_ds, "_double_sampled_", keep_extra, "_extra_", tn_site, "_", year, "_net.rdata")
    } else {
      path <- paste0(sharepoint_path, "/rep2si-data/", tolower(site), "-data/processed/", site, "_", node_set, "_", use_ds, "_double_sampled_", keep_extra, "_extra", "_net.rdata")
    }
    print(paste("saving to", path))
    save(net, file = path)
  }
}