require(tidyverse)

#' Function to load list set of surveyed respondents
#'
#' @param site The site code (e.g., "TN", "WL", "WH", "TM", "TZ", "NP", "UP").
#' @param tn_site Optional parameter for TN site to specify "ala" or "ten".
#' @param edge_type Ignored for all but WL! The type of edges to include in the network, can be "sum" (the default) or a vector of name generators (e.g., c("exchange_giv", "exchange_req")). Options include: "loan_req", "loan_giv", "exchange_req", "exchange_giv", "f_behav", "m_behav", "f_info", "m_info", "govt", "external", Additional for some sites (mostly AZ/TE, NP, UP): "impmat", "work", "generous", "influence", "devout", "sloan_req", "sloan_giv", "work"; TM specific: "golchil", "godpar"; TZ specific "behav" (since no m/f diff), NP specific: "parma_call", "mela_go".
#' @param sharepoint_path The path to the SharePoint directory containing the data files.
#'
#' @return The loaded kinship list object

load_netsurvlist <- function(site, tn_site = NA, edge_type = "sum", sharepoint_path = Sys.getenv("REP2SI_DIR")) {
## basically, this is complicated for WL because Elly thought it would be fun to add in the peer ratings!
  net_dat <- load_netdat(
    site = site,
    sharepoint_path = sharepoint_path,
    tn_site = tn_site
  )

  if(site %in% c("TN","TM","TZ","NP","UP")) {
    respondents <- sort(unique(net_dat$Ego))
  }

  if(site %in% c("WL", "WH")) { ## WL and WH special case
    peer_rate_types <- c("contempt","distrust","dominant","fear","friends_pr","generous","like","ostracized","passive","poor","popular","powerful","powerless","respect","rich","selfish","trust")
    supp_types <- c("behav","exchange_giv","exchange_req","govt","info","loan_giv","loan_req")
    if(edge_type == "sum") {
      net_dat <- subset(net_dat, !net_dat$Question %in% peer_rate_types)
      respondents <- sort(unique(net_dat$Ego))
    } else { ## then what was given was single or multiple edge types
      if (substring(edge_type, 1, 2) == "c(") { ## if multiple
      all_edge_types <- eval(parse(text = edge_type))
        if (any(all_edge_types %in% peer_rate_types) & any(!all_edge_types %in% peer_rate_types)) {
          stop("Don't mix social support and peer rating edge types -- respondents differ!")
        }
        if (any(all_edge_types %in% peer_rate_types)) {
          net_dat <- subset(net_dat, net_dat$Question %in% peer_rate_types)
        }
      } else { ## if single
        if(!edge_type %in% c(peer_rate_types, supp_types)) {
          stop("edge_type not recognized for WL!")
        }
        if(edge_type %in% peer_rate_types) {
          net_dat <- subset(net_dat, net_dat$Question %in% peer_rate_types)
        } else {
          net_dat <- subset(net_dat, !net_dat$Question %in% peer_rate_types)
        }
      }
      respondents <- sort(unique(net_dat$Ego))
    }
  }
  return(respondents)
}