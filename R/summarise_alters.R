require(tidyverse)

#' Function to summarise alters from ego-alter nominations
#'
#' @param site Character string indicating the site (e.g., "TN", "UP")
#' @param tn_site Character string indicating the specific site within TN (options "ala" or "ten")
#' @param sharepoint_path Character string indicating the path to save the output
#' @param year Year to be used to define residency and adult age (defaults to 2024)
#' @param ng_to_ignore Vector of questions that should be excluded *from the summed set of alters* (e.g., reputation or double-sampled questions)
#' @param close_cutoff Numeric threshold for defining close kinship (default is 0.25)
#'
#' @return A list containing two data frames summarising alters by question and overall

summarise_alters <- function(
  site,
  tn_site = NA,
  year = 2024,
  sharepoint_path = Sys.getenv("REP2SI_DIR"),
  ng_to_ignore = c(
    "generous",
    "influence",
    "devout",
    "exchange_req",
    "loan_req",
    "Relations",
    "related",
    "unrelated",
  ), # default reputation, double-sampled questions, and relatedness layers
  close_cutoff = 0.25
) {
  el <- load_netdat(
    site = site,
    tn_site = tn_site,
    sharepoint_path = sharepoint_path
  )
  indiv_df <- load_indiv(
    site = site,
    sharepoint_path = sharepoint_path,
    year = year
  )
  res_list <- load_reslist(
    site = site,
    tn_site = tn_site,
    year = year,
    sharepoint_path = sharepoint_path
  )
  kin_ties <- load_relatedness(site = site, sharepoint_path = sharepoint_path)

  # subset kinship to retain only those above specified threshold for closeness
  close_con_kin <- kin_ties$rel[
    kin_ties$rel$r >= close_cutoff & kin_ties$rel$i != kin_ties$rel$j,
  ]
  close_aff_kin <- kin_ties$aff[
    kin_ties$aff$s >= close_cutoff & kin_ties$aff$i != kin_ties$aff$j,
  ]

  alters_summary_by_ng <- el %>%
    dplyr::mutate(Question = forcats::fct_relevel(Question)) %>% # including this generates 0s for null cases
    dplyr::group_by(Ego, Question, .drop = FALSE) %>%
    dplyr::summarise(
      .groups = "drop_last",
      alter_ids = list(unique(Alter)),
      alter_count = length(unique(Alter)), # using this, not n() in case of repeat noms
      external_ids = list(unique(Alter[!Alter %in% res_list])),
      external_count = length(unique(Alter[!Alter %in% res_list])),
      # filter to keep only cases of different group or gender
      group_diff_ids = list(unique(Alter[
        !indiv_df$group[match(Ego, indiv_df$ID)] ==
          indiv_df$group[match(Alter, indiv_df$ID)]
      ])),
      group_diff_count = length(unique(Alter[
        !indiv_df$group[match(Ego, indiv_df$ID)] ==
          indiv_df$group[match(Alter, indiv_df$ID)]
      ])),
      gender_diff_ids = list(unique(Alter[
        !indiv_df$gender[match(Ego, indiv_df$ID)] ==
          indiv_df$gender[match(Alter, indiv_df$ID)]
      ])),
      gender_diff_count = length(unique(Alter[
        !indiv_df$gender[match(Ego, indiv_df$ID)] ==
          indiv_df$gender[match(Alter, indiv_df$ID)]
      ])),
      # filter to keep only cases of non-close relatives
      non_con_kin_ids = list(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_con_kin$i, close_con_kin$j)
      ])),
      non_con_kin_count = length(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_con_kin$i, close_con_kin$j)
      ])),
      non_aff_kin_ids = list(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_aff_kin$i, close_aff_kin$j)
      ])),
      non_aff_kin_count = length(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_aff_kin$i, close_aff_kin$j)
      ]))
    )

  # almost identical to above, but not also group_by Question
  # two additional vars for counts of nominations, not just alters
  alters_summary_sum <- el %>%
    ## remove reputation questions and double-sampled as not alters in the same sense
    dplyr::filter(!Question %in% ng_to_ignore) %>%
    dplyr::mutate(Question = forcats::fct_relevel(Question)) %>%
    (\(x) {
      message(
        "For the sum network, the retained name generators are: ",
        paste(unique(x$Question), collapse = ", ")
      )
      x
    })() %>%
    dplyr::group_by(Ego, .drop = FALSE) %>%
    dplyr::summarise(
      alter_ids = list(unique(Alter)),
      alter_count = length(unique(Alter)),
      nom_count = dplyr::n(),
      external_ids = list(unique(Alter[!Alter %in% res_list])),
      external_count = length(unique(Alter[!Alter %in% res_list])),
      external_nom_count = length(Alter[!Alter %in% res_list]),
      # filter to keep only cases of different group or gender
      group_diff_ids = list(unique(Alter[
        !indiv_df$group[match(Ego, indiv_df$ID)] ==
          indiv_df$group[match(Alter, indiv_df$ID)]
      ])),
      group_diff_count = length(unique(Alter[
        !indiv_df$group[match(Ego, indiv_df$ID)] ==
          indiv_df$group[match(Alter, indiv_df$ID)]
      ])),
      gender_diff_ids = list(unique(Alter[
        !indiv_df$gender[match(Ego, indiv_df$ID)] ==
          indiv_df$gender[match(Alter, indiv_df$ID)]
      ])),
      gender_diff_count = length(unique(Alter[
        !indiv_df$gender[match(Ego, indiv_df$ID)] ==
          indiv_df$gender[match(Alter, indiv_df$ID)]
      ])),
      # filter to keep only cases of non-close relatives
      non_con_kin_ids = list(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_con_kin$i, close_con_kin$j)
      ])),
      non_con_kin_count = length(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_con_kin$i, close_con_kin$j)
      ])),
      non_aff_kin_ids = list(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_aff_kin$i, close_aff_kin$j)
      ])),
      non_aff_kin_count = length(unique(Alter[
        !paste(Ego, Alter) %in% paste(close_aff_kin$i, close_aff_kin$j)
      ]))
    )

  qs <- unique(alters_summary_by_ng$Question)
  alters_summary_by_ng$unique_ids <- NA
  alters_summary_by_ng$unique_count <- NA

  for (id in alters_summary_sum$Ego) {
    for (ng in qs) {
      alters_for_this_ng <- unlist(alters_summary_by_ng$alter_ids[
        alters_summary_by_ng$Ego == id & alters_summary_by_ng$Question == ng
      ])
      alters_for_other_ngs <- unique(unlist(alters_summary_by_ng$alter_ids[
        alters_summary_by_ng$Ego == id & alters_summary_by_ng$Question != ng
      ]))
      unique_alters <- alters_for_this_ng[
        !alters_for_this_ng %in% alters_for_other_ngs
      ]
      alters_summary_by_ng$unique_count[
        alters_summary_by_ng$Ego == id & alters_summary_by_ng$Question == ng
      ] <- length(unique_alters)
      if (length(unique_alters) > 0) {
        alters_summary_by_ng$unique_ids[
          alters_summary_by_ng$Ego == id & alters_summary_by_ng$Question == ng
        ] <- list(unique_alters)
      }
    }
  }

  alters_summary_by_ng <- alters_summary_by_ng %>%
    dplyr::mutate(
      external_prop = external_count / alter_count,
      group_diff_prop = group_diff_count / alter_count,
      gender_diff_prop = gender_diff_count / alter_count,
      non_con_kin_prop = non_con_kin_count / alter_count,
      non_aff_kin_prop = non_aff_kin_count / alter_count
    )

  alters_summary_sum <- alters_summary_sum %>%
    dplyr::mutate(
      external_prop = external_count / alter_count,
      group_diff_prop = group_diff_count / alter_count,
      gender_diff_prop = gender_diff_count / alter_count,
      non_con_kin_prop = non_con_kin_count / alter_count,
      non_aff_kin_prop = non_aff_kin_count / alter_count,
      nom_average = nom_count / alter_count
    )

  alters_summary <- list(alters_summary_by_ng, alters_summary_sum)
  names(alters_summary) <- c("by_ng", "sum")

  if (site == "TN") {
    save_path <- file.path(
      sharepoint_path,
      "rep2si-data",
      paste0(tolower(site), "-data"),
      "processed",
      paste0(tolower(tn_site), "_alters_summary.rds")
    )
  } else {
    save_path <- file.path(
      sharepoint_path,
      "rep2si-data",
      paste0(tolower(site), "-data"),
      "processed",
      paste0(tolower(site), "_alters_summary.rds")
    )
  }

  #saveRDS(alters_summary, file = save_path)
  return(alters_summary)
}



## Commenting out, but below creates plots of overlap of nominations across ties

# require(gridExtra)
#
# ## looking at overlap of nominations
# to_use <- c("loan_req", "exchange_req", "behav", "info")
#
# plots <- list()
#
# for (id in alters_summary_sum$Ego) {
#   sum <- list()
#   for (ng in to_use) {
#     sum[ng] <- alters_summary_by_ng$alter_ids[alters_summary_by_ng$Ego == id & alters_summary_by_ng$Question == ng]
#   }
#
#   ## two options for plot:
#   ## 1: heat
#   plot <- ggVennDiagram::ggVennDiagram(sum, label_alpha = 0, set_size = 3, label_size = 3) +
#    ggtitle(id) +
#    scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,10)) +
#    theme(legend.position = "none")
#   plots[[id]] <- plot
#
#   ## 2: list out each alter
#   # v <- VennDiagram::venn.diagram(sum,
#   #         category.names = c("Loan", "Exchange", "Behav", "Info"),
#   #         cex = 0.5,
#   #         fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
#   #         filename = NULL)
#   # ## 9 = only behav
#   # v[[9]]$label <- paste(setdiff(sum$behav, c(sum$exchange_req, sum$loan_req, sum$info)), collapse="\n")
#   # ## 10 = behav + info no loan_req or exchange_req
#   # v[[10]]$label <- paste(setdiff(intersect(sum$behav, sum$info), c(sum$loan_req, sum$exchange_req)), collapse="\n")
#   # ## 11 = only info
#   # v[[11]]$label <- paste(setdiff(sum$info, c(sum$exchange_req, sum$behav, sum$loan_req)), collapse="\n")
#   # ## 12 = loan_req + behav no info or exchange_req
#   # v[[12]]$label <- paste(setdiff(intersect(sum$loan_req, sum$behav), c(sum$info, sum$exchange_req)), collapse="\n")
#   # ## 13 = loan_req + behav + info no exchange_req
#   # v[[13]]$label <- paste(setdiff(intersect(intersect(sum$loan_req, sum$behav), sum$info), sum$exchange_req), collapse="\n")
#   # ## 14 = full intersection
#   # v[[14]]$label <- paste(intersect(intersect(intersect(sum$info, sum$exchange_req), sum$behav), sum$loan), collapse="\n")
#   # ## 15 = info + exchange_req + behav not loan_req
#   # v[[15]]$label <- paste(setdiff(intersect(intersect(sum$info, sum$exchange_req), sum$behav), sum$loan), collapse="\n")
#   # ## 16 = info + exchange_req no behav or loan_req
#   # v[[16]]$label <- paste(setdiff(intersect(sum$info, sum$exchange_req), c(sum$loan_req, sum$behav)), collapse="\n")
#   # ## 17 = just loan_req
#   # v[[17]]$label <- paste(setdiff(sum$loan_req, c(sum$exchange_req, sum$behav, sum$info)), collapse="\n")
#   # ## 18 = info + loan_req no exchange_req or behav
#   # v[[18]]$label <- paste(setdiff(intersect(sum$info, sum$loan_req), c(sum$exchange_req, sum$behav)), collapse="\n")
#   # ## 19 = info + loan_req + exchange_req not behav
#   # v[[19]]$label <- paste(setdiff(intersect(intersect(sum$info, sum$loan_req), sum$exchange_req), sum$behav), collapse="\n")
#   # ## 20 = behav + loan + exchange_req not info
#   # v[[20]]$label <- paste(setdiff(intersect(intersect(sum$behav, sum$loan_req), sum$exchange_req), sum$info), collapse="\n")
#   # ## 21 = behav + exchange_req not loan_req or info
#   # v[[21]]$label <- paste(setdiff(intersect(sum$behav, sum$exchange_req), c(sum$loan_req, sum$info)), collapse="\n")
#   # ## 22 = just exchange_req
#   # v[[22]]$label <- paste(setdiff(sum$exchange_req, c(sum$loan_req,sum$behav, sum$info)), collapse="\n")
#   # ## 23 = loan_req + exchange_req not behav or info
#   # v[[23]]$label <- paste(setdiff(intersect(sum$laon_req, sum$exchange_req), c(sum$behav, sum$info)), collapse="\n")
#   #
#   # plots[[id]] <- gridExtra::grid.arrange(gTree(children = v), top = id)
#
# }

## then can select set to plot
#do.call("grid.arrange", c(plots[60:75], ncol = 4))

## similarly could look at reputational nominations, where we have this, etc
