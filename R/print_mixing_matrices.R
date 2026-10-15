# print_mixing_matrices --------------------------------------------------------
# Prints (as Rmd-ready HTML with markdown headings) the proportion and
# observed/expected-ratio mixing tables for every site and requested layer.
#
# Arguments
#   mixing_list : output of compute_mixing_matrices() (must carry the
#                 "var_name" attribute).
#   edge_types  : character vector of layers to print; NULL = all layers
#                 available for the first site.
#
# Depends on: format_mixing_table()  (source("format_mixing_table.R") first)
# Packages:   purrr (via tidyverse), kableExtra (scroll_box)

library(tidyverse)
library(kableExtra)

source("format_mixing_table.R")

print_mixing_matrices <- function(mixing_list, edge_types = NULL) {
  # Extract var_name from attributes
  var_name <- attr(mixing_list, "var_name")
  if (is.null(var_name)) {
    stop("mixing_list must have 'var_name' attribute. Use compute_mixing_matrices() to create it.")
  }

  # If edge_types not specified, use all available
  if (is.null(edge_types)) {
    edge_types <- names(mixing_list[[1]])
  }

  mixing_list %>%
    walk2(names(.), ~ {
      cat("\n\n###", .y, "-", var_name, "\n\n")

      # Loop through specified edge types
      for (edge_type in edge_types) {
        if (edge_type %in% names(.x)) {
          cat("\n\n#### ", edge_type, " - Proportions\n\n")
          format_mixing_table(.x[[edge_type]]$prob,
                             title = paste(.y, edge_type, var_name, "Mixing Matrix - Proportions")) %>%
            scroll_box(width = "100%", height = "100%") %>%
            print()

          cat("\n\n#### ", edge_type, " - Observed / Expected Ratio\n\n")
          format_mixing_table(.x[[edge_type]]$obs_exp_ratio,
                             title = paste(.y, edge_type, var_name, "Mixing Matrix - Obs/Exp Ratio"),
                             diverging = TRUE,
                             center_val = 1) %>%
            scroll_box(width = "100%", height = "100%") %>%
            print()
        }
      }
    })
}
