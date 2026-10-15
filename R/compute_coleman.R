# compute_coleman --------------------------------------------------------------
# Computes the Coleman homophily index (per group) for every site and layer.
#
# Arguments
#   data_list : nested list [[site]][[layer]], each layer a list with `$net`
#               (igraph). e.g. `sites` or `sites_su`.
#   var_name  : vertex attribute to compute homophily on
#               (e.g. "group" or "group_macro").
#
# Returns a list by site, each a tibble with one row per layer (`edge_type`)
# and one column per group; `var_name` is stored as an attribute (used by
# print_coleman()).
#
# NB: won't work with `sites_allnoms`, due to blank entries for "group".
#
# Dependencies: purrr, tibble (via tidyverse), igraph, netseg

library(tidyverse)
library(igraph)

compute_coleman <- function(data_list, var_name) {
  result <- data_list %>%
    map(~ {
      .x %>%
        keep(~ !is.null(.x$net)) %>%
        map(~ {
          netseg::coleman(
            simplify(.$net, remove.loops = TRUE),
            vattr = as.character(vertex_attr(
              simplify(.$net, remove.loops = TRUE),
              var_name
            ))
          )
        })
    }) %>%
    map(~ {
      .x %>%
        map_dfr(~ as_tibble_row(.x), .id = "edge_type")
    })

  # Store var_name as an attribute
  attr(result, "var_name") <- var_name
  return(result)
}
