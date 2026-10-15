# compute_mixing_matrices ------------------------------------------------------
# Computes, for every site and layer, the mixing matrix of ties between groups
# (counts), row-wise percentages, expected counts under a null model of random
# mixing by group size, and the observed/expected ratio.
#
# Arguments
#   data_list : nested list [[site]][[layer]], each layer a list with `$net`
#               (igraph) and `$node_df` (data frame). e.g. `sites` or `sites_su`.
#   var_name  : name of the vertex attribute / node_df column to mix on
#               (e.g. "group" or "group_macro").
#
# Returns a nested list [[site]][[layer]] of list(counts, prob, expected,
# obs_exp_ratio), with `var_name` stored as an attribute (used by
# print_mixing_matrices()).
#
# NB: will throw errors with `sites_allnoms`, as group_macro is not written
# onto the igraph vertices for that object.
#
# Dependencies: purrr (via tidyverse), dplyr, tibble, igraph, netseg

library(tidyverse)
library(igraph)

compute_mixing_matrices <- function(data_list, var_name) {
  result <- data_list %>%
    map(~ {
      .x %>%
        keep(~ !is.null(.x$net)) %>%
        map(~ {
          # Get mixing matrix (counts)
          mix_counts <- netseg::mixingm(
            simplify(.$net, remove.loops = TRUE),
            var_name
          )

          # Get group sizes, to compute expected mixing
          group_sizes_raw <- .x$node_df %>%
            count(.data[[var_name]]) %>%
            deframe()

          # Align group_sizes to the actual categories present in mix_counts,
          # filling any missing category (present in node_df but absent from
          # the graph, e.g. due to NA attrs or isolates removed by simplify())
          # with 0, and dropping any category in node_df not used by mixingm.
          mix_categories <- colnames(mix_counts)
          group_sizes <- setNames(
            rep(0, length(mix_categories)),
            mix_categories
          )
          matched <- intersect(names(group_sizes_raw), mix_categories)
          group_sizes[matched] <- group_sizes_raw[matched]

          # Row-wise probability matrix
          mix_prob <- mix_counts / rowSums(mix_counts) * 100

          # Expected mixing under null model
          ego_ties <- rowSums(mix_counts)
          alter_props <- group_sizes / sum(group_sizes)
          mix_expected <- outer(ego_ties, alter_props)
          dimnames(mix_expected) <- dimnames(mix_counts)

          # Return all
          list(
            counts = mix_counts,
            prob = mix_prob,
            expected = mix_expected,
            obs_exp_ratio = mix_counts / mix_expected
          )
        })
    })

  # Store var_name as an attribute
  attr(result, "var_name") <- var_name
  return(result)
}
