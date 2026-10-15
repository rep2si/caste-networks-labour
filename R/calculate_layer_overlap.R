# calculate_layer_overlap ------------------------------------------------------
# For one site's dyad table, calculates: among edges present in a reference
# layer, the proportion (and count) that also exist in each other layer.
#
# Arguments
#   df : a dyad data frame for one site (an element of `dyad_list`), containing
#        one `<layer>_egonom` 0/1 column per layer.
#
# Returns a tibble: reference_layer, other_layer, n_edges_in_ref, n_overlap,
# prop_overlap. Reference layers with no edges are skipped.
#
# GLOBAL DEPENDENCY: reads the global named vector `edges` (the layer
# definitions), excluding the "sum" and "nonwork" entries. This must exist in
# the calling environment.
#
# Packages: dplyr, purrr, stringr, tibble (via tidyverse)

library(tidyverse)

calculate_layer_overlap <- function(df) {

  # Get layer column names (exclude 'from' and 'to')
  layer_cols <- paste0(edges[setdiff(names(edges), c("sum", "nonwork"))], "_egonom")

  # Create results dataframe
  results <- map_dfr(layer_cols, function(ref_layer) {

    # Filter to edges where reference layer == 1
    edges_in_ref <- df %>% filter(.data[[ref_layer]] == 1)

    if (nrow(edges_in_ref) == 0) {
      return(NULL)  # Skip if no edges in this layer
    }

    # Calculate proportion of each other layer that also == 1
    overlap_props <- map_dbl(layer_cols, function(other_layer) {
      sum(edges_in_ref[[other_layer]] == 1) / nrow(edges_in_ref)
    })

    tibble(
      reference_layer = str_remove(ref_layer, "_egonom"),
      other_layer = str_remove(layer_cols, "_egonom"),
      n_edges_in_ref = nrow(edges_in_ref),
      n_overlap = map_dbl(layer_cols, ~ sum(edges_in_ref[[.x]] == 1)),
      prop_overlap = overlap_props
    )
  })

  return(results)
}

# Example usage (as in the original script):
# layer_overlap <- map_dfr(names(dyad_list), function(site_name) {
#   calculate_layer_overlap(dyad_list[[site_name]]) %>%
#     mutate(site = site_name, .before = 1)
# })
