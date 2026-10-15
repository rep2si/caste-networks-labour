# create_overlap_matrix --------------------------------------------------------
# Builds a square layer-by-layer matrix for one site: cell [ref, other] is the
# proportion of edges in `ref` that also exist in `other`.
#
# Arguments
#   df : a dyad data frame for one site (an element of `dyad_list`), containing
#        one `<layer>_egonom` 0/1 column per layer.
#
# Returns a numeric matrix with rows/columns named by layer. Rows for layers
# with no edges are left as NA.
#
# GLOBAL DEPENDENCY: reads the global named vector `edges` (the layer
# definitions), excluding the "sum" and "nonwork" entries. This must exist in
# the calling environment.
#
# Packages: dplyr (via tidyverse)

library(tidyverse)

create_overlap_matrix <- function(df) {
  layer_cols <- paste0(edges[setdiff(names(edges), c("sum", "nonwork"))], "_egonom")

  overlap_mat <- matrix(NA,
                        nrow = length(layer_cols),
                        ncol = length(layer_cols),
                        dimnames = list(layer_cols, layer_cols))

  for (ref in layer_cols) {
    edges_in_ref <- df %>% filter(.data[[ref]] == 1)
    if (nrow(edges_in_ref) > 0) {
      for (other in layer_cols) {
        overlap_mat[ref, other] <- sum(edges_in_ref[[other]] == 1) / nrow(edges_in_ref)
      }
    }
  }

  rownames(overlap_mat) <- edges[setdiff(names(edges), c("sum", "nonwork"))]
  colnames(overlap_mat) <- edges[setdiff(names(edges), c("sum", "nonwork"))]

  return(overlap_mat)
}

# Example usage (as in the original script):
# overlap_matrices <- map(dyad_list, create_overlap_matrix)
