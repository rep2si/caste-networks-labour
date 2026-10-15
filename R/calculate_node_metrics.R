library(igraph)
library(dplyr)

#' Calculate Node-Level Network Metrics
#'
#' Computes a comprehensive set of centrality and structural measures for each
#' node in a network, with support for weighted and directed networks.
#'
#' @param g An igraph object
#' @param weights Character string specifying the name of an edge attribute to
#'   use as weights (interpreted as tie strength), or NULL for unweighted
#'   calculations. Default is NULL.
#' @param group_attr Character string specifying the name of a node attribute
#'   to use for calculating the E-I index, or NULL to skip. Default is NULL.
#' @param remove_loops TRUE/FALSE for whether self-loops should be removed.
#'   Default is TRUE.
#' @return A tibble with one row per node containing all calculated measures
#' @examples
#' # Unweighted network
#' g <- sample_gnp(50, 0.1)
#' measures <- calculate_node_metrics(g)
#'
#' # Weighted network (weights = tie strength)
#' E(g)$weight <- runif(ecount(g))
#' measures <- calculate_node_metrics(g, weights = "weight")
#'
#' # With group attribute for E-I index
#' V(g)$group <- sample(c("A", "B", "C"), vcount(g), replace = TRUE)
#' measures <- calculate_node_metrics(g, weights = "weight", group_attr = "group")
calculate_node_metrics <- function(
    g,
    weights = NULL,
    group_attr = NULL,
    remove_loops = TRUE
) {
  
  # Validate input
  if (!inherits(g, "igraph")) {
    stop("Input must be an igraph object")
  }
  
  # Check if node names exists
  if (!"name" %in% vertex_attr_names(g)) {
    stop(sprintf("Vertices not named with 'name' as attribute"))
  }
  
  # Check if weights attribute exists
  if (!is.null(weights) && !weights %in% edge_attr_names(g)) {
    stop(sprintf("Edge attribute '%s' not found in graph", weights))
  }
  
  # Check if group attribute exists
  if (!is.null(group_attr) && !group_attr %in% vertex_attr_names(g)) {
    stop(sprintf("Node attribute '%s' not found in graph", group_attr))
  }
  
  # Remove self-loops, if specified
  if (remove_loops == TRUE) {
    g <- simplify(g, remove.loops = TRUE, remove.multiple = FALSE)
  }
  
  # Extract weights if specified
  weight_vec <- if (!is.null(weights)) edge_attr(g, weights) else NULL
  
  # For betweenness, invert weights so that stronger ties = shorter distances
  # Small constant added to avoid division by zero
  betweenness_weights <- if (!is.null(weight_vec)) {
    1 / (weight_vec + 1e-10)
  } else {
    NULL
  }
  
  # Check if graph is directed
  is_directed <- is_directed(g)
  
  # Initialize results data frame
  results <- tibble(
    ID = V(g)$name
  )
  
  # Calculate undirected, unweighted measures
  results <- results %>%
    mutate(
      degree = degree(g, mode = "all"),
      betweenness_und_unw = scales::rescale(betweenness(g, normalized = TRUE, directed = FALSE, weights = NA)),
      eigenvector_und_unw = eigen_centrality(g, directed = FALSE, weights = NA)$vector,
      clustering_coef = transitivity(g, type = "local", weights = NA, isolates = "NaN")
    )

  # Calculate undirected, weighted measures
  if (!is.null(weights)) {
    results <- results %>%
      mutate(
        strength = strength(g, mode = "all", weights = weight_vec),
        betweenness_und_w = scales::rescale(betweenness(g, normalized = TRUE, directed = FALSE, weights = betweenness_weights)),
        eigenvector_und_w = eigen_centrality(g, directed = FALSE, weights = weight_vec)$vector,
        # commenting out weighted clustering coefficient, as getting error about multi-edges, even when none exist!
        # unweighted, undirected clustering coefficient is much more typically used and should suffice
        #clustering_coef_w = transitivity(g, type = "weighted", weights = weight_vec, isolates = "NaN")
        )
  }
  
  # Calculate directed measures
  if (is_directed) {
    # unweighted measures
    results <- results %>%
      mutate(
        in_degree = degree(g, mode = "in"),
        out_degree = degree(g, mode = "out"),
        betweenness_d_unw = scales::rescale(betweenness(g, normalized = TRUE, directed = TRUE, weights = NA)),
        eigenvector_d_unw = eigen_centrality(g, directed = TRUE, weights = NULL)$vector,
        pagerank_d_unw = scales::rescale(page_rank(g, directed = TRUE, weights = NA)$vector)
      )

    # weighted measures
    if (!is.null(weights)) {
      results <- results %>%
        mutate(
          in_strength = strength(g, mode = "in", weights = weight_vec),
          out_strength = strength(g, mode = "out", weights = weight_vec),
          betweenness_d_w = scales::rescale(betweenness(g, normalized = TRUE, directed = TRUE, weights = betweenness_weights)),
          eigenvector_d_w = eigen_centrality(g, directed = TRUE, weights = weight_vec)$vector,
          pagerank_d_w = scales::rescale(page_rank(g, weights = weight_vec)$vector)
        )
    }
  }
  
  # Calculate E-I index if group attribute specified
  if (!is.null(group_attr)) {
    node_groups <- vertex_attr(g, group_attr)
    
    ei_index <- sapply(seq_len(vcount(g)), function(i) {
      # Get nominated nodes (directed) or all neighbors (undirected)
      if (is_directed) {
        neighbors <- neighbors(g, i, mode = "out")
      } else {
        neighbours <- neighbors(g, i, mode = "all")
      }
      
      if (length(neighbors) == 0) {
        return(NA_real_)
      }
      
      # Get groups of neighbors
      neighbor_groups <- node_groups[neighbors]
      node_group <- node_groups[i]
      
      # Calculate internal and external alters
      internal_alters <- sum(neighbor_groups == node_group, na.rm = TRUE)
      external_alters <- sum(neighbor_groups != node_group, na.rm = TRUE)
      
      total_alters <- internal_alters + external_alters
      if (total_alters == 0) {
        return(NA_real_)
      }
      
      return((external_alters - internal_alters) / total_alters)
    })
    
    results <- results %>%
      mutate(
        ei_index = ei_index
      )
  }
  
  return(results)
}
