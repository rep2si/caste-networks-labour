library(igraph)

#' Function to calculate network summary statistics
#'
#' @param net igraph object to analyze
#' @param continuous_attrs character vector of continuous vertex attribute names for assortativity and inequality metrics
#' @param categorical_attrs character vector of categorical vertex attribute names for assortativity
#' @return named numeric vector with network metrics

calculate_network_metrics <- function(
  net,
  continuous_attrs = NULL,
  categorical_attrs = NULL
  ) {

  # Input validation
  if (is.null(net) || !is_igraph(net)) {
    warning("Invalid or NULL network provided")
    return(setNames(numeric(0), character(0)))
  }

  if (vcount(net) == 0) {
    warning("Empty network provided")
    return(setNames(numeric(0), character(0)))
  }

  # Calculate base network metrics
  base_metrics <- c(
    vcount = vcount(net),
    ecount = ecount(net),
    density = edge_density(net),
    mean_degree = mean(degree(net)),
    diameter = diameter(net),
    mean_dist = mean_distance(net, unconnected = TRUE),
    deg_centralization = centr_degree(net, mode = "all")$centralization,
    transitivity = transitivity(net),
    assort_deg = assortativity_degree(net)
  )

  # Add direction-specific measures for directed networks only
  if (is_directed(net)) {
    base_metrics <- c(
      base_metrics,
      reciprocity = reciprocity(net),
      ideg_centralization = centr_degree(net, mode = "in")$centralization,
      odeg_centralization = centr_degree(net, mode = "out")$centralization
    )
  }

  # Initialize vectors for assortativity metrics
  continuous_metrics <- setNames(numeric(0), character(0))
  gini_metrics <- setNames(numeric(0), character(0))
  exp_gini_metrics <- setNames(numeric(0), character(0))
  categorical_metrics <- setNames(numeric(0), character(0))

  # Calculate continuous metrics
  if (!is.null(continuous_attrs)) {
    continuous_metrics <- sapply(continuous_attrs, function(attr) {
      # Check if attribute exists
      if (!attr %in% vertex_attr_names(net)) {
        warning(paste("Continuous attribute", attr, "not found in network"))
        return(NA)
      }

      attr_vals <- vertex_attr(net, attr)

      # For assortativity, handle missing values by replacing with median
      if (any(is.na(attr_vals))) {
        attr_median <- median(attr_vals, na.rm = TRUE)
        attr_vals[is.na(attr_vals)] <- attr_median
        message(paste("Replaced", sum(is.na(vertex_attr(net, attr))),
                     "missing values in", attr, "with median:", round(attr_median, 3)))
      }

      # Calculate assortativity
      tryCatch({
        assortativity(net, attr_vals)
      }, error = function(e) {
        warning(paste("Error calculating assortativity for", attr, ":", e$message))
        return(NA)
      })
    })

    # Calculate Gini coefficient for continuous attributes
      gini_metrics <- sapply(continuous_attrs, function(attr) {
        if (!attr %in% vertex_attr_names(net)) {
          return(NA)
        }
        attr_vals <- vertex_attr(net, attr)

        # Calculate Gini coefficient
        tryCatch({
          DescTools::Gini(attr_vals, na.rm = TRUE)
        }, error = function(e) {
          warning(paste("Error calculating Gini coefficient for", attr, ":", e$message))
          return(NA)
        })
      })

      # Calculate experienced Gini coefficient for continuous attributes
      exp_gini_metrics <- sapply(continuous_attrs, function(attr) {
        if (!attr %in% vertex_attr_names(net)) {
          return(NA)
        }
        attr_vals <- vertex_attr(net, attr)

        # Calculate Gini coefficient
        tryCatch({
          calculate_expgini(net, attribute = attr)
        }, error = function(e) {
          warning(paste("Error calculating experienced Gini coefficient for", attr, ":", e$message))
          return(NA)
        })
      })

    # Add prefixes to names
    names(gini_metrics) <- paste0("gini_", continuous_attrs)
    names(exp_gini_metrics) <- paste0("exp_gini_", continuous_attrs)
    names(continuous_metrics) <- paste0("assort_", continuous_attrs)
  }

  # Calculate categorical assortativity metrics
  if (!is.null(categorical_attrs)) {
    categorical_metrics <- sapply(categorical_attrs, function(attr) {
      # Check if attribute exists
      if (!attr %in% vertex_attr_names(net)) {
        warning(paste("Categorical attribute", attr, "not found in network"))
        return(NA)
      }

      attr_vals <- vertex_attr(net, attr)

      # Convert to factor (handles NA as a level if present)
      attr_factor <- as.factor(attr_vals)

      # Calculate nominal assortativity
      tryCatch({
        assortativity_nominal(net, attr_factor)
      }, error = function(e) {
        warning(paste("Error calculating nominal assortativity for", attr, ":", e$message))
        return(NA)
      })
    })

    # Add prefix to names
    names(categorical_metrics) <- paste0("assort_", categorical_attrs)
  }

  # Combine all metrics into a single named vector
  all_metrics <- c(base_metrics, gini_metrics, exp_gini_metrics, continuous_metrics, categorical_metrics)

  return(all_metrics)
}