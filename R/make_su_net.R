require(tidyverse)

#' Function to aggregate individual-level network to household-level network
#'
#' Nodes with valid household IDs are aggregated to household level.
#' Nodes without valid household IDs can be retained as individual nodes or dropped.
#'
#' @param inet An igraph object representing the individual-level network
#' @param individ The vertex attribute name for individual IDs (default is "name")
#' @param suid The vertex attribute name for household IDs (default is "su")
#' @param drop_invalid_suid Logical indicating whether to drop nodes without valid household IDs (default is FALSE, which retains them)
#' @param remove_hh_loops Logical indicating whether to remove within-household ties (default is FALSE)
#' @param aggregate_attrs Logical indicating whether to aggregate attributes for household nodes (default is TRUE)
#' @param attrs_to_aggregate Character vector of attribute names to aggregate (default is NULL, which aggregates all except name/individ/suid)
#' @param keep_as_list Logical indicating whether to keep aggregated attributes as list-columns (default is TRUE). Set to FALSE to apply summary functions.
#' @param summary_functions Named list of summary functions to apply to numeric attributes. Names are attribute names, values are function names ("mean", "median", "max", "min", "sum", "sd", "var") or functions. Only used when keep_as_list=FALSE.
#' @param logical_functions Named list of summary functions to apply to logical attributes. Names are attribute names, values are function names ("any", "all", "mean", "mode") or functions. Only used when keep_as_list=FALSE.
#' @param collapse_character Logical, character vector, or NULL. If TRUE, collapses all character/factor attributes to unique concatenated values. If character vector, only collapses specified attributes. If NULL or FALSE, keeps as vectors.
#' @param collapse_sep Separator string for concatenating unique character values (default is "_")
#'
#' @return A household-level (or mixed household/individual) network object
#'
#' @details
#' Valid household IDs are non-NA, non-NULL, and non-empty strings.
#'
#' When drop_invalid_suid=FALSE (default):
#' - Individuals without valid household IDs are retained with their original vertex name
#' - Result is a mixed household/individual network
#' - Edges can be household-household, household-individual, or individual-individual
#'
#' When drop_invalid_suid=TRUE:
#' - Individuals without valid household IDs are dropped entirely
#' - Edges involving these individuals are also dropped
#' - Result is a pure household-level network
#'
#' When aggregate_attrs=TRUE and keep_as_list=TRUE, attributes for household nodes contain
#' vectors of all values from constituent individuals.
#'
#' When keep_as_list=FALSE, you can specify:
#' - summary_functions for numeric attributes (e.g., list(age = "mean", wealth = "sum"))
#' - logical_functions for logical attributes (e.g., list(employed = "any", literate = "all"))
#' - collapse_character to reduce character attributes to unique concatenated values
#'
#' For logical attributes, available functions are:
#' - "any": TRUE if any member is TRUE
#' - "all": TRUE if all members are TRUE
#' - "mean": Proportion of TRUE values (0 to 1)
#' - "mode": Most common value (TRUE or FALSE)
#'
make_su_net <- function(
  inet,
  individ = "name",
  suid = "su",
  drop_invalid_suid = FALSE,
  remove_hh_loops = FALSE,
  aggregate_attrs = TRUE,
  attrs_to_aggregate = NULL,
  keep_as_list = TRUE,
  summary_functions = NULL,
  logical_functions = NULL,
  collapse_character = NULL,
  collapse_sep = "_"
) {
  # Get individual and household IDs
  indiv_ids <- igraph::vertex_attr(inet, individ)
  household_ids <- igraph::vertex_attr(inet, suid)

  # Identify which individuals have valid household IDs
  # Valid means: not NA, not NULL, not empty string
  has_valid_suid <- !is.na(household_ids) &
    nchar(as.character(household_ids)) > 0

  # Create mapping:
  # - For those with valid suid: map to household ID
  # - For those without valid suid: map to their individual ID (if not dropping)
  if (drop_invalid_suid) {
    # Map valid suid to household, invalid to NA (will be filtered out)
    recodes <- ifelse(
      has_valid_suid,
      as.character(household_ids),
      NA_character_
    )
  } else {
    # Map valid suid to household, invalid to their individual ID (retain)
    recodes <- ifelse(
      has_valid_suid,
      as.character(household_ids),
      as.character(indiv_ids)
    )
  }
  names(recodes) <- indiv_ids

  # Get edge list and recode endpoints
  iel <- igraph::as_edgelist(inet)
  suel <- iel %>%
    as.data.frame() %>%
    mutate(
      ego = recode(V1, !!!recodes),
      alter = recode(V2, !!!recodes)
    )

  # If dropping invalid suid, filter out edges involving NA nodes
  if (drop_invalid_suid) {
    suel <- suel %>%
      filter(!is.na(ego) & !is.na(alter))
  }

  # Count edges
  suel <- suel %>%
    count(ego, alter, name = "weight")

  # Create network from aggregated edge list
  sunet <- igraph::graph_from_data_frame(
    suel,
    directed = igraph::is_directed(inet)
  )

  # Remove self-loops if requested
  if (remove_hh_loops == TRUE) {
    sunet <- igraph::simplify(
      sunet,
      remove.loops = TRUE,
      remove.multiple = FALSE
    )
  }

  # Add isolated nodes
  if (drop_invalid_suid) {
    # Only add isolated households (not individuals)
    isolated_households <- unique(household_ids[
      has_valid_suid & igraph::degree(inet, mode = "all") == 0
    ])
    isolated_to_add <- setdiff(isolated_households, igraph::V(sunet)$name)
  } else {
    # Add both isolated households and isolated individuals
    all_nodes_mapped <- recodes[igraph::degree(inet, mode = "all") == 0]
    isolated_to_add <- setdiff(unique(all_nodes_mapped), igraph::V(sunet)$name)
  }

  if (length(isolated_to_add) > 0) {
    sunet <- igraph::add_vertices(
      sunet,
      length(isolated_to_add),
      name = isolated_to_add
    )
  }

  # Aggregate attributes
  if (aggregate_attrs) {
    # Determine which attributes to aggregate
    if (is.null(attrs_to_aggregate)) {
      # Default: all vertex attributes except name, individ, and suid
      attrs_to_aggregate <- setdiff(
        igraph::vertex_attr_names(inet),
        c("name", individ, suid)
      )
    }

    # Create a mapping data frame for aggregation
    # Only include individuals that will be in the final network
    if (drop_invalid_suid) {
      # Only include individuals with valid suid
      valid_indices <- which(has_valid_suid)
      mapping_df <- data.frame(
        individual = indiv_ids[valid_indices],
        mapped_to = recodes[valid_indices],
        stringsAsFactors = FALSE
      )
    } else {
      # Include all individuals
      mapping_df <- data.frame(
        individual = indiv_ids,
        mapped_to = recodes,
        stringsAsFactors = FALSE
      )
    }

    # For each attribute, aggregate values
    for (attr in attrs_to_aggregate) {
      # Get original attribute values (only for individuals in mapping_df)
      if (drop_invalid_suid) {
        orig_values <- igraph::vertex_attr(inet, attr)[valid_indices]
      } else {
        orig_values <- igraph::vertex_attr(inet, attr)
      }

      # Add to mapping dataframe
      mapping_df[[attr]] <- orig_values

      # Aggregate by mapped node
      # Group by mapped_to and collect all values into a list
      aggregated <- mapping_df %>%
        group_by(mapped_to) %>%
        summarise(
          aggregated_values = list(.data[[attr]]),
          .groups = "drop"
        )

      # Create attribute vector for new network
      # Initialize with empty list for each node
      new_attr_values <- vector("list", igraph::vcount(sunet))
      names(new_attr_values) <- igraph::V(sunet)$name

      # Fill in aggregated values
      for (i in seq_len(nrow(aggregated))) {
        node_name <- aggregated$mapped_to[i]
        if (node_name %in% names(new_attr_values)) {
          new_attr_values[[node_name]] <- aggregated$aggregated_values[[i]]
        }
      }

      # Determine attribute type
      is_numeric <- is.numeric(orig_values)
      is_logical <- is.logical(orig_values)
      is_char_or_factor <- is.character(orig_values) || is.factor(orig_values)

      # Apply transformations based on keep_as_list and other parameters
      if (!keep_as_list) {
        # For numeric attributes: apply summary functions
        if (
          is_numeric &&
            !is.null(summary_functions) &&
            attr %in% names(summary_functions)
        ) {
          func_spec <- summary_functions[[attr]]

          # Convert string to function if needed
          if (is.character(func_spec)) {
            func <- switch(
              func_spec,
              "mean" = mean,
              "median" = median,
              "max" = max,
              "min" = min,
              "sum" = sum,
              "sd" = sd,
              "var" = var,
              stop(sprintf("Unknown function: %s", func_spec))
            )
          } else if (is.function(func_spec)) {
            func <- func_spec
          } else {
            stop(sprintf(
              "summary_functions[[%s]] must be a function name or function",
              attr
            ))
          }

          # Apply the function
          new_attr_values <- lapply(new_attr_values, function(x) {
            if (length(x) > 0) func(x, na.rm = TRUE) else NA
          })
          new_attr_values <- unlist(new_attr_values)
        } else if (is_numeric) {
          # No summary function specified for numeric - just unlist single values
          new_attr_values <- lapply(new_attr_values, function(x) {
            if (length(x) == 1) return(x[[1]]) else return(x)
          })
        }

        # For logical attributes: apply logical functions
        if (
          is_logical &&
            !is.null(logical_functions) &&
            attr %in% names(logical_functions)
        ) {
          func_spec <- logical_functions[[attr]]

          # Convert string to function if needed
          if (is.character(func_spec)) {
            func <- switch(
              func_spec,
              "any" = function(x, na.rm = TRUE) any(x, na.rm = na.rm),
              "all" = function(x, na.rm = TRUE) all(x, na.rm = na.rm),
              "mean" = function(x, na.rm = TRUE) mean(x, na.rm = na.rm),
              "mode" = function(x, na.rm = TRUE) {
                if (na.rm) {
                  x <- x[!is.na(x)]
                }
                if (length(x) == 0) {
                  return(NA)
                }
                tab <- table(x)
                as.logical(names(tab)[which.max(tab)])
              },
              stop(sprintf(
                "Unknown logical function: %s. Use 'any', 'all', 'mean', or 'mode'",
                func_spec
              ))
            )
          } else if (is.function(func_spec)) {
            func <- func_spec
          } else {
            stop(sprintf(
              "logical_functions[[%s]] must be a function name or function",
              attr
            ))
          }

          # Apply the function
          new_attr_values <- sapply(new_attr_values, function(x) {
            if (length(x) > 0) func(x, na.rm = TRUE) else NA
          })
        } else if (is_logical) {
          # No logical function specified - just unlist single values
          new_attr_values <- lapply(new_attr_values, function(x) {
            if (length(x) == 1) return(x[[1]]) else return(x)
          })
        }

        # For character/factor attributes: collapse to unique if requested
        if (is_char_or_factor) {
          should_collapse <- FALSE

          if (is.logical(collapse_character) && isTRUE(collapse_character)) {
            should_collapse <- TRUE
          } else if (
            is.character(collapse_character) && attr %in% collapse_character
          ) {
            should_collapse <- TRUE
          }

          if (should_collapse) {
            new_attr_values <- sapply(new_attr_values, function(x) {
              if (length(x) > 0) {
                unique_vals <- unique(x[!is.na(x)])
                if (length(unique_vals) > 0) {
                  paste(unique_vals, collapse = collapse_sep)
                } else {
                  NA_character_
                }
              } else {
                NA_character_
              }
            })
          } else {
            # Just unlist single values
            new_attr_values <- lapply(new_attr_values, function(x) {
              if (length(x) == 1) return(x[[1]]) else return(x)
            })
          }
        }

        # For other types, just unlist single values
        if (!is_numeric && !is_logical && !is_char_or_factor) {
          new_attr_values <- lapply(new_attr_values, function(x) {
            if (length(x) == 1) return(x[[1]]) else return(x)
          })
        }
      }

      # Set the attribute in the new network
      sunet <- igraph::set_vertex_attr(sunet, attr, value = new_attr_values)
    }
  }

  return(sunet)
}