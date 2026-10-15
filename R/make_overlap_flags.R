# make_overlap_flags -----------------------------------------------------------
# For each named set of layers, creates a logical column `overlap_<set_name>`
# that is TRUE when the dyad has an ego nomination in at least one layer of
# that set.
#
# Arguments
#   df         : dyad data frame containing `<layer>_egonom` columns.
#   layer_sets : named list of character vectors of layer names, e.g.
#                list(credit = c("loan_req", "sloan_req"),
#                     access_influence = c("impmat", "govt")).
#
# Returns a tibble with one logical column per layer set (bind it onto `df`
# with bind_cols()). Stops with an error if any expected column is missing.
#
# Packages: dplyr, purrr, tibble (via tidyverse)

library(tidyverse)

make_overlap_flags <- function(df, layer_sets) {
  purrr::imap_dfc(layer_sets, function(layers, set_name) {
    cols <- paste0(layers, "_egonom")
    missing_cols <- setdiff(cols, names(df))

    if (length(missing_cols) > 0) {
      stop(
        paste(
          "Missing expected columns:",
          paste(missing_cols, collapse = ", ")
        )
      )
    }

    tibble(
      !!paste0("overlap_", set_name) :=
        rowSums(dplyr::select(df, dplyr::all_of(cols)), na.rm = TRUE) > 0
    )
  })
}

# Example usage (as in the original script):
# up_work_dyads <- up_work_dyads %>%
#   select(-any_of(overlap_flag_names)) %>%
#   bind_cols(make_overlap_flags(., overlap_layer_sets))
