#' Function to make weighted matrices of distances between residences
#'
#' @param ids A vector with a set of individual IDs
#' @param key A data frame recording the IDs and sharing unit IDs of each person
#' @param id_col The name of the column in key for the IDs. Default is 'ID'
#' @param su_col The name of the column in key for the SUIDs. Default is 'SUID'
#' @param df A data frame with three columns, the first two corresponding to the 
#' IDs included in the ids parameter, and the third being a measure of distance, 
#' like what results from the make_distance_object() function.
#' @param treat_nas How to treat any missing distances. Options are 'leave' or 'median' (the default).
#'
#' @return A matrix of size length(ids) x length(ids) with values filled from the df
#' i.e., a weighted adjacency matrix with distance in meters

make_distance_matrix <- function(ids, key, id_col = "ID", su_col = "SUID", df, treat_nas = "median") {
  su_ids <- key[[su_col]][match(ids, key[[id_col]])]
  
  # create empty matrix
  mat <- matrix(0,
    nrow = length(ids),
    ncol = length(ids),
    dimnames = list(su_ids, su_ids)
  )
  
  # For each unique combination in df, find all matrix positions that should get that value
  for (i in 1:nrow(df)) {
    # Find all positions where su_ids match the current df row
    row_matches <- su_ids == df[i, 1] & !is.na(su_ids)
    col_matches <- su_ids == df[i, 2] & !is.na(su_ids)
    
    # Set the value for all matching combinations
    mat[row_matches, col_matches] <- as.numeric(df[i, 3])
  }
  
  # Set dimnames to ids
  dimnames(mat) <- list(ids, ids)
  
  # deal with NAs, if any exist. 
  if (anyNA(mat)) {
    perc_missing <- round(table(is.na(mat))[2]/length(mat) * 100, 2)
    if (treat_nas == "median") {
      message(paste0("NA distances (", perc_missing , "% of cells) turned to median."))
      mat[is.na(mat)] <- median(mat, na.rm = TRUE)
      return(mat)
    } else if (treat_nas == "leave") {
      message(paste0("NA distances (", perc_missing, "% of cells)  left as is."))
      return(mat)
    } else {
      stop("Specify how to treat NA distances. Current options are 'leave' or 'median'.")
      return(NA)
    }
  } else {
    return(mat)
  }
}

