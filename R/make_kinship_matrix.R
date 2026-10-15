#' Function to make weighted matrices of relatedness between individuals
#'
#' @param ids A vector with a list of individual IDs, which should be unique.
#' @param df A data frame with three columns, the first two corresponding to the 
#' IDs included in the ids parameter, and the third being a measure of relatedness
#' (expected to be either consanguineal or affinal relatedness), like what 
#' results from the make_kinship_objects() function.
#' @param lower_bound A value below which all relatedness is set to 0 (defaults to 0.0625)
#'
#' @return A matrix of size length(ids) x length(ids) with values filled from the df
#' i.e., a weighted adjacency matrix with relatedness coefficients

make_kinship_matrix <- function(ids, df, lower_bound = 0.0625) {
  # Create empty matrix
  mat <- matrix(0, 
                nrow = length(ids), 
                ncol = length(ids),
                dimnames = list(ids, ids))
  
  # Pre-compute row and column indices
  row_indices <- match(df[, 1], ids)
  col_indices <- match(df[, 2], ids)
  
  # Remove NA matches (if any)
  valid <- !is.na(row_indices) & !is.na(col_indices)
  
  # Vectorized assignment
  mat[cbind(row_indices[valid], col_indices[valid])] <- as.numeric(df[valid, 3])
  
  # assume all missing values are 0 relatedness
  mat[is.na(mat)] <- 0
  
  # make sure people perfectly related to selves & no values higher than 1
  # which would result from close-kin parents
  diag(mat) <- 1
  mat[mat > 1 ] <- 1
  
  # set all values below threshold to 0
  mat[mat < lower_bound ] <- 0
  
  return(mat)
}
