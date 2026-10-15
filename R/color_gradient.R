#' Function to turn a numeric vector into a color gradient
#'
#' @param attr A numeric vector of attributes to be mapped to colors.
#' @param base_color The base color to use for the gradient. Like "black" or "blue".
#' @param min_alpha The minimum alpha value for the gradient. Default is 0.1
#' @param max_alpha The maximum alpha value for the gradient. Default is 1.0
#'
#' @return A character vector of colors corresponding to the input attributes.

color_gradient <- function(attr, base_color = "black", min_alpha = 0.1, max_alpha = 1.0) {
  normalized_attr <- (attr - min(attr)) / (max(attr) - min(attr))
  alpha_values <- min_alpha + normalized_attr * (max_alpha - min_alpha)
  colors <- sapply(alpha_values, function(a) grDevices::adjustcolor(base_color, alpha.f = a))
  return(colors)
}
