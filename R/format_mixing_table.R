# format_mixing_table ----------------------------------------------------------
# Turns a mixing matrix into a colour-coded HTML kable table.
#
# Arguments
#   mat       : numeric matrix (e.g. `$prob` or `$obs_exp_ratio` from
#               compute_mixing_matrices()).
#   title     : table caption.
#   diverging : if TRUE, use a red-white-green diverging palette centred on
#               `center_val`; otherwise use a viridis-style sequential scale.
#   center_val: midpoint for the diverging scale (1 for obs/exp ratios).
#   digits    : decimal places shown in cells.
#
# NaN cells are shaded grey.
#
# Dependencies: knitr, kableExtra (cell_spec, spec_color, kable_styling,
#               column_spec), grDevices (colorRampPalette)

library(knitr)
library(kableExtra)

format_mixing_table <- function(mat, title = "",
                                diverging = FALSE,
                                center_val = 1,
                                digits = 2) {

  # Flatten matrix to get all values
  vals <- as.numeric(mat)

  # Identify NaN values
  nan_mask <- is.nan(vals)

  if (diverging) {
    # For diverging scale centered at center_val (red-white-green)
    # Only calculate range from non-NaN values
    vals_no_nan <- vals[!nan_mask]
    max_diff <- max(abs(vals_no_nan - center_val))

    # Create red-white-green palette
    color_func <- colorRampPalette(c("#d73027", "#ffffff", "#1a9850"))

    # Rescale values to 0-1 range for color mapping
    val_range <- c(center_val - max_diff, center_val + max_diff)
    val_rescaled <- (vals - val_range[1]) / (val_range[2] - val_range[1])

    # Generate 100 colors and map values to them
    palette <- color_func(100)
    color_indices <- pmin(pmax(round(val_rescaled * 99) + 1, 1), 100)
    colors <- palette[color_indices]

  } else {
    # For regular scale
    colors <- spec_color(vals, begin = 0.9, end = 0.1)
  }

  # Set NaN cells to grey
  colors[nan_mask] <- "#d3d3d3"

  # Round values and apply colors to each cell
  mat_colored <- matrix(
    cell_spec(round(mat, digits), "html", background = colors, align = "c"),
    nrow = nrow(mat),
    dimnames = dimnames(mat)
  )

  # Create table
  mat_colored %>%
    kable(format = "html", caption = title, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) %>%
    column_spec(1, bold = TRUE)
}
