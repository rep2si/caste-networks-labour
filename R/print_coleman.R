# print_coleman ----------------------------------------------------------------
# Prints, for each site, the Coleman index table from compute_coleman() as an
# HTML kable with a markdown heading (for use inside an Rmd/Quarto chunk).
#
# Arguments
#   coleman_list : output of compute_coleman() (must carry the "var_name"
#                  attribute).
#
# Packages: purrr (via tidyverse), knitr, kableExtra

library(tidyverse)
library(knitr)
library(kableExtra)

print_coleman <- function(coleman_list) {
  # Extract var_name from attributes
  var_name <- attr(coleman_list, "var_name")
  if (is.null(var_name)) {
    stop("coleman_list must have 'var_name' attribute. Use compute_coleman() to create it.")
  }

  coleman_list %>%
    iwalk(~ {
      cat("\n\n### ", .y, "-", var_name, "\n\n")
      .x %>%
        kable() %>%
        kable_styling() %>%
        scroll_box(width = "100%", height = "350px") %>%
        print()
    })
}
