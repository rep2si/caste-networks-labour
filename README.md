# caste-networks-labour

This repository contains the analysis code for:

> **"Caste, Social Networks and Informal Labour Markets"**
> by Komal Chauhan and Eleanor A. Power

The study explores the labour and social support relations between households in a village in Uttar Pradesh. It builds on ethnographic work, as well as multilayer social support network data to characterise the caste-mediated relations that structure the community.

This repo provides code to generate all of the figures and tables, using faux data. With the use of faux data, the resulting tables/figures will not reproduce the actual figures/tables in the paper but instead fully show the code.

Installation of the relevant R packages should be straightforward and take a matter of minutes.

The main script is `net_descriptives.R`, which provides the range of descriptive measures of the social networks, as well as the occupation and landholding data. A second script, `ergm.R` fits an exponential random graph model to the work network. Custom functions called in these scripts are stored in the R/ folder.

## R Package Dependencies

Install all required packages before running:

```r
install.packages(c(
  "igraph","ergm","network","sna",
  "dplyr","tidyr","purrr","tibble",
  "stringr","ggplot2","knitr","kableExtra",
  "paletteer","janitor","netseg",
  "scales","patchwork","booktabs",
  "makecell","threeparttable","graphicx"
))
```

The analysis has been tested on **R 4.6.1** on macOS.
