install.packages("pak") # fast installer tool like uv for python

pak::pkg_install(c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "ggrepel",
  "readr",
  "scales",
  "stringr",
  "readxl",
  "here",
  "patchwork"
))
