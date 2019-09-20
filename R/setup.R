# PACKAGE INSTALLATION
# run this once on your system (or to update)
# do not source this file


pkgs <- c("assertthat","crayon","data.table","devtools",
          "dplyr","ggplot2","ggthemes","htmltools","knitr",
          "magrittr","purrr","questionr","reshape","reshape2",
          "rmarkdown","stringi","stringr","survey","testthat",
          "tibble","tidyr","utils", "here")

install.packages(pkgs)

# Install githubs packages 
i_git <- function(pkg){
  remotes::install_github(
    pkg,
    build_opts = c(),
    build_vignettes = TRUE
  )
}

Sys.setenv("TAR" = "internal")

i_git("mabafaba/koboquest")
i_git("mabafaba/kobostandards")
i_git("mabafaba/xlsformfill")
i_git("mabafaba/composr")
i_git("mabafaba/hypegrammaR")
i_git("sharonorengo/visualisationIMPACT")
