# ==================================================================================
# This is a collection of miscellaneous functions that are used throughout modules
#
# ==================================================================================



#####################################################################################
# load.libraires: an easier and cleaner way to install and load libraries. Function
# by Colin Sheppard, GitHub user colinsheppard, in the colinmisc repository.
#####################################################################################
# 

load.libraries <- function (needed.packs, quietly = T) 
{
  installed.packs <- installed.packages()
  for (pack in needed.packs) {
    if (!pack %in% installed.packs) {
      install.packages(pack, repos = "http://cran.cnr.Berkeley.edu")
    }
    if (quietly) {
      suppressPackageStartupMessages(library(pack, character.only = T, 
                                             quietly = quietly))
    }
    else {
      library(pack, character.only = T)
    }
  }
}