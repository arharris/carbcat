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

cell_to_acres <- 90 * 0.000247105 # 30 m x 30 m cell converted to acres

# (Mass emissions species) / (Mass carbon emitted)
CO2_carbon_fraction <- 44.009/12.011
CH4_carbon_fraction <- 16.0426/12.011
CO_carbon_fraction <- 28.010/12.011
# These are currently dummy values
PM10_carbon_fraction <- 31/12.011
PM2.5_carbon_fraction <- 45/12.011
VOC_carbon_fraction <- 60/12.011