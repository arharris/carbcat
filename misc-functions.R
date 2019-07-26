# ==================================================================================
# This is a collection of miscellaneous functions and constants that are used 
# throughout modules
#
# ==================================================================================

########################
########################
# FUNCTIONS
########################
########################

#####################################################################################
# load.libraires: an easier and cleaner way to install and load libraries. Function
# by Colin Sheppard, GitHub user colinsheppard, in the colinmisc repository.
#####################################################################################
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

# Most of our packages are loaded in the main script, but some of the static variables work best as data tables.
# So we are loading data.table here.
load.libraries("data.table")

#####################################################################################
# %notin%: converse of "%in%" function, identifies when one item is not in a target
# object. Use: "a" %notin% c("b","c","d") will yiled TRUE.
#####################################################################################
"%notin%" <- function(x, table) match(x, table, nomatch = 0) == 0

########################
########################
# CONSTANTS
########################
########################

cell_to_acres <- 900 * 0.000247105 # 30 m x 30 m cell converted to acres

# (Mass emissions species) / (Mass carbon emitted)
CO2_carbon_fraction <- 44.009/12.011
CH4_carbon_fraction <- 16.0426/12.011
CO_carbon_fraction <- 28.010/12.011
PM10_carbon_fraction <- 1/0.6
PM2.5_carbon_fraction <- 1/0.6
plant_SO2_SOx_fraction <- 0.97 # percent of SOx emitted as SO2, power plant
equipment_SO2_SOx_fraction <- 0.97 # percent of SOx emitted as SO2, harvest/processing equipment
fire_N2O_NOx_fraction <- 0.0568 # Percent of NOx emitted as N2O, prescribed burn and wildfire; based on average burn values from CARB

# These are currently dummy values
warning("VOC and char carbon fractions must be confirmed.")
VOC_carbon_fraction <- 1/0.4
char_carbon_fraction <- 1/0.71 # Current assumption is that char is 70% carbon, based on summary of literature values.

# Also, we assume the combustion/char/unburned fractions for pile burns are fixed (and can be hard-coded) at 90% / .1% / 9.9%. This holds for all size classes.
pile.burn.combustion.frac <- 0.9
pile.burn.char.frac <- 0.01

# Biomass plant type will have a set percentage of unburned fuel. Build a reference table for that here
biomass_plant_unburnt_fuel <- data.table(plant.type=c("Biomass stoker","Fluidized bed combustor","Cyclone combustor","Gasifier"),unburned.fuel.frac=c(0.035,0.025,0.03,0.025))

###################################################################################################
# My code refers to the silvicultural treatments by code; the wildfire file names use full names.
# This will allow us to move between the two easily.
###################################################################################################
treatment_name_lookup <- data.table(treat.code=c("RM100", "TFA20", "TFA40", "TFA60", "TFA80", "TFB20", "TFB40", "TFB60", "TFB80", "TP20", "TP40", "TP60", "TP80"),treat.name=c("Clearcut", "20_Thin_From_Above", "40_Thin_From_Above", "60_Thin_From_Above", "80_Thin_From_Above", "20_Thin_From_Below", "40_Thin_From_Below", "60_Thin_From_Below", "80_Thin_From_Below", "20_Proportional_Thin", "40_Proportional_Thin", "60_Proportional_Thin", "80_Proportional_Thin"))
