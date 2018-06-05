# ==========================================================================================
# AUTHOR: Andrew Harris
#	  Schatz Energy Research Center
#	  Humboldt State University
# ==========================================================================================
# VERSION:
#   2018-05-25 Initial Development
# ==========================================================================================
# This defines an object class to store location information highlighted from GIS
# applications that will be used for the CARBCAT model. However we interface GIS to R, the 
# relevant information is stored in this "location.data" object class, to ensure a single 
# uniform data structure throighout CARBCAT.
# 
# For the decay rate data frame, we are going to want 6 k-values, covering production of 
# each GHG (CO2, CH4, N2O) for both piled and scattered scenarios. Convention: 1 row, 6 columns

# ==========================================================================================
setClass("location.data",
         slots=c(
           land.ownership = "character",
           forestry.decay.rates = "data.frame", # series of kts for forestry; 1 for scattered, 1 for piled.
           slope = "numeric",
           biomass.market.volume = "character",
           pulp.market = "logical",
           primary.harvest.species = "character"
         )
        )