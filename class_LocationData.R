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
# ==========================================================================================
setClass("location.data",
         slots=c(
           land.ownership = "character",
           size.class.decay.rates = "data.frame",
           slope = "numeric",
           biomass.market.volume = "character",
           pulp.market = "logical",
           primary.harvest.species = "character"
         )
        )