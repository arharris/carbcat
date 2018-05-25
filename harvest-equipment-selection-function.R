# ==========================================================================================
# AUTHOR: Andrew Harris
#	  Schatz Energy Research Center
#	  Humboldt State University
# ==========================================================================================
# VERSION:
#   2018-05-25 Initial Development
# ==========================================================================================
# This function will eventually select appropriate equipment based on relevant information, 
# and return the corresponding carbon intensity.
# For now, it just selects random equipment and returns that dummy carbon intensity.
# ==========================================================================================

select.harvest.equipment <- function(is.random){ # Is random is a boolean if we want to select a specific piece of equipment ()
  # Load up the harvest equipment CIs
  load('Lookup_Data/harvest-equipment-CI.Rdata')
  
  # Select a random value, unless is.random is FALSE, in which case, select the dump truck value.
  return.value <- ifelse(is.random,
                         sample(harvest.equipment.CI[,Tons.CO2eq.per.BDT.biomass],1),
                         harvest.equipment.CI[Equipment=='Dump Truck',Tons.CO2eq.per.BDT.biomass])
  
  # Eliminate the CI table for now
  remove(harvest.equipment.CI)
  
  ##############################################################################################
  # WE HAVE SELECTED RANDOM EQUIPMENT SO I CAN BUILD OUT HARVEST-PROCESSING. THIS FUNCTION DOES
  # NOT GENERATE ANY REAL VALUE.
  ##############################################################################################
  return(return.value)
}