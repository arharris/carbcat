# ==========================================================================================
# AUTHOR: Andrew Harris
#	  Schatz Energy Research Center
#	  Humboldt State University
# ==========================================================================================
# VERSION:
#   2018-06-22 Initial Development
# ==========================================================================================
# This function will eventually select appropriate equipment based on relevant information, 
# and return the corresponding carbon intensity.
# For now, it just selects random equipment and returns that dummy carbon intensity.
# ==========================================================================================

FIX THIS ONCE WE GET AN IDEA OF WHAT OUR PROCESSING EQUIPMENT IS AND IF WE NEED A DIFFERENT PROCESSING DATA FILE


select.processing.equipment <- function(is.random){ # Is random is a boolean if we want to select a specific piece of equipment ()
  # Load up the harvest equipment CIs
  load('Lookup_Data/processing-CI.Rdata')
  
  # Select a random value, unless is.random is FALSE, in which case, select the dump truck value.
  if(is.random){
    return.value <- processing.CI[sample(1:nrow(harvest.equipment.CI),1),c(Tons.CO2.per.BDT.biomass,Tons.CH4.per.BDT.biomass,Tons.N2O.per.BDT.biomass)]
  } else {
    return.value <- harvest.equipment.CI[Equipment=='Dump Truck',c(Tons.CO2.per.BDT.biomass,Tons.CH4.per.BDT.biomass,Tons.N2O.per.BDT.biomass)]
  }
  
  # Eliminate the CI table for now
  remove(harvest.equipment.CI)
  
  ##############################################################################################
  # WE HAVE SELECTED RANDOM EQUIPMENT SO I CAN BUILD OUT HARVEST-PROCESSING. THIS FUNCTION DOES
  # NOT GENERATE ANY REAL VALUE.
  ##############################################################################################
  return(return.value)
}