# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-06-22: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#   1)    Discuss with team about combining decay functions for in-field    IN-PROGRESS
#         and harvest-processing. Forestry decay is based on the location 
#         function; the Ag excel spreadsheets have 2 different methods for
#         harvest-processing and in-field, with in-field including region,
#         processing, and land management. For now, use the same method as 
#         harvest-processing.
#
#   2)    Fix the processing equipment selector function so it matches      IN-PROGRESS
#         what we might need for processing.
#   3)    "Primary Harvest Species" is depreciated. Update, and account    Waiting
#         for ag/forestry differences.

# ---------------------------------
# INPUTS:
# ag.or.forest: Value of "Agriculture" or "Forestry"
# treatment.type:
# initial.moisture: 
# processing.opt: Residue processing method; default value "None"
# scattered.fraction: The decimal fraction of the matterial LEFT IN-FIELD that is 
#     scattered
# piled.fraction: The decimal fraction of the matterial LEFT IN-FIELD that is 
#     piled
#
# ---------------------------------
# OBJECTIVE:
#
# This module will calculate carbon emissions for the in-field agriculture phase
# of CARBCAT. (Expand this explanation)
#
# ---------------------------------
# OUTPUTS:
# 1) A data table covering emissions for years 1-100 (Get more details here)
# 2) Mass loss through processing/comminution
# 3) Moisture loss through passive drying
#
# =============================================================================
source("misc-functions.R")
source("class_LocationData.R")
source("class_ModuleOutput.R")
# source("harvest-equipment-selection-function.R")
source("decay-functions.R")
source("moisture-loss-functions.R")

# Required Libraries
load.libraries(c('data.table','zoo'))

in.field.agriculture <- function(burn.opt,         # Pile burn or field burn?
                                 initial.moisture, # Residue moisture content entering this module
                                 burn.year,        # Year in which residues are burned - if no residue is being burned, this will be arbitrarily large.
                                 comminution.opt,  # Will residues be ground or diced?
                                 land.management) {# Scattering residues? Re-incorporating residues
  
  # Year should be 1-100. KEEP YEAR COLUMN FOR TRANSPARENCY
  emissions.annual.profile <- data.table(Year=1:100,
                                         Burn_Tons.CO2.per.BDT.biomass=0,
                                         Burn_Tons.CH4.per.BDT.biomass=0,
                                         Burn_Tons.N2O.per.BDT.biomass=0,
                                         Decay_Tons.CO2.per.BDT.biomass=0,
                                         Decay_Tons.CH4.per.BDT.biomass=0,
                                         Decay_Tons.N2O.per.BDT.biomass=0,
                                         Comminution_Tons.CO2.per.BDT.biomass=0,
                                         Comminution_Tons.CH4.per.BDT.biomass=0,
                                         Comminution_Tons.N2O.per.BDT.biomass=0)
  
  # There are three emissions sources: residue burning, residue decay, and residue 
  # comminution. comminution will require matching the correct emissions factor with
  # the correct option, and applying to the correct year.
  
  # Lookup table type information - processing equipment carbon intensities are stored
  # in a directory labeled "Lookup_Data" in the main directory. For speed and file size
  # reasons, I want them as Rdata files. We can then clear out the data after we've
  # performed the calculations we need to perform.
  
  # Add comminution emissions to year 0. Until I get an argument that any ag operation would wait to grind or dice, I'm
  # operating under the reasoning that if comminution will happen, it will happen at the primary harvest when residues are
  # generated.
  load("Lookup_Data/comminution-CI.Rdata")
  emissions.annual.profile[Year==1,':='(Comminution_Tons.CO2.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.CO2.per.BDT.biomass],
                                        Comminution_Tons.CH4.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.CH4.per.BDT.biomass],
                                        Comminution_Tons.N2O.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.N2O.per.BDT.biomass])]
  comminution.mass.loss <- comminution.CI[Comminution==comminution.opt,Post_Comminution.Mass.Loss]
  remove(comminution.CI)
  
  # Now to tackle decay emissions. In spreadsheet form, we calculated the mass loss and 
  # looked up the appropriate values. This led to giant lookup tables. Instead, we have 
  # a file of decay functions labeled by decay function selection criteria, i.e. 
  # "Forestry.Mixed", Agriculture.Corn", or "Agriculture.Cotton". Using "match.fun"
  # ('get' would also work), we can select the needed decay function and calculate only
  # what we need based on the year.
  
  ##########################################################################################################################
  ##########################################################################################################################
  ##########################################################################################################################
  # SORT OUT HOW WE SELECT THE RIGHT DECAY FUNCTION
  ##########################################################################################################################
  ##########################################################################################################################
  ##########################################################################################################################
  
  # Currently, the labels for the dummy functions are ag.or.forest and the primary harvest species. 
  # I've also only written three dummy functions.
  decay.function.key <- paste(ag.or.forest,this.location@primary.harvest.species,'decay',sep='.')
  decay.function <- match.fun(decay.function.key)
  
  # Now that we have the appropriate decay function, create a column in emissions.annual.profile
  # for the cumulative decay at year X. In addition to the standard input data needed to calculate
  # decay, the decay function also requires the specific emissions species (CO2, CH4, N2O). The 
  # output will be the tons emissions/BDT biomass
  
  emissions.annual.profile[Year<=burn.year,':='(Cumulative.Decay_Tons.CO2.per.BDT.biomass=mapply(decay.function,"CO2",Year,scattered.fraction,piled.fraction),
                                                Cumulative.Decay_Tons.CH4.per.BDT.biomass=mapply(decay.function,"CH4",Year,scattered.fraction,piled.fraction),
                                                Cumulative.Decay_Tons.N2O.per.BDT.biomass=mapply(decay.function,"N2O",Year,scattered.fraction,piled.fraction))]
  
  # Use na.locf to extend the cumulative emissions beyond the burn year, if necessary
  emissions.annual.profile[,':='(Cumulative.Decay_Tons.CO2.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.CO2.per.BDT.biomass,na.rm=FALSE),
                                 Cumulative.Decay_Tons.CH4.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.CH4.per.BDT.biomass,na.rm=FALSE),
                                 Cumulative.Decay_Tons.N2O.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.N2O.per.BDT.biomass,na.rm=FALSE))]
  
  # Calculate the annual emissions difference from the cumulative emissions
  emissions.annual.profile[,':='(Annual.Decay_Tons.CO2.per.BDT.biomass=Cumulative.Decay_Tons.CO2.per.BDT.biomass - c(0,Cumulative.Decay_Tons.CO2.per.BDT.biomass[(.I-1)]),
                                 Annual.Decay_Tons.CH4.per.BDT.biomass=Cumulative.Decay_Tons.CH4.per.BDT.biomass - c(0,Cumulative.Decay_Tons.CH4.per.BDT.biomass[(.I-1)]),
                                 Annual.Decay_Tons.N2O.per.BDT.biomass=Cumulative.Decay_Tons.N2O.per.BDT.biomass - c(0,Cumulative.Decay_Tons.N2O.per.BDT.biomass[(.I-1)]))]
  
  # Comminution emissions and decay emissions are calculated - the last emissions source is burning. 
  
  
  
  # Add up the total annual emissions, then determine the cumulative
  emissions.annual.profile[,':='(Annual.Total_Tons.CO2.per.BDT.biomass = Harvest_Tons.CO2.per.BDT.biomass + Comminution_Tons.CO2.per.BDT.biomass + Processing_Tons.CO2.per.BDT.biomass + Annual.Decay_Tons.CO2.per.BDT.biomass,
                                 Annual.Total_Tons.CH4.per.BDT.biomass = Harvest_Tons.CH4.per.BDT.biomass + Comminution_Tons.CH4.per.BDT.biomass + Processing_Tons.CH4.per.BDT.biomass + Annual.Decay_Tons.CH4.per.BDT.biomass,
                                 Annual.Total_Tons.N2O.per.BDT.biomass = Harvest_Tons.N2O.per.BDT.biomass + Comminution_Tons.N2O.per.BDT.biomass + Processing_Tons.N2O.per.BDT.biomass + Annual.Decay_Tons.N2O.per.BDT.biomass)]
  
  emissions.annual.profile[,':='(Cumulative.Total_Tons.CO2.per.BDT.biomass = cumsum(Annual.Total_Tons.CO2.per.BDT.biomass),
                                 Cumulative.Total_Tons.CH4.per.BDT.biomass = cumsum(Annual.Total_Tons.CH4.per.BDT.biomass),
                                 Cumulative.Total_Tons.N2O.per.BDT.biomass = cumsum(Annual.Total_Tons.N2O.per.BDT.biomass))]
  
  # Processing, comminution, and decay emissions have been calculated, and comminution/
  # processing mass loss calculations are complete. Last, we calculate moisture loss
  moisture.loss.function.key <- paste(ag.or.forest,this.location@primary.harvest.species,'moisture.loss',sep='.')
  moisture.loss.function <- match.fun(moisture.loss.function.key)
  harvest.processing.moisture.loss <- moisture.loss.function(harvest.collection.year.diff)
  
  harvest.processing.return.obj <- new("module.output",
                                       emissions.timeline = emissions.annual.profile,
                                       mass.loss = processing.mass.loss+comminution.mass.loss,
                                       remaining.moisture = initial.moisture * harvest.processing.moisture.loss
  )
  
  return(harvest.processing.return.obj)
}