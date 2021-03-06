# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-05-24: Initialized Script
#   2018-05-30: Version 1 complete; takes specified input and location object,
#               returns output object.
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
# [1]     Develop decay functions for remainder of forestry/ag options     On-hold
# [2]     "Primary Harvest Species" is depreciated. Update, and account    Waiting
#         for ag/forestry differences.
# ---------------------------------
# INPUTS:
# ag.or.forest: Value of "Agriculture" or "Forestry"
# treatment.type:
# initial.moisture: 
# harvest.collection.year.diff: Years between residue generation and residue collection;
#     default value is 0 (residues collected same year they are produced)
# comminution.opt: Residue comminution method; default value "None"
# post.harvest.processing: Residue processing method; default value "None"
# scattered.fraction: The decimal fraction of the matterial TO BE HARVESTED that is 
#     scattered
# piled.fraction: The decimal fraction of the matterial TO BE HARVESTED that is 
#     piled
#
# ---------------------------------
# OBJECTIVE:
#
# This module will calculate carbon emissions for the harvest-processing phase
# of CARBCAT. (Expand this explanation)
#
# ---------------------------------
# OUTPUTS:
# 1) A data table covering emissions for years 1-100 (Get more details here)
# 2) Mass loss through processing/comminution
# 3) Moisture loss through passive drying
#
# =============================================================================

# Source files
source("misc-functions.R")
source("class_LocationData.R")
source("class_ModuleOutput.R")
source("harvest-equipment-selection-function.R")
source("decay-functions.R")
source("moisture-loss-functions.R")

# Required Libraries
load.libraries(c('data.table','zoo'))

harvest.processing <- function(ag.or.forest,
                               treatment.type,
                               initial.moisture,
                               harvest.collection.year.diff,
                               comminution.opt,
                               post.harvest.processing,
                               scattered.fraction,
                               piled.fraction) {

  ################################################
  ################################################
  # Debugging values
  ################################################
  ################################################
#  ag.or.forest <- 'Agriculture'
#  treatment.type <- 'None'
#  initial.moisture <- 35
#  harvest.collection.year.diff <- 4
#  comminution.opt <- 'Grinding'
#  post.harvest.processing <- 'None'
#  scattered.fraction <- 0.5
#  piled.fraction <- 0.5
  
  # Year should be 1-100. KEEP YEAR COLUMN FOR TRANSPARENCY
  emissions.annual.profile <- data.table(Year=1:100,
                                         Harvest_Tons.CO2.per.BDT.biomass=0,
                                         Harvest_Tons.CH4.per.BDT.biomass=0,
                                         Harvest_Tons.N2O.per.BDT.biomass=0,
                                         Comminution_Tons.CO2.per.BDT.biomass=0,
                                         Comminution_Tons.CH4.per.BDT.biomass=0,
                                         Comminution_Tons.N2O.per.BDT.biomass=0,
                                         Processing_Tons.CO2.per.BDT.biomass=0,
                                         Processing_Tons.CH4.per.BDT.biomass=0,
                                         Processing_Tons.N2O.per.BDT.biomass=0)
  
  # There are four emissions sources: harvest equipment, processing, comminution, and
  # any pre-harvest decay. Equipment, processing, and comminution will require
  # matching the correct emissions factor with the correct option, and applying to
  # the harvest year.
 
  # Lookup table type information - harvest equipment, processing, and comminution 
  # carbon intensities are stored in a directory labeled "Lookup_Data" in the main 
  # directory. For speed and file size reasons, I want them as Rdata files. We
  # can then clear out the data after we've performed the calculations we need to perform.
  
  # select.harvest.equipment will match our criteria to the appropriate equipment. 
  # The function will return a 3-value vector; [CO2 intensity, CH4 intensity, N2O intensity]
  selected.equipment <- select.harvest.equipment(TRUE)
  
  # Add harvest emissions to the harvest year. 
  emissions.annual.profile[Year==harvest.collection.year.diff,':='(Harvest_Tons.CO2.per.BDT.biomass=selected.equipment[1],
                                                                   Harvest_Tons.CH4.per.BDT.biomass=selected.equipment[2],
                                                                   Harvest_Tons.N2O.per.BDT.biomass=selected.equipment[3])]
  
  # Load up the comminution data, add the appropriate emissions to the profile, and then
  # remove the data. 
  load("Lookup_Data/comminution-CI.Rdata")
  emissions.annual.profile[Year==harvest.collection.year.diff,':='(Comminution_Tons.CO2.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.CO2.per.BDT.biomass],
                                                                   Comminution_Tons.CH4.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.CH4.per.BDT.biomass],
                                                                   Comminution_Tons.N2O.per.BDT.biomass=comminution.CI[Comminution==comminution.opt,Tons.N2O.per.BDT.biomass])]
  comminution.mass.loss <- comminution.CI[Comminution==comminution.opt,Post_Comminution.Mass.Loss]
  remove(comminution.CI)
  
  # Repeat for processing
  load("Lookup_Data/processing-CI.Rdata")
  emissions.annual.profile[Year==harvest.collection.year.diff,':='(Processing_Tons.CO2.per.BDT.biomass=processing.CI[Processing==post.harvest.processing,Tons.CO2.per.BDT.biomass],
                                                                   Processing_Tons.CH4.per.BDT.biomass=processing.CI[Processing==post.harvest.processing,Tons.CH4.per.BDT.biomass],
                                                                   Processing_Tons.N2O.per.BDT.biomass=processing.CI[Processing==post.harvest.processing,Tons.N2O.per.BDT.biomass])]
  processing.mass.loss <- processing.CI[Processing==post.harvest.processing,Post_Processing.Mass.Loss]
  remove(processing.CI)
  
  # Now to tackle decay emissions. In spreadsheet form, we calculated the mass loss and 
  # looked up the appropriate values. This led to giant lookup tables. Instead, we have 
  # a file of decay functions labeled by decay function selection criteria, i.e. 
  # "Forestry.Mixed", Agriculture.Corn", or "Agriculture.Cotton". Using "match.fun"
  # ('get' would also work), we can select the needed decay function and calculate only
  # what we need based on the year.
  
  # Currently, the labels for the dummy functions are ag.or.forest and the primary harvest species. 
  # I've also only written three dummy functions.
  decay.function.key <- paste(ag.or.forest,this.location@primary.harvest.species,'decay',sep='.')
  decay.function <- match.fun(decay.function.key)
  
  # Now that we have the appropriate decay function, create a column in emissions.annual.profile
  # for the cumulative decay at year X. In addition to the standard input data needed to calculate
  # decay, the decay function also requires the specific emissions species (CO2, CH4, N2O). The 
  # output will be the tons emissions/BDT biomass
  
  emissions.annual.profile[Year<=harvest.collection.year.diff,':='(Cumulative.Decay_Tons.CO2.per.BDT.biomass=mapply(decay.function,"CO2",Year,scattered.fraction,piled.fraction),
                                                                   Cumulative.Decay_Tons.CH4.per.BDT.biomass=mapply(decay.function,"CH4",Year,scattered.fraction,piled.fraction),
                                                                   Cumulative.Decay_Tons.N2O.per.BDT.biomass=mapply(decay.function,"N2O",Year,scattered.fraction,piled.fraction))]
  
  emissions.annual.profile[,':='(Cumulative.Decay_Tons.CO2.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.CO2.per.BDT.biomass,na.rm=FALSE),
                                 Cumulative.Decay_Tons.CH4.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.CH4.per.BDT.biomass,na.rm=FALSE),
                                 Cumulative.Decay_Tons.N2O.per.BDT.biomass=na.locf(Cumulative.Decay_Tons.N2O.per.BDT.biomass,na.rm=FALSE))]

  # Calculate the annual emissions difference from the cumulative emissions
  emissions.annual.profile[,':='(Annual.Decay_Tons.CO2.per.BDT.biomass=Cumulative.Decay_Tons.CO2.per.BDT.biomass - c(0,Cumulative.Decay_Tons.CO2.per.BDT.biomass[(.I-1)]),
                                 Annual.Decay_Tons.CH4.per.BDT.biomass=Cumulative.Decay_Tons.CH4.per.BDT.biomass - c(0,Cumulative.Decay_Tons.CH4.per.BDT.biomass[(.I-1)]),
                                 Annual.Decay_Tons.N2O.per.BDT.biomass=Cumulative.Decay_Tons.N2O.per.BDT.biomass - c(0,Cumulative.Decay_Tons.N2O.per.BDT.biomass[(.I-1)]))]

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