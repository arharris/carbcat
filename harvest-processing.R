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
#         
# ---------------------------------
# INPUTS:
# ag.or.forest: Value of "Agriculture" or "Forestry"
# treatment.type: (explain better)

# ---------------------------------
# OBJECTIVE:
#
# This module will calculate carbon emissions for the harvest-processing phase
# of CARBCAT. (Expand this explanation)
#
# ---------------------------------
# OUTPUTS:
# 1) A data table covering emissions for years 0-100 (Get more details here)
# 2) Mass loss through processing/comminution
# 3) Moisture loss through passive drying
#
# =============================================================================

# Source files
source("misc-functions.R")
source("class_LocationData.R")
source("class_ModuleOutput.R")
# Make a dummy location data
#dummy.location.obj <- new("location.data",
#         land.ownership = "Private",
#         slope = 0,
#         biomass.market.volume = "High Volume",
#         pulp.market = FALSE,
#         primary.harvest.species = "Mixed"
#)
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
                               processing.opt,
                               location.obj) {
  
  # Debugging values
#  ag.or.forest <- 'Forestry'
#  treatment.type <- 'None'
#  initial.moisture <- 35
#  harvest.collection.year.diff <- 4
#  comminution.opt <- 'Chipping'
#  processing.opt <- 'Pelletizer'

  # I am imagining that we would load in the relevant tables? We can then clear out the data 
  # after we've performed the calculations we need to perform.
  
  # Let's get some basic construction out of the way. If there is a way to index row 
  # number for use in decay formulas, that would be amazing.
  emissions.annual.profile <- data.table(Year=0:100,
                                         Harvest_Tons.CO2eq.per.BDT.biomass=0,
                                         Comminution_Tons.CO2eq.per.BDT.biomass=0,
                                         Processing_Tons.CO2eq.per.BDT.biomass=0)
  
  # There are four emissions sources: harvest equipment, processing, comminution, and
  # any pre-harvest decay. Equipment, processing, and comminution will require
  # matching the correct emissions factor with the correct option, and applying to
  # the harvest year.
  #################################################################################
  #################################################################################
  # DESIGN DECISION ANDY IS MAKING
  # I want to store lookup table type information - harvest equipment, processing,
  # and comminution carbon intensities, in a directory labeled "Lookup_Data" in the
  # main directory. For speed and file size reasons, I want them as Rdata files.
  #################################################################################
  #################################################################################
  
  # Add harvest emissions to the harvest year
  emissions.annual.profile[Year==harvest.collection.year.diff,Harvest_Tons.CO2eq.per.BDT.biomass:=select.harvest.equipment(TRUE)]
  
  # Load up the comminution data, add the appropriate emissions to the profile, and then
  # remove the data.
  load("Lookup_Data/comminution-CI.Rdata")
  emissions.annual.profile[Year==harvest.collection.year.diff,Comminution_Tons.CO2eq.per.BDT.biomass:=comminution.CI[Comminution==comminution.opt,Tons.CO2eq.per.BDT.biomass]]
  comminution.mass.loss <- comminution.CI[Comminution==comminution.opt,Post_Comminution.Mass.Loss]
  remove(comminution.CI)
  
  # Repeat for processing
  load("Lookup_Data/processing-CI.Rdata")
  emissions.annual.profile[Year==harvest.collection.year.diff,Processing_Tons.CO2eq.per.BDT.biomass:=processing.CI[Processing==processing.opt,Tons.CO2eq.per.BDT.biomass]]
  processing.mass.loss <- processing.CI[Processing==processing.opt,Post_Processing.Mass.Loss]
  remove(processing.CI)
  
  # Now to tackle decay emissions. In spreadsheet form, we calculated the mass loss and 
  # looked up the appropriate values. This led to giant lookup tables. Instead, we have 
  # a file of decay functions labeled by decay function selection criteria, i.e. 
  # "Forestry.Mixed", Agriculture.Corn", or "Agriculture.Cotton". Using "match.fun"
  # ('get' would also work), we can select the needed decay function and calculate only
  # what we need based on the year.
  
  # Currently, the labels for the dummy functions are ag.or.forest and the primary harvest species. 
  # I've also onl;u written three dummy functions.
  decay.function.key <- paste(ag.or.forest,dummy.location.obj@primary.harvest.species,'decay',sep='.')
  decay.function <- match.fun(decay.function.key)
  
  # Now that we have the appropriate decay function, create a column in emissions.annual.profile
  # for the cumulative decay at year X.
  #################################################################################
  #################################################################################
  # DESIGN DECISION ANDY IS MAKING
  # Outputs of the decay function are the CO2eq emissions, not just the carbon lost.
  # (this is important if there are instances where decay leads to more than CO2).
  # SECOND DESIGN DECISION ANDY IS MAKING
  # I do not know what the decay functions will look like, but I am assuming that they
  # will not result in decay = 0 at year 0; so we are not assessing decay emissions at year 0
  #################################################################################
  #################################################################################
  emissions.annual.profile[Year==0,Cumulative.Decay_Tons.CO2eq.per.BDT.biomass:=0]
  emissions.annual.profile[Year>0&Year<harvest.collection.year.diff,Cumulative.Decay_Tons.CO2eq.per.BDT.biomass:=sapply(Year,decay.function)]
  emissions.annual.profile[,Cumulative.Decay_Tons.CO2eq.per.BDT.biomass:=na.locf(Cumulative.Decay_Tons.CO2eq.per.BDT.biomass,na.rm=FALSE)]

  # Calculate the annual emissions difference from the cumulative emissions
  emissions.annual.profile[Year==0,Annual.Decay_Tons.CO2eq.per.BDT.biomass:=0]
  emissions.annual.profile[Year>0,Annual.Decay_Tons.CO2eq.per.BDT.biomass:=Cumulative.Decay_Tons.CO2eq.per.BDT.biomass - c(0,Cumulative.Decay_Tons.CO2eq.per.BDT.biomass[(.I-1)])]

  # Add up the total annual emissions, then determine the cumulative
  emissions.annual.profile[,Annual.Total_Tons.CO2eq.per.BDT.biomass := Harvest_Tons.CO2eq.per.BDT.biomass + Comminution_Tons.CO2eq.per.BDT.biomass + Processing_Tons.CO2eq.per.BDT.biomass + Annual.Decay_Tons.CO2eq.per.BDT.biomass]
  emissions.annual.profile[,Cumulative.Total_Tons.CO2eq.per.BDT.biomass:=cumsum(Annual.Total_Tons.CO2eq.per.BDT.biomass)]
  
  # Processing, comminution, and decay emissions have been calculated, and comminution/
  # processing mass loss calculations are complete. Last, we calculate moisture loss
  moisture.loss.function.key <- paste(ag.or.forest,dummy.location.obj@primary.harvest.species,'moisture.loss',sep='.')
  moisture.loss.function <- match.fun(moisture.loss.function.key)
  harvest.processing.moisture.loss <- moisture.loss.function(harvest.collection.year.diff)
  
  harvest.processing.return.obj <- new("module.output",
           emissions.timeline = emissions.annual.profile,
          #################################################
          # Reality check the mass loss calculation
          ################################################
           mass.loss = processing.mass.loss*comminution.mass.loss,
           remaining.moisture = initial.moisture * harvest.processing.moisture.loss
  )
  
  return(harvest.processing.return.obj)
}