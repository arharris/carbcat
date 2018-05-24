# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-05-24: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#   1     Need to include location (or location-specific information) as    IN PROGRESS
#         input
#   2     Include primary harvest species information. Based on location 
#         only for forestry, allow as user option for agriculture           IN PROGRESS
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
# =============================================================================

# Source files
source("misc-functions.R")

# Required Libraries
load.libraries(c())

harvest.processing <- function(ag.or.forest,treatment.type,initial.moisture,harvest.collection.year.diff,comminution.opt,processing.opt) {
  
}