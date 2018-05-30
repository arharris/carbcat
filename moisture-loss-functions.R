# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-05-30: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#         
# ---------------------------------
# INPUTS:
# This will change as we nail down moisture loss functions. However, every 
# function must take the same inputs, whatever they are. These inputs might
# not be necessary for the calculation, but these functiuons must work with
# a generic call function.
#   1) Time (Years)
# ---------------------------------
# OBJECTIVE:
# This module contains moisture loss functions for use in in-field modules and the 
# harvest-processing module.
#
# ---------------------------------
# OUTPUTS:
# The percentage of moisture remaining from the beginning of the module
#
# =============================================================================

# These functions should be named as a concatenation of the relevant criteria

# Dummy function
Forestry.Mixed.moisture.loss <- function(years) {
  if(years>0&years<70) {
    return(1-years/100)
  } else if(years>=70) {
    return(0.3)
  } else {
    return(1)
  }
}

# Dummy function
Agriculture.Corn.moisture.loss <- function(years) {
  if(years>0&years<90) {
    return(1-years/150)
  } else if(years>=90) {
    return(0.4)
  } else {
    return(1)
  }
}

# Dummy function
Agriculture.Cotton.moisture.loss <- function(years) {
  if(years>0&years<50) {
    return(1-years/55)
  } else if(years>=50) {
    return(0.09)
  } else {
    return(1)
  }
}