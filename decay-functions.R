# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-05-29: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#         
# ---------------------------------
# INPUTS:
# This will change as we nail down decay functions. However, every function must
# take the same inputs, whatever they are. These inputs might not be necessary for
# the calculation, but these functiuons must work with a generic call function.
#   1) Time (Years)
# ---------------------------------
# OBJECTIVE:
# This module contains decay fundtions for use in in-field modules and the 
# harvest-processing module.
#
# ---------------------------------
# OUTPUTS:
# The cumulative CO2eq emissions at year X due to decay
#
# =============================================================================

# Sample decay format: (A0 * exp(R1*years) + A1 * exp(R2*years)) / 100

# These functions should be named as a concatenation of the relevant criteria
Forestry.Mixed.decay <- function(species,years) {
  if(species=='CO2') {
    return(1-(25*exp(-0.0003*years)+20*exp(-0.0004*years))/100)
  } else if(species=='CH4') {
    return(1-(24*exp(-0.00025*years)+19*exp(-0.00035*years))/100)
  } else if(species=='N2O') {
    return(1-(23*exp(-0.00023*years)+18*exp(-0.00033*years))/100)
  } else {
    cat('Calling a decay function on something other than CO2, CH4, and N2O')
    return(-9999)
  }
}

# Dummy function
Agriculture.Corn.decay <- function(species,years) {
  if(species=='CO2') {
    return(1-(31.7*exp(-0.0026*years)+4.79*exp(-0.00037*years))/100)
  } else if(species=='CH4') {
    return(1-(30.7*exp(-0.0023*years)+3.79*exp(-0.00034*years))/100)
  } else if(species=='N2O') {
    return(1-(29.7*exp(-0.0020*years)+2.79*exp(-0.00031*years))/100)
  } else {
    cat('Calling a decay function on something other than CO2, CH4, and N2O')
    return(-9999)
  }
}

# Dummy function
Agriculture.Cotton.decay <- function(species,years) {
  if(species=='CO2') {
    return(1-(17.3*exp(-0.00038*years)+18.9*exp(-0.00044*years))/100)
  } else if(species=='CH4') {
    return(1-(16.3*exp(-0.00033*years)+17.9*exp(-0.00039*years))/100)
  } else if(species=='N2O') {
    return(1-(15.3*exp(-0.00028*years)+16.9*exp(-0.00034*years))/100)
  } else {
    cat('Calling a decay function on something other than CO2, CH4, and N2O')
    return(-9999)
  }
}