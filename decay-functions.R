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
#   1) Decay species (CO2, CH4, N2O)
#   2) Time (Years)
#   3) Location object
#   4) Scattered Fraction
#   5) Piled Fraction
# ---------------------------------
# OBJECTIVE:
# This module contains decay fundtions for use in in-field modules and the 
# harvest-processing module.
#
# ---------------------------------
# OUTPUTS:
# The cumulative CO2, CH4, or N2O emissions at year X due to decay
#
# =============================================================================

# These functions should be named as a concatenation of the relevant criteria
Forestry.Mixed.decay <- function(species,years,scattered.fraction,piled.fraction) {
  if(species=='CO2') {
    return(scattered.fraction*exp(this.location@forestry.decay.rates$scattered.CO2.k*years)+piled.fraction*exp(this.location@forestry.decay.rates$piled.CO2.k*years))
  } else if(species=='CH4') {
    return(scattered.fraction*exp(this.location@forestry.decay.rates$scattered.CH4.k*years)+piled.fraction*exp(this.location@forestry.decay.rates$piled.CH4.k*years))
  } else if(species=='N2O') {
    return(scattered.fraction*exp(this.location@forestry.decay.rates$scattered.N2O.k*years)+piled.fraction*exp(this.location@forestry.decay.rates$piled.N2O.k*years))
  } else {
    cat('Calling a decay function on something other than CO2, CH4, and N2O')
    return(-9999)
  }
}

# Sample Ag decay format: (A0 * exp(R1*years) + A1 * exp(R2*years)) / 100

# Dummy function
Agriculture.Corn.decay <- function(species,years,scattered.fraction,piled.fraction) {
  if(species=='CO2') {
    #test <- location.obj@scattered.CO2.k
    return(1-(31.7*exp(-0.0026*years)+4.79*exp(-0.00037*years))/100)
  } else if(species=='CH4') {
    #test <- location.obj@scattered.CH4.k
    return(1-(30.7*exp(-0.0023*years)+3.79*exp(-0.00034*years))/100)
  } else if(species=='N2O') {
    #test <- location.obj@scattered.N2O.k
    return(1-(29.7*exp(-0.0020*years)+2.79*exp(-0.00031*years))/100)
  } else {
    cat('Calling a decay function on something other than CO2, CH4, and N2O')
    return(-9999)
  }
}

# Dummy function
Agriculture.Cotton.decay <- function(species,years,scattered.fraction,piled.fraction) {
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