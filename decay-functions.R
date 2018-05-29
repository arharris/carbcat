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
# Currently, the percentage of carbon lost to decay, cumulatively
#
# =============================================================================

# Sample decay format: (A0 * exp(R1*years) + A1 * exp(R2*years)) / 100

# These functions should have more descriptive names
dummy.decay.1 <- function(years) {
  return((25*exp(-0.0003*years)+20*exp(-0.0004*years))/100)
}

# These functions should have more descriptive names
dummy.decay.2 <- function(years) {
  return((31.7*exp(-0.0026*years)+4.79*exp(-0.00037*years))/100)
}

# These functions should have more descriptive names
dummy.decay.3 <- function(years) {
  return((17.3*exp(-0.00038*years)+18.9*exp(-0.00044*years))/100)
}