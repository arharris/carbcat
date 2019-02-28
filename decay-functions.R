# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
# 
#         Micah Wright (wrightmicahc@gmail.com)
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2019-02-27: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#         
# ---------------------------------
# INPUTS:
# 
# ---------------------------------
# OBJECTIVE:
# This module contains decay functions for use in the reference case scenarios
#
# ---------------------------------
# OUTPUTS:
#
# =============================================================================

decay_fun <- function(residue, k_val, t) {
  
  return(residue * exp(-k_val * t))
  
}
