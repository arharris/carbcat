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
#   2019-03-19: Updated decay_fun to include all decay calculations
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This module contains decay functions for use in CBREC
#
# ---------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# ---------------------------------
# OBJECTIVE:
# This module contains decay functions for use in CBREC
# =============================================================================

# Global constants
duff_decay_mass_fraction <- 0.02
duff_k_val <- 0.002
CH4_decay_emissions_factor <- 0.05 # Mass CH4 emitted/mass biomass decayed.
#########################################################################################
# THIS WILL REQUIRE A CHANGE - CONVERT TO CARBON FRACTIONS!
#########################################################################################

# Multi-Year Decay
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate the remaining mass after t years.  
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# residue: The initial mass of residue. If set to 1, it just calculates the fraction of decay
# k_val: The annual decay rate
# t: The time (in years) for which we want to calculate the decayed mass.
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Mass value of residue after t years. Idf the initial residue is set to 1, this caould also be 
# used as the fraction of initial mass that has decayed.
# -------------------------------------------------------------------------------------------------
multi_year_decay_fun <- function(residue, k_val, t) {
  
  return(residue * exp(-k_val * t))
  
}

# One-Year Decay
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate the mass decayed in a one-year period  
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# residue: The initial mass of residue before the single year of decay.
# k_val: The annual decay rate
# t: The year for which we want to calculate the decay from the year prior (i.e., if t=3, we would 
# calculate the decay between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Mass value of residue decayed between year t and t-1. 
# -------------------------------------------------------------------------------------------------
one_year_decay_fun <- function(residue, k_val, t) {
  
  return(residue * (exp(-k_val * (t-1)) - exp(-k_val * t)) )
  
}

# Main Decay Function
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will, for a given residue segment (scattered, field piled, landing piled) calculate
# the mass decayed in a one-year period along all residue size classes (EXCLUDING duff). The decayed
# mass is either allocated to duff or emissions; if decay has progressed enough, foliage decay 
# entirely to duff is addressed.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# residue.segment: Residue disposition; scattered, field piled or landing piled
# year.i: The year for which we want to calculate the decay and emissions from the year prior (i.e.,
# if t=3, we would calculate the decay between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# -------------------------------------------------------------------------------------------------

decay_fun <- function(cbrec.dt,residue.segment,year.i) {
  cbrec.dt[,':='(decay_CWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"CWD_tonsAcre",sep="_")),CWD_cm,year.i),
                 decay_FWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"FWD_tonsAcre",sep="_")),FWD_cm,year.i),
                 decay_Foliage_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"Foliage_tonsAcre",sep="_")),Foliage_cm,year.i),
                 prev_fired_decay_CWD_mass_year_i = one_year_decay_fun(prev_fired_CWD_mass,CWD_cm,year.i),
                 prev_fired_decay_FWD_mass_year_i = one_year_decay_fun(prev_fired_FWD_mass,FWD_cm,year.i),
                 prev_fired_decay_Foliage_mass_year_i = one_year_decay_fun(prev_fired_Foliage_mass,Foliage_cm,year.i),
                 decay_Foliage_cumfrac = multi_year_decay_fun(1,Foliage_cm,year.i))]
  
  # Calculate mass converted to duff, and populate the duff mass column
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre + duff_decay_mass_fraction * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
  
  # Repeat for the fire exposed but unburned mass.
  cbrec.dt[,prev_fired_Duff_mass := prev_fired_Duff_mass + duff_decay_mass_fraction * (prev_fired_decay_CWD_mass_year_i + prev_fired_decay_FWD_mass_year_i + prev_fired_decay_Foliage_mass_year_i)]
  
  # Apply the mass lost to the dynamic mass columns.
  cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) - decay_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) - decay_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) - decay_Foliage_mass_year_i]
  
  # Repeat for the fire exposed but unburned mass.
  cbrec.dt[,':='(prev_fired_CWD_mass = prev_fired_CWD_mass - prev_fired_decay_CWD_mass_year_i,
                 prev_fired_FWD_mass = prev_fired_FWD_mass - prev_fired_decay_FWD_mass_year_i,
                 prev_fired_Foliage_mass = prev_fired_Foliage_mass - prev_fired_decay_Foliage_mass_year_i)]
  
  # Check if the foliage mass should be at least 1/2 decayed using decay_Foliage_cumfrac; if so, convert the rest of the foliage to duff.
  cbrec.dt[decay_Foliage_cumfrac <= 0.5, Duff_tonsAcre := Duff_tonsAcre + get(paste(residue.segment,"Foliage_tonsAcre",sep="_"))]
  cbrec.dt[decay_Foliage_cumfrac <= 0.5, paste(residue.segment,"Foliage_tonsAcre",sep="_") := 0]
  
  # The unburned foliage would revert to duff as well.
  cbrec.dt[decay_Foliage_cumfrac <= 0.5, prev_fired_Duff_mass := prev_fired_Duff_mass + prev_fired_Foliage_mass]
  cbrec.dt[decay_Foliage_cumfrac <= 0.5, prev_fired_Foliage_mass := 0]
  
  # Calculate emissions - decay will generate CO2 and CH4, all other species are 0.
  # CH4 - be sure to convert from tons carbon to tons CH4
  cbrec.dt[,paste("Emissions_CH4_year_",year.i,sep='') := get(paste("Emissions_CH4_year_",year.i,sep='')) + (1-duff_decay_mass_fraction) * CH4_decay_emissions_factor * CH4_carbon_fraction * Carbon_frac * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
  
  # CO2 - be sure to convert from tons carbon to tons CO2. Any carbon not emitted as CH4 will be as CO2.
  cbrec.dt[,paste("Emissions_CO2_year_",year.i,sep='') := get(paste("Emissions_CO2_year_",year.i,sep='')) + (1-duff_decay_mass_fraction) * (1 - CH4_decay_emissions_factor) * CO2_carbon_fraction * Carbon_frac * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
  
  
  # Repeat decay emissions for previously fired materials.
  cbrec.dt[,paste("Emissions_CH4_year_",year.i,sep='') := get(paste("Emissions_CH4_year_",year.i,sep='')) + (1-duff_decay_mass_fraction) * CH4_decay_emissions_factor * CH4_carbon_fraction * Carbon_frac * (prev_fired_decay_CWD_mass_year_i + prev_fired_decay_FWD_mass_year_i + prev_fired_decay_Foliage_mass_year_i)]
  cbrec.dt[,paste("Emissions_CO2_year_",year.i,sep='') := get(paste("Emissions_CO2_year_",year.i,sep='')) + (1-duff_decay_mass_fraction) * (1 - CH4_decay_emissions_factor) * CO2_carbon_fraction * Carbon_frac * (prev_fired_decay_CWD_mass_year_i + prev_fired_decay_FWD_mass_year_i + prev_fired_decay_Foliage_mass_year_i)]
  
  return(cbrec.dt)
}


# Duff Decay Function
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate the mass decayed in a one-year period. This is separate from the main
# residue segments so that we can eliminate the residue segment loop.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# year.i: The year for which we want to calculate the decay and emissions from the year prior (i.e.,
# if t=3, we would calculate the decay between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# -------------------------------------------------------------------------------------------------

duff_decay_fun <- function(cbrec.dt,year.i) {
  # We should have accumulated duff from all residues prior to calling this function. Now we can decay duff, add the resulting emissions, and adjust duff mass totals
  cbrec.dt[,paste("Emissions_CH4_year_",year.i,sep='') := get(paste("Emissions_CH4_year_",year.i,sep='')) + one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i)] * CH4_decay_emissions_factor * Carbon_frac * CH4_carbon_fraction
  cbrec.dt[,paste("Emissions_CO2_year_",year.i,sep='') := get(paste("Emissions_CO2_year_",year.i,sep='')) + one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i)] * (1 - CH4_decay_emissions_factor) * Carbon_frac * CO2_carbon_fraction
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre - one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i)]
  
  # Repeat for previously fired duff.
  cbrec.dt[,paste("Emissions_CH4_year_",year.i,sep='') := get(paste("Emissions_CH4_year_",year.i,sep='')) + one_year_decay_fun(prev_fired_Duff_mass,duff_k_val,year.i)] * CH4_decay_emissions_factor * Carbon_frac * CH4_carbon_fraction
  cbrec.dt[,paste("Emissions_CO2_year_",year.i,sep='') := get(paste("Emissions_CO2_year_",year.i,sep='')) + one_year_decay_fun(prev_fired_Duff_mass,duff_k_val,year.i)] * (1 - CH4_decay_emissions_factor) * Carbon_frac * CO2_carbon_fraction
  cbrec.dt[,prev_fired_Duff_mass := prev_fired_Duff_mass - one_year_decay_fun(prev_fired_Duff_mass,duff_k_val,year.i)]
  
  return(cbrec.dt)
}


