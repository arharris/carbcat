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

#####################################################################################
# piled_k_const: A function to calculate the decay constant for a given cell 
# incorporating pile fractions. 
#####################################################################################
# Piled k constant (Function by Max Blasdel)
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate calculate the decay constant for a given cell incorporating pile fractions
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# k_const: the k value for given cell/residue size
# per_ag: percent above ground, the proportion of a pile that receives the piled coefficient
# per_gc: percent ground contact, the proportion that is considered the same as scattered from decay
# coEf: the piled coefficient value that characterizes a change in decay for suspended materials
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Adjusted pile k constant for given cell/residue size
# -------------------------------------------------------------------------------------------------
piled_k_const <- function(k_const, coEf = 0.721, per_ag = .892, per_gc = .108) {
  
  k_pile <- ((k_const * coEf) * per_ag) + (k_const * per_gc)
  
  return(k_pile)
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
  decay_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, N2O_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SOx_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  # Piled residues will have a slightly different decay rate becasue of varying ground contact. The piled k constant is dervied from the original k constant
  if(residue.segment=='Piled') { 
    # Piled residues; use pile-adjusted k constant
    cbrec.dt[,':='(decay_CWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"CWD_tonsAcre",sep="_")),piled_k_const(CWD_cm),year.i),
                   decay_FWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"FWD_tonsAcre",sep="_")),piled_k_const(FWD_cm),year.i),
                   decay_Foliage_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"Foliage_tonsAcre",sep="_")),piled_k_const(Foliage_cm),year.i),
                   decay_Foliage_cumfrac = multi_year_decay_fun(1,piled_k_const(Foliage_cm),year.i))]
  } else {
    # Non-piled residues, use standard k constant
    cbrec.dt[,':='(decay_CWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"CWD_tonsAcre",sep="_")),CWD_cm,year.i),
                   decay_FWD_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"FWD_tonsAcre",sep="_")),FWD_cm,year.i),
                   decay_Foliage_mass_year_i = one_year_decay_fun(get(paste(residue.segment,"Foliage_tonsAcre",sep="_")),Foliage_cm,year.i),
                   decay_Foliage_cumfrac = multi_year_decay_fun(1,Foliage_cm,year.i))]
  }
  
  # Apply the mass lost to the dynamic mass columns.
  cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) - decay_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) - decay_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) - decay_Foliage_mass_year_i]
  
  # Calculate mass converted to duff, and populate the duff mass column. If the the residue segment is previously fired residues, this will go to the previously fired duff column.
  if(residue.segment=='prev_fired') {
    cbrec.dt[,prev_fired_Duff_tonsAcre := prev_fired_Duff_tonsAcre + duff_decay_mass_fraction * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
    
    # Check if the foliage mass should be at least 1/2 decayed using decay_Foliage_cumfrac; if so, convert the rest of the foliage to duff. NOTE TO ANDY: if you get the brilliant idea to track decay loss, 
    # this will not work - if (theoretically) 75% of the mass is lost to wildfire before it can decay, the transition to duff would never happen. Base the switch on the expected decay.
    
    cbrec.dt[decay_Foliage_cumfrac <= 0.5, prev_fired_Duff_tonsAcre := prev_fired_Duff_tonsAcre + get(paste(residue.segment,"Foliage_tonsAcre",sep="_"))]
    cbrec.dt[decay_Foliage_cumfrac <= 0.5, paste(residue.segment,"Foliage_tonsAcre",sep="_") := 0]
  } else {
    cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre + duff_decay_mass_fraction * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
    
    # Check if the foliage mass should be at least 1/2 decayed using decay_Foliage_cumfrac; if so, convert the rest of the foliage to duff. NOTE TO ANDY: if you get the brilliant idea to track decay loss, 
    # this will not work - if (theoretically) 75% of the mass is lost to wildfire before it can decay, the transition to duff would never happen. Base the switch on the expected decay.
    
    cbrec.dt[decay_Foliage_cumfrac <= 0.5, Duff_tonsAcre := Duff_tonsAcre + get(paste(residue.segment,"Foliage_tonsAcre",sep="_"))]
    cbrec.dt[decay_Foliage_cumfrac <= 0.5, paste(residue.segment,"Foliage_tonsAcre",sep="_") := 0]
  }
  
  
    # Calculate emissions - decay will generate CO2 and CH4, all other species are 0.
  # CH4 
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum(decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)] * (1-duff_decay_mass_fraction) * cell_to_acres * CH4_decay_emissions_factor]
  
  # CO2 Any carbon not emitted as CH4 will be as CO2.
  decay_emissions_profile[, CO2_tonnes := CO2_tonnes + cbrec.dt[,sum((decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i) * (Carbon_frac - CH4_decay_emissions_factor / CH4_carbon_fraction))] * (1-duff_decay_mass_fraction) * cell_to_acres * CO2_carbon_fraction]
  
  return(list(decay_emissions_profile,cbrec.dt))
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
  decay_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, N2O_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SOx_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  # We should have accumulated duff from all residues prior to calling this function. Now we can decay duff, add the resulting emissions, and adjust duff mass totals
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum(one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i))] * cell_to_acres * CH4_decay_emissions_factor]
  decay_emissions_profile[, CO2_tonnes := CO2_tonnes + cbrec.dt[,sum(one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i) * (Carbon_frac - CH4_decay_emissions_factor / CH4_carbon_fraction))] * cell_to_acres * CO2_carbon_fraction]
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre - one_year_decay_fun(Duff_tonsAcre,duff_k_val,year.i)]
  
  # Repeat for previously fired duff.
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum(one_year_decay_fun(prev_fired_Duff_tonsAcre,duff_k_val,year.i))] * cell_to_acres * CH4_decay_emissions_factor]
  decay_emissions_profile[, CO2_tonnes := CO2_tonnes + cbrec.dt[,sum(one_year_decay_fun(prev_fired_Duff_tonsAcre,duff_k_val,year.i) * (Carbon_frac - CH4_decay_emissions_factor / CH4_carbon_fraction))] * cell_to_acres * CO2_carbon_fraction]
  cbrec.dt[,prev_fired_Duff_tonsAcre := prev_fired_Duff_tonsAcre - one_year_decay_fun(prev_fired_Duff_tonsAcre,duff_k_val,year.i)]
  
  return(list(decay_emissions_profile,cbrec.dt))
}


