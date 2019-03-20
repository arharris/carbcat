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
#   2019-03-19: Initialized Script
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate mass loss and emissions along all residue size classes due to prescribed
# burning. Unburnt mass remains in the base category, because it may be exposed to wildfire.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# residue.segment: Residue disposition; scattered, field piled or landing piled
# year.i: The year for which we want to calculate the char and emissions from the year prior (i.e.,
#   if t=3, we would calculate the emissions between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# =================================================================================================

prescribed_burn_fun <- function(cbrec.dt, residue.segment, burn.type) {
  # The prescribed burn will affect different residue segments 
  
  # Though wildfire will in reality strike one year and burn a large portion of residue, because wildfires are probabilistic, we model 
  # it as a series of annual, small wildfires, with the mass fraction of residues exposed to wildfire equal to the wildfire probability.
  # The first step is to determine the mass of residue exposed to wildfire using the residue mass and the MassExposedToWildfire fraction
  cbrec.dt[,fire_exposed_CWD_mass_year_i := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) * MassExposedToWildfire]
  cbrec.dt[,fire_exposed_FWD_mass_year_i := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) * MassExposedToWildfire]
  cbrec.dt[,fire_exposed_Foliage_mass_year_i := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) * MassExposedToWildfire]
  cbrec.dt[,fire_exposed_Duff_mass_year_i := Duff_tonsAcre * MassExposedToWildfire]
  
  # Apply the mass lost to the dynamic mass columns. 
  cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) - fire_exposed_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) - fire_exposed_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) - fire_exposed_Foliage_mass_year_i]
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre - fire_exposed_Duff_mass_year_i]
  
  # The fraction of biomass exposed to wildfire will never be exposed again. Once exposed, biomass is either:
  # 1) Consumed by wildfire; the mass will be transferred to emissions 
  # 2) Charred; the mass will no longer decay.
  # 3) Left unburned, to decay normally. Previously exposed/unburned material will be decayed before this, and newly exposed/unburnt added below.
  
  # Apply the mass from the fire exposed to each category, starting with emissions
  cbrec.dt[,paste("Emissions_year_",year.i,sep='') := get(paste("Emissions_year_",year.i,sep='')) + 
             CWD_CombustionFrac * fire_exposed_CWD_mass_year_i + 
             FWD_CombustionFrac * fire_exposed_FWD_mass_year_i + 
             Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i + 
             Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i]
  
  # Next, char
  cbrec.dt[,paste("char_year_",year.i,sep='') := get(paste("char_year_",year.i,sep='')) + 
             CWD_CharFrac * fire_exposed_CWD_mass_year_i + 
             FWD_CharFrac * fire_exposed_FWD_mass_year_i + 
             Foliage_CharFrac * fire_exposed_Foliage_mass_year_i + 
             Foliage_CharFrac * fire_exposed_Duff_mass_year_i]
  
  # And the unburned
  cbrec.dt[,':='(prev_fired_CWD_mass = prev_fired_CWD_mass + (CWD_UnburendFrac * fire_exposed_CWD_mass_year_i),
                 prev_fired_FWD_mass = prev_fired_FWD_mass + (FWD_UnburendFrac * fire_exposed_FWD_mass_year_i),
                 prev_fired_Foliage_mass = prev_fired_Foliage_mass + (Foliage_UnburendFrac * fire_exposed_Foliage_mass_year_i),
                 prev_fired_Duff_mass = prev_fired_Duff_mass + (Foliage_UnburendFrac * fire_exposed_Duff_mass_year_i)
  )]
  
  return(cbrec.dt)
}