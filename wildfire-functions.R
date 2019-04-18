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
# This function will, for a given residue segment (scattered, field piled, landing piled) calculate
# the mass burned in a one-year period along all residue size classes (excluding duff, which will 
# be called after the other residue segments.)
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

annual_wildfire_fun <- function(cbrec.dt, residue.segment, year.i) {
  ##############################################################################################################
  ##############################################################################################################
  # MICAH IS STILL WORKING ON WILDFIRE - WE MAY NEED TO ADJUST VARIABLE NAMES, OR POSSIBLY SOME MINOR DETAILS
  # OF THE APPROACH. SOME COLUMNS WERE CREATED BEFORE THE LOOP, BUT THE WILDFIRE SEGMENT STARTS HERE.
  ##############################################################################################################
  ##############################################################################################################  
  
  # Though wildfire will in reality strike one year and burn a large portion of residue, because wildfires are probabilistic, we model 
  # it as a series of annual, small wildfires, with the mass fraction of residues exposed to wildfire equal to the wildfire probability.
  # The first step is to determine the mass of residue exposed to wildfire using the residue mass and the MassExposedToWildfire fraction
  cbrec.dt[,fire_exposed_CWD_mass_year_i := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) * MassExposedToWildfire]
  cbrec.dt[,fire_exposed_FWD_mass_year_i := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) * MassExposedToWildfire]
  cbrec.dt[,fire_exposed_Foliage_mass_year_i := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) * MassExposedToWildfire]

  # Apply the mass lost to the dynamic mass columns. 
  cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := get(paste(residue.segment,"CWD_tonsAcre",sep="_")) - fire_exposed_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := get(paste(residue.segment,"FWD_tonsAcre",sep="_")) - fire_exposed_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) - fire_exposed_Foliage_mass_year_i]
  
  # The fraction of biomass exposed to wildfire will never be exposed again. Once exposed, biomass is either:
  # 1) Consumed by wildfire; the mass will be transferred to emissions 
  # 2) Charred; the mass will no longer decay.
  # 3) Left unburned, to decay normally. Previously exposed/unburned material will be decayed before this, and newly exposed/unburnt added below.
  
  # Apply the mass from the fire exposed to each category, starting with emissions
  cbrec.dt[,paste("Emissions_year_",year.i,sep='') := get(paste("Emissions_year_",year.i,sep='')) + 
                                                      CWD_CombustionFrac * fire_exposed_CWD_mass_year_i + 
                                                      FWD_CombustionFrac * fire_exposed_FWD_mass_year_i + 
                                                      Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i]
  
  # Next, char
  cbrec.dt[,paste("char_year_",year.i,sep='') := get(paste("char_year_",year.i,sep='')) + 
                                                 CWD_CharFrac * fire_exposed_CWD_mass_year_i + 
                                                 FWD_CharFrac * fire_exposed_FWD_mass_year_i + 
                                                 Foliage_CharFrac * fire_exposed_Foliage_mass_year_i]
  
  # And the unburned
  cbrec.dt[,':='(prev_fired_CWD_mass = prev_fired_CWD_mass + (CWD_UnburnedFrac * fire_exposed_CWD_mass_year_i),
                 prev_fired_FWD_mass = prev_fired_FWD_mass + (FWD_UnburnedFrac * fire_exposed_FWD_mass_year_i),
                 prev_fired_Foliage_mass = prev_fired_Foliage_mass + (Foliage_UnburnedFrac * fire_exposed_Foliage_mass_year_i)
          )]
  
  return(cbrec.dt)
}

duff_annual_wildfire_fun <- function(cbrec.dt, year.i) {
  ##############################################################################################################
  ##############################################################################################################
  # MICAH IS STILL WORKING ON WILDFIRE - WE MAY NEED TO ADJUST VARIABLE NAMES, OR POSSIBLY SOME MINOR DETAILS
  # OF THE APPROACH. SOME COLUMNS WERE CREATED BEFORE THE LOOP, BUT THE WILDFIRE SEGMENT STARTS HERE.
  ##############################################################################################################
  ##############################################################################################################  
  
  # Duff will burn in the same manner as the other size classes, but we calculate it in a separate function so it does
  # not get triple-counted with the other residue segments, and I can keep the functions general.
  cbrec.dt[,fire_exposed_Duff_mass_year_i := Duff_tonsAcre * MassExposedToWildfire]
  
  # Apply the mass lost to the dynamic mass column. 
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre - fire_exposed_Duff_mass_year_i]
  
  # Apply the mass from the fire exposed to emissions, then char, then unburned.
  cbrec.dt[,paste("Emissions_year_",year.i,sep='') := get(paste("Emissions_year_",year.i,sep='')) + 
            Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i]
  
  # Next, char
  cbrec.dt[,paste("char_year_",year.i,sep='') := get(paste("char_year_",year.i,sep='')) + 
            Foliage_CharFrac * fire_exposed_Duff_mass_year_i]
  
  # And the unburned
  cbrec.dt[,prev_fired_Duff_mass := prev_fired_Duff_mass + (Foliage_UnburnedFrac * fire_exposed_Duff_mass_year_i)]
  
  return(cbrec.dt)
}
