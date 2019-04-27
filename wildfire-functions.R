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
  
  wildfire_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, N2O_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SOx_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  ##############################################################################################################
  ##############################################################################################################
  # MICAH IS STILL WORKING ON WILDFIRE - WE MAY NEED TO ADJUST VARIABLE NAMES, OR POSSIBLY SOME MINOR DETAILS
  # OF THE APPROACH. SOME COLUMNS WERE CREATED BEFORE THE LOOP, BUT THE WILDFIRE SEGMENT STARTS HERE.
  ##############################################################################################################
  ##############################################################################################################  
  
  CH4_burn_emissions_factor <- 0.00382
  CO_burn_emissions_factor <- 0.0718
  N2O_burn_emissions_factor <- 0.00357 # Not in Micah's code
  NOx_burn_emissions_factor <- 0.00242
  PM10_burn_emissions_factor <- 0.0085904
  PM2.5_burn_emissions_factor <- 0.00728
  SOx_burn_emissions_factor <- 0.00098
  VOC_burn_emissions_factor <- 0.017342
  
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
  
  wildfire_emissions_profile[,':='(CO_tonnes = CO_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * CO_burn_emissions_factor,
                                        N2O_tonnes = N2O_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * N2O_burn_emissions_factor,
                                        CH4_tonnes = CH4_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * CH4_burn_emissions_factor,
                                        NOx_tonnes = NOx_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * NOx_burn_emissions_factor,
                                        PMUnder10um_tonnes = PMUnder10um_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * PM10_burn_emissions_factor,
                                        PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * PM2.5_burn_emissions_factor,
                                        SOx_tonnes = SOx_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * SOx_burn_emissions_factor,
                                        VOC_tonnes = VOC_tonnes + cbrec.dt[,sum((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres * VOC_burn_emissions_factor,
                                        char_tonnes = char_tonnes + cbrec.dt[,sum((CWD_CharFrac * fire_exposed_CWD_mass_year_i) + (FWD_CharFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CharFrac * fire_exposed_Foliage_mass_year_i))] * cell_to_acres)]
  
  
  # Any carbon not emitted as CH4, CO, PM10, PM2.5, or VOC is emitted as CO2. So we need to determine how much of the carbon was emitted in those constituents, 
  # take the remainder emitted as CO2 and convert to mass CO2 emissions. Carbon fraction variables are species mass / carbon mass
  
  # Calculate the fraction of carbon emitted in non-CO2 form
  non_CO2_combusted_carbon_frac <- CO_burn_emissions_factor / CO_carbon_fraction + CH4_burn_emissions_factor / CH4_carbon_fraction + PM10_burn_emissions_factor / PM10_carbon_fraction + PM2.5_burn_emissions_factor / PM2.5_carbon_fraction + VOC_burn_emissions_factor / VOC_carbon_fraction
  
  # Allocate emissions to CO2
  wildfire_emissions_profile[, CO2_tonnes := CO2_tonnes + cbrec.dt[,sum(((CWD_CombustionFrac * fire_exposed_CWD_mass_year_i) + (FWD_CombustionFrac * fire_exposed_FWD_mass_year_i) + (Foliage_CombustionFrac * fire_exposed_Foliage_mass_year_i)) * (Carbon_frac - non_CO2_combusted_carbon_frac))] * cell_to_acres * CO2_carbon_fraction]

  # And address unburned residue
  cbrec.dt[,':='(prev_fired_CWD_mass = prev_fired_CWD_mass + (CWD_UnburnedFrac * fire_exposed_CWD_mass_year_i),
                 prev_fired_FWD_mass = prev_fired_FWD_mass + (FWD_UnburnedFrac * fire_exposed_FWD_mass_year_i),
                 prev_fired_Foliage_mass = prev_fired_Foliage_mass + (Foliage_UnburnedFrac * fire_exposed_Foliage_mass_year_i)
          )]
  
  return(list(wildfire_emissions_profile,cbrec.dt))
}

duff_annual_wildfire_fun <- function(cbrec.dt, year.i) {
  ##############################################################################################################
  ##############################################################################################################
  # MICAH IS STILL WORKING ON WILDFIRE - WE MAY NEED TO ADJUST VARIABLE NAMES, OR POSSIBLY SOME MINOR DETAILS
  # OF THE APPROACH. SOME COLUMNS WERE CREATED BEFORE THE LOOP, BUT THE WILDFIRE SEGMENT STARTS HERE.
  ##############################################################################################################
  ##############################################################################################################  
  wildfire_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, N2O_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SOx_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  CH4_burn_emissions_factor <- 0.00382
  CO_burn_emissions_factor <- 0.0718
  N2O_burn_emissions_factor <- 0.00357 # Not in Micah's code
  NOx_burn_emissions_factor <- 0.00242
  PM10_burn_emissions_factor <- 0.0085904
  PM2.5_burn_emissions_factor <- 0.00728
  SOx_burn_emissions_factor <- 0.00098
  VOC_burn_emissions_factor <- 0.017342
  
  # Duff will burn in the same manner as the other size classes, but we calculate it in a separate function so it does
  # not get triple-counted with the other residue segments, and I can keep the functions general.
  cbrec.dt[,fire_exposed_Duff_mass_year_i := Duff_tonsAcre * MassExposedToWildfire]
  
  # Apply the mass lost to the dynamic mass column. 
  cbrec.dt[,Duff_tonsAcre := Duff_tonsAcre - fire_exposed_Duff_mass_year_i]
  
  # Apply the mass from the fire exposed to emissions, char, and unburned.
  wildfire_emissions_profile[,':='(CO_tonnes = CO_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * CO_burn_emissions_factor,
                                        N2O_tonnes = N2O_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * N2O_burn_emissions_factor,
                                        CH4_tonnes = CH4_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * CH4_burn_emissions_factor,
                                        NOx_tonnes = NOx_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * NOx_burn_emissions_factor,
                                        PMUnder10um_tonnes = PMUnder10um_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * PM10_burn_emissions_factor,
                                        PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * PM2.5_burn_emissions_factor,
                                        SOx_tonnes = SOx_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * SOx_burn_emissions_factor,
                                        VOC_tonnes = VOC_tonnes + cbrec.dt[,sum(Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres * VOC_burn_emissions_factor,
                                        char_tonnes = char_tonnes + cbrec.dt[,sum(Foliage_CharFrac * fire_exposed_Duff_mass_year_i)] * cell_to_acres)]
  
  # Any carbon not emitted as CH4, CO, PM10, PM2.5, or VOC is emitted as CO2. So we need to determine how much of the carbon was emitted in those constituents, 
  # take the remainder emitted as CO2 and convert to mass CO2 emissions. Carbon fraction variables are species mass / carbon mass
  
  # Calculate the fraction of carbon emitted in non-CO2 form
  non_CO2_combusted_carbon_frac <- CO_burn_emissions_factor / CO_carbon_fraction + CH4_burn_emissions_factor / CH4_carbon_fraction + PM10_burn_emissions_factor / PM10_carbon_fraction + PM2.5_burn_emissions_factor / PM2.5_carbon_fraction + VOC_burn_emissions_factor / VOC_carbon_fraction
  
  # Allocate emissions to CO2
  wildfire_emissions_profile[, CO2_tonnes := CO2_tonnes + cbrec.dt[,sum((Foliage_CombustionFrac * fire_exposed_Duff_mass_year_i) * (Carbon_frac - non_CO2_combusted_carbon_frac))] * cell_to_acres * CO2_carbon_fraction]
  
  # And the unburned
  cbrec.dt[,prev_fired_Duff_mass := prev_fired_Duff_mass + (Foliage_UnburnedFrac * fire_exposed_Duff_mass_year_i)]
  
  return(list(wildfire_emissions_profile,cbrec.dt))
}
