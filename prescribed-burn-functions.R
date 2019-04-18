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

prescribed_burn_fun <- function(cbrec.dt, residue.segment, burn.type, year.i) {
  # The prescribed burn will affect different residue segments depending on the burn type.
  if(burn.type=="Pile Burn") {
    # Burn type will affect residue segment affected AND emissions factors used.
    # Dummy emissions factors; units are (mass emissions species) / (mass biomass consumed)
    CH4_burn_emissions_factor <- 0.00382
    CO_burn_emissions_factor <- 0.0718
    N2O_burn_emissions_factor <- 0.00357 # Not in Micah's code
    NOx_burn_emissions_factor <- 0.00242
    PM10_burn_emissions_factor <- 0.0085904
    PM2.5_burn_emissions_factor <- 0.00728
    SOx_burn_emissions_factor <- 0.00098
    VOC_burn_emissions_factor <- 0.017342
    
    # CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; so we will need carbon fractions as well.
    # These are kept in misc-functions.R
    
    ######################################################################################################################################################
    ######################################################################################################################################################
    # Currently, we are assuming that 100% of appropriate residues are exposed to prescribed burns. Not all will burn to completion, but all is exposed.
    # Also, we assume the combustion/char/unburned fractions for pile burns are fixed (and can be hard-coded) at 90% / 1% / 9%.
    # THIS MAY CHANGE FOR BROADCAST BURN
    ######################################################################################################################################################
    ######################################################################################################################################################
    prescribed.burn.combustion.frac <- 0.9
    prescribed.burn.char.frac <- 0.01
    
    total_segment_residues <- cbrec.dt[,sum(get(paste(residue.segment,"CWD_tonsAcre",sep="_")),get(paste(residue.segment,"FWD_tonsAcre",sep="_")),get(paste(residue.segment,"Foliage_tonsAcre",sep="_")))] * cell_to_acres
    total_segment_carbon <- cbrec.dt[,sum(Carbon_frac * get(paste(residue.segment,"CWD_tonsAcre",sep="_")), Carbon_frac * get(paste(residue.segment,"FWD_tonsAcre",sep="_")), Carbon_frac * get(paste(residue.segment,"Foliage_tonsAcre",sep="_")))] * cell_to_acres
    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    century_emissions_profile[year.i,':='(CO_tonnes = CO_tonnes + prescribed.burn.combustion.frac * total_segment_residues * CO_burn_emissions_factor,
                                          N2O_tonnes = N2O_tonnes + prescribed.burn.combustion.frac * total_segment_residues * N2O_burn_emissions_factor,
                                          CH4_tonnes = CH4_tonnes + prescribed.burn.combustion.frac * total_segment_residues * CH4_burn_emissions_factor,
                                          NOx_tonnes = NOx_tonnes + prescribed.burn.combustion.frac * total_segment_residues * NOx_burn_emissions_factor,
                                          PMUnder10um_tonnes = PMUnder10um_tonnes + prescribed.burn.combustion.frac * total_segment_residues * PM10_burn_emissions_factor,
                                          PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + prescribed.burn.combustion.frac * total_segment_residues * PM2.5_burn_emissions_factor,
                                          SOx_tonnes = SOx_tonnes + prescribed.burn.combustion.frac * total_segment_residues * SOx_burn_emissions_factor,
                                          VOC_tonnes = VOC_tonnes + prescribed.burn.combustion.frac * total_segment_residues * VOC_burn_emissions_factor,
                                          char_tonnes = char_tonnes + prescribed.burn.char.frac * total_segment_residues)]
    
    # Any carbon nbot emitted as CH4, CO, PM10, PM2.5, or VOC is emitted as CO2. So we need to determine how much of the carbon was emitted in those constituents, 
    # take the remainder emitted as CO2 and convert to mass CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non_CO2_carbon <- (prescribed.burn.combustion.frac * total_segment_residues * CO_burn_emissions_factor / CO_carbon_fraction +
                       prescribed.burn.combustion.frac * total_segment_residues * CH4_burn_emissions_factor / CH4_carbon_fraction +
                       prescribed.burn.combustion.frac * total_segment_residues * PMUnder10um_burn_emissions_factor / PM10_carbon_fraction +
                       prescribed.burn.combustion.frac * total_segment_residues * PMUnder2.5um_burn_emissions_factor / PM2.5_carbon_fraction +
                       prescribed.burn.combustion.frac * total_segment_residues * VOC_burn_emissions_factor / VOC_carbon_fraction)
    
    century_emissions_profile[year.i, CO2_tonnes = CO2_tonnes + (total_segment_carbon * prescribed.burn.combustion.frac - non_CO2_carbon) * CO2_carbon_fraction]
    
    # Apply the mass lost to the dynamic mass columns. 
    cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac) * get(paste(residue.segment,"CWD_tonsAcre",sep="_"))]
    cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac) * get(paste(residue.segment,"FWD_tonsAcre",sep="_"))]
    cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac) * get(paste(residue.segment,"Foliage_tonsAcre",sep="_"))]
  }
  
  if(burn.type=="Broadcast Burn") {
    
    # Burn type will affect residue segment affected AND emissions factors used.
    # Dummy emissions factors; units are (mass emissions species) / (mass biomass consumed)
    CH4_burn_emissions_factor <- 0.00382
    CO_burn_emissions_factor <- 0.0718
    N2O_burn_emissions_factor <- 0.00357 # Not in Micah's code
    NOx_burn_emissions_factor <- 0.00242
    PM10_burn_emissions_factor <- 0.0085904
    PM2.5_burn_emissions_factor <- 0.00728
    SOx_burn_emissions_factor <- 0.00098
    VOC_burn_emissions_factor <- 0.017342
    
    # CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; so we will need carbon fractions as well.
    # These are kept in misc-functions.R
    
    total_combusted_residues <- cbrec.dt[,sum(get(paste(residue.segment,"CWD_tonsAcre",sep="_")) * CWD_CombustionFrac,
                                              get(paste(residue.segment,"FWD_tonsAcre",sep="_")) * FWD_CombustionFrac,
                                              get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) * Foliage_CombustionFrac)] * cell_to_acres
    total_charred_residues <- cbrec.dt[,sum(get(paste(residue.segment,"CWD_tonsAcre",sep="_")) * CWD_CharFrac,
                                            get(paste(residue.segment,"FWD_tonsAcre",sep="_")) * FWD_CharFrac,
                                            get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) * Foliage_CharFrac)] * cell_to_acres
    total_combusted_carbon <- cbrec.dt[,sum(get(paste(residue.segment,"CWD_tonsAcre",sep="_")) * CWD_CombustionFrac * Carbon_frac,
                                            get(paste(residue.segment,"FWD_tonsAcre",sep="_")) * FWD_CombustionFrac * Carbon_frac,
                                            get(paste(residue.segment,"Foliage_tonsAcre",sep="_")) * Foliage_CombustionFrac * Carbon_frac)] * cell_to_acres
    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    century_emissions_profile[year.i,':='(CO_tonnes = CO_tonnes + total_combusted_residues * CO_burn_emissions_factor,
                                          N2O_tonnes = N2O_tonnes + total_combusted_residues * N2O_burn_emissions_factor,
                                          CH4_tonnes = CH4_tonnes + total_combusted_residues * CH4_burn_emissions_factor,
                                          NOx_tonnes = NOx_tonnes + total_combusted_residues * NOx_burn_emissions_factor,
                                          PMUnder10um_tonnes = PMUnder10um_tonnes + total_combusted_residues * PM10_burn_emissions_factor,
                                          PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + total_combusted_residues * PM2.5_burn_emissions_factor,
                                          SOx_tonnes = SOx_tonnes + total_combusted_residues * SOx_burn_emissions_factor,
                                          VOC_tonnes = VOC_tonnes + total_combusted_residues * VOC_burn_emissions_factor,
                                          char_tonnes = char_tonnes + total_charred_residues)]
    
    # Any carbon nbot emitted as CH4, CO, PM10, PM2.5, or VOC is emitted as CO2. So we need to determine how much of the carbon was emitted in those constituents, 
    # take the remainder emitted as CO2 and convert to mass CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non_CO2_combusted_carbon <- (total_combusted_residues * CO_burn_emissions_factor / CO_carbon_fraction +
                                 total_combusted_residues * CH4_burn_emissions_factor / CH4_carbon_fraction +
                                 total_combusted_residues * PM10_burn_emissions_factor / PM10_carbon_fraction +
                                 total_combusted_residues * PM2.5_burn_emissions_factor / PM2.5_carbon_fraction +
                                 total_combusted_residues * VOC_burn_emissions_factor / VOC_carbon_fraction)
    
    century_emissions_profile[year.i, CO2_tonnes = CO2_tonnes + (total_combusted_carbon - non_CO2_carbon) * CO2_carbon_fraction]
    
    # Apply the mass lost to the dynamic mass columns. 
    cbrec.dt[,(paste(residue.segment,"CWD_tonsAcre",sep="_")) := (1 - CWD_CombustionFrac - CWD_CharFrac) * get(paste(residue.segment,"CWD_tonsAcre",sep="_"))]
    cbrec.dt[,(paste(residue.segment,"FWD_tonsAcre",sep="_")) := (1 - FWD_CombustionFrac - FWD_CharFrac) * get(paste(residue.segment,"FWD_tonsAcre",sep="_"))]
    cbrec.dt[,(paste(residue.segment,"Foliage_tonsAcre",sep="_")) := (1 - Foliage_CombustionFrac - Foliage_CharFrac) * get(paste(residue.segment,"Foliage_tonsAcre",sep="_"))]
  }
}