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

# FUNCTION: Prescribed Burn Processing Function
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will collect and calculate needed data from the wildfire model outputs for year 1 prescribed burning
# -------------------------------------------------------------------------------------------------
# INPUTS:
# wildfire.data.directory: The directory storing all of the wildfire data
# scenario.ID: The scenario matrix identifier string to select the correct wildfire data files
# tile.list: The wildfire tiles contained within the study area
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# wildfire.data: combined combustion fraction, char fraction, and emissions factor data from prescribed burns in 
# year 1.
# =================================================================================================

prescribed_burn_processing_fun <- function(cbrec.dt,wildfire.data.directory,scenario.ID, tile.list) {
  
  burn.emissions.directory <- paste(wildfire.data.directory,"burn_emissions/",sep="")
  
  prescribed.burn.data <- data.table()
  
  # Cycle through tile_list, load the .rds files, and combine into a single data table.
  # This is a prescribed burn, so we use the "first" file for year 0.
  for (tile in tile.list) {
    # We'll need to load up fire data and trim unneeded columns. 
    prescribed.burn.filename <- list.files(burn.emissions.directory)[str_detect(list.files(burn.emissions.directory),paste(scenario.ID,tile,"0.rds",sep="-"))]
    new.tile <- readRDS(paste(burn.emissions.directory,prescribed.burn.filename,sep=""))
    # new.tile[,':='(fuelbed_number=NULL, FCID2018=NULL, ID=NULL, Silvicultural_Treatment=NULL, Fraction_Piled=NULL, Fraction_Scattered=NULL, Secondary_Burn=NULL, Burn_Type=NULL, Biomass_Collection=NULL, Pulp_Market=NULL, Year=NULL, total_except_pile_char=NULL, total_except_pile_CH4=NULL, total_except_pile_CO=NULL, total_except_pile_CO2=NULL, total_except_pile_NOx=NULL, total_except_pile_PM10=NULL, total_except_pile_PM2.5=NULL, total_except_pile_SO2=NULL, total_except_pile_VOC=NULL, total_pile_clean_PM10=NULL, total_pile_vdirty_PM10=NULL, total_pile_clean_PM2.5=NULL, total_pile_vdirty_PM2.5=NULL, total_pile_CH4=NULL, total_pile_CO=NULL, total_pile_CO2=NULL, total_pile_NOx=NULL, total_pile_SO2=NULL, total_pile_VOC=NULL, pile_char=NULL)]
    prescribed.burn.data <- rbind(prescribed.burn.data,new.tile)
    
    new.tile <- NULL
  }
  
  # From the fire data, we need to calculate: emissions factors, combustion fractions, char fractions, and unburned fractions.All of these values
  # will have the residue_exposed as the denominator, so we need to be sure that we don't have a div/0 situation. So, when the value of a residue_exposed
  # column is 0, add a very small number (let's say 1e-6) to residue_exposed.
  prescribed.burn.data[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + 1e-6]
  prescribed.burn.data[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + 1e-6]
  prescribed.burn.data[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + 1e-6]
  prescribed.burn.data[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + 1e-6]
  
  prescribed.burn.data[,':='(
    # Combustion and char fractions; foliage and duff do not char
    CWD_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    # CO
    Duff_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    # NOx
    Duff_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    # PM10
    Duff_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    # PM2.5
    Duff_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    # SO2
    Duff_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    # VOC
    Duff_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed
  )]
  
  # Trim the now-un-needed columns
  prescribed.burn.data[,':='(char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_fuel_consumed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  # Round x and y coordinates for prescribed burn variables
  prescribed.burn.data[,":="(x=round(x,digits=1),y=round(y,digits=1))] # Round the X and Y coordinates so we can merge with cbrec.dt
  
  # Merge with cbrec.dt
  setkey(cbrec.dt,x,y)
  setkey(prescribed.burn.data,x,y)
  cbrec.dt <- prescribed.burn.data[cbrec.dt]
  
  # There will be cells where we have UW residue data but not fire data. This is likely due to a discrepancy between UW data and FCCS data for fire emissions, where UW says there are trees and FCCS disagrees. For fire modeling, 
  # we side with FCCS. This is generally a smaller number of cells, for tile 118, it was half of one percent. We have elected to remove these wildfire-less points from cbrec.dt, essentially removing them from the study.
  cbrec.dt <- cbrec.dt[!is.na(CWD_CombustionFrac)]
  
  return(cbrec.dt)
  
}

# Prescribed burning emissions & mass reduction
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate mass loss and emissions along all residue size classes due to prescribed
# burning. Unburnt mass remains in the base category, because it may be exposed to wildfire.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# residue.disposition: Residue disposition; scattered, field piled or landing piled
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# =================================================================================================

prescribed_burn_fun <- function(cbrec.dt, burn.type) {
  # Create an emissions output object
  prescribed_burn_emissions <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  # The prescribed burn will affect different residue segments depending on the burn type.
  if(burn.type=="Pile") {
    # Burn type will affect residue segment affected AND emissions factors used. CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; 
    # so we will need carbon fractions as well. These are kept in misc-functions.R
    
    ######################################################################################################################################################
    ######################################################################################################################################################
    # Currently, we are assuming that 100% of piled residues are exposed to prescribed burns. Not all will burn to completion, but all is exposed.
    # Also, we assume the combustion/char/unburned fractions for pile burns are fixed (and can be hard-coded) at 90% / .1% / 9.9%.
    ######################################################################################################################################################
    ######################################################################################################################################################
    prescribed.burn.combustion.frac <- 0.9
    prescribed.burn.char.frac <- 0.001
    
    total.segment.carbon <- cbrec.dt[,sum(Carbon_frac * Piled_CWD_tonnesAcre, Carbon_frac * Piled_FWD_tonnesAcre, Carbon_frac * Piled_Foliage_tonnesAcre)] * cell_to_acres
    total.unburned.carbon <- total.segment.carbon * (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac)
    
    # Calculate some year.i emissions; we'll use this several times
    CO.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_CO_EmFac + Piled_FWD_tonnesAcre * FWD_CO_EmFac + Piled_Foliage_tonnesAcre * Foliage_CO_EmFac))] * cell_to_acres
    CH4.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_CH4_EmFac + Piled_FWD_tonnesAcre * FWD_CH4_EmFac + Piled_Foliage_tonnesAcre * Foliage_CH4_EmFac))] * cell_to_acres
    NOx.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_NOx_EmFac + Piled_FWD_tonnesAcre * FWD_NOx_EmFac + Piled_Foliage_tonnesAcre * Foliage_NOx_EmFac))] * cell_to_acres
    PM10.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_PM10_EmFac + Piled_FWD_tonnesAcre * FWD_PM10_EmFac + Piled_Foliage_tonnesAcre * Foliage_PM10_EmFac))] * cell_to_acres
    PM2.5.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_PM2.5_EmFac + Piled_FWD_tonnesAcre * FWD_PM2.5_EmFac + Piled_Foliage_tonnesAcre * Foliage_PM2.5_EmFac))] * cell_to_acres
    SO2.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_SO2_EmFac + Piled_FWD_tonnesAcre * FWD_SO2_EmFac + Piled_Foliage_tonnesAcre * Foliage_SO2_EmFac))] * cell_to_acres
    VOC.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre * CWD_VOC_EmFac + Piled_FWD_tonnesAcre * FWD_VOC_EmFac + Piled_Foliage_tonnesAcre * Foliage_VOC_EmFac))] * cell_to_acres
    char.year.i <- cbrec.dt[,sum(prescribed.burn.char.frac * (Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre))] * cell_to_acres # no foliage char
    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    prescribed_burn_emissions[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                    CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                    NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                    PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                    PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                    SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                    VOC_tonnes = VOC_tonnes + VOC.emissions.year.i,
                                    char_tonnes = char_tonnes + char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- (CO.emissions.year.i / CO_carbon_fraction +
                                 CH4.emissions.year.i / CH4_carbon_fraction +
                                 PM10.emissions.year.i / PM10_carbon_fraction + 
                                 VOC.emissions.year.i / VOC_carbon_fraction)
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    total.combusted.carbon <- total.segment.carbon - total.unburned.carbon - char.carbon
    
    prescribed_burn_emissions[,CO2_tonnes := CO2_tonnes + (total.combusted.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # Apply the mass lost to the dynamic mass columns. 
    cbrec.dt[,':='(Piled_CWD_tonnesAcre = (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac) * Piled_CWD_tonnesAcre,
                   Piled_FWD_tonnesAcre = (1 - prescribed.burn.combustion.frac - prescribed.burn.char.frac) * Piled_FWD_tonnesAcre,
                   Piled_Foliage_tonnesAcre = (1 - prescribed.burn.combustion.frac) * Piled_Foliage_tonnesAcre)]
  }
  
  if(burn.type=="Broadcast") { # Broadcast burn affects both piled and scattered residues
    
    # Burn type will affect residue segment affected AND emissions factors
    # CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; so we will need carbon fractions as well.
    # These are kept in misc-functions.R
    
    total.segment.carbon <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * Carbon_frac,
                                          Scattered_FWD_tonnesAcre * Carbon_frac,
                                          Scattered_Foliage_tonnesAcre * Carbon_frac)] * cell_to_acres

    total.unburned.carbon <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * Carbon_frac * (1 - CWD_CombustionFrac - CWD_CharFrac),
                                           Scattered_FWD_tonnesAcre * Carbon_frac * (1 - FWD_CombustionFrac - FWD_CharFrac),
                                           Scattered_Foliage_tonnesAcre * Carbon_frac * (1 - Foliage_CombustionFrac))] * cell_to_acres
    

    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    CO.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_CO_EmFac + Scattered_FWD_tonnesAcre * FWD_CO_EmFac + Scattered_Foliage_tonnesAcre * Foliage_CO_EmFac)] * cell_to_acres
    CH4.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_CH4_EmFac + Scattered_FWD_tonnesAcre * FWD_CH4_EmFac + Scattered_Foliage_tonnesAcre * Foliage_CH4_EmFac)] * cell_to_acres
    NOx.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_NOx_EmFac + Scattered_FWD_tonnesAcre * FWD_NOx_EmFac + Scattered_Foliage_tonnesAcre * Foliage_NOx_EmFac)] * cell_to_acres
    PM10.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_PM10_EmFac + Scattered_FWD_tonnesAcre * FWD_PM10_EmFac + Scattered_Foliage_tonnesAcre * Foliage_PM10_EmFac)] * cell_to_acres
    PM2.5.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_PM2.5_EmFac + Scattered_FWD_tonnesAcre * FWD_PM2.5_EmFac + Scattered_Foliage_tonnesAcre * Foliage_PM2.5_EmFac)] * cell_to_acres
    SO2.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_SO2_EmFac + Scattered_FWD_tonnesAcre * FWD_SO2_EmFac + Scattered_Foliage_tonnesAcre * Foliage_SO2_EmFac)] * cell_to_acres
    VOC.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_VOC_EmFac + Scattered_FWD_tonnesAcre * FWD_VOC_EmFac + Scattered_Foliage_tonnesAcre * Foliage_VOC_EmFac)] * cell_to_acres
    char.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_CharFrac + Scattered_FWD_tonnesAcre * FWD_CharFrac)] * cell_to_acres # No foliage char
    
    prescribed_burn_emissions[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                    CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                    NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                    PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                    PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                    SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                    VOC_tonnes = VOC_tonnes + VOC.emissions.year.i,
                                    char_tonnes = char_tonnes + char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- (CO.emissions.year.i / CO_carbon_fraction +
                                 CH4.emissions.year.i / CH4_carbon_fraction +
                                 PM10.emissions.year.i / PM10_carbon_fraction +
                                 VOC.emissions.year.i / VOC_carbon_fraction)
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    prescribed_burn_emissions[,CO2_tonnes := CO2_tonnes + (total.segment.carbon - total.unburned.carbon - char.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # Apply the mass lost to the dynamic mass columns. 
    cbrec.dt[,':='(Scattered_CWD_tonnesAcre = (1 - CWD_CombustionFrac - CWD_CharFrac) * Scattered_CWD_tonnesAcre,
                   Scattered_FWD_tonnesAcre = (1 - FWD_CombustionFrac - FWD_CharFrac) * Scattered_FWD_tonnesAcre,
                   Scattered_Foliage_tonnesAcre = (1 - Foliage_CombustionFrac) * Scattered_Foliage_tonnesAcre,
                   Piled_CWD_tonnesAcre = (1 - CWD_CombustionFrac - CWD_CharFrac) * Piled_CWD_tonnesAcre,
                   Piled_FWD_tonnesAcre = (1 - FWD_CombustionFrac - FWD_CharFrac) * Piled_FWD_tonnesAcre,
                   Piled_Foliage_tonnesAcre = (1 - Foliage_CombustionFrac) * Piled_Foliage_tonnesAcre
              )]
  }
  
  return(list(prescribed_burn_emissions,cbrec.dt))
}

########################################################################################################
########################################################################################################
# This code was used to debug and compare results with a verification spreadsheet; it may have use yet.
########################################################################################################
########################################################################################################

# cbrec.dt[,':='(total.pile.combusted.carbon = prescribed.burn.combustion.frac * (Carbon_frac * Piled_CWD_tonnesAcre + Carbon_frac * Piled_FWD_tonnesAcre + Carbon_frac * Piled_Foliage_tonnesAcre),
#                total.broad.combusted.carbon = (Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_CombustionFrac * Carbon_frac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_CombustionFrac * Carbon_frac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_CombustionFrac * Carbon_frac)]
# cbrec.dt[,total.combusted.carbon := total.pile.combusted.carbon + total.broad.combusted.carbon]
# 
# cbrec.dt[,':='(CO.emissions.pile  = (Piled_CWD_tonnesAcre * CWD_CO_EmFac + Piled_FWD_tonnesAcre * FWD_CO_EmFac + Piled_Foliage_tonnesAcre * Foliage_CO_EmFac),
#                CH4.emissions.pile = (Piled_CWD_tonnesAcre * CWD_CH4_EmFac + Piled_FWD_tonnesAcre * FWD_CH4_EmFac + Piled_Foliage_tonnesAcre * Foliage_CH4_EmFac),
#                NOx.emissions.pile = (Piled_CWD_tonnesAcre * CWD_NOx_EmFac + Piled_FWD_tonnesAcre * FWD_NOx_EmFac + Piled_Foliage_tonnesAcre * Foliage_NOx_EmFac),
#                PM10.emissions.pile= (Piled_CWD_tonnesAcre * CWD_PM10_EmFac + Piled_FWD_tonnesAcre * FWD_PM10_EmFac + Piled_Foliage_tonnesAcre * Foliage_PM10_EmFac),
#                PM2.5.emissions.pile=(Piled_CWD_tonnesAcre * CWD_PM2.5_EmFac + Piled_FWD_tonnesAcre * FWD_PM2.5_EmFac + Piled_Foliage_tonnesAcre * Foliage_PM2.5_EmFac),
#                SO2.emissions.pile = (Piled_CWD_tonnesAcre * CWD_SO2_EmFac + Piled_FWD_tonnesAcre * FWD_SO2_EmFac + Piled_Foliage_tonnesAcre * Foliage_SO2_EmFac),
#                VOC.emissions.pile = (Piled_CWD_tonnesAcre * CWD_VOC_EmFac + Piled_FWD_tonnesAcre * FWD_VOC_EmFac + Piled_Foliage_tonnesAcre * Foliage_VOC_EmFac),
#                char.pile          = prescribed.burn.char.frac * (Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre),
#                CO.emissions.broad   = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_CO_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_CO_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_CO_EmFac),
#                CH4.emissions.broad  = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_CH4_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_CH4_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_CH4_EmFac),
#                NOx.emissions.broad  = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_NOx_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_NOx_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_NOx_EmFac),
#                PM10.emissions.broad = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_PM10_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_PM10_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_PM10_EmFac),
#                PM2.5.emissions.broad= ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_PM2.5_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_PM2.5_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_PM2.5_EmFac),
#                SO2.emissions.broad  = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_SO2_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_SO2_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_SO2_EmFac),
#                VOC.emissions.broad  = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_VOC_EmFac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_VOC_EmFac + (Scattered_Foliage_tonnesAcre + Recovered_Foliage_tonnesAcre) * Foliage_VOC_EmFac),
#                char.broad           = ((Scattered_CWD_tonnesAcre + Recovered_CWD_tonnesAcre) * CWD_CharFrac + (Scattered_FWD_tonnesAcre + Recovered_FWD_tonnesAcre) * FWD_CharFrac))]
# 
# # Calculate the non - CO2 combusted carbon
# cbrec.dt[,non.CO2.combusted.carbon.broad := CO.emissions.broad / CO_carbon_fraction + CH4.emissions.broad / CH4_carbon_fraction + PM10.emissions.broad / PM10_carbon_fraction + VOC.emissions.broad / VOC_carbon_fraction]
# cbrec.dt[,non.CO2.combusted.carbon.pile := CO.emissions.pile / CO_carbon_fraction + CH4.emissions.pile / CH4_carbon_fraction + PM10.emissions.pile / PM10_carbon_fraction + VOC.emissions.pile / VOC_carbon_fraction]
# 
# # Calulate CO2 emissions
# cbrec.dt[,CO2.emissions.pile := (total.pile.combusted.carbon - non.CO2.combusted.carbon.pile) * CO2_carbon_fraction]
# cbrec.dt[,CO2.emissions.broad := (total.broad.combusted.carbon - non.CO2.combusted.carbon.broad) * CO2_carbon_fraction]
# 
# cbrec.dt[,':='(total.CO2.emissions = CO2.emissions.pile + CO2.emissions.broad,
#                total.CO.emissions = CO.emissions.pile + CO.emissions.broad,
#                total.CH4.emissions = CH4.emissions.pile + CH4.emissions.broad,
#                total.NOx.emissions = NOx.emissions.pile + NOx.emissions.broad,
#                total.PM10.emissions = PM10.emissions.pile + PM10.emissions.broad,
#                total.PM2.5.emissions = PM2.5.emissions.pile + PM2.5.emissions.broad,
#                total.SO2.emissions = SO2.emissions.pile + SO2.emissions.broad,
#                total.VOC.emissions = VOC.emissions.pile + VOC.emissions.broad)]