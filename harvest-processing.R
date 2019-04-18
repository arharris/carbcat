# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2019-04-02: Deleted old, unworkable code, developed initialized Script
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
#
#
# ---------------------------------
# OUTPUTS:
#
# =============================================================================

# Source files


# Required Libraries
# Calling line: 
# harvest_processing_fun(study_area_FCID, high_volume_cell_threshold_density)

harvest_processing_fun <- function(cbrec.dt,density.threshold,processing.mass.loss) {
  # The cell density threshold is applied to the entire study area; if the mean recovered cell density is less than the density threshold, we've got a low volume harvest.
  mean.study.area.density <- sum(cbrec.dt[,mean(Recovered_CWD_tonsAcre)],cbrec.dt[,mean(Recovered_FWD_tonsAcre)],cbrec.dt[,mean(Recovered_Foliage_tonsAcre)])
  
  # If the total residue volume is less than 1000 BDT, we've got a small harvest.
  total.recovered.residue <- cbrec.dt[,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)]
  
  # Initialize harvest processing emissions; this will regurgitate one number to add to year 1 emissions.
  harvest_processing_emissions <- data.table(CO2_kg=0, CO_kg=0, N2O_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SOx_kg=0, VOC_kg=0)
  
  # Create a column for recovered residues taht will make it to the power plant. Currently assuming that after comminution, size class is irrelevant.
  cbrec.dt[,mass_to_plant_tonsAcre:=0]
  
  ##########################
  # Harvest type decision
  ##########################
  if(mean.study.area.density>density.threshold & total.recovered.residue > 1000) { ##### HIGH VOLUME HARVEST #####
    # After the high/low volume determination, options will vary based upon: Harvest type (whole tree or cut-to-length), Slope, Treatment Type, and harvest.comminution.opt
    # Processing equipment for cut-to-length harvests depend on the treatment type.
    if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.2' | Equipment_code=='L.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='C.1' | Equipment_code=='L.1',]
      ctl_low_slope_grind <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3'| Equipment_code=='G.2',]
      ctl_low_slope_chip <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3'| Equipment_code=='C.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.2' | Equipment_code=='L.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='C.1' | Equipment_code=='L.1',]
      ctl_low_slope_grind <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20'| Equipment_code=='G.2',]
      ctl_low_slope_chip <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20'| Equipment_code=='C.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.2' | Equipment_code=='L.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='C.1' | Equipment_code=='L.1',]
      ctl_low_slope_grind <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40'| Equipment_code=='G.2',]
      ctl_low_slope_chip <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40'| Equipment_code=='C.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.2' | Equipment_code=='L.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='C.1' | Equipment_code=='L.1',]
      ctl_low_slope_grind <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60'| Equipment_code=='G.2',]
      ctl_low_slope_chip <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60'| Equipment_code=='C.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.2' | Equipment_code=='L.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='C.1' | Equipment_code=='L.1',]
      ctl_low_slope_grind <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80'| Equipment_code=='G.2',]
      ctl_low_slope_chip <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80'| Equipment_code=='C.1',]
    }
    
    # Whole tree harvests do not vary based upon treatment type.
    wt_high_slope_grind <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]
    wt_high_slope_chip <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',]
    wt_low_slope_grind <- equipment_emissions[Equipment_code=='G.2',]
    wt_low_slope_chip <- equipment_emissions[Equipment_code=='C.1',]
    
    # Transportation equipment depends on slope and residue moisture
    if(user_inputs[variable=='residue_moisture',value]=='Dry') {
      trans_under_10_slope <- equipment_emissions[Equipment_code=='H.5',]
      trans_10_to_35_slope <- equipment_emissions[Equipment_code=='H.4',]
      trans_over_35_slope <- equipment_emissions[Equipment_code=='H.10',]
    } else if(user_inputs[variable=='residue_moisture',value]=='Green') {
      trans_under_10_slope <- equipment_emissions[Equipment_code=='H.5',]
      trans_10_to_35_slope <- equipment_emissions[Equipment_code=='H.1',]
      trans_over_35_slope <- equipment_emissions[Equipment_code=='H.3',]
    }
    
    if(user_inputs[variable=='harvest_comminution_opt',value]=='chip') {
      ###################################
      # harvest.comminution.opt decision
      ###################################
      # Processing emissions - Chipping
      harvest_processing_emissions[,':='(
        CO2_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CO2_kg)],
        
        CO_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CO_kg)], 
        
        N2O_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(N2O_kg)], 
        
        CH4_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CH4_kg)], 
        
        NOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(NOx_kg)], 
        
        PMUnder10um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(PMUnder10um_kg)], 
        
        PMUnder2.5um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(SOx_kg)], 
        
        VOC_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(VOC_kg)]
      )]
    }
    # Processing emissions - Grinding
    if(user_inputs[variable=='harvest_comminution_opt',value]=='grind') {
      harvest_processing_emissions[,':='(
        CO2_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CO2_kg)],
        
        CO_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CO_kg)], 
        
        N2O_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(N2O_kg)], 
        
        CH4_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CH4_kg)], 
        
        NOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(NOx_kg)], 
        
        PMUnder10um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(PMUnder10um_kg)], 
        
        PMUnder2.5um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(SOx_kg)], 
        
        VOC_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(VOC_kg)]
      )]
    }
    
    # Processing emissions - Chip & Grind. Chips CWD, grinds the rest.
    if(user_inputs[variable=='harvest_comminution_opt',value]=='chipandgrind') {
      harvest_processing_emissions[,':='(
        CO2_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO2_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CO2_kg)],
        
        CO_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO_kg)] +
                cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CO_kg)],
        
        N2O_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(N2O_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(N2O_kg)],
        
        CH4_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CH4_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(CH4_kg)],
        
        NOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(NOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(NOx_kg)], 
        
        PMUnder10um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder10um_kg)] +
                         cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(PMUnder10um_kg)], 
        
        PMUnder2.5um_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder2.5um_kg)] +
                          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(SOx_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(SOx_kg)], 
        
        VOC_kg = cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_low_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_low_slope_chip[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_low_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(VOC_kg)] +
                 cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_low_slope_grind[,sum(VOC_kg)]
      )]
    }
      
    # Transportation Emissions - incorporate distance, as emissions are in tons * km
    harvest_processing_emissions[,':='(
      CO2_kg = CO2_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CO2_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CO2_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CO2_kg)]],
      
      CO_kg = CO_kg +
              cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CO_kg)]] +
              cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CO_kg)]] +
              cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CO_kg)]], 
      
      N2O_kg = N2O_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(N2O_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(N2O_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(N2O_kg)]],
      
      CH4_kg = CH4_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CH4_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CH4_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CH4_kg)]],
      
      NOx_kg = NOx_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(NOx_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(NOx_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(NOx_kg)]],
      
      PMUnder10um_kg = PMUnder10um_kg +
                       cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(PMUnder10um_kg)]] +
                       cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(PMUnder10um_kg)]] +
                       cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(PMUnder10um_kg)]], 
      
      PMUnder2.5um_kg = PMUnder2.5um_kg +
                        cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(PMUnder2.5um_kg)]] +
                        cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(PMUnder2.5um_kg)]] +
                        cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(PMUnder2.5um_kg)]],
      
      SOx_kg = SOx_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(SOx_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(SOx_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(SOx_kg)]],
      
      VOC_kg = VOC_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(VOC_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(VOC_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(VOC_kg)]]
    )]
    
    # Processing mass loss and mass transfer to power plant
    # In the high volume scenario, the only residues that are NOT processed and NOT sent to the power plant are those with the merge column "Do_Not_Harvest"
    # Step 1, populate the mass_to_plant_tonsAcre from the recovered columns.
    cbrec.dt[merge_column!="Do_Not_Harvest",mass_to_plant_tonsAcre := Recovered_CWD_tonsAcre * (1-processing.mass.loss) + Recovered_FWD_tonsAcre * (1-processing.mass.loss) + Recovered_Foliage_tonsAcre * (1-processing.mass.loss)]
    
    # Step 2, remove processed materials from the recovered materials. Leftover materials will be exposed to decay and wildfire.
    cbrec.dt[merge_column!="Do_Not_Harvest",':='(Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * processing.mass.loss,
                                                 Recovered_FWD_tonsAcre = Recovered_FWD_tonsAcre * processing.mass.loss,
                                                 Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * processing.mass.loss)]
      
  } else { ##### LOW VOLUME HARVEST #####
    # After the high/low volume determination, options will vary based upon: Harvest type (whole tree or cut-to-length), Slope, Treatment Type, and harvest.comminution.opt
    # Processing equipment for cut-to-length harvests depend on the treatment type. We need to break out CWD and FWD/Foliage.
    
    if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin
      ctl_CWD_chip <- equipment_emissions[Equipment_code=='SS.1.CTL' | Equipment_code=='T.3' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
      ctl_CWD_grind <- equipment_emissions[Equipment_code=='SS.1.CTL' | Equipment_code=='T.3' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
      ctl_FinesFoliage_chip <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='C.2',]
      ctl_FinesFoliage_grind <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='C.2' | Equipment_code=='L.1',]
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.1' | Equipment_code=='L.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing
      ctl_CWD_chip <- equipment_emissions[Equipment_code=='SS.1.CTL.20' | Equipment_code=='T.3.20' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
      ctl_CWD_grind <- equipment_emissions[Equipment_code=='SS.1.CTL.20' | Equipment_code=='T.3.20' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
      ctl_FinesFoliage_chip <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='C.2',]
      ctl_FinesFoliage_grind <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='C.2' | Equipment_code=='L.1',]
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.1' | Equipment_code=='L.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing
      ctl_CWD_chip <- equipment_emissions[Equipment_code=='SS.1.CTL.40' | Equipment_code=='T.3.40' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
      ctl_CWD_grind <- equipment_emissions[Equipment_code=='SS.1.CTL.40' | Equipment_code=='T.3.40' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
      ctl_FinesFoliage_chip <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='C.2',]
      ctl_FinesFoliage_grind <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='C.2' | Equipment_code=='L.1',]
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.1' | Equipment_code=='L.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing
      ctl_CWD_chip <- equipment_emissions[Equipment_code=='SS.1.CTL.60' | Equipment_code=='T.3.60' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
      ctl_CWD_grind <- equipment_emissions[Equipment_code=='SS.1.CTL.60' | Equipment_code=='T.3.60' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
      ctl_FinesFoliage_chip <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='C.2',]
      ctl_FinesFoliage_grind <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='C.2' | Equipment_code=='L.1',]
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.1' | Equipment_code=='L.1',]
    }
    if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing
      ctl_CWD_chip <- equipment_emissions[Equipment_code=='SS.1.CTL.80' | Equipment_code=='T.3.80' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
      ctl_CWD_grind <- equipment_emissions[Equipment_code=='SS.1.CTL.80' | Equipment_code=='T.3.80' | Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
      ctl_FinesFoliage_chip <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='C.2',]
      ctl_FinesFoliage_grind <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.1',]
      ctl_high_slope_chip <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='C.2' | Equipment_code=='L.1',]
      ctl_high_slope_grind <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.1' | Equipment_code=='L.1',]
    }
    
    # Whole tree harvests do not vary based upon treatment type.
    wt_CWD_chip <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='C.2',]
    wt_CWD_grind <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='P.1' | Equipment_code=='G.1',]
    wt_FinesFoliage_chip <- equipment_emissions[Equipment_code=='C.2',]
    wt_FinesFoliage_grind <- equipment_emissions[Equipment_code=='G.1',]
    wt_high_slope_chip <- equipment_emissions[Equipment_code=='C.2' | Equipment_code=='L.1',]
    wt_high_slope_grind <- equipment_emissions[Equipment_code=='G.1' | Equipment_code=='L.1',]
    
    if(user_inputs[variable=='residue_moisture',value]=='Dry') {
      trans_under_10_slope <- equipment_emissions[Equipment_code=='H.5',]
      trans_10_to_35_slope <- equipment_emissions[Equipment_code=='H.4',]
      trans_over_35_slope <- equipment_emissions[Equipment_code=='H.10',]
    } else if(user_inputs[variable=='residue_moisture',value]=='Green') {
      trans_under_10_slope <- equipment_emissions[Equipment_code=='H.5',]
      trans_10_to_35_slope <- equipment_emissions[Equipment_code=='H.1',]
      trans_over_35_slope <- equipment_emissions[Equipment_code=='H.3',]
    }
    
    # Harvest/Processing emissions - UPDATE FOR LOW VOLUME SCENARIO
    if(user_inputs[variable=='harvest_comminution_opt',value]=='chip') {
      ###################################
      # harvest.comminution.opt decision
      ###################################
      # Processing emissions - Chipping
      harvest_processing_emissions[,':='(
        CO2_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CO2_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(CO2_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO2_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CO2_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(CO2_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO2_kg)],
        
        CO_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CO_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(CO_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CO_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(CO_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO_kg)],
        
        N2O_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(N2O_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(N2O_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(N2O_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(N2O_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(N2O_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(N2O_kg)],
        
        CH4_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CH4_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(CH4_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CH4_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CH4_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(CH4_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CH4_kg)],
        
        NOx_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(NOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(NOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(NOx_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(NOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(NOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(NOx_kg)],
        
        PMUnder10um_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(PMUnder10um_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(PMUnder10um_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder10um_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(PMUnder10um_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(PMUnder10um_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder10um_kg)],
        
        PMUnder2.5um_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(PMUnder2.5um_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(PMUnder2.5um_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder2.5um_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(PMUnder2.5um_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(PMUnder2.5um_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(SOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(SOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(SOx_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(SOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(SOx_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(SOx_kg)],
        
        VOC_kg = # Cut to length
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(VOC_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_chip[,sum(VOC_kg)] +
              cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(VOC_kg)] +
              # Whole tree
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(VOC_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_chip[,sum(VOC_kg)] +
              cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(VOC_kg)]
      )]
    }
    # Processing emissions - Grinding
    if(user_inputs[variable=='harvest_comminution_opt',value]=='grind') {
      harvest_processing_emissions[,':='(
        CO2_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO2_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO2_kg)],
        
        CO_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO_kg)],
        
        N2O_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(N2O_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(N2O_kg)],
        
        CH4_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CH4_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CH4_kg)],
        
        NOx_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(NOx_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(NOx_kg)],
        
        PMUnder10um_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder10um_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder10um_kg)],
        
        PMUnder2.5um_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder2.5um_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(SOx_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(SOx_kg)],
        
        VOC_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(VOC_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(VOC_kg)]
      )]
    }
    
    # Processing emissions - Chip & Grind. Chips CWD, grinds the rest.
    if(user_inputs[variable=='harvest_comminution_opt',value]=='chipandgrind') {
      harvest_processing_emissions[,':='(
        CO2_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO2_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO2_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO2_kg)],
        
        CO_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CO_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CO_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CO_kg)],
        
        N2O_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(N2O_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(N2O_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(N2O_kg)],
        
        CH4_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(CH4_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(CH4_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(CH4_kg)],
        
        NOx_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(NOx_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(NOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(NOx_kg)],
        
        PMUnder10um_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder10um_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder10um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder10um_kg)],
        
        PMUnder2.5um_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(PMUnder2.5um_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(PMUnder2.5um_kg)],
        
        SOx_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(SOx_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(SOx_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(SOx_kg)],
        
        VOC_kg = # Cut to length
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_CWD_chip[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_FinesFoliage_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * ctl_high_slope_chip[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Cut_to_Length' | merge_column=='Cable-Cut_to_Length') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * ctl_high_slope_grind[,sum(VOC_kg)] +
          # Whole tree
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_CWD_chip[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope<35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_FinesFoliage_grind[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_CWD_tonsAcre)] * cell_to_acres * wt_high_slope_chip[,sum(VOC_kg)] +
          cbrec.dt[(merge_column=='Ground-Whole_Tree' | merge_column=='Cable-Whole_Tree') & cell_slope>=35,sum(Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre)] * cell_to_acres * wt_high_slope_grind[,sum(VOC_kg)]
      )]
    }
    
    # Transportation Emissions
    # incorporate distance, as emissions are in tons * km. Remember: slope >=35 is not harvested.
    harvest_processing_emissions[,':='(
      CO2_kg = CO2_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CO2_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CO2_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CO2_kg)]],
      
      CO_kg = CO_kg +
              cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CO_kg)]] +
              cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CO_kg)]] +
              cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CO_kg)]],

      N2O_kg = N2O_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(N2O_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(N2O_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(N2O_kg)]],

      CH4_kg = CH4_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(CH4_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(CH4_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(CH4_kg)]],
      
      NOx_kg = NOx_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(NOx_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(NOx_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(NOx_kg)]],
      
      PMUnder10um_kg = PMUnder10um_kg +
                       cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(PMUnder10um_kg)]] +
                       cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(PMUnder10um_kg)]] +
                       cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(PMUnder10um_kg)]],
      
      PMUnder2.5um_kg = PMUnder2.5um_kg +
                        cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(PMUnder2.5um_kg)]] +
                        cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(PMUnder2.5um_kg)]] +
                        cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(PMUnder2.5um_kg)]],
      
      SOx_kg = SOx_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(SOx_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(SOx_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(SOx_kg)]],
      
      VOC_kg = VOC_kg +
               cbrec.dt[cell_slope<=10,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_under_10_slope[,sum(VOC_kg)]] +
               cbrec.dt[cell_slope>10 & cell_slope<35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_10_to_35_slope[,sum(VOC_kg)]] +
               cbrec.dt[cell_slope>=35,sum(Recovered_CWD_tonsAcre, Recovered_FWD_tonsAcre, Recovered_Foliage_tonsAcre) * cell_to_acres * TotalRoad_miles * trans_over_35_slope[,sum(VOC_kg)]]
    )]
    
    # Processing mass loss and mass transfer to power plant
    # In the high volume scenario, the only residues that are NOT processed and NOT sent to the power plant are those with the merge column "Do_Not_Harvest"
    # Step 1, populate the mass_to_plant_tonsAcre from the recovered columns.
    cbrec.dt[merge_column!="Do_Not_Harvest",mass_to_plant_tonsAcre := Recovered_CWD_tonsAcre * (1-processing.mass.loss) + Recovered_FWD_tonsAcre * (1-processing.mass.loss) + Recovered_Foliage_tonsAcre * (1-processing.mass.loss)]
    
    # Step 2, remove processed materials from the recovered materials. Leftover materials will be exposed to decay and wildfire.
    cbrec.dt[merge_column!="Do_Not_Harvest",':='(Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * processing.mass.loss,
                                                 Recovered_FWD_tonsAcre = Recovered_FWD_tonsAcre * processing.mass.loss,
                                                 Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * processing.mass.loss)]
    
    
    
    
    # Processing mass loss and mass transfer to power plant
    # In the low volume scenario, the only residues that are NOT processed and NOT sent to the power plant are those with the merge column "Do_Not_Harvest"
    # Step 1, populate the mass_to_plant_tonsAcre from the recovered columns.
    cbrec.dt[merge_column!="Do_Not_Harvest", mass_to_plant_tonsAcre := Recovered_CWD_tonsAcre * (1-processing.mass.loss) + Recovered_FWD_tonsAcre * (1-processing.mass.loss) + Recovered_Foliage_tonsAcre * (1-processing.mass.loss)]
    
    # Step 2, remove processed materials from the recovered materials. Leftover materials will be exposed to decay and wildfire.
    cbrec.dt[merge_column!="Do_Not_Harvest",':='(Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * processing.mass.loss,
                                                 Recovered_FWD_tonsAcre = Recovered_FWD_tonsAcre * processing.mass.loss,
                                                 Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * processing.mass.loss)]
  }
    
  return(harvest_processing_emissions)
}