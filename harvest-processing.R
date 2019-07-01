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

harvest_processing_fun <- function(cbrec.dt,density.threshold) {
  # The cell density threshold is applied to the entire study area; if the mean recovered cell density is less than the density threshold, we've got a low volume harvest.
  mean.study.area.density <- sum(cbrec.dt[,mean(Recovered_CWD_tonnesAcre)],cbrec.dt[,mean(Recovered_FWD_tonnesAcre)],cbrec.dt[,mean(Recovered_Foliage_tonnesAcre)])
  
  # If the total residue volume is less than 1000 BDT, we've got a small harvest.
  total.recovered.residue <- cbrec.dt[,sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)]
  
  # Initialize harvest processing emissions; this will regurgitate one row to add to year 1 emissions.
  harvest.processing.emissions <- data.table(CO2_kg=0, CO_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SO2_kg=0, VOC_kg=0,char_kg=0)

  # Do the same for transportation emissions.
  transportation.emissions <- data.table(CO2_kg=0, CO_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SO2_kg=0, VOC_kg=0,char_kg=0)
  
  # Create a column for recovered residues that will make it to the power plant. Currently assuming that after comminution, size class is irrelevant.
  cbrec.dt[,mass_to_plant_tonnesAcre:=0]
  
  ##########################
  # Harvest type decision
  ##########################
  if(mean.study.area.density>density.threshold & total.recovered.residue > 1000) { 
    ##### HIGH VOLUME HARVEST #####
    # After the high/low volume determination, options will vary based upon: moisture/dirt content, comminution, residue collection type, treatment type, and slope. 
    # Next, we differentiate based on residue collection.
    if(user_inputs[variable=='biomass_collection',value]=='Piles Only') {
      # "Dry/Green" will only affect the high volume equipment
      if(user_inputs[variable=='residue_moisture',value]=='Green') {
        # Within this category, treatment type has no effect.
        Chip_LessThan10 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',]
        Chip_10To35 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',]
        Chip_Over35 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',]

        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]

        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.6',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.9',]
        
        # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
        # If not, they are the same as transport1
        if(user_inputs[variable=='harvest_transfer_point',value]) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
          CellToRoadMod <- 5
          RoadToPlantMod <- -5
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.6',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.9',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }

      } else if(user_inputs[variable=='residue_moisture',value]=='Dry') {
        # Within this category, treatment type has no effect. Chipping is not applied to these residues.
        if(user_inputs[variable=='harvest_comminution_opt',value]=='chip') {
          warning("Dry residues cannot be chipped; emissions for grinding equipment substituted")
        }
        
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]

        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
        
        # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
        # If not, they are the same as transport1
        if(user_inputs[variable=='harvest_transfer_point',value]) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
          CellToRoadMod <- 5
          RoadToPlantMod <- -5
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
      } else {
        stop("Residue moisture in input file must be 'Green' or 'Dry'") # Shoot an error message if the variable is wrong
      }
    } else if(user_inputs[variable=='biomass_collection',value]=='Piles and Scattered') {
      # "Dry/Green" will only affect the high volume equipment
      if(user_inputs[variable=='residue_moisture',value]=='Green') {
        if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing
          
          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing
          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.2' | Equipment_code=='L.1',]
        }
        
        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.6',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.9',]
        
        # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
        if(user_inputs[variable=='harvest_transfer_point',value]) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
          CellToRoadMod <- 5
          RoadToPlantMod <- -5
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.6',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.9',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
      } else if(user_inputs[variable=='residue_moisture',value]=='Dry') {
        # Chipping is not an option.
        if(user_inputs[variable=='harvest_comminution_opt',value]=='chip') {
          warning("Dry residues cannot be chipped; emissions for grinding equipment substituted")
        }
        if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]

        }
        
        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
        
        # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
        if(user_inputs[variable=='harvest_transfer_point',value]) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',] 
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
          CellToRoadMod <- 5
          RoadToPlantMod <- -5
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
        
      } else {
        stop("Residue moisture in input file must be 'Green' or 'Dry'") # Shoot an error message if the variable is wrong
      }
    } else {
      stop("Biomass collection in input file must be 'Piles Only' or 'Piles and Scattered'") # Shoot an error message if the variable is wrong
    }
    
    if(user_inputs[variable=='residue_moisture',value]=='Green') { 
      if(user_inputs[variable=='harvest_comminution_opt',value]=='chip') {
        # Processing emissions - Chipping
        harvest.processing.emissions[,':='(
          CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(CO2_kg)],
          
          CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(CO_kg)], 
          
          CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(CH4_kg)], 
          
          NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(NOx_kg)], 
          
          PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(PMUnder10um_kg)], 
          
          PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(PMUnder2.5um_kg)],
          
          SO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(SO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(SO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(SO2_kg)], 
          
          VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_LessThan10[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_10To35[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Chip_Over35[,sum(VOC_kg)]
        )]
        
      }
      # Processing emissions - Grinding
      if(user_inputs[variable=='harvest_comminution_opt',value]=='grind') {
        harvest.processing.emissions[,':='(
          CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO2_kg)],
          
          CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO_kg)],
          
          CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CH4_kg)],
          
          NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(NOx_kg)],
          
          PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder10um_kg)],
          
          PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder2.5um_kg)],
          
          SO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(SO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(SO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(SO2_kg)],
          
          VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(VOC_kg)]
        )]
        
      }
    } else { # Dry residues; if they were not "Green" or "Dry", the module would stop and an error message thrown before now. 
      # Likewise, a warning will have been thrown if someone selected "chipped" Grinding values have been substituted.
      harvest.processing.emissions[,':='(
        CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO2_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO2_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO2_kg)],

        CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO_kg)],

        CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CH4_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CH4_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CH4_kg)],

        NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(NOx_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(NOx_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(NOx_kg)],

        PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder10um_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder10um_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder10um_kg)],

        PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder2.5um_kg)],

        SO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(SO2_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(SO2_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(SO2_kg)],

        VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(VOC_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(VOC_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(VOC_kg)]
      )]
    }
  } else { 
    ##### LOW VOLUME HARVEST #####
    # After the high/low volume determination, options will vary based upon: residue collection type, treatment type and slope.
    # Unlike high volume harvests, low volume harvests do not vary based upon resdidue moisture, and the only comminution option is grinding. Because these are user variables,
    # shoot a warning message to show what up.
    warning("Study area calculated to have a low-volume harvest. Moisture/dirt content does not affect harvest, and the only comminution option is grinding - if you selected 'chip', that option has been overriden.")
    
    if(user_inputs[variable=='biomass_collection',value]=='Piles Only') {
      if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }

      Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
      Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
      Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
      
      # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
      # If not, they are the same as transport1
      if(user_inputs[variable=='harvest_transfer_point',value]) {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
        
        # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
        CellToRoadMod <- 5
        RoadToPlantMod <- -5
      } else {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
        
        # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
        CellToRoadMod <- 0
        RoadToPlantMod <- 0
      }
        
    } else if(user_inputs[variable=='biomass_collection',value]=='Piles and Scattered') {
      # "Dry/Green" will only affect the high volume equipment
      if(user_inputs[variable=='treatment_type',value]=='RM100') { # 100% thin
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA20' | user_inputs[variable=='treatment_type',value]=='TFB20' | user_inputs[variable=='treatment_type',value]=='TP20') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA40' | user_inputs[variable=='treatment_type',value]=='TFB40' | user_inputs[variable=='treatment_type',value]=='TP40') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA60' | user_inputs[variable=='treatment_type',value]=='TFB60' | user_inputs[variable=='treatment_type',value]=='TP60') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(user_inputs[variable=='treatment_type',value]=='TFA80' | user_inputs[variable=='treatment_type',value]=='TFB80' | user_inputs[variable=='treatment_type',value]=='TP80') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SO2_kg = SO2_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
      Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
      Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]

      # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
      if(user_inputs[variable=='harvest_transfer_point',value]) {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
        
        # Add an extra 5 miles to CellToRoad to get to the trasfer point, and subtract that 5 miles from the RoadToPlant distance
        CellToRoadMod <- 5
        RoadToPlantMod <- -5
      } else {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
        
        # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
        CellToRoadMod <- 0
        RoadToPlantMod <- 0
      }
    } else {
      stop("Biomass collection in input file must be 'Piles Only' or 'Piles and Scattered'") # Shoot an error message if the variable is wrong
    }
    # Processing emissions - for small harvest, grind only
    harvest.processing.emissions[,':='(
      CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO2_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO2_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO2_kg)],

      CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CO_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CO_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CO_kg)],

      CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(CH4_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(CH4_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(CH4_kg)],

      NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(NOx_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(NOx_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(NOx_kg)],

      PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder10um_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder10um_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder10um_kg)],

      PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(PMUnder2.5um_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(PMUnder2.5um_kg)],

      SO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(SO2_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(SO2_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(SO2_kg)],

      VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_LessThan10[,sum(VOC_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_10To35[,sum(VOC_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * cell_to_acres * Grind_Over35[,sum(VOC_kg)]
      )]
    
  }

  # Transportation Emissions - incorporate distance, as emissions are in tons * km. "Transportation 1" is applied for CellToRoad and an additional 5 miles to a theorhetical transfer point, and "Transportation 2" is appied to the 
  # the Road to Plant distance, minus the 5 miles to the transfer point. If CellToRoadDist is less than 5 miles, then only Transportation 1 is used.
  transportation.emissions[,':='(
    CO2_kg = CO2_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(CO2_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(CO2_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(CO2_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(CO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(CO2_kg)])],

    CO_kg = CO_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(CO_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(CO_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(CO_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(CO_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(CO_kg)])],
    
    CH4_kg = CH4_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(CH4_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(CH4_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(CH4_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(CH4_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(CH4_kg)])],

    NOx_kg = NOx_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(NOx_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(NOx_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(NOx_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(NOx_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(NOx_kg)])],

    PMUnder10um_kg = PMUnder10um_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(PMUnder10um_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(PMUnder10um_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(PMUnder10um_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(PMUnder10um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(PMUnder10um_kg)])],

    PMUnder2.5um_kg = PMUnder2.5um_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(PMUnder2.5um_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(PMUnder2.5um_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(PMUnder2.5um_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(PMUnder2.5um_kg)])],

    SO2_kg = SO2_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(SO2_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(SO2_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(SO2_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(SO2_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(SO2_kg)])],

    VOC_kg = VOC_kg +
      # Emissions from cells with less than 5 miles between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad and Road to Plant.
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_LessThan10[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_10To35[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles <= 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + RoadToPlant_miles) * Trans1_Over35[,sum(VOC_kg)])] +
      # Emissions from cells with more than 5 miles between road and plant; this will utilize transportation 1 and 2. Transportation 1 will cover CelltoRoad and CellToRoadMod, Trans2 will cover RoadToPlant and RoadToPlantMod.
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_LessThan10[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_10To35[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (CellToRoadDist_miles + CellToRoadMod) * Trans1_Over35[,sum(VOC_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_LessThan10[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_10To35[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * (RoadToPlant_miles + RoadToPlantMod) * Trans2_Over35[,sum(VOC_kg)])] +
      
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_LessThan10[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_10To35[,sum(VOC_kg)])] +
      cbrec.dt[RoadToPlant_miles > 5 & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * cell_to_acres * TransLoad_Over35[,sum(VOC_kg)])]
  )]

  # Processing mass loss and mass transfer to power plant
  # In the high volume scenario, the only residues that are NOT processed and NOT sent to the power plant are those with the merge column "Do_Not_Harvest"
  # Step 1, populate the mass_to_plant_tonnesAcre from the recovered columns.
  cbrec.dt[,mass_to_plant_tonnesAcre := Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre]
  
  # Step 2, remove processed materials from the recovered materials. Processing and transfer losses are accounted for in the scenario matrix, so this is just zeroing the recovered mass columns.
  cbrec.dt[,':='(Recovered_CWD_tonnesAcre = 0,
                 Recovered_FWD_tonnesAcre = 0,
                 Recovered_Foliage_tonnesAcre = 0)]
  
  return(list(harvest.processing.emissions,transportation.emissions,cbrec.dt))
}