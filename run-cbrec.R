###############################################
# Source relevant module and function files.
###############################################
source("harvest-processing.R")
source("prescribed-burn-functions.R")
source("decay-functions.R")
source('wildfire-functions.R')
source('power-plant-functions.R')
source("misc-functions.R")

###############################################
# Load needed library functions
###############################################
load.libraries(c("raster","rgdal","rgeos","tidyverse","beepr")) # beepr is for testing

#############################################################################################################################################
# To make multiple runs with different options, easier, we're putting all user options and filepaths in an external csv file,
# which we read in here.
#############################################################################################################################################
user_input_filepath <- '/Volumes/744GB-Storage/Dropbox/serc/carbcat/cbrec_inputs_261.csv'
user_inputs <- data.table(read.csv(user_input_filepath, stringsAsFactors = FALSE))
initial_user_inputs <- copy(user_inputs)

#####################################################
# Load up emissions factor/rate data from csv files.
#####################################################
equipment_emissions <- data.table(read.csv(user_inputs[variable=='equipment_emissions_filepath',value]))
warning("CS.1 emissions are PER HOUR, not per BDT. This must be updated!")
warning("CO emissions factors for all plants taken from 'PowerPlantEmissionSummary.xlsx' for Current Gen Combustion Plant.")

#############################################################################################################################################
# Load up the location-specific information gleaned from GIS data. 
# To load up the location-specific date, first we load the FCID_code raster layer.This has over a billion data points, and if we try to make
# a data table out of the full set, we will run into memory problems, maximum data table size columns, and probably crash RStudio. We will 
# crop the raster down to a shapefile supplied by the user (or, more accurately, mask the raster). As rasters are always rectangular (and 
# California is not), the full dataset would contain mostly empty points, and even smaller datasets may still have many empty poitns.
# Once loaded, convert the RasterLayer to a data frame, and extract the subset of points that correspond to actual data. 
# 
# Once we have the smaller data set, convert to a data table and combine with the residue and decay tables.
# 
# ANDY: LOOK HERE
# Our model runs cases individually, rather than paired scenarios, because of memory constraints of running two data pairs in a single run. BUT scenarios are going to cover the same geographic study area, 
# and the most runtime-heavy code segment is the raster cropping and masking. So, if the first time we run a case, we run the raster process, save the resultant study_area_FCID, and THEN apply scenario_matrix
# variables, we make this whiole thing run much quicker. This will require clear design for input/output file structure.
#############################################################################################################################################

if(user_inputs[variable=='ag_or_forest',value]=='forest') {
  study_area_raster <- raster(user_inputs[variable=='fcid_code_filepath',value])
  slope_raster <- raster(user_inputs[variable=='slope_filepath',value])
  names(slope_raster) <- "cell_slope"
  
  # If the user inputs specify a study area mask file, load the shapefile, crop the original raster to the shapefile extents, and mask the raster to the shape file.
  if(!is.na(user_inputs[variable=='study_area_mask_file',value])) {
    study_area_mask <- readOGR(user_inputs[variable=='study_area_mask_file',value])
    study_area_raster <- crop(study_area_raster,extent(study_area_mask))
    study_area_raster <- mask(study_area_raster,study_area_mask)
    slope_raster <- crop(slope_raster,extent(study_area_mask))
    slope_raster <- mask(slope_raster,study_area_mask)
  }
  
  # convert raster objects to data frames. This may crash things.
  ##########################################################################################
  ##########################################################################################
  # Look into if we can combine the subset line below into the as.data.frame line here. Also, 
  # look if we can stack FCID and slope rasters to see if we can load them at the same time.
  ##########################################################################################
  ##########################################################################################
  study_area_df <- as.data.frame(study_area_raster,xy=TRUE)
  
  names(study_area_df) <- c('x','y','FCID_code')
  
  # The full raster dataset is a square, so it will include non-forested areas within California, a chunk of the Pacifc Ocean,
  # and a fair bit of Nevada. These locations will have an FCID value of NA; trim it out using subset()
  study_area_df <- subset(study_area_df,!is.na(FCID_code))
  
  # Repeat with slope
  slope_df <- as.data.frame(slope_raster,xy=TRUE)
  slope_df <- subset(slope_df,!is.na(cell_slope))
  
  # Convert to a data table, and delete the data frame
  study_area_FCID <- data.table(study_area_df)
  study_area_df <- NULL
  setkey(study_area_FCID,x,y)
  
  # Repeat with slope
  slope_dt <- data.table(slope_df)
  slope_df <- NULL
  setkey(slope_dt,x,y)
  
  # Combine the slope with the FCID data tables - THIS MAY NOT BE NECESSARY IF WE CAN LOAD AS STACKS.
  study_area_FCID <- slope_dt[study_area_FCID]
  
  # Delete the slope data table to save on memory
  slope_dt <- NULL
  
  # Read in the residue load values from a csv file. These values are indexed by FCID_code, so that is how we will eventually join the 2 tables
  fcid_table <- data.table(read.csv(user_inputs[variable=='residue_treatment_filepath',value]))
  # Eliminate the TPA column
  fcid_table[,TPA:=NULL]
  
  # Round the x and y values to the first decimal, otherwise rounding errors are going to muck with joins with the decay rates.
  study_area_FCID[,':='(x=round(x,digits=1),y=round(y,digits=1))]
  
  # Time to add information about the biomass properties, specifically heating value and carbon fraction. 
  biomass_properties <- data.table(read.csv(user_inputs[variable=='biomass_properties_filepath',value]))
  setkey(biomass_properties,FCID_code)
  setkey(study_area_FCID,FCID_code)
  study_area_FCID <- biomass_properties[study_area_FCID]
  
  # Re-key
  setkey(study_area_FCID,x,y)
}

if(user_inputs[variable=='ag_or_forest',value]=='agricultural') {
  print('This code has yet to be written')
}

#############################################
# Load the decay rate rasters
#############################################

# There are currently 3 decay rasters: one for CWD, FWD, and foliage. From our notes, there should be six rates (for piled and scattered), not just 3.
# Figure out if this is an issue.
CWD_decay_raster <- raster(user_inputs[variable=='CWD_decay_filepath',value])
FWD_decay_raster <- raster(user_inputs[variable=='FWD_decay_filepath',value])
foliage_decay_raster <- raster(user_inputs[variable=='foliage_decay_filepath',value])

# If the user inputs specify a study area mask file, we will have a shapefile loaded from earlier. Use the same shape file to crop and mask our rasters.
if(!is.na(user_inputs[variable=='study_area_mask_file',value])) {
  # CWD raster
  CWD_decay_raster <- crop(CWD_decay_raster,extent(study_area_mask))
  CWD_decay_raster <- mask(CWD_decay_raster,study_area_mask)
  
  # FWD raster  
  FWD_decay_raster <- crop(FWD_decay_raster,extent(study_area_mask))
  FWD_decay_raster <- mask(FWD_decay_raster,study_area_mask)
  
  # foliage raster
  foliage_decay_raster <- crop(foliage_decay_raster,extent(study_area_mask))
  foliage_decay_raster <- mask(foliage_decay_raster,study_area_mask)
}

# Now that the decay rasters are loaded and masked, convert them to data frames and then data tables
CWD_decay_df <- as.data.frame(CWD_decay_raster,xy=TRUE)
CWD_decay_rates <- data.table(CWD_decay_df)
CWD_decay_df <- NULL
setkey(CWD_decay_rates,x,y)
CWD_decay_rates[,':='(x=round(x,digits=1),y=round(y,digits=1))]

FWD_decay_df <- as.data.frame(FWD_decay_raster,xy=TRUE)
FWD_decay_rates <- data.table(FWD_decay_df)
FWD_decay_df <- NULL
setkey(FWD_decay_rates,x,y)
FWD_decay_rates[,':='(x=round(x,digits=1),y=round(y,digits=1))]

foliage_decay_df <- as.data.frame(foliage_decay_raster,xy=TRUE)
foliage_decay_rates <- data.table(foliage_decay_df)
foliage_decay_df <- NULL
setkey(foliage_decay_rates,x,y)
foliage_decay_rates[,':='(x=round(x,digits=1),y=round(y,digits=1))]

# Join decay data tables with the study area FCID, and then clear the data tables
setkey(study_area_FCID,x,y)
setkey(CWD_decay_rates,x,y)
setkey(FWD_decay_rates,x,y)
setkey(foliage_decay_rates,x,y)

study_area_FCID <- CWD_decay_rates[FWD_decay_rates[foliage_decay_rates[study_area_FCID]]]

# There may be a couple more NA points, with no reource available and no decay rate assigned. Clear it out.
study_area_FCID <- study_area_FCID[!is.na(CWD_cm)&!is.na(FWD_cm)&!is.na(Foliage_cm),]

CWD_decay_rates <- NULL
FWD_decay_rates <- NULL
foliage_decay_rates <- NULL

############################################################################################################################
# The longest run-time elemtns of CBREC are reading in the rasters, masking them to the study area, and reading in the 
# FCID table. These will only change from study area to study area, and not scenario to scenario (or case to case). We 
# load these once, and then keep them in memory and refresh for each scenario/case. So we copy the initial FCID/Slope/Decay
# data here.
############################################################################################################################

# Cycle through scenaio matrix variables listed in user_inputs. If they are listed as "All", we cycle through all possible values. Otherwise, let that value stand.
if(user_inputs[variable == "treatment_type",value]=="All") {
  treatment.list <- c("RM100", "TFA20", "TFA40", "TFA60", "TFA80", "TFB20", "TFB40", "TFB60", "TFB80", "TP20", "TP40", "TP60", "TP80")
} else {
  treatment.list <- user_inputs[variable == "treatment_type",value]
}

if(user_inputs[variable == "fraction_piled_residues",value]=="All") {
  fraction.piled.list <- c("0%", "30%", "50%", "70%")
} else {
  fraction.piled.list <- user_inputs[variable == "fraction_piled_residues",value]
}

if(user_inputs[variable == "burn_type",value]=="All") {
  burn.type.list <- c("No", "Pile", "Broadcast", "Pile and Broadcast")
} else {
  burn.type.list <- user_inputs[variable == "burn_type",value]
}

if(user_inputs[variable == "biomass_collection",value]=="All") {
  biomass.collection.list <- c("All Tech Recoverable", "No", "Piles Only")
} else {
  biomass.collection.list <- user_inputs[variable == "biomass_collection",value]
}

if(user_inputs[variable == "has_pulp_market",value]=="All") {
  pulp.market.list <- c("Yes", "No")
} else {
  pulp.market.list <- user_inputs[variable == "has_pulp_market",value]
}

user_input_iterations <- data.table(expand.grid(treatment.list, fraction.piled.list, burn.type.list, biomass.collection.list, pulp.market.list))
setnames(user_input_iterations, c("Var1", "Var2", "Var3", "Var4", "Var5"), c("treatment", "frac.piled", "prescribed.burn.type", "biomass.collection","pulp.market"))
# add scattered fractions
user_input_iterations[,frac.scattered := "100%"]
user_input_iterations[frac.piled == "30%",frac.scattered := "70%"]
user_input_iterations[frac.piled == "50%",frac.scattered := "50%"]
user_input_iterations[frac.piled == "70%",frac.scattered := "30%"]

for(user.input.row.i in 1:nrow(user_input_iterations)) {
  # Copy the original study_area_FCID data into a data structure that will dynamically change.
  case.data <- copy(study_area_FCID)
  
  # Replace the user inputs with the appropriate variables from user_input_iterations
  user_inputs[variable=='fraction_piled_residues',value := user_input_iterations[user.input.row.i, frac.piled]]
  user_inputs[variable=='fraction_scattered_residues',value := user_input_iterations[user.input.row.i, frac.scattered]]
  user_inputs[variable=='treatment_type',value := user_input_iterations[user.input.row.i, treatment]] 
  user_inputs[variable=='burn_type',value := user_input_iterations[user.input.row.i, prescribed.burn.type]]
  user_inputs[variable=='biomass_collection',value := user_input_iterations[user.input.row.i, biomass.collection]]
  user_inputs[variable=='has_pulp_market',value := user_input_iterations[user.input.row.i, pulp.market]]
  
  # The technically available residues will be based on slope, silvicultural treatment, the fraction of piled/scattered residues, burn type, 
  # biomass collection, and pulp market. Within a scenario, treatment, burn type, biomass collection, and pulp market are identical across 
  # the entire study area. Due to harvest restrictions baked into the residue percentages, residue quantites will be affected by slope, which
  # varies from cell to cell. Rather than create an ID for case.data and merging with the complete scenario martix, create a smaller 
  # scenario matrix filtered through the user inputs, and merge with case.data
  complete_scenario_matrix <- data.table(read.csv(user_inputs[variable=='scenario_matrix_file',value]))
  
  # Trim to the treatment, burn type, biomass collection, and pulp market
  scenario_matrix <- copy(complete_scenario_matrix[Fraction_Piled_Residues==user_inputs[variable=='fraction_piled_residues',value] &
                                                     Silvicultural.Treatment==user_inputs[variable=='treatment_type',value] & 
                                                     Burn.Type==user_inputs[variable=='burn_type',value] & 
                                                     Biomass.Collection==user_inputs[variable=='biomass_collection',value] & 
                                                     Pulp.Market==user_inputs[variable=='has_pulp_market',value],])
  
  # Not all combinations of user inputs are valid in the scenario matrix; if there are 0% piled residues, you cannot collect or burn piles.
  # If scenario_matrix is empty, skip to the next iteration.
  if(nrow(scenario_matrix)<1) {next}
  
  #########################################################################################################################################
  # The wildfire data will be selected based on some scenario matrix identifiers.
  # The order: Silvicultural_Treatment, Burn_Type, Fraction_Piled, Fraction_Scattered, secondary_burn, Biomass_Collection, Pulp_Market, ID
  # We don't have secondary burn, but we can access the ID, and given an ID and tile number, we can pull this off.
  #########################################################################################################################################
  scenario_ID <- paste(treatment_name_lookup[treat.code==user_inputs[variable=='treatment_type',value],treat.name], user_inputs[variable=='burn_type',value], str_remove(user_inputs[variable=='fraction_piled_residues',value],"%"),
                       str_remove(user_inputs[variable=='fraction_scattered_residues',value],"%"), "first", user_inputs[variable=='biomass_collection',value], user_inputs[variable=='has_pulp_market',value], unique(scenario_matrix[,ID]),sep='-')
  
  # I'm thinking we'll need the scenario ID number for for output organization
  # we may want to enable running based off of a row id in the scenario matrix file, rather than requiring all of the user input variables expressly.
  scenario_ID_num <- unique(scenario_matrix[,ID])
  
  
  # Combine case.data with the FCID-specific residue loading, specific to out scenario treatment.
  setkey(fcid_table,FCID2018)
  setkey(case.data,FCID_code)
  case.data <- case.data[fcid_table[Treatment==user_inputs[variable=='treatment_type',value]]]
  setkey(case.data,x,y)
  
  # Eliminate the treatment columns, we do not need it. The FCID code is still needed for power plant efficiency.
  case.data[,Treatment:=NULL]
  
  # There will be some overlap for FCID values that don't appear in the data set. They will have non-existant x & y values. Trim them.
  case.data <- case.data[!is.na(x)]
  
  ########################################
  # Combine stem and bark residues.
  ########################################
  
  # Merge senario matrix with case.data; this will be based on cell slope (>40%, <40%, or remove from study)
  scenario_matrix[,':='(ID=NULL, Fraction_Piled_Residues=NULL, Fraction_Scattered_Residues=NULL, Silvicultural.Treatment=NULL, Burn.Type=NULL, Biomass.Collection=NULL, Pulp.Market=NULL)]
  setkey(scenario_matrix,Slope)
  
  ###########################################################################################################################################
  # We now have a spatially-indexed data table with residue loading information, decay rates, and cell slope values. Now we figure out what
  # the technically recoverable residue is. We may be able to speed this up by converting to factors
  ###########################################################################################################################################
  
  setkey(case.data,cell_slope)
  case.data[,merge_column := "GT40"]
  case.data[cell_slope<40,merge_column := "LT40"]
  
  # Delete entries after the slope cutoff point
  case.data <- case.data[cell_slope<as.numeric(user_inputs[variable=='residue_slope_cutoff',value])]
  
  # Merge case.data with the scenario matrix
  setkey(case.data,merge_column)
  case.data <- scenario_matrix[case.data]
  
  # Combine the technically available fractions with the gross resource base.
  case.data[,':='(Recovered_Stem9Plus_tonsAcre = Recovered_Stem9Plus * Stem9Plus_tonsAcre,
                  Recovered_Stem6to9_tonsAcre = Recovered_Stem6to9 * Stem6to9_tonsAcre,
                  Recovered_Stem4to6_tonsAcre = Recovered_Stem4to6 * Stem4to6_tonsAcre,
                  Recovered_Branch_tonsAcre = Recovered_Branch * Branch_tonsAcre,
                  Recovered_Foliage_tonsAcre = Recovered_Foliage * Foliage_tonsAcre,
                  Piled_Stem9Plus_tonsAcre = Piled_Stem9Plus * Stem9Plus_tonsAcre,
                  Piled_Stem6to9_tonsAcre = Piled_Stem6to9 * Stem6to9_tonsAcre,
                  Piled_Stem4to6_tonsAcre = Piled_Stem4to6 * Stem4to6_tonsAcre,
                  Piled_Branch_tonsAcre = Piled_Branch * Branch_tonsAcre,
                  Piled_Foliage_tonsAcre = Piled_Foliage * Foliage_tonsAcre,
                  Scattered_Stem9Plus_tonsAcre = Scattered_Stem9Plus * Stem9Plus_tonsAcre,
                  Scattered_Stem6to9_tonsAcre = Scattered_Stem6to9 * Stem6to9_tonsAcre,
                  Scattered_Stem4to6_tonsAcre = Scattered_Stem4to6 * Stem4to6_tonsAcre,
                  Scattered_Branch_tonsAcre = Scattered_Branch * Branch_tonsAcre,
                  Scattered_Foliage_tonsAcre = Scattered_Foliage * Foliage_tonsAcre
  )]
  
  # Trim out the columns we no longer need.
  warning("This chunk of code will need to be re-written if we remove bark columns from the scenario matrix")
  case.data[,':='(Slope=NULL, Recovered_Stem9Plus = NULL, Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9 = NULL, Stem6to9_tonsAcre = NULL, Recovered_Stem4to6 = NULL,
                  Stem4to6_tonsAcre = NULL, Recovered_Bark9Plus = NULL, Recovered_Bark6to9 = NULL, Recovered_Bark4to6 = NULL, Recovered_Branch = NULL, Branch_tonsAcre = NULL, 
                  Recovered_Foliage = NULL, Foliage_tonsAcre = NULL, Merchantable_Stem9Plus = NULL, Merchantable_Stem6to9 = NULL, Merchantable_Stem4to6 = NULL,  Merchantable_Bark9Plus = NULL,
                  Merchantable_Bark6to9 = NULL, Merchantable_Bark4to6 = NULL, Merchantable_Branch = NULL, Merchantable_Foliage = NULL, Piled_Stem9Plus = NULL, 
                  Piled_Stem6to9 = NULL, Piled_Stem4to6 = NULL, Piled_Bark9Plus = NULL, Piled_Bark6to9 = NULL, Piled_Bark4to6 = NULL, Piled_Branch = NULL, Piled_Foliage = NULL,
                  Scattered_Stem9Plus = NULL, Scattered_Stem6to9 = NULL, Scattered_Stem4to6 = NULL, Scattered_Bark9Plus = NULL, Scattered_Bark6to9 = NULL,
                  Scattered_Bark4to6 = NULL, Scattered_Branch = NULL, Scattered_Foliage = NULL
  )]
  
  #########################################################################################################################################
  # Add wildfire data and emissions factors. These are stored in .rds files by tile and scenario-definition variables; there are over 
  # 13,000 tiles, multiple scenarios for each tile, and for multiple time periods for each scenario. We need to find and load the 
  # scenario-specific tile data that overlaps with our study area and then crop it to match our study area exactly.
  #########################################################################################################################################
  
  # Load up the full tile shape file
  full_tile_set <- readOGR(user_inputs[variable=="tile_shape_file",value])
  study_area_tiles <- raster::intersect(study_area_mask,full_tile_set)
  
  # To get the tile IDs, convert study_area_tiles into a data frame and extract the ID column.
  # NOTE: THERE WILL BE ISSUES IF THE MASK FILE HAS AN "ID" ATTRIBUTE.
  tile_list <- as.data.frame(study_area_tiles)$ID
  
  # Extracting the values we need from the wildfire data is going to be long and a little messy. So I'm throwing it in a module.
  # Calculating emissions factors and combustion/char_fractions should remove the need to convert to metric - since EVERYTHING is 
  # imperial, we essentially get a massless factor, and the conversion factors will cancel out.
  wildfire_data <- wildfire_processing_fun(user_inputs[variable=="wildfire_data_directory",value], scenario_ID, user_inputs[variable=='burn_type',value], tile_list)
  
  wildfire_data[,':='(x=round(x,digits=1),y=round(y,digits=1))]
  setkey(wildfire_data,x,y)
  
  ############################################################
  # Prepare columns for Burn/Decay/Wildfire Emissions
  ############################################################
  
  # Step 1: combine all mass categories into CWD, FWD, and foliage. There will also be a duff column, though this will start empty. 
  # There will be several categories with different decay rates - recovered, scattered (including merchantable breakage), field piled, and landing piled
  case.data[,':='(Recovered_CWD_tonsAcre = Recovered_Stem9Plus_tonsAcre + Recovered_Stem6to9_tonsAcre + Recovered_Stem4to6_tonsAcre,
                  Scattered_CWD_tonsAcre = Scattered_Stem9Plus_tonsAcre + Scattered_Stem6to9_tonsAcre + Scattered_Stem4to6_tonsAcre,
                  Piled_CWD_tonsAcre = Piled_Stem9Plus_tonsAcre + Piled_Stem6to9_tonsAcre + Piled_Stem4to6_tonsAcre
  )]
  
  # Clear out old columns, as well as merchantable branch/foliage (which should've been empty anyway)
  case.data[,':='(Recovered_Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9_tonsAcre = NULL, Recovered_Stem4to6_tonsAcre = NULL,
                  Scattered_Stem9Plus_tonsAcre = NULL, Scattered_Stem6to9_tonsAcre = NULL, Scattered_Stem4to6_tonsAcre = NULL,
                  Piled_Stem9Plus_tonsAcre = NULL, Piled_Stem6to9_tonsAcre = NULL, Piled_Stem4to6_tonsAcre = NULL
  )]
  
  # The residue loading columns are currently in imperial units: tons (2000 lbs/acre), and we want it in metric (tonnes (1000 kg) /acre). 1 ton equals 0.907185 tonnes
  case.data[,':='(
    Recovered_Branch_tonsAcre = Recovered_Branch_tonsAcre * 0.907185,
    Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * 0.907185, 
    Piled_Branch_tonsAcre = Piled_Branch_tonsAcre * 0.907185,
    Piled_Foliage_tonsAcre = Piled_Foliage_tonsAcre * 0.907185,
    Scattered_Branch_tonsAcre = Scattered_Branch_tonsAcre * 0.907185,
    Scattered_Foliage_tonsAcre = Scattered_Foliage_tonsAcre * 0.907185,
    Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * 0.907185,
    Scattered_CWD_tonsAcre = Scattered_CWD_tonsAcre * 0.907185,
    Piled_CWD_tonsAcre = Piled_CWD_tonsAcre * 0.907185
  )]
  
  # FWD consists of branches, and foliage is obviously foliage. Renamed the "branch" columns to "FWD", and rename all of the columns labeled "_tonsAcre" to "_tonnesAcre" so we know we are in metric.
  setnames(case.data,
           c("Recovered_Branch_tonsAcre","Recovered_Foliage_tonsAcre","Piled_Branch_tonsAcre","Piled_Foliage_tonsAcre","Scattered_Branch_tonsAcre","Scattered_Foliage_tonsAcre","Recovered_CWD_tonsAcre","Scattered_CWD_tonsAcre","Piled_CWD_tonsAcre"),
           c("Recovered_FWD_tonnesAcre","Recovered_Foliage_tonnesAcre","Piled_FWD_tonnesAcre","Piled_Foliage_tonnesAcre","Scattered_FWD_tonnesAcre","Scattered_Foliage_tonnesAcre","Recovered_CWD_tonnesAcre","Scattered_CWD_tonnesAcre","Piled_CWD_tonnesAcre"))
  
  # Copy the residue columns for Piled and Scattered residues. These will be the initial mass residues in Year 1. We will also need some blank columns to track material exposed to fire but left unburned.
  case.data[,':='(Recovered_CWD_tonnesAcre_INITIAL = Recovered_CWD_tonnesAcre,
                  Recovered_FWD_tonnesAcre_INITIAL = Recovered_FWD_tonnesAcre,
                  Recovered_Foliage_tonnesAcre_INITIAL = Recovered_Foliage_tonnesAcre,
                  Piled_CWD_tonnesAcre_INITIAL = Piled_CWD_tonnesAcre,
                  Piled_FWD_tonnesAcre_INITIAL = Piled_FWD_tonnesAcre,
                  Piled_Foliage_tonnesAcre_INITIAL = Piled_Foliage_tonnesAcre,
                  Scattered_CWD_tonnesAcre_INITIAL = Scattered_CWD_tonnesAcre,
                  Scattered_FWD_tonnesAcre_INITIAL = Scattered_FWD_tonnesAcre,
                  Scattered_Foliage_tonnesAcre_INITIAL = Scattered_Foliage_tonnesAcre
  )]
  
  # Make columns for duff, mass previously exposed to fire but unburnt, and year.i columns. If space becomes an issue, we can create/delete year.i column in the decay function, but I;m guessing that will hit our speed.
  case.data[,':='(Duff_tonnesAcre=0, decay_CWD_mass_year_i=0, decay_FWD_mass_year_i=0, decay_Foliage_mass_year_i=0,
                  fire_exposed_CWD_mass_year_i=0, fire_exposed_FWD_mass_year_i=0, fire_exposed_Foliage_mass_year_i=0, fire_exposed_Duff_mass_year_i=0,
                  prev_fired_CWD_tonnesAcre=0, prev_fired_FWD_tonnesAcre=0, prev_fired_Foliage_tonnesAcre=0, prev_fired_Duff_tonnesAcre=0)]
  
  mass_tracking <- data.table(
    In.field.non.char.scattered_tonnes = rep(0,100),
    In.field.non.char.piled_tonnes = rep(0,100),
    In.field.char.scattered_tonnes = rep(0,100),
    In.field.char.piled_tonnes = rep(0,100),
    wildfire.burned.residue_tonnes = rep(0,100),
    decayed.residue_tonnes = rep(0,100)
  )
  
  emissions_tracking <- data.table(
    wildfire.CO2_tonnes = rep(0,100), 
    wildfire.CO_tonnes = rep(0,100), 
    wildfire.CH4_tonnes = rep(0,100), 
    wildfire.NOx_tonnes = rep(0,100), 
    wildfire.N2O_tonnes = rep(0,100), 
    wildfire.PMUnder10um_tonnes = rep(0,100), 
    wildfire.PMUnder2.5um_tonnes = rep(0,100), 
    wildfire.SO2_tonnes = rep(0,100), 
    wildfire.VOC_tonnes = rep(0,100), 
    decay.CO2_tonnes = rep(0,100),
    decay.CH4_tonnes = rep(0,100)
  )
  
  # Some values will only ever be calculated for year 0. No reason to make 100 rows of data for that.
  year_0_mass_tracking <- data.table(
    field.residue.removed_tonnes = 0,
    power.plant.waste_tonnes = 0,
    broadcast.burned.residue_tonnes = 0,
    pile.burned.residue_tonnes = 0,
    residue.burned.to.electricity_tonnes = 0,
    residue.burned.to.heat_tonnes = 0,
    total.biomass.mobilized_tonnesAcre = 0
  )
  
  year_0_emissions_tracking <- data.table(
    broadcast.burn.CO2_tonnes = 0, 
    broadcast.burn.CO_tonnes = 0, 
    broadcast.burn.CH4_tonnes = 0, 
    broadcast.burn.NOx_tonnes = 0,
    broadcast.burn.N2O_tonnes = 0,
    broadcast.burn.PMUnder10um_tonnes = 0, 
    broadcast.burn.PMUnder2.5um_tonnes = 0, 
    broadcast.burn.SO2_tonnes = 0, 
    broadcast.burn.VOC_tonnes = 0, 
    pile.burn.CO2_tonnes = 0, 
    pile.burn.CO_tonnes = 0, 
    pile.burn.CH4_tonnes = 0, 
    pile.burn.NOx_tonnes = 0,
    pile.burn.N2O_tonnes = 0,
    pile.burn.PMUnder10um_tonnes = 0, 
    pile.burn.PMUnder2.5um_tonnes = 0, 
    pile.burn.SO2_tonnes = 0, 
    pile.burn.VOC_tonnes = 0,
    collection.processing.diesel.CO2_tonnes = 0, 
    collection.processing.diesel.CO_tonnes = 0, 
    collection.processing.diesel.CH4_tonnes = 0, 
    collection.processing.diesel.NOx_tonnes = 0,
    collection.processing.diesel.N2O_tonnes = 0,
    collection.processing.diesel.PMUnder10um_tonnes = 0, 
    collection.processing.diesel.PMUnder2.5um_tonnes = 0, 
    collection.processing.diesel.SO2_tonnes = 0, 
    collection.processing.diesel.VOC_tonnes = 0, 
    transportation.diesel.CO2_tonnes = 0, 
    transportation.diesel.CO_tonnes = 0, 
    transportation.diesel.CH4_tonnes = 0, 
    transportation.diesel.NOx_tonnes = 0,
    transportation.diesel.N2O_tonnes = 0,
    transportation.diesel.PMUnder10um_tonnes = 0, 
    transportation.diesel.PMUnder2.5um_tonnes = 0, 
    transportation.diesel.SO2_tonnes = 0, 
    transportation.diesel.VOC_tonnes = 0, 
    power.plant.for.electricity.CO2_tonnes = 0, 
    power.plant.for.electricity.CO_tonnes = 0, 
    power.plant.for.electricity.CH4_tonnes = 0, 
    power.plant.for.electricity.NOx_tonnes = 0,
    power.plant.for.electricity.N2O_tonnes = 0,
    power.plant.for.electricity.PMUnder10um_tonnes = 0, 
    power.plant.for.electricity.PMUnder2.5um_tonnes = 0, 
    power.plant.for.electricity.SO2_tonnes = 0, 
    power.plant.for.electricity.VOC_tonnes = 0, 
    power.plant.for.heat.CH4.offset_tonnes = 0, 
    powerplant.energy.production_MWh = 0,
    powerplant.energy.production_MMBtu = 0
  )
  
  #############################################################################################################################################
  # The data is set up, now get into the emissions meat of CBREC. If our case involves biomass collection, transportation and equipment 
  # becomes an issue. This involves more raster work, which will take a little time. So, let's skip this if biomass collection ain't happening.
  #############################################################################################################################################
  
  if(user_inputs[variable=="biomass_collection",value]!="No") {
    
    #######################################################################################################################################
    # Determine specific travel distances - cell to road, then road to plant.
    #######################################################################################################################################
    # Load the cell to road raster (the user input is to the transportation raster directory; the specific raster will always have the same name.) Be sure to trim to the study area, or we may hit memory problems.
    cell_to_road_raster <- raster(paste(user_inputs[variable=="transportation_filepath",value],"Transportation_DistanceToRoad.tif",sep=""))
    # Be sure to trim to the study area, or we may hit memory problems.
    cell_to_road_raster <- crop(cell_to_road_raster,extent(study_area_mask))
    cell_to_road_raster <- mask(cell_to_road_raster,study_area_mask)
    cell_to_road <- as.data.frame(cell_to_road_raster,xy=T)
    cell_to_road <- data.table(cell_to_road)
    
    # Rename the value column, bearing in mind that the distance raster is in meters.
    setnames(cell_to_road,c('x','y','CellToRoadDist_meters'))
    
    # Convert the meters to miles; eliminate meters
    cell_to_road[,CellToRoadDist_miles := CellToRoadDist_meters * 0.000621371]
    cell_to_road[,CellToRoadDist_meters := NULL]
    
    # Combine with studay_area_FCID. This first requires rounding coordinates to the nearest tenth.
    cell_to_road[,':='(x=round(x,digits=1),y=round(y,digits=1))]
    setkey(cell_to_road,x,y)
    case.data <- cell_to_road[case.data]
    
    # Repeat for the road to plant raster. The file name will be derived from the user-selected plant code.
    road_to_plant_raster <- raster(paste(user_inputs[variable=="transportation_filepath",value],user_inputs[variable=="power_plant_location",value],".tif",sep=""))
    # road_to_plant_raster <- raster("/Volumes/SharedData/Main Folder - Andy H/CBREC_input_directory/geospatial_data/transportation_data/NearestOperationalPlant.tif")
    # Again, trim to the study area, or we may hit memory problems.
    road_to_plant_raster <- crop(road_to_plant_raster,extent(study_area_mask))
    road_to_plant_raster <- mask(road_to_plant_raster,study_area_mask)
    
    road_to_plant <- as.data.frame(road_to_plant_raster,xy=T)
    
    # The transportation rasters are compressed, so as.data.frame will get all coordinates, but lose much of the data. This line gets it back.
    road_to_plant$RoadToPlant_miles <- road_to_plant_raster[] %>% as.double()
    
    # Trim uneeded columns, convert to data frame.
    road_to_plant <- road_to_plant %>% select(x,y,RoadToPlant_miles)
    road_to_plant <- data.table(road_to_plant)
    
    # Combine with case.data. This first requires rounding coordinates to the nearest tenth.
    road_to_plant[,':='(x=round(x,digits=1),y=round(y,digits=1))]
    setkey(road_to_plant,x,y)
    
    # Add a column to indicate the specific plant location. This is set in the user variables; if the selected location is "NearestOperationalPlant", it works differently
    if(user_inputs[variable=="power_plant_location",value]=="NearestOperationalPlant") {
      # There will be multiple plant IDs. We will need to read in the plant ID raster, pair in codes from a plant ID raster, and fill in any gaps in the operational plant raster with a default max distance.
      nearest_plant_raster <- raster(paste(user_inputs[variable=="transportation_filepath",value],"NearestPlantID.tif",sep=""))
      # The "NearestPlantID" raster is ever so slightly misaligned with the other rasters, despite having the same projection. So, we resample it.
      # Remember to select "ngb" (nearest neighbor) as the method.
      nearest_plant_resampled <- projectRaster(nearest_plant_raster,road_to_plant_raster,method = "ngb")
      nearest_plant <- as.data.frame(nearest_plant_resampled,xy=T)
      nearest_plant <- data.table(nearest_plant)
      setkey(nearest_plant,NearestPlantID)
      
      # Read in location ID names, and join with nearest_plant
      location_names <- data.table(read.csv(user_inputs[variable=="power_plant_location_code_filepath",value]))
      setkey(location_names,OID)
      nearest_plant <- location_names[nearest_plant]
      
      # Trim unneeded columns, and re-name
      nearest_plant[,':='(OID=NULL,Status=NULL, MW_Nmpl=NULL, Prjct_T=NULL, City=NULL, County=NULL)]
      setnames(nearest_plant,"Name","plant_location")
      
      # Round coordinates
      nearest_plant[,':='(x=round(x,digits=1),y=round(y,digits=1))]
      
      # Combine with road_to_plant
      setkey(road_to_plant,x,y)
      setkey(nearest_plant,x,y)
      road_to_plant <- road_to_plant[nearest_plant]
    } else {
      road_to_plant[,plant_location:=user_inputs[variable=="power_plant_location",value]]
    }
    case.data <- road_to_plant[case.data]
    
    # Now to determine equipment. This will vary depending on whther the harvest is high or low volume; this will be determined through average cell residue density
    
    #######################################################################################################################################
    # Calculate harvest/processing emissions, and adjust mass totals.
    #######################################################################################################################################
    high_volume_cell_threshold_density <- 13 # BDT/scre; remember that tons/acre is the unit for all of our residues.
    
    harvest_processing_output <- harvest_processing_fun(case.data, high_volume_cell_threshold_density)
    # 3 outputs in harvest_processing_output:
    # harvest_processing_output[[1]] : Collection & Processing emissions
    # harvest_processing_output[[2]]: Transportation emissions
    case.data <- harvest_processing_output[[3]]
    
    # The mass_to_plant is the field residue removed. By dividing that mass by the total study land area (nrow(case.data) * cell_to_acres) we get the tonnes/acre
    year_0_mass_tracking[,':='(field.residue.removed_tonnes = sum(case.data[,mass_to_plant_tonnesAcre]) * cell_to_acres,
                               total.biomass.mobilized_tonnesAcre = sum(case.data[,mass_to_plant_tonnesAcre]) / (nrow(case.data) * cell_to_acres))]
    
    # Harvesting and power plant emissions are given in kg, we need them in tonnes
    year_0_emissions_tracking[,':='(
      collection.processing.diesel.CO2_tonnes = harvest_processing_output[[1]]$CO2_kg/1000, 
      collection.processing.diesel.CO_tonnes = harvest_processing_output[[1]]$CO_kg/1000, 
      collection.processing.diesel.N2O_tonnes = harvest_processing_output[[1]]$N2O_kg/1000,
      collection.processing.diesel.CH4_tonnes = harvest_processing_output[[1]]$CH4_kg/1000, 
      collection.processing.diesel.NOx_tonnes = harvest_processing_output[[1]]$NOx_kg/1000, 
      collection.processing.diesel.PMUnder10um_tonnes = harvest_processing_output[[1]]$PMUnder10um_kg/1000, 
      collection.processing.diesel.PMUnder2.5um_tonnes = harvest_processing_output[[1]]$PMUnder2.5um_kg/1000, 
      collection.processing.diesel.SO2_tonnes = harvest_processing_output[[1]]$SOx_kg * equipment_SO2_SOx_fraction / 1000, 
      collection.processing.diesel.VOC_tonnes = harvest_processing_output[[1]]$VOC_kg/1000, 
      
      transportation.diesel.CO2_tonnes = harvest_processing_output[[2]]$CO2_kg/1000, 
      transportation.diesel.CO_tonnes = harvest_processing_output[[2]]$CO_kg/1000, 
      transportation.diesel.N2O_tonnes = harvest_processing_output[[2]]$N2O_kg/1000,
      transportation.diesel.CH4_tonnes = harvest_processing_output[[2]]$CH4_kg/1000, 
      transportation.diesel.NOx_tonnes = harvest_processing_output[[2]]$NOx_kg/1000, 
      transportation.diesel.PMUnder10um_tonnes = harvest_processing_output[[2]]$PMUnder10um_kg/1000, 
      transportation.diesel.PMUnder2.5um_tonnes = harvest_processing_output[[2]]$PMUnder2.5um_kg/1000, 
      transportation.diesel.SO2_tonnes = harvest_processing_output[[2]]$SOx_kg * equipment_SO2_SOx_fraction / 1000, 
      transportation.diesel.VOC_tonnes = harvest_processing_output[[2]]$VOC_kg/1000
    )]
    
    #######################################################################################################################################
    # Calculate power plant emissions.
    #######################################################################################################################################
    power_emission_factors <- data.table(read.csv(user_inputs[variable=='power_plant_emissions_filepath',value]))
    
    if(user_inputs[variable=='power_plant_location',value]=='Nearest') { # Multiple potential power plant locations
      power_plant_output <- power_plant_fun(case.data, power_emission_factors, unique(case.data[,plant_location]))
    } else { # Single power plant location
      power_plant_output <- power_plant_fun(case.data, power_emission_factors, user_inputs[variable=='power_plant_location',value])
    }
    
    # power_plant_output has 2 items: (will probably add a third for heat production)
    # power_plant_output[[1]]: power plant emissions
    # power_plant_output[[2]]: power plant enery production
    
    year_0_mass_tracking[,':='(
      power.plant.waste_tonnes = power_plant_output[[1]]$char_kg/1000, # Include ash?
      residue.burned.to.electricity_tonnes = sum(case.data[,mass_to_plant_tonnesAcre]),
      residue.burned.to.heat_tonnes = 0 # Discuss this with Jerome and figure out
    )]
    
    year_0_emissions_tracking[,':='(
      power.plant.for.electricity.CO2_tonnes = power_plant_output[[1]]$CO2_kg/1000, 
      power.plant.for.electricity.CO_tonnes = power_plant_output[[1]]$CO_kg/1000, 
      power.plant.for.electricity.CH4_tonnes = power_plant_output[[1]]$CH4_kg/1000, 
      power.plant.for.electricity.NOx_tonnes = power_plant_output[[1]]$NOx_kg/1000,
      power.plant.for.electricity.N2O_tonnes = power_plant_output[[1]]$N2O_kg/1000,
      power.plant.for.electricity.PMUnder10um_tonnes = power_plant_output[[1]]$PMUnder10um_kg/1000, 
      power.plant.for.electricity.PMUnder2.5um_tonnes = power_plant_output[[1]]$PMUnder2.5um_kg/1000, 
      power.plant.for.electricity.SO2_tonnes = power_plant_output[[1]]$SOx_kg + plant_SO2_SOx_fraction / 1000, 
      power.plant.for.electricity.VOC_tonnes = power_plant_output[[1]]$VOC_kg/1000,
      # We need to figure out the CHP process. Leave these as 0 for now.
      power.plant.for.heat.CH4.offset_tonnes = 0,
      
      powerplant.energy.production_MWh = power_plant_output[[2]],
      # Gotta figure out CHP
      powerplant.energy.production_MMBtu = 0
    )]
  }
  
  for (year.i in 1:100) {
    print(year.i)
    
    mass_tracking[year.i,':='(
      In.field.non.char.scattered_tonnes = case.data[,sum(Scattered_CWD_tonnesAcre) + sum(Scattered_FWD_tonnesAcre) + sum(Scattered_Foliage_tonnesAcre) + sum(Duff_tonnesAcre)],
      In.field.non.char.piled_tonnes = case.data[,sum(Piled_CWD_tonnesAcre) + sum(Piled_FWD_tonnesAcre) + sum(Piled_Foliage_tonnesAcre)]
    )]
    
    # A few things will happen only in year 1 (if at all): residues collection (and processing, transportations, and power plant use), and prescribed burning.
    # Harvesting, processing, transportation, and power plant emissions will have been addressed already, because there is an easy variable (biomass_collection)
    # to differentiate when this does or does not happen. Calculate emissions and mass loss from these activities before decay/wildfire. Note: prescribed burn and
    # residue collection will affect different residue segments, so it does not matter which comes first.
    if(year.i==1) {
      
      mass_tracking[year.i,':='(
        In.field.char.scattered_tonnes = 0,
        In.field.char.piled_tonnes = 0
      )]
      
      # Step one: figure out the emission factors, combustion fractions, and char fractions
      case.data <- prescribed_burn_processing_fun(case.data,user_inputs[variable=="wildfire_data_directory",value],scenario_ID,tile_list)
      
      # Make sure that the residue segment matches correctly with the prescribed burn method
      
      if(user_inputs[variable=='burn_type',value] == "Broadcast") {
        prescribed_burn_output <- prescribed_burn_fun(case.data, user_inputs[variable=='burn_type',value])
        
        # 2 items from prescribed_burn_output:
        # prescribed_burn_output[[1]]: Prescribed burn emissions
        # prescribed_burn_output[[2]]: Updated case.data
        
        # Update the case data with prescribed_burn_output[[2]]
        case.data <- prescribed_burn_output[[2]]
        
        # Update the emissions data with prescribed_burn_output[[1]]
        year_0_emissions_tracking[year.i,':='(
          broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
          broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
          broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
          broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
          broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
          broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
          broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
          broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
          broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
        )]
        
        # Add char production from this year to the next year's start-of-year char mass
        mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[1]]$char_tonnes]
      }
      if(user_inputs[variable=='burn_type',value] == "Pile") {
        warning("Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in version 1.2")
        prescribed_burn_output <- prescribed_burn_fun(case.data, user_inputs[variable=='burn_type',value])
        
        # 2 items from prescribed_burn_output:
        # prescribed_burn_output[[1]]: Prescribed burn emissions
        # prescribed_burn_output[[2]]: Updated case.data
        
        # Update the case data with prescribed_burn_output[[2]]
        case.data <- prescribed_burn_output[[2]]
        
        # Update the emissions data with prescribed_burn_output[[1]]
        year_0_emissions_tracking[year.i,':='( 
          pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
          pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
          pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
          pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
          pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
          pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
          pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
          pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
          pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
        )]
        
        # Add char production from this year to the next year's start-of-year char mass
        mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[1]]$char_tonnes]
      }
      if(user_inputs[variable=='burn_type',value] == "Pile and Broadcast") {
        prescribed_burn_output <- prescribed_burn_fun(case.data, "Broadcast")
        
        # 2 items from prescribed_burn_output:
        # prescribed_burn_output[[1]]: Prescribed burn emissions
        # prescribed_burn_output[[2]]: Updated case.data
        
        # Update the case data with prescribed_burn_output[[2]]
        case.data <- prescribed_burn_output[[2]]
        
        # Update the emissions data with prescribed_burn_output[[1]]
        year_0_emissions_tracking[year.i,':='(
          broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
          broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
          broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
          broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
          broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
          broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
          broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
          broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
          broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
        )]
        
        # Add char production from this year to the next year's start-of-year char mass
        mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[1]]$char_tonnes]
        
        warning("Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in version 1.2")
        # Because of this transmutation, pile burning has to happen after scattered debris, else it will be burned twice.
        prescribed_burn_output <- prescribed_burn_fun(case.data, "Pile")
        
        # 2 items from prescribed_burn_output:
        # prescribed_burn_output[[1]]: Prescribed burn emissions
        # prescribed_burn_output[[2]]: Updated case.data
        
        # Update the case data with prescribed_burn_output[[2]]
        case.data <- prescribed_burn_output[[2]]
        
        # Update the emissions data with prescribed_burn_output[[1]]
        year_0_emissions_tracking[year.i,':='( 
          pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
          pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
          pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
          pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
          pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
          pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
          pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
          pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
          pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
        )]
        
        # Add char production from this year to the next year's start-of-year char mass
        mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[1]]$char_tonnes]
      }
      
      # Clear out the combustion/char fractions and emissions factors; they will be re-added with wildfire data.
      case.data[,':='(CWD_Scattered_CombustionFrac = NULL,FWD_Scattered_CombustionFrac = NULL, Foliage_Scattered_CombustionFrac = NULL, Duff_Scattered_CombustionFrac = NULL, CWD_Scattered_CharFrac = NULL, FWD_Scattered_CharFrac = NULL, Duff_Scattered_CH4_EmFac = NULL, Foliage_Scattered_CH4_EmFac = NULL, FWD_Scattered_CH4_EmFac = NULL, CWD_Scattered_CH4_EmFac = NULL, Piled_CH4_EmFac = NULL, Duff_Scattered_CO_EmFac = NULL, Foliage_Scattered_CO_EmFac = NULL, FWD_Scattered_CO_EmFac = NULL, CWD_Scattered_CO_EmFac = NULL, Piled_CO_EmFac = NULL, Duff_Scattered_NOx_EmFac = NULL, Foliage_Scattered_NOx_EmFac = NULL, FWD_Scattered_NOx_EmFac = NULL, CWD_Scattered_NOx_EmFac = NULL, Piled_NOx_EmFac = NULL, Duff_Scattered_PM10_EmFac = NULL, Foliage_Scattered_PM10_EmFac = NULL, FWD_Scattered_PM10_EmFac = NULL, CWD_Scattered_PM10_EmFac = NULL, Piled_PM10_EmFac = NULL, Duff_Scattered_PM2.5_EmFac = NULL, Foliage_Scattered_PM2.5_EmFac = NULL, FWD_Scattered_PM2.5_EmFac = NULL, CWD_Scattered_PM2.5_EmFac = NULL, Piled_PM2.5_EmFac = NULL, Duff_Scattered_SO2_EmFac = NULL, Foliage_Scattered_SO2_EmFac = NULL, FWD_Scattered_SO2_EmFac = NULL, CWD_Scattered_SO2_EmFac = NULL, Piled_SO2_EmFac = NULL, Duff_Scattered_VOC_EmFac = NULL, Foliage_Scattered_VOC_EmFac = NULL, FWD_Scattered_VOC_EmFac = NULL, CWD_Scattered_VOC_EmFac = NULL, Piled_VOC_EmFac = NULL)] 
    }
    
    # Figure out the emission factors, combustion fractions, and char fractions for wildfire.
    case.data <- annual_fire_detail_fun(case.data,year.i,wildfire_data,user_inputs[variable=="wildfire_data_directory",value],study_area_raster)
    
    #################################
    # Decay
    #################################
    # Calculate the total decayed mass. "Previously Fired" must be a category; addressing previously fired material decay in each segment will run that decay 3 times over.
    decay_output <- decay_fun(case.data,residue.disposition = "Scattered",year.i)
    # 2 items from decay_output:
    # decay_output[[1]]: Decay emissions
    # decay_output[[2]]: Updated case.data
    case.data <- decay_output[[2]]
    mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    emissions_tracking[year.i,':='(
      decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
      decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    )]
    
    decay_output <- decay_fun(case.data,residue.disposition = "Piled",year.i)
    # 2 items from decay_output:
    # decay_output[[1]]: Decay emissions
    # decay_output[[2]]: Updated case.data
    case.data <- decay_output[[2]]
    mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    emissions_tracking[year.i,':='(
      decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
      decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    )]
    
    decay_output <- decay_fun(case.data,residue.disposition = "prev_fired",year.i)
    # 2 items from decay_output:
    # decay_output[[1]]: Decay emissions
    # decay_output[[2]]: Updated case.data
    case.data <- decay_output[[2]]
    mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    emissions_tracking[year.i,':='(
      decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
      decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    )]
    
    # Repeat for duff. Because we only run duff decay once, we can address both previously fired and non-fired duff.
    decay_output <- duff_decay_fun(case.data,year.i)
    # 3 items from duff decay_output:
    # decay_output[[1]]: Decay emissions
    # decay_output[[2]]: Updated case.data
    # decay_output[[3]]: duff mass/acres decayed in year.i
    case.data <- decay_output[[2]]
    mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[3]] * cell_to_acres]
    emissions_tracking[year.i,':='(
      decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
      decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    )]
    
    #################################
    # Wildfire
    #################################
    # Address wildfire emissions and mass loss. 
    wildfire_output <- annual_wildfire_fun(case.data,residue.disposition = "Scattered",year.i)
    # 2 items from wildfire_output:
    # wildfire_output[[1]]: Wildfire emissions
    # wildfire_output[[2]]: Updated case.data
    case.data <- wildfire_output[[2]]
    
    # Add the exposed-to-fire mass to wildfire.burned.residue
    mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + sum(fire_exposed_FWD_mass_year_i) + sum(fire_exposed_Foliage_mass_year_i)] * cell_to_acres]
    
    # Add char produced this year to the starting char for the next year. Skip for year 100.
    if(year.i < 100) {
      mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + wildfire_output[[1]]$char_tonnes]
    }
    
    emissions_tracking[year.i,':='(
      wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
      wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
      wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
      wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
      wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
      wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
      wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
      wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
      wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    )]
    
    if(year.i==1|year.i==100) {warning("ALL pile residues remaining after wildfire are added to COARSE previously fired debris - a fire model inconsistency we should fix in version 1.2")}
    wildfire_output <- annual_wildfire_fun(case.data,residue.disposition = "Piled",year.i)
    # 2 items from wildfire_output:
    # wildfire_output[[1]]: Wildfire emissions
    # wildfire_output[[2]]: Updated case.data
    case.data <- wildfire_output[[2]]
    
    # Add the exposed-to-fire mass to wildfire.burned.residue
    mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + sum(fire_exposed_FWD_mass_year_i) + sum(fire_exposed_Foliage_mass_year_i)] * cell_to_acres]
    
    # Add char produced this year to the starting char for the next year. Skip for year 100.
    if(year.i < 100) {
      mass_tracking[year.i+1, In.field.char.piled_tonnes := In.field.char.piled_tonnes + wildfire_output[[1]]$char_tonnes]
    }
    
    emissions_tracking[year.i,':='(
      wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
      wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
      wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
      wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
      wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
      wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
      wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
      wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
      wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    )]
    
    wildfire_output <- duff_annual_wildfire_fun(case.data,year.i)
    # 2 items from wildfire_output:
    # wildfire_output[[1]]: Wildfire emissions
    # wildfire_output[[2]]: Updated case.data
    case.data <- wildfire_output[[2]]
    
    # Add the exposed-to-fire mass to wildfire.burned.residue
    mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_Duff_mass_year_i)] * cell_to_acres]
    
    emissions_tracking[year.i,':='(
      wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
      wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
      wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
      wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
      wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
      wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
      wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
      wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
      wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    )]
    
    # Clear out the combustion/char fractions and emissions factors; they will be re-added next year.
    case.data[,':='(Wildfire_Probability = NULL, CWD_Scattered_CombustionFrac = NULL, FWD_Scattered_CombustionFrac = NULL, Foliage_Scattered_CombustionFrac = NULL, Duff_Scattered_CombustionFrac = NULL, 
                    CWD_Scattered_CharFrac = NULL, FWD_Scattered_CharFrac = NULL, 
                    Duff_Scattered_CH4_EmFac = NULL, Foliage_Scattered_CH4_EmFac = NULL, FWD_Scattered_CH4_EmFac = NULL, CWD_Scattered_CH4_EmFac = NULL, Piled_CH4_EmFac = NULL,
                    Duff_Scattered_CO_EmFac = NULL, Foliage_Scattered_CO_EmFac = NULL, FWD_Scattered_CO_EmFac = NULL, CWD_Scattered_CO_EmFac = NULL, Piled_CO_EmFac = NULL,
                    Duff_Scattered_NOx_EmFac = NULL, Foliage_Scattered_NOx_EmFac = NULL, FWD_Scattered_NOx_EmFac = NULL, CWD_Scattered_NOx_EmFac = NULL, Piled_NOx_EmFac = NULL, 
                    Duff_Scattered_PM10_EmFac = NULL, Foliage_Scattered_PM10_EmFac = NULL, FWD_Scattered_PM10_EmFac = NULL, CWD_Scattered_PM10_EmFac = NULL, Piled_PM10_EmFac = NULL, 
                    Duff_Scattered_PM2.5_EmFac = NULL, Foliage_Scattered_PM2.5_EmFac = NULL, FWD_Scattered_PM2.5_EmFac = NULL, CWD_Scattered_PM2.5_EmFac = NULL, Piled_PM2.5_EmFac = NULL,
                    Duff_Scattered_SO2_EmFac = NULL, Foliage_Scattered_SO2_EmFac = NULL, FWD_Scattered_SO2_EmFac = NULL, CWD_Scattered_SO2_EmFac = NULL, Piled_SO2_EmFac = NULL, 
                    Duff_Scattered_VOC_EmFac = NULL, Foliage_Scattered_VOC_EmFac = NULL, FWD_Scattered_VOC_EmFac = NULL, CWD_Scattered_VOC_EmFac = NULL, Piled_VOC_EmFac = NULL)] 
  }
  
  # Save data - with harvest
  saveRDS(list("mass tracking" = mass_tracking, "emissions tracking" = emissions_tracking, "year 0 mass tracking" = year_0_mass_tracking, "year 0 emissions tracking" = year_0_emissions_tracking),file=paste("/Volumes/SharedData/Main Folder - Andy H/CBREC_output_directory/",scenario_ID_num,".rds",sep=""))
}