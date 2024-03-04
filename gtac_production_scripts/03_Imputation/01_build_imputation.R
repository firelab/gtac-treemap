# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 2/13/2024

# PART 1: 
# - BUILD x and y tables
# - save x and y tables
# - Build model for a zone
# - Save model validation

# TO DO: 
# - replace coords here (currently  buffered from FIA db) with full coords


##################################################
# Set inputs
###################################################

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-1))],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

##########################################################

# Load target rasters - just for metatada
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as raster stack
rs2 <- terra::rast(flist.tif[1])

# get raster names 
raster_names <- flist.tif[1] %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(rs2) <- raster_names

# Load X table
# ----------------------------------------------------------#

allplot <- read.csv(xtable_path)

# Load EVT Group Remap table
# ----------------------------------------------------------#

evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

####################################################################
# Prepare input data
####################################################################

# Convert plot coordinates to meters
#----------------------------------------------------------#
# NOTE: this is currently a stand-in. the allplot table has abbreviated records of the lat and long 
# (to 2 decimal places)

# convert allplot to spatial object
allplot_vect <- terra::vect(cbind(allplot$ACTUAL_LON, allplot$ACTUAL_LAT))

# set input projection
crs(allplot_vect) <- "epsg:4326"

# reproject to desired projection
allplot_vect %<>% terra::project(crs(rs2))

# extract lat/long in meters
allplot_xy <- terra::geom(allplot_vect) %>%
  data.frame() %>%
  dplyr::rename("POINT_X" = x,
                "POINT_Y" = y) %>%
  dplyr::select(POINT_X, POINT_Y)

# bind back with allplot table
allplot <- cbind(allplot, allplot_xy)


# Remap EVT Group
# ---------------------------------#

#Limit allplot to just the veg types in the remap table
plot.df <- allplot[allplot$EVT_GP %in% evt_gp_remap_table$EVT_GP,]

####Reclass evgs
n.evgs <- nrow(evt_gp_remap_table)

#reassign object to evg.reclass
evg.reclass <- evt_gp_remap_table %>%
  dplyr::select(EVT_GP, EVT_GP_remap)

#remap evgs using vector
evg.out <- rep(0, dim(plot.df)[1])
evg.vec <- plot.df$EVT_GP
for(i in 1:n.evgs)  
{  
  cur.evg <- evg.reclass[i, 1]  
  sub.ind <- evg.vec == cur.evg  
  evg.out[sub.ind] <- i  
}	

# re-assign EVT_GP
plot.df$EVT_GP <- evg.out

#Ensure plot.df variables are factors
#--------------------------------------#

plot.df %<>%
  mutate(EVT_GP = factor(EVT_GP))

# Re-calculate aspect - to northing and easting
#----------------------------------------------------------#
plot.df %<>%
  dplyr::mutate(radians = (pi/180)*ASPECT,
         NORTHING = cos(radians),
         EASTING = sin(radians)) %>%
  dplyr::select(-radians)

# Calculate binary disturbance code and convert to factor
#----------------------------------------------------------#
plot.df %<>%
  mutate(disturb_code = ifelse(disturb_code > 0, 1, disturb_code),
         disturb_code = factor(disturb_code))

# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X.df <- plot.df %>% dplyr::select(SLOPE, ELEV, PARI, PPTI, RELHUMI, TMAXI, TMINI, VPDI,
              disturb_code, disturb_year, canopy_cover, canopy_height, EVT_GP, NORTHING, EASTING,
              POINT_X, POINT_Y)

# add plot id as row names
rownames(X.df) <- plot.df$ID


# Create Y table (aka variables to be predicted)
Y.df <- plot.df %>%
  dplyr::select(canopy_cover, canopy_height, EVT_GP, disturb_code)

rownames(Y.df) <- plot.df$ID


# Export X and Y tables
# ------------------------------------------------------#

#create output directory
if(!file.exists(glue('{output_dir}/xytables'))){
  dir.create(glue('{output_dir}/xytables'))
}

write.csv(X.df, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_bin.csv'))
write.csv(Y.df, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_bin.csv'))


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH)
# -----------------------------------------------------------------------#
set.seed(56789)

tic()
yai.treelist.bin <- yai(X.df, Y.df, 
                        method = "randomForest", 
                        ntree = 250)
toc()

# Export model
write_rds(yai.treelist.bin, glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS'))


# Report model accuracy for Y variables (EVC, EVH, EVG)
# ------------------------------------------------------------------------#

#RF summary
RF_sum <- yaiRFsummary(yai.treelist.bin)

# Confusion matrices
cm_EVC <- yai.treelist.bin$ranForest$canopy_cover$confusion
cm_EVH <- yai.treelist.bin$ranForest$canopy_height$confusion
cm_EVT_GP <- yai.treelist.bin$ranForest$EVT_GP$confusion
cm_DC <- yai.treelist.bin$ranForest$disturb_code$confusion

# variable importance
varImp <- data.frame(RF_sum$scaledImportance)

# process variable importance table for plotting
varImp$outVar <- rownames(varImp)
rownames(varImp) <- NULL

varImp %<>% 
  tidyr::pivot_longer(1:ncol(varImp)-1, names_to = "var")

#plot variable importance
p <- varImp %>%
  ggplot()+
  geom_col(aes(x=var, y = value))+
  coord_flip()+
  facet_wrap(~outVar)+
  theme_bw() + 
  ggtitle(glue('RF Variable Importance for {cur.zone.zero}'))

#create output directory
if(!file.exists(glue('{output_dir}/eval'))){
  dir.create(glue('{output_dir}/eval'))
}


# export to file
ggsave(glue('{output_dir}/eval/{output_name}_varImp.png'), width = 7, height = 5)
write.csv(cm_EVC, glue('{output_dir}/eval/{output_name}_CM_canopyCover.csv'))
write.csv(cm_EVH, glue('{output_dir}/eval/{output_name}_CM_canopyHeight.csv'))
write.csv(cm_EVT_GP, glue('{output_dir}/eval/{output_name}_CM_EVT_Group.csv'))
write.csv(cm_DC, glue('{output_dir}/eval/{output_name}_CM_DisturbanceCode.csv'))

# clear unused memory
gc()

