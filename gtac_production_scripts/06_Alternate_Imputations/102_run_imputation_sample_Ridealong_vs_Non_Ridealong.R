# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"

# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 9/17/24

# This script accomplishes the following tasks: 
# - Run imputation over provided input area and target rasters
# - Save outputs as raster tiles


###########################################################################
# Set inputs
###########################################################################

# Parallelization settings
#----------------------------------------#
# Number of cores
ncores <- 35

# Tiling settings
# ---------------------------------------#
# set dimensions of tile - value is the length of one side of a tile, in pixels
tile_size <- 2000

# Select tiles to run
# if NA, defaults to all tiles in list
#----------------------------------------#
which_tiles <- NA

####################################################################
# Load data
####################################################################

message("loading data")

# Load imputation model
# ---------------------------------------------------------- #

#load models
yai <- readr::read_rds(model_path)
yai_non_ridealong <- readr::read_rds(model_path_no_ridealong_fix)


# get names of variables included in model
model_vars <- names(yai$xRefs)
model_vars %<>% str_subset("point_", negate = TRUE) %>% sort() # remove point_x and point_y as these are calculated tile by tile in the imputation step

# Load target rasters
# ---------------------------------------------------------- #

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to target rasters of interest
flist_tif <- filter_disturbance_rasters(flist_tif, dist_layer_type) # custom function

# remove aspect if it's present 
flist_tif %<>%
  str_subset("aspect", negate = TRUE)

# load rasters using custom function
rs2 <- load_and_name_rasters(flist_tif)

# Prep binary disturbance code layer
# ---------------------------------------------------------- #
# Reclass disturbance to binary
rs2$disturb_code_bin <- terra::classify(rs2$disturb_code, cbind(2, 1))
names(rs2$disturb_code_bin) <- "disturb_code_bin"
varnames(rs2$disturb_code_bin) <- "disturb_code_bin"

# remove original disturb code - model runs with binary
rs2$disturb_code <- NULL

# subset layers to vars present in model
rs2 <- subset(rs2, model_vars)

# add XY coords to raster
rs2$actual_lat <- terra::init(rs2, "y")
names(rs2$actual_lat) <- "actual_lat"
rs2$actual_lon<- terra::init(rs2, "x")
names(rs2$actual_lon) <- "actual_lon"
#


# Work with the raster stack to generate a random sample of pixels to impute on -----

# convert raster to matrix and dataframe
mat <- as.matrix(rs2)
mat_df<- as.data.frame(mat)

# Find valid rows in matrix
mat_valid_ids<- which(!rowSums(!is.finite(mat)))

#
# Randomly select 100,000 pixels, or full number of pixels in zone, whichever is lower
set.seed(12345)
n_pixels_to_select<- min(100000, length(mat_valid_ids))
pixels_to_select<- sample(x=mat_valid_ids, size=n_pixels_to_select, replace=F)

# Keep only the randomly selected pixels
mat_df_random_sample<- mat_df[pixels_to_select,]




# Prep for imputation on the random sample ----

#give dat the name we use in this function
extract.currow <- data.frame(mat_df_random_sample)

#### Get dimensions of current row
colseq <- 1:length(extract.currow[,1])
valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
ncols.df <- dim(extract.currow)[2]

# Remove NAs
extract.currow <- na.exclude(extract.currow)

# convert to data frame
X.df.temp <- data.frame(extract.currow)

# Set up template for output of imputation
nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
nc.orig <-length(colseq) # number of values in row, including NAs

# Default output from imputation - all NAs 
impute.out <- rep(NA,nc.orig) 

# Get data from yai
#-----------------------------------------------#
id.table <- as.numeric(row.names(yai$xRefs))
maxrow <- max(id.table)

# EVG handling - 
#### Identify EVGs in zone that don't appear in X.df   
#-------------------------------------------------------#

evg.orig <- levels(yai$xRefs$evt_gp)
evg.val.temp <- X.df.temp$evt_gp  
n.evgs.orig <- length(sort(unique(evg.orig)))  

nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
n.dummy.rows <- length(nonappearing.evgs)  

# Create dummy rows for non-appearing EVGs
# Question: are dummy rows necessary? 
if(n.dummy.rows > 0)
{    
  dummy.rows <- X.df.temp[1:n.dummy.rows,]    
  tempchar <- as.character(X.df.temp$evt_gp)    
  X.df.temp$evt_gp <- tempchar    
  dummy.rows$evt_gp <- as.character(nonappearing.evgs)
  X.df.temp <- rbind(X.df.temp, dummy.rows)    
}





# Set factor levels
#make sure they match input factor levels in reference data used in model
#-------------------------------------------------------#
names(X.df.temp)[names(X.df.temp) == "actual_lat"]<- "point_y"
names(X.df.temp)[names(X.df.temp) == "actual_lon"]<- "point_x"

X.df.temp <- 
  X.df.temp %>%
  dplyr::mutate(evt_gp_remap = factor(evt_gp_remap, levels = levels(yai$xRefs$evt_gp_remap)),
                disturb_code_bin = factor(disturb_code_bin, levels = levels(yai$xRefs$disturb_code_bin))) %>%
  # put columns in order expected
  dplyr::select(names(yai$xRefs))

# Format row names for X.df.temp - cannot overlap with input X.df
colseq.out <- 1:dim(X.df.temp)[1]
rownames.all <- colseq.out+maxrow
rownames(X.df.temp) <- paste0("T- ", rownames.all)





### Perform imputation with ridealong fix ----

# take object from formed random forests model and use X.df.temp dataframe to make predictions
temp.newtargs <- yaImpute::newtargets(yai, newdata = X.df.temp)

#### Get outputs of interest
out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).

#### Format outputs into imputation results
yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell
impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation
#
imputation_with_ridealong_fix<- impute.out

print("Finished running random sample imputation with ridealong fix")


### Perform imputation without ridealong fix ----

# take object from formed random forests model and use X.df.temp dataframe to make predictions
temp.newtargs <- yaImpute::newtargets(yai_non_ridealong, newdata = X.df.temp)

#### Get outputs of interest
out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).

#### Format outputs into imputation results
yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell
impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation
#
imputation_without_ridealong_fix<- impute.out

print("Finished running random sample imputation without ridealong fix")

gc()



# Make spatial points from the randomly selected pixels, for accuracy calculations against the imputations
x_pts <- terra::vect(mat_df_random_sample, geom = c("actual_lon", "actual_lat"), crs = crs(rs2))
# remove data
x_pts<- x_pts[,-c(1:ncol(x_pts))]

# Now load EVC, EVH, EVG, DC rasters, and extract these onto x_pts
evc<- rs2$evc
evh<- rs2$evh
evt_gp_remap<- rs2$evt_gp_remap
disturb_code_bin<- rs2$disturb_code_bin



# Extract onto points
x_pts$evc<- terra::extract(evc,x_pts,method="simple")[,2]
x_pts$evh<- terra::extract(evh,x_pts,method="simple")[,2]
x_pts$evt_gp_remap<- terra::extract(evt_gp_remap,x_pts,method="simple")[,2]
x_pts$disturb_code_bin<- terra::extract(disturb_code_bin,x_pts,method="simple")[,2]


# Convert to data frame
x_df<- as.data.frame(x_pts)


# read in the evg remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path) 

# join the raw EVT Group
x_df<- left_join(x_df, evt_gp_remap_table , by=c("evt_gp_remap" = "EVT_GP_remap"))




# Get lookup for both imputations -----
# First work with the plot.df
plot.df<- read.csv(xtable_path_model)
plot.df$ID<- as.numeric(plot.df$tm_id)
#
eval_vars<- c("evc", "evh", "evt_gp_remap", "disturb_code_bin")
eval_vars_full_names<- c("evc", "evh", "evt_gp_remap", "disturb_code_bin")

# Create lookup table that only has IDs present in zone

# With Ridealong Fix
id_list_with_ridealong_fix<- as.data.frame(imputation_with_ridealong_fix)
names(id_list_with_ridealong_fix)<- "PLOTID"

imputed_vars_with_ridealong <- left_join(id_list_with_ridealong_fix, plot.df, by = c("PLOTID" = "ID")) %>%
  dplyr::select(PLOTID, CN, all_of(eval_vars)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))


# Without Ridealong Fix
id_list_without_ridealong_fix<- as.data.frame(imputation_without_ridealong_fix)
names(id_list_without_ridealong_fix)<- "PLOTID"

imputed_vars_without_ridealong <- left_join(id_list_without_ridealong_fix, plot.df, by = c("PLOTID" = "ID")) %>%
  dplyr::select(PLOTID, CN, all_of(eval_vars)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))



# Calculate Overall Accuracies

# With ridealong fix
with_ridealong_fix_accuracies<- data.frame("evc"=NA,
                                           "evh"=NA,
                                           "evt_gp"=NA,
                                           "dc_bin"=NA)
#
with_ridealong_fix_accuracies$evc<- length(which(imputed_vars_with_ridealong$evc == x_df$evc)) / n_pixels_to_select
with_ridealong_fix_accuracies$evh<- length(which(imputed_vars_with_ridealong$evh == x_df$evh)) / n_pixels_to_select
with_ridealong_fix_accuracies$evt_gp<- length(which(imputed_vars_with_ridealong$evt_gp_remap == x_df$evt_gp_remap)) / n_pixels_to_select
with_ridealong_fix_accuracies$dc_bin<- length(which(imputed_vars_with_ridealong$disturb_code_bin == x_df$disturb_code_bin)) / n_pixels_to_select



# Without ridealong fix
without_ridealong_fix_accuracies<- data.frame("evc"=NA,
                                           "evh"=NA,
                                           "evt_gp"=NA,
                                           "dc_bin"=NA)
#
without_ridealong_fix_accuracies$evc<- length(which(imputed_vars_without_ridealong$evc == x_df$evc)) / n_pixels_to_select
without_ridealong_fix_accuracies$evh<- length(which(imputed_vars_without_ridealong$evh == x_df$evh)) / n_pixels_to_select
without_ridealong_fix_accuracies$evt_gp<- length(which(imputed_vars_without_ridealong$evt_gp_remap == x_df$evt_gp_remap)) / n_pixels_to_select
without_ridealong_fix_accuracies$dc_bin<- length(which(imputed_vars_without_ridealong$disturb_code_bin == x_df$disturb_code_bin)) / n_pixels_to_select


# Now also calculate some overall stats

identical_imputation_rate<- length(which(imputation_with_ridealong_fix == imputation_without_ridealong_fix)) / length(imputation_with_ridealong_fix)
imputation_num_with_ridealong_fix<- length(unique(imputation_with_ridealong_fix))
imputation_num_without_ridealong_fix<- length(unique(imputation_without_ridealong_fix))


### Assemble all data wanted to save out -----
comparison_df<- data.frame("zone"=zone_input,
                           "with_fix_n"=imputation_num_with_ridealong_fix,
                           "without_fix_n"=imputation_num_without_ridealong_fix,
                           "identical_imputation_rate"=identical_imputation_rate,
                           "with_fix_evc_acc"= with_ridealong_fix_accuracies$evc,
                           "without_fix_evc_acc"= without_ridealong_fix_accuracies$evc,
                           "with_fix_evh_acc"= with_ridealong_fix_accuracies$evh,
                           "without_fix_evh_acc"= without_ridealong_fix_accuracies$evh,
                           "with_fix_evt_gp_acc"= with_ridealong_fix_accuracies$evt_gp,
                           "without_fix_evt_gp_acc"= without_ridealong_fix_accuracies$evt_gp,
                           "with_fix_binary_dc_acc"= with_ridealong_fix_accuracies$dc_bin,
                           "without_fix_binary_dc_acc"= without_ridealong_fix_accuracies$dc_bin)


# Save out the comparison dataframe, after making a directory to save results
dir.create(glue::glue("{assembled_dir}/03_Ridealong_Fix_Comparison"))

write.csv(comparison_df, glue::glue("{assembled_dir}/03_Ridealong_Fix_Comparison/accuracy_comparison_with_without_ridealong_fix.csv"), row.names = F, quote=F)




# garbage collection
gc()

