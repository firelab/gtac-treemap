# Create input disturbance layer using LCMS data

# Written by: Lila Leatherman (lila.leatherman@usda.gov)
# Redcastle Resources and USFS Geospatial Technology and Applications Center (GTAC)

# Last updated: 3/13/24

# Input rasters: 
# - Annual raw probability of slowloss, fast loss, and gain from LCMS
# - Annual Landfire disturbance 

# Output rasters
# - lcms slow loss years
# - lcms slow loss binary

# TO DO: Add progress function to loop over LCMS slow loss years

###############################
# Set Inputs
###############################

# breakup factor - how many tiles to break the area into? as a factor of area px 
# 1 = 1 tile, 5 = many tiles
break.up <- 4

# get path to inputs script
this_dir <- this.path::this.dir()
inputs_script <- glue::glue('{this_dir}/00b_setup_targetdata.R')

source(inputs_script)

# Parallelization settings
#--------------------------------------#

# set number of cores used for parallelization
ncores <- 5

# set up dopar
cl <- makeCluster(ncores, outfile = glue::glue("{tmp_dir}/cl_report.txt"))
registerDoParallel(cl)
#registerDoSEQ() # option to register sequentially - for testing

# load packages to each cluster
clusterCall(cl, function(){
  library(tidyverse);
  library(magrittr);
  #library(glue);
  library(terra);
  library(doParallel);
  library(foreach)
  })


###################################################
# LOAD DATA
###################################################

# load lcms projections
lcms_crs <- crs(lcms_proj)

#load landfire projection
landfire_crs <- crs(landfire_proj)

# load LF zone data
LF_zones <- vect(lf_zones_path)

###################################################
# WORK ON ZONE
##################################################

tic()
# Prep zone
#-----------------------------------------#

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone %<>%
  terra::project(lcms_crs)

# get name of zone
zone_name <- glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')


# Optional subset
#---------------------------------------#

if (!is.na(aoi_path)) {
  # load aoi subset - utah uintas only
  aoi <- vect(aoi_path) %>%
    project(lcms_crs)

  # reassign
  zone <- aoi
  zone_name <- aoi_name
  print("using input shapefile as AOI")
} else{
  print("using landfire zone as AOI")
}

# Final zone prep
#---------------------------------------------#

# set aoi_name field if it doesn't already exist via aoi subset
if(is.na(aoi_name)) {
  aoi_name <- ""
}

# rasterize zone
zone_r <- terra::rast(terra::ext(zone), crs = lcms_crs, resolution = 30)
zone_r <- terra::rasterize(zone, zone_r)

#####################################################
# PREP LCMS SLOW LOSS
####################################################

# Make tiles
#----------------------------------------------------#

# bookkeeping
print("preparing LCMS slow loss")

# how big is the zone? 
# maybe: if zone > size, then tile
expanse <- terra::expanse(zone, unit = "km")

# break up raster into multiple sections to speed up processing
h <- base::ceiling(ncol(zone_r)/break.up)
v <- base::ceiling(nrow(zone_r)/break.up)

# aggregate - template for making tiles to divvy up zone
agg <- terra::aggregate(zone_r, fact = c(h,v), na.rm = TRUE)
agg[] <- 1:ncell(agg)

# inspect zones
plot(agg, alpha = 0.5)
plot(zone, add = TRUE)

# subset the raster and create temporary files
# tiles with only NA values are omitted
# the function returns file names for the temporary files
tiles <- zone_r %>%
  terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)

# Convert raw probability layers into change layers
# Loop over tiles, and within tiles, loop over years
#---------------------------------------------------------------#

# foreach loop dopar over tiles
f <- foreach(i = 1:length(tiles),
             .packages= c("tidyverse", "terra", "doParallel", "foreach")
) %dopar% {
  
  # for testing
  #i = 1
  
  fn <- tiles[i]
  
  # read raster tile into memory
  tile_r <- terra::rast(fn) %>%
    terra::trim()
  
  # foreach loop do over years
  slowloss_tile <- foreach(j = 1:length(year_list),
               .combine = 'c',
               .packages = c("tidyverse", "terra")
  ) %do% {
    
  
    # for testing
    #j = 1
  
    year <- year_list[j]
    
    # bookkeeping
    #print(glue("working on {year}"))
  
    # list raw probability tile layers for a given year
    year_files <- list.files(lcms_dir, pattern = paste0(year, '.+.tif$'), full.names = TRUE)

    # prep raw probability files
    raw_prob <- lapply(year_files, terra::rast) %>%
      sprc() %>% # convert to packed raster collection
      terra::crop(tile_r) %>% # crop to zone
      terra::mosaic() # mosaic
    
    # make sure extents align for masking
    tile_r %<>% terra::crop(raw_prob) %>%
      terra::resample(raw_prob)  
    
    # finish prepping raw probability files 
    raw_prob %<>% terra::mask(tile_r) %>% # mask 
      terra::classify(cbind(LCMS_NAvalue, NA)) # update NA values
    
    gc()
    
    # add layer names for clarity
    # names(raw_prob) <- c( "FastLoss_Raw_Prob", "SlowLoss_Raw_Prob", "Gain_Raw_Prob")
    
    
    # #inspect
    # rbind(freq(raw_prob[[1]], value = NA),
    #       freq(raw_prob[[1]]))
    # plot(raw_prob)

    # prepare non-processing area mask
    NPArea_mask <-
      raw_prob %>%
      min()
  
    # reclassify classes: values below threshold go to NA
    for(k in 1:3) {
      
      raw_prob[[k]] <- terra::classify(raw_prob[[k]], cbind(seq(1, LCMS_change_thresholds[k], 1), NA))
      
      gc()
    }
    
    # get highest probability class
    maxProbClass <-
      raw_prob %>%
      which.max() %>% # get index of highest value that exceeds threshold
      terra::classify(cbind(c(1,3), NA)) %>% # reclass to only slow loss - value = 2
      mask(NPArea_mask) # mask with NP area mask
  
    # name to year 
    names(maxProbClass) <- year
    
    #inspect
    # maxProbClass
    # rbind(freq(maxProbClass),
    #            freq(maxProbClass, value = NA))
    # plot(maxProbClass)
  
    #remove unused files
    rm(raw_prob, NPArea_mask)
    
    # clear memory
    gc()
    
    # return to do loop
    maxProbClass
  
  } # end foreach do over years
  
  # Get most recent year of slow loss
  #-----------------------------------------------#
  
  # get most recent year of slow loss
  slowloss_tile <- 
    terra::app(slowloss_tile, which.max.hightie)     %>% # identify year with maximum value; ties go to highest index
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) %>% # reclassify index values to years
    terra::project(landfire_crs) # reproject to desired crs
    
  # write out single tile as tmp file (then read all in later as .vrt)
  terra::writeRaster(slowloss_tile,
          filename = paste0(tmp_dir, "/lcms/slowloss_years_tile", i, ".tif"),
          datatype = "INT2U",
          overwrite = TRUE)
  
  gc()

  } # end foreach over tiles
  
stopCluster(cl)

# Assemble LCMS slow loss
# -----------------------------------#

# List year files
lcms_files <- list.files(path = glue::glue('{tmp_dir}/lcms/'), 
                         full.names = TRUE)

# Read in as vrt
lcms_slowloss_years <- terra::vrt(lcms_files,  glue('{tmp_dir}/lcms_slowloss.vrt'), overwrite = TRUE)

#inspect
#lcms_slowloss_years
#plot(lcms_slowloss_years)


# convert to binary indicator of slow loss
# slow loss value for input to TreeMap disturbance layer = 2
lcms_slowloss_binary <-
  lcms_slowloss_years %>%
  terra::classify(cbind(year_list, 2))

gc()

# inspect
# lcms_slowloss
# freq(lcms_slowloss)
plot(lcms_slowloss_years)
plot(lcms_slowloss_binary)

# Export
#----------------------------#

writeRaster(lcms_slowloss_years, 
            lcms_slowloss_years_outpath, 
            datatype = "INT2U",
            overwrite = TRUE)

writeRaster(lcms_slowloss_binary, 
            lcms_slowloss_binary_outpath, 
            datatype = "INT2U",
            overwrite = TRUE)




