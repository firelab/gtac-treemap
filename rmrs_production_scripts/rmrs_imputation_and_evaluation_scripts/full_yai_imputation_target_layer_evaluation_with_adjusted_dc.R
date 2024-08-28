library(terra)

# Load the landfire zones 
lf_zones<- vect("../landfire_mapzones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)
#lf_zone_nums<- lf_zone_nums[-which(lf_zone_nums==18)]


for (z in lf_zone_nums){
  
  print(paste0("Starting Zone ",z))
  
  setwd(this.path::here())
  
  library(yaImpute)
  library(raster)
  library(foreign)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(terra)
  library(tictoc)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(glue)
  library(ModelMetrics)
  library(randomForest)
  
  
  # Source the functions required
  source("./imputation_functions.R")
  
  ## Run this to allow for sufficient digits to differentiate plot cn numbers
  options("scipen"=100, "digits"= 6)
  
  
  # 1. Set up and imputation model ----
  #------------------------------------------#
  
  ifelse(z<10, 
         out.dir<- paste0("../outputs/z0", z),
         out.dir<- paste0("../outputs/z", z))
  
  
  # Prepare creating output directories 
  dir.create("../outputs")
  dir.create(out.dir)
  #
  dir.create(paste0(out.dir,"/model/"))
  dir.create(paste0(out.dir,"/rows/"))
  dir.create(paste0(out.dir,"/raster/"))
  dir.create(paste0(out.dir,"/raster/tiles/"))
  dir.create(paste0(out.dir,"/assembled/"))
  dir.create(paste0(out.dir,"/plot_df/"))
  
  
  # Read in Data --
  
  # Read in X-table
  x_table<- read.csv(paste0("../X_tables_by_zone/x_table_", z, ".csv"))
  
  # Read in the EVG reclass table for the zone
  ifelse(z<10, 
        evg_reclass<- read.csv(paste0("../TargetRasters/v2020/post_mask/z0",z,"/evt_gp_remap.csv")),
        evg_reclass<- read.csv(paste0("../TargetRasters/v2020/post_mask/z",z,"/evt_gp_remap.csv")))
  
  
  # Now filter the FIA data to include only data with EVG's found in the EVG reclass for the zone
    # Note - this may no longer be necessary
  x_table<- x_table[x_table$EVT_GP %in% evg_reclass$EVT_GP,]
  
  # Read in the FIA plot coordinates and join those to the x-table
  fia_plot_coords<- read.csv("../select_TREEMAP2022_2send/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv")
  
  # Merge the x-table and coordinates, and call it "plot.df"
  plot.df<- merge(x_table, fia_plot_coords[,c("PLT_CN", "ACTUAL_LAT", "ACTUAL_LON")], 
                   by = "PLT_CN") 
  
  ###Now use the EVG reclass to convert the true EVT GPs for this zone to the Remap
  plot.df<- left_join(plot.df, evg_reclass, by = c("EVT_GP"))
  
  # Convert remap to factor and overwrite original EVT GP
  plot.df$EVT_GP<- as.factor(plot.df$EVT_GP_remap)
  # Remove the newly merged field
  plot.df<- plot.df[,-which(names(plot.df) == "EVT_GP_remap")]

  
  # Calculate northing and easting from aspect. 
    # Also address plots where aspect = 0 and slope = 0. These actually have no aspect, and need northing = 0 and easting = 0
  plot.df$ASPECT<- (pi/180)*plot.df$ASPECT
  plot.df$EASTING<- sin(plot.df$ASPECT)
  plot.df$NORTHING<- cos(plot.df$ASPECT)
  #
  #Address no aspect issue by setting easting and northing to 0 anywhere with 0 slope and 0 aspect
  plot.df$EASTING[plot.df$SLOPE == 0 & plot.df$ASPECT == 0]<- 0
  plot.df$NORTHING[plot.df$SLOPE == 0 & plot.df$ASPECT == 0]<- 0
  
  # Address issue with slopes. X-table includes slope in percent, but target layers have slope in degrees. 
  plot.df$SLOPE<- atan(plot.df$SLOPE / 100) * 180 / pi
  
  # Because target layers are all lower case, change field names to all lower case
  colnames(plot.df)<- tolower(colnames(plot.df))
  #
  # And change other necessary column names
  colnames(plot.df)[which(colnames(plot.df) == "elev")]<- "elevation"
  colnames(plot.df)[which(colnames(plot.df) == "canopy_cover")]<- "evc"
  colnames(plot.df)[which(colnames(plot.df) == "canopy_height")]<- "evh"
  
  
  ##Build X predictor matrix ---
  
  # First work with disturbance codes
  dc.code.fac <- as.factor(plot.df$disturb_code)
  dc.year.fac <- as.factor(plot.df$disturb_year)
  dc.year.num <- as.numeric(plot.df$disturb_year)
  #
  lev.dc <- levels(dc.code.fac)
  lev.year <- levels(dc.year.fac)
  
  # rename "tm_id" to "ID"
  colnames(plot.df)[which(colnames(plot.df) == "tm_id")]<- "ID"
  
  #Create X Tables for imputation ---
  x.df<- plot.df[,which(names(plot.df) %in% c("slope", "elevation", "prcp",
                                               "srad", "swe", "tmax", "tmin", "vp",
                                               "vpd", "disturb_code", "disturb_year",
                                               "evc", "evh", "evt_gp", 
                                               "actual_lat", "actual_lon", 
                                               "easting", "northing"))]
  
  rownames(x.df) <- plot.df$ID
  id.table <-  plot.df$ID
  
  # Create Y Tables ---
  
  # Keep: EVG, EVC, EVH. Then disturbance code will be joined too
  # Updated
  y.df<- plot.df[,which(names(plot.df) %in% c("evc", "evh", "evt_gp"))]
  rownames(y.df) <- plot.df$ID
  #
  dc.bin <- as.character(x.df$disturb_code)
  dc.bin[dc.bin !="0"] <- "1"
  dc.bin <- as.factor(dc.bin)
  #
  x.df$disturb_code <- dc.bin
  y.df$disturb_code <- dc.bin
  
  
  
 
  # Now build the Random Forest Imputation model ---
  
  ## First, handle NA's currently in data by removing incomplete cases
  x.df<- x.df[complete.cases(x.df),]
  y.df<- y.df[row.names(x.df),]
  #
  plot.df<- plot.df[complete.cases(plot.df),]
  

  # RF START-----
  # Run RF Imputation
  set.seed(56789)
  yai.treelist.bin <- yai(x.df, y.df, method = "randomForest", ntree = 300,
                              mtry = 5, oob = T)
  
  # Look at summary
  rf_summary<- yaiRFsummary(yai.treelist.bin)
  rf_summary
  
  # Variable importance / confusion matrices
  
  varImp<- data.frame(rf_summary$scaledImportance)
  yaiVarImp(yai.treelist.bin)
  
  yai.treelist.bin$ranForest$canopy_cover$confusion
  yai.treelist.bin$ranForest$canopy_height$confusion
  yai.treelist.bin$ranForest$EVT_GP
  yai.treelist.bin$ranForest$disturb_code$confusion
  
  
  # Save the model outputs and results
  saveRDS(yai.treelist.bin, paste0(out.dir,"/model/yai_model.rds"))
  saveRDS(rf_summary, paste0(out.dir,"/model/yai_model_summary.rds"))
  write.csv(round(varImp,4), paste0(out.dir,"/model/yai_model_variable_importance.csv"))
  #
  #remove actual coords from the plot df for confidentiality
  plot.df.save<- plot.df[ , -which(names(plot.df) %in% c("actual_lat","actual_lon"))]
  write.csv(plot.df.save, paste0(out.dir,"/plot_df/plot_df.csv"), quote=F, row.names = F)
  
  #
  
  
  # Run Raster imputation ---
  # Terra options
  
  # Increase memory fraction available
  terraOptions(memfrac = 0.8)
  
  # Parallelization settings
  #--------------------------------------#
  
  ##depends on number of cores your machine, below this gets overriden by the memory available. This will require tweaking depending on your machine
  ncores <- 32
  
  # set up dopar
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # load packages to each cluster
  clusterCall(cl, function() {
    library(tidyverse);
    library(yaImpute);
    library(randomForest);
    library(glue)})
  
  
  # Set inputs ----
  
  
  # Tiling settings
  # ---------------------------------------#
  # set dimensions of tile - value is the length of one side
  tile_size <- 2000
  
  # # select tiles to run
  # # if NA, defaults to all tiles in list
  which_tiles <- NA
  
  # Standard inputs
  #---------------------------------------------#
  
  
  #
  # Load target rasters
  # --------------------------------------------------------------------#
  
  # list target raster files
  ifelse(z<10, 
         flist.tif <- list.files(path = paste0("../TargetRasters/v2020/post_mask/z0",z), pattern = "*.tif$", recursive = TRUE, full.names = T),
         flist.tif <- list.files(path = paste0("../TargetRasters/v2020/post_mask/z",z), pattern = "*.tif$", recursive = TRUE, full.names = T))
  
  # load raster files as terra raster
  rs2 <- terra::rast(flist.tif)
  
  # get raster layer names
  ifelse(z<10,
         layer_names<- gsub(".tif","",gsub(paste0("../TargetRasters/v2020/post_mask/z0",z,"/"),"", flist.tif)),
         layer_names<- gsub(".tif","",gsub(paste0("../TargetRasters/v2020/post_mask/z",z,"/"),"", flist.tif)))
  
  
  #add names to raster list
  names(rs2) <- layer_names
  
  
  # Set up run
  # ---------------------------------------------------------- #
  
  # first row to start test on
  
  # set up rows to run over
  row1 <- 1
  row2 <- tile_size
  
  # Set up tiles for imputation
  
  # aggregate - template for making tiles
  # get values to see which tiles are retained vs NA
  agg <- terra::aggregate(rs2[[1]], fact = tile_size,
                          fun = "median", na.rm = TRUE)
  
  
  # subset the raster and create temporary files
  # tiles with only NA values are omitted
  # the function returns file names for the temporary files
  tiles <- rs2 %>%
    terra::makeTiles(agg, paste0(tempfile(), "_.tif"), na.rm = TRUE)
  
  # garbage collector
  gc()
  
  
  
  # Inspect tiles
  #--------------------------------------------#
  # prepare tiles for visualization
  p <- terra::init(agg, "cell") %>%
    terra::mask(agg) %>%
    terra::as.polygons(values  = TRUE)
  
  names(p) <- "cell"
  p$cell <- seq(1:nrow(p))
  
  # plot tiles
  plot(rs2[[2]], legend = FALSE)
  plot(p, "cell", alpha = 0.25, add=TRUE)
  
  
  
  # Select tiles to run
  #--------------------------------------------#
  
  # if which_tiles is NA, default to all tiles
  if(is.na(which_tiles[1])) {
    which_tiles <- 1:length(tiles)
  }
  
  
  
  
  # 2. Apply Imputation over tiles ----
  #------------------------------------------#
  
  tic()
  
  for(j in which_tiles) {
    
    # select tile to run
    fn <- tiles[j]
    print(glue::glue("working on tile {j} of {length(which_tiles)}!"))
    
    # read raster tile into memory
    ras <- terra::rast(fn)
    
    # get metadata from raster tile
    nrow_r <- nrow(ras)
    ncol_r <- ncol(ras)
    ext_r <- terra::ext(ras)
    crs_r <- terra::crs(ras)
    
    # Prep raster inputs
    #--------------------------------------------#
    
    # Reclass disturbance to binary
    ras$disturb_code <- terra::classify(ras$disturb_code, cbind(2, 1))
    
    gc()
    
    # add XY coords to raster
    ras$actual_lat <- terra::init(ras, "x")
    names(ras$actual_lat) <- "actual_lat"
    ras$actual_lon<- terra::init(ras, "y")
    names(ras$actual_lon) <- "actual_lon"
    
    # Rename evt_gp_remap raster to evt_gp
    names(ras)[which(names(ras)== "evt_gp_remap")] <- "evt_gp"
    
    # remove aspect
    ras$aspect <- NULL
    
    gc()
    
    # convert raster to matrix
    mat <- as.matrix(ras)
    
    
    # Do work on tile
    #--------------------------------#
    
    # PARALLEL WITH DOPAR
    #----------------------------#
    
    f <- foreach(i = 1:nrow_r,
                 .packages = c("tidyverse", "yaImpute", "glue")
                 #.export = c("mat", "impute.row", "yai_treelist_bin")
     ) %dopar% {
      
    
      # get extracted values from each field for each row of input raster
      d <- data.frame(
        mat[(ncol_r * (i - 1) + 1):(ncol_r * i), ])
      
      # impute on row of data - input is named row of data + yai
      row <- impute.row(dat = d,
                        yai = yai.treelist.bin, test = FALSE)
      
      
      # label for row - to keep rows in order
      i_out <- if (i < 10) { glue::glue("000{i}")
      } else if (i < 100) { glue::glue("00{i}")
      } else if (i < 1000) { glue::glue("0{i}")
      } else if (i < 10000) {glue::glue("{i}")
      }
      
      
      saveRDS(row,
              paste0(out.dir,"/rows/row", i_out, ".RDS") # glue doesn't work in parallel
      )
      
    } # end do par
    
    paste0(out.dir,"/rows/")
    # read rows back in 
    rlist <- list.files(glue::glue(out.dir,"/rows/"),
                        "row[0-9]*.RDS", full.names = TRUE)
    
    # potential for error catch - if length(rlist) > nrow_r
    
    # bind rows together
    mout <- do.call(rbind,
                    lapply(rlist, readRDS))
    
    # Turn rows into a raster tile
    #-----------------------------------------#
    if (nrow(mout) < nrow_r) {
      # fill any missing rows in tile, when compared to input raster tile
      tile_out <- fill_matrix_to_raster(mout, ncol_r, nrow_r, row1)
    } else {
      # convert to raster
      tile_out <- mout %>% terra::rast()
    }
    
    #post-process tile - with metadata from input raster
    terra::ext(tile_out) <- ext_r
    terra::crs(tile_out) <- crs_r
    
    # trim NAs from tile, now that we have appropriate extent and CRS
    tile_out <- terra::trim(tile_out)
    
    # inspect
    plot(tile_out)
    
    
    # export
    terra::writeRaster(tile_out,
                       # glue doesn't work for file names within future loop
                       paste0(out.dir,"/raster/tiles/",'_tilesz',tile_size,'_nT',length(tiles),'_tile',j,'.tif'),
                       datatype = "INT4U",
                       overwrite = TRUE)
    
    gc() # end of work on tile
    
    # delete rows from tmp dir - fresh start for next tile
    do.call(unlink, list(rlist))
    
    print(glue::glue("done with tile {j}!"))
  
  }
  
  
  
  stopCluster(cl)
  
  
  # 3. Run Aggregation of tiles ----
  #------------------------------------------#
  
  # list tiles from path
  tile.list <- list.files(path = paste0(out.dir,"/raster/tiles/"), pattern = "*.tif$", recursive = TRUE, full.names = TRUE)
  
  # load tiles as .vrt
  vrt <- terra::vrt(tile.list)
  
  #inspect
  plot(vrt)
  
  # export as single raster per zone
  writeRaster(vrt, 
              paste0(out.dir,"/assembled/imputation.tif"),
              overwrite = TRUE)
  
  write(toc()$callback_msg, paste0(out.dir,"/assembled/time_elapsed_full_imputaton.txt"))
  
  gc()
  
  
  
  
  
  # 4. Evaluate target layers and accuracy ----
  #------------------------------------------#
  
  # Remove all objects from environment except z and out.dir
  rm(list=setdiff(ls(), c("z", "out.dir", "evg_reclass", "plot.df")))
  
  # Source the required functions
  source("./cms_and_evaluation_functions.R")
  
  
  # Which variables to evaluate accuracy of
  eval_vars<- c("evc", "evh", "evt_gp", "disturb_code")
  eval_vars_full_names<- c("canopy_cover", "canopy_height", "evt_gp", "disturb_code")

  
  # Get Landfire rasters
  ifelse(z<10, 
         target_files <- list.files(path = paste0("../TargetRasters/v2020/post_mask/z0",z), pattern = "*.tif$", recursive = TRUE, full.names = T),
         target_files <- list.files(path = paste0("../TargetRasters/v2020/post_mask/z",z), pattern = "*.tif$", recursive = TRUE, full.names = T))
  
  
  #
  # Only keep evc, evh, evt_gp, disturbance code
  target_files_to_keep<-rep(NA, length(eval_vars))
  #
  for(i in seq_along(eval_vars)){
    target_files_to_keep[i]<- grep(eval_vars[i], target_files)
  }
  
  target_files<- target_files[target_files_to_keep]
  
  # Load those
  lf <- terra::vrt(target_files, options = "-separate", overwrite = TRUE)
  
  # Name them
  names(lf) <- eval_vars
  
  # make binary disturbance code
  lf$binary_disturbance<- lf$disturb_code
  lf$binary_disturbance[lf$binary_disturbance == 2]<- 1
  
  # Now add binary disturbance code to the variables being assessed
  eval_vars<- c("evc", "evh", "evt_gp", "disturb_code", "binary_disturbance")
  eval_vars_full_names<- c("canopy_cover", "canopy_height", "evt_gp", "disturb_code", "binary_disturbance")
  
  # Name as their full names
  names(lf) <- eval_vars
  
  #lf<-lf[[4:5]]
  #eval_vars<- c( "disturb_code","binary_disturbance")
  #eval_vars_full_names<- c("disturb_code","binary_disturbance")
  #trim away any NAs on the sides - helps match extent with outputs
  lf<- terra::trim(lf)
  gc()
  
  
  # Load imputed Raster
  
  # Load raw imputation output raster
  ras <- terra::rast(paste0(out.dir,"/assembled/imputation.tif"))
  
  # trim off NA values
  ras <- terra::trim(ras)
  ras<- terra::project(ras, crs(lf))
  
  # crop lf and mask by each other to be safe
  lf<- terra::crop(lf, ras, mask=T)
  ras<- terra::crop(ras, lf[[1]], mask=T)
  compareGeom(lf, ras)
  
  #set name of input column
  names(ras) <- c("value")
  
  # clear memory
  gc()
  
  
  # Prep for assembly
  
  # get list of IDs present in ras
  id_list <- freq(ras)$value %>%
    sort() %>%
    as.data.frame()
  names(id_list) <- "PLOTID"
  
  evg_reclass_orig<- evg_reclass
  
  # Change plot.df so it has original EVT_GP, not remapped
  #Convert EVT_GP to original from remapped
  evg_reclass$EVT_GP_remap<- as.factor(evg_reclass$EVT_GP_remap)
  plot.df<- left_join(plot.df, evg_reclass, by = c("evt_gp" = "EVT_GP_remap"))
  plot.df$evt_gp<- plot.df$EVT_GP
  plot.df<- plot.df[, -which(names(plot.df) %in% c("EVT_GP"))]
  #
  #
  plot.df$ID<- as.numeric(plot.df$ID)
  ##
  
  # Create binary disturbance code field in plot.df
  plot.df$binary_disturbance<- plot.df$disturb_code
  plot.df$binary_disturbance[plot.df$binary_disturbance == 2]<- 1
  
  # Join list of ids with x table
  # create lookup table that only has IDs present in zone
  lookup <- left_join(id_list, plot.df, by = c("PLOTID" = "ID")) %>%
    dplyr::select(PLOTID, plt_cn, all_of(eval_vars)) %>%
    mutate(across(where(is.numeric), ~na_if(., NA)))
  
  # Evaluation: Calculate confusion matrices of Imputation vs Landfire / reference rasters
  dir.create(paste0(out.dir, "/target_layer_comparison"))
  
  # apply function to all layers
  cms <- eval_vars %>%
    map(\(x) assembleCM(x, 
                        raster = ras,
                        lookup = lookup,
                        id_field = "PLOTID",
                        stackin_compare = lf,
                        stackin_compare_name =  "Landfire",
                        remapEVT_GP = F,
                        EVT_GP_remap_table =  evg_reclass,
                        exportTF = TRUE,
                        export_path = paste0(out.dir, "/target_layer_comparison/")
    ))
  
  names(cms) <- eval_vars
  
  
  
  # export as RDS
  saveRDS(cms, paste0(out.dir, "/target_layer_comparison/CMs_TargetLayerComparison.RDS"))
  
  
  
  
  # 5. Create target layer evaluation document from RMarkdown ----
  
  # Set up
  packages<- list("this.path", "terra", "tidyverse", "magrittr", 
                  "glue", "tictoc", "caret", "yaImpute", "randomForest", "ggrepel")
  lapply(packages, require, character.only = TRUE)
  
  ###
  eval_type <- "TargetLayerComparison"
  round_dig <- 4
  zone_num<- z
  raster_name<- "Imputed Target Layers"
  
  
  ####
  
  # Get path to rmd
  rmd_path <- ("./target_layer_evaluation_rmd.Rmd")
  
  # set dir for temporary outputs - needs to be a place w/ write permissions for R
  dir.create(paste0(out.dir,"/temp"))
  tmpout_dir <- paste0(out.dir,"/temp")
  
  # Define plot labels
  plot_labels <- c("Imputed", "Observed")
  
  # Load evaluation data
  #------------------------------------------------------#
  
  #load raw imputation output raster
  raw_imputed_raster<- terra::rast(paste0(out.dir,"/assembled/imputation.tif"))
  
  # Load CMS
  cms_path<- paste0(out.dir, "/target_layer_comparison/CMs_TargetLayerComparison.RDS")
  plot_labels <- c("Imputed", "Observed (Target Layers)")
  
  cms_all <- readRDS(cms_path)
  
  
  #
  # Load EVG Reclass
  ifelse(z<10, 
         evg_reclass<- read.csv(paste0("../TargetRasters/v2020/post_mask/z0",z,"/evt_gp_remap.csv")),
         evg_reclass<- read.csv(paste0("../TargetRasters/v2020/post_mask/z",z,"/evt_gp_remap.csv")))
  names(evg_reclass)[1]<- "EVT_GP"
  
  
  
  
  # Render report
  #-----------------------------------------------------#
  
  rmarkdown::render(rmd_path, 
                    output_format = "word_document",
                    output_file = ("target_layer_evaluation"),
                    output_dir = tmpout_dir,
                    #build df of params to share with rmd
                    params = list(raster_name = raster_name, 
                                  zone_num = zone_num,
                                  eval_type = eval_type,
                                  eval_vars= eval_vars, 
                                  cms_path = cms_path)
  )
  
  
  # File Organization
  #------------------------------------------------#
  
  # move report from tmpout dir to desired out dir
  file.copy(from=paste0(tmpout_dir,"/target_layer_evaluation.docx"),
            to=paste0(out.dir, "/target_layer_comparison/target_layer_evaluation.docx"),
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  
  # delete the temp directory
  unlink(tmpout_dir,recursive=TRUE) 
  
  
  # Remove all objects from environment except z
  rm(list=setdiff(ls(), "z"))
  gc()

}
