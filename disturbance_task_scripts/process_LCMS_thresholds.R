# load libraries
library(terra)
library(tidyverse)
library(stringr)

# set tmp directory
tmp_dir <- "E:/tmp/"

#set home dir
home_dir <- "//166.2.126.25/TreeMap/"

#set tif_dir
tif_dir <- paste0(home_dir, "01_Data/05_LCMS/01_Threshold_Testing/01_Raw/")

#set export dir
out_dir <- paste0(home_dir, "01_Data/05_LCMS/01_Threshold_Testing/03_Mosaics/")

# list years
years_list <- seq(1999, 2016, 1)

# list thresholds
# 14 is the default threshold
thresh_list <- c(5, 10, 20, 25)

# list files in raw directory
tifs_list <- list.files(path = tif_dir,
                        pattern = "\\.tif$",
                        full.names = TRUE
                        )


# SETUP
###################################################

# set desired end crs 
#crs <- crs("epsg:5070") # tree map output data is in  NAD83 Albers
crs <- crs("//166.2.126.25/TreeMap/01_Data/05_LCMS/00_Supporting/lcms_crs_albers.prj") # load 

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#terraOptions(tempdir = tmp_dir)
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()

#increase memory fraction available
terraOptions(memfrac = 0.8)

terraOptions(progress = 1)

#identical(crs(r1), 
#          paste(read_lines("//166.2.126.25/TreeMap/01_Data/05_LCMS/00_Supporting/lcms_crs_albers.prj"), collapse = "\n"))

# RASTER PROCESSING
#########################################################

# subset to files that match specific year x threshold combination

#for(i in 1:length(years_list)){
  
  #for testing
  i = 1
  
  year = years_list[i]
  
  #for(j in 1:length(thresh_list)){
    
    #for testing
    j = 1
    
    thresh = thresh_list[j]
    
    # list all rasters for single year x threshold 
    tifs_in <- str_subset(str_subset(tifs_list, as.character(year)), paste0("t", thresh))
    
    #test on subset - get first 3 rasters in list
    tifs_in <- tifs_in[c(1:3)]
    
    # read in all rasters in list and mosaic
    
    # test reading in one raster
    r <- terra::rast(tifs_in[1])
    
    # update NA value
    
    
    # reproject 0- josh said i would need to, but the crs is the same
    #r <- terra::project(r1, crs)
    r <- terra::subst(r, -32768, NA )
    r <- project(r, crs)
    writeRaster(r,
                "//166.2.126.25/TreeMap/01_Data/05_LCMS/01_Threshold_Testing/02_Reprojected/r_test_reproject_reclass.tif" )
    gc()
  
    r1 <- rast(tifs_in[1])
    r2 <- rast(tifs_in[2])
    r3 <- rast(tifs_in[3])
    
    ext(r1)
    ext(r2)
    ext(r3)
    
    plot(r1)
    plot(r2)
    plot(r3)
    
    test_merge <- terra::merge(r1, r2, r3)
    test_merge2 <- terra::merge(r1, r2, r3,
                                filename = paste0(out_dir, "merge_test.tif"))
    
    
    # test vrt
    v <- terra::vrt(tifs_in)
    m <- writeRaster(v, 
                paste0(out_dir, "vrt_test.tif"),
                overwrite = TRUE)
    
    rm(v)
    
    
  }
}



# mosaic rasters together

# reproject

# export and save

# clear memory