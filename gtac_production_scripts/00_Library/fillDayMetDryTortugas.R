# Manually fill missing values for Dry Tortugas area.
# This was one missing pixel too far from others to fill by the focal function
# This manually fills the needed pixel with values from the nearest pixel
library(docstring)

fillDayMetDryTortugas <- function(input_raster,
                                  climate_variable){
  
  
  #' # Manually fill missing values for Dry Tortugas area.
  #' This was one missing pixel too far from others to fill by the focal function
  #' This manually fills the needed pixel with values from the nearest pixel
  #'
  #' @param input_raster National DayMet climate variable to fill
  #' @param climate_variable String for specific climate variable to process; options are: "prcp", "swe", "srad", "tmax", "tmin", "vp", "vpd"
  #'
  #' @return CONUS_wide raster filled for empty spot in Dry Tortugas
  #' @export
  #'
  #' @examples 
  #' 
 
  
  # Original code
  
  # values(prcp)[45100880]<- 1064.475342
  # values(srad)[17256450]<- 305.387207
  # values(swe)[45100880]<- 0
  # values(tmax)[45100880]<- 28.243658
  # values(tmin)[45100880]<- 22.773148
  # values(vp)[45100880]<- 2670.494629
  # values(vpd)[45100880]<- 720.197021
  
  require(terra, tidyverse, glue)
  
  # Create data frame of raster name, px to reclass, and value to reclass
  var_name <- c("prcp", "srad","swe",  "tmax", "tmin", "vp", "vpd")
  px_to_reclass <- c(45100880,17256450,45100880,45100880,45100880,45100880,45100880)
  reclass_value <- c(1064.475342,
                     305.387207,
                     0,
                     28.243658,
                     22.773148,
                     2670.494629,
                     720.197021)
  
  t <- data.frame(cbind(var_name, px_to_reclass, reclass_value))
  
  # Error handling
  if(climate_variable %notin% t$var_name) {
    message(glue::glue("Choose a valid variable name! Valid variable names are: {t$var_name}"))
  } else {
    
    # load raster
    r <- input_raster
    
    # which pixel to reclass?
    px = t %>%
      dplyr::filter(var_name == climate_variable) %>%
      dplyr::select(px_to_reclass) %>%
      as.numeric()
    
    # get value for reclassed pixel
    val = t %>%
      dplyr::filter(var_name == climate_variable) %>%
      dplyr::select(reclass_value) %>%
      as.numeric()
    
    # fill the pixel
    values(r)[px] <- val
    
    return(r)
    
  }
  
  
  
  
   
}





  
  #' Create environment variables to facilitate target data preparation
  #'
  #' @param zone_input Number; Landfire zone currently being worked on
  #'
  #' @return Environment variables for target data preparation
  #' @export
  #'
  #' @examples 
  #' 