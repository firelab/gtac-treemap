# write EVG remap files
# written by Karin Riley 2/15/2019
# updated by Lila Leatherman 11/2023

library(terra)
library(foreign)

#zones <- c(seq(1,10),seq(12,66),98,99)
zones <- c(16)
zone_num <- zones[1]

# load LF zone data
LF_zones <- vect('//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) 

# load original evg data 
evg_forest_mask <- terra::rast("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/02_TreeMask/EVT_maskedEVCforest_reclass.tif")

# subset to zone of interest
evg_forest_mask_z <- terra::crop(evg_forest_mask, zone, mask = TRUE)

# write out for test 
writeRaster(evg_forest_mask_z, 
            '//166.2.126.25/TreeMap/03_Outputs/00_test/evg/z16_EVG_ForestMask.tif')

#for (j in 1:length(zones))
#{
  #curtable <- paste0("E:\\Tree_List_c2014\\target_data\\draft\\z", zones[j], "\\z", zones[j], "_EVG_forest.tif.vat.dbf")
  #curtable <- '//166.2.126.25/TreeMap/03_Outputs/00_test/evg/z16_EVG_ForestMask.tif.vat.dbf'
  curtable <- freq(evg_forest_mask_z)[,2:3]  

  #vat <- read.dbf(curtable)
  vat <- curtable
  evgseq <- seq(1:dim(vat)[[1]])
  outtable <- data.frame()
  for (k in 1:dim(vat)[[1]])
  {
    outtable[k,1] <- paste(vat[k,1],":",evgseq[k], sep="")
  }
  #outfile <- paste0("E:\\Tree_List_c2014\\target_data\\working_KLR\\EVG_remap\\z", zones[j], "_EVG_remap.txt")
  #write.table(outtable, outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
#}

