# get unique layer names for forest types in TreeMap

library(terra)
library(tidyverse)
library(randomcoloR)
library(rjson)
library(magrittr)

#make %notin% function
`%notin%` <- Negate('%in%')


# ORIGINAL: get forest types from raster attribute table
##########################################################

# treemap <- rast("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")
# 
# dat <- treemap 

# #list all categories
# names(cats(dat)[[1]])
# 
# activeCat(dat)
# 
# 
# #get FORTYPCD
# activeCat(dat) <- 2 # FORTYPCD
# fortypcd <- levels(dat)[[1]][,2]
# 
# #get FORTYPNAME
# activeCat(dat) <- 3 #ForTypName
# fortypname<- levels(dat)[[1]][,2]
# 
# #get FLTYPCD
# activeCat(dat) <- 4 # FldTypCD
# fldtypcd <- levels(dat)[[1]][,2]
# 
# #get FLDTYPNAME
# activeCat(dat) <- 5 # FldTypName
# fldtypname <- levels(dat)[[1]][,2]
# 
# #get unique values for each
# fortyp_table <- data.frame(fortypcd, fortypname)
# fortyp_table <- unique(fortyp_table) %>%
#   arrange(fortypcd)
# 
# # get unique values for each
# fldtyp_table <- data.frame(fldtypcd, fldtypname)
# fldtyp_table <- unique(fldtyp_table) %>%
#   arrange(fldtypcd)
# 
# #look at correspondence
# unique_names_fortyp <- fortyp_table$fortypname[fortyp_table$fortypname %notin% fldtyp_table$fldtypname]
# unique_names_fortyp
# 
# unique_names_fldtyp <- fldtyp_table$fldtypname[fldtyp_table$fldtypname %notin% fortyp_table$fortypname]
# unique_names_fldtyp
# 
# 
# #get full table of all types in both
# all_typ_table <- data.frame(code = c(fortyp_table$fortypcd, fldtyp_table$fldtypcd),
#                             names = c(fortyp_table$fortypname, fldtyp_table$fldtypname))
# all_typ_table <- unique(all_typ_table)

# UPDATED: USE FOREST TYPE TABLE FROM FIA DATABASE
###################################################

forest_types <- read.csv("//166.2.126.25/TreeMap/01_Data/04_FIA/05_FIA_DataMart/CSV/FIADB_REFERENCE/REF_FOREST_TYPE.csv")

dat <- forest_types

all_typ_table <- dat

all_typ_table %<>% 
  dplyr::rename("code" = VALUE,
                "names" = MEANING) %>%
  dplyr::select(code, name) %>%
  dplyr::filter(!str_detect(name, "retired"))


# inspect
all_typ_table %>%
  arrange(name)

all_typ_table %>%
  arrange(code)

# sort
all_typ_table %<>% dplyr::arrange(code)

# GET COLORS
#####################################################################
  
# get colors - random colors
n <- nrow(all_typ_table)

# get random colors
# REPLACE THIS WITH SOMETHING THAT YOU CAN SET A SEED
set.seed(1)
palette <- distinctColorPalette(n)
palette

# write out palette
all_typ_table$palette <- palette

###### 
#export palette table as JSON -- to be a lookup tables

all_typ_table_Json <- toJSON(all_typ_table)

#export
write(all_typ_table_Json, 
      "C:/Users/lleatherman/OneDrive - USDA/Documents/GitHub/gtac-treemap/gee_viz_setup_scripts/forest_type_palette_lookup.json")
write(all_typ_table_Json,
      "C:/Users/lleatherman/Documents/GitHub/GTAC-LCMS-Viewer/src/data/geojson/forest_type_palette_lookup_20250708.json")


#######################################################################


# write out FULL palette with all numbers (necessary for js)
nums <- seq(1, max(all_typ_table$code), 1)
nums_null <- nums[nums %notin% all_typ_table$code]

nums_df <- data.frame(code = nums_null, palette = rep("#000000", length(nums_null)))

palette_out <- bind_rows(all_typ_table, nums_df) %>%
  arrange(code) %>%
  mutate(palette_which = ifelse(name %in% unique_names_fldtyp, "FLDTYP", 
         ifelse(name %in% unique_names_fortyp, "FORTYP", "BOTH")),
         palette_fldtyp = ifelse(name %in% unique_names_fldtyp, palette, "#000000"),
         palette_fortyp = ifelse(name %in% unique_names_fortyp, palette, "#000000"))

#write out in format for js
head(palette_out$palette)

palette_out %>%
  filter(palette_which == "BOTH" | palette_which == "FLDTYP") %>%
  select(palette)

write.table(palette_out$palette ,
            "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gee_viz_setup_scripts/forest_type_palette_full.txt", 
            append = FALSE, sep = " ", dec = ".", eol = ",",
            row.names = FALSE, col.names = FALSE)

write.table(palette_out$palette_fldtyp ,
            "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gee_viz_setup_scripts/forest_type_palette_fldtyp.txt", 
            append = FALSE, sep = " ", dec = ".", eol = ",",
            row.names = FALSE, col.names = FALSE)

write.table(palette_out$palette_fortyp ,
            "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gee_viz_setup_scripts/forest_type_palette_fortyp.txt", 
            append = FALSE, sep = " ", dec = ".", eol = ",",
            row.names = FALSE, col.names = FALSE)

