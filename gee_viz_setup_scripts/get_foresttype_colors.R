# get unique layer names for forest types in TreeMap

library(terra)
library(tidyverse)
library(randomcoloR)

#make %notin% function
`%notin%` <- Negate('%in%')

treemap <- rast("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

treemap

#list all categories
names(cats(treemap)[[1]])


activeCat(treemap)

#get FORTYPCD
activeCat(treemap) <- 2 # FORTYPCD
fortypcd <- levels(treemap)[[1]][,2]

#get FORTYPNAME
activeCat(treemap) <- 3 #ForTypName
fortypname<- levels(treemap)[[1]][,2]

#get FLTYPCD
activeCat(treemap) <- 4 # FldTypCD
fldtypcd <- levels(treemap)[[1]][,2]

#get FLDTYPNAME
activeCat(treemap) <- 5 # FldTypName
fldtypname <- levels(treemap)[[1]][,2]

#get unique values for each
fortyp_table <- data.frame(fortypcd, fortypname)
fortyp_table <- unique(fortyp_table)

# get unique values for each
fldtyp_table <- data.frame(fldtypcd, fldtypname)
fldtyp_table <- unique(fldtyp_table)

#get full table of all types in both
all_typ_table <- data.frame(code = c(fortyp_table$fortypcd, fldtyp_table$fldtypcd),
                            name = c(fortyp_table$fortypname, fldtyp_table$fldtypname))
all_typ_table <- unique(all_typ_table)

# sort
all_typ_table %>% dplyr::arrange(code)

  
# get colors

n <- nrow(all_typ_table)

palette <- distinctColorPalette(n)

# write out palette
all_typ_table$palette <- palette

# write out FULL palette with all numbers (necessary for js)
nums <- seq(1, max(all_typ_table$code), 1)
nums_null <- nums[nums %notin% all_typ_table$code]

nums_df <- data.frame(code = nums_null, palette = rep("#000000", length(nums_null)))

palette_out <- bind_rows(all_typ_table, nums_df) %>%
  arrange(code)

#write out in format for js

head(palette_out$palette)

write.table(
  paste0("[", palette_out$palette,
         "]",),
            "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gee_viz_setup_scripts/forest_type_palette.txt", 
            append = FALSE, sep = " ", dec = ".", 
            row.names = FALSE, col.names = FALSE)
