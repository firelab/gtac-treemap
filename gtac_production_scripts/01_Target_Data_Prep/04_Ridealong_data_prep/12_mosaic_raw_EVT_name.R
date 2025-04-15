# Set up
home_dir = "//166.2.126.25/TreeMap/"
year = 2023

library(terra)

# Direct to files
evt_files<- list.files(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v{year}/pre_mask/"),
                        recursive = T,
                        pattern = "evt_name.tif$",
                        full.names = T)
evt_vrt<- vrt(evt_files)

###

# Get the valid categories from each EVT
for(i in seq_along(evt_files)){
  evt<- rast(evt_files[i])
  cats<-  cats(evt)[[1]]
  assign(paste0("cats_",i), cats)
}

# Bind them all into one
complete_cats<- do.call(rbind, (mget(ls(pattern="cats_"))))

# Remove duplicates
unique_cats<- complete_cats[-which(duplicated(complete_cats)),]

# How many EVT names are there?
length(unique(unique_cats$EVT_NAME))

#How many EVT Groups?
length(unique(unique_cats$EVT_GP))

# How many EVT's are there in non-tree EVT-Lifeforms?
length(which(unique_cats$EVT_LF != "Tree"))

# How many EVT's are there in non-tree-dominated EVT-Orders?
length(which(unique_cats$EVT_ORDER != "Tree-dominated"))
