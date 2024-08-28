# attempt to join disturbance codes from John Shaw with other data

##############################

# load libraries
library(terra)
library(tidyverse)
library(magrittr)

# Paths to data
#------------------------#

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# path to John's export
john_dc_path <- glue::glue("{FIA_dir}/03_FullShp/FIA_US.shp")

# path to reference data from Karin
KR_xtable_path <- glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt")

# path to RAT
rat_path <- glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# Load data - get everything as data frames
#--------------------------------------------#

pts_dc <- data.frame(terra::vect(john_dc_path))

X_df <- read.csv(KR_xtable_path)

rat <- terra::rast(rat_path)
rat <- data.frame(cats(rat))

# Prep data - so we can join it
#----------------------------------------#

rat %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  select(-Value)

# JOIN: X_df and RAT by CN
# --------------------------------------#

refs <- X_df %>%
   left_join(rat, by = "CN") 
#%>%
# select(c(CN, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
# mutate(dataset = "Ground_FIA")%>%
#mutate_at(eval_vars_cont, ~ ifelse(.x == -99, 0, .x))

# for comparison: full join has the same number of rows as a left join 
refs2 <- X_df %>% 
  full_join(rat, by = "CN")

summary(X_df)
summary(rat)
summary(refs)

nrow(X_df) - nrow(rat)

refs3 <- X_df %>%
  left_join(rat, by = c("CN" = "CN"), c("ID" = "tm_id"))

summary(refs3)

identical(refs2, refs3)

# JOIN ATTEMPT: pts_dc
#--------------------------------------------------------#
names(pts_dc)

dc_refs <- 
  X_df %>%
  left_join(pts_dc, by = "CN")

summary(dc_refs) # NAs for all dc vars

dc_refs2 <- X_df %>%
  left_join(pts_dc, by = c("CN" = "PREV_PLT_C"))

summary(dc_refs2)

dc_refs3 <- X_df %>%
  left_join(pts_dc, by = c("ID" = "PLOT"))

summary(dc_refs3)
