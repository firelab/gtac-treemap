# load original RAT
# load new RAT

# plot vars and ensure they look like we want them to

# rat path 
rat_path_orig = "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2022/03_Raster_attributes/TM2022_RAT_tmid.csv"

rat_path_new = "//afssxgtacnas311/ForestMAP/80_Workspace/40_TreeMap_Volume/Attribute_assembly/00_Raster_attribute_table/TreeMap_RAT_SOUNDVOLUME.csv"


# Load libraries etc
#-------------------------------------------------------#
this_proj <- this.path::this.proj()
this_dir <- this.path::this.dir()

## load treemap library
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


# Load data 
#---------------------------------------------------------#

rat_orig = load_RAT(rat_path_orig, 
                             CN_column = "PLT_CN", 
                             ID_column = "TM_ID")

rat_new = read.csv(rat_path_new)

# inspect
#----------------------------------------------------------------#

rat_out = rat_new

hist(rat_out$VOLCFSND_L)
hist(rat_out$VOLCFNET_L)
hist(rat_out$VOLCFSND_D)

plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L)
plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L, xlim = c(0, 10000), ylim = c(0, 10000))
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
plot(rat_out$VOLCFSND_L, rat_out$VOLCFSND_D)
plot(rat_out$VOLCFNET_L, rat_out$VOLCFNET_D)
plot()

attributes_export = c("VOLCFSND_L_PX", "VOLCFSND_D_PX")

rat_out %>%
  filter(VOLCFNET_L== 0) %>%
  ggplot() +
  geom_point(aes(VOLCFNET_L, VOLCFSND_L))

rat_out %>%
  filter(VOLCFNET_L== 0,
         VOLCFSND_L > 0)