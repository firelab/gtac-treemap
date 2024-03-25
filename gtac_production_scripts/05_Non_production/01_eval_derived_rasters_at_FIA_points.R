# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Objective: Compare output rasters - extract values to FIA points 

# Last update: 3/21/24


########################################################
# Setup
########################################################

# home_dir
home_dir <- "//166.2.126.25/TreeMap/"

# path to first raster to compare
r1_path <- glue::glue("{home_dir}/03_Outputs/99_Projects/2016_GTAC_Test/02_Assembled_model_outputs/z16/01_Imputation/2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36.tif")
r1_name <- "LFOrig"

# path to second raster to compare
r2_path <- glue::glue("{home_dir}/03_Outputs/99_Projects/2016_GTAC_LCMSDist/02_Assembled_model_outputs/z16/01_Imputation/2016_GTAC_LCMSDist_tilesz2000_nT36.tif")
r2_name <- "LCMSDist"

# path to save output figures
export_fig_path <- glue::glue("{home_dir}03_Outputs/99_Projects/2016_GTAC_LCMSDist/03_Evaluation/z16/04_Model_Comparison/")

# list variables to evaluate
eval_vars_cat <- c("canopy_cover", "canopy_height", "EVT_GP", 
                   "disturb_year", "disturb_code")

eval_vars_cont <- c("GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_D", "TPA_DEAD", "TPA_LIVE")

# path to shapefile or coords of points
pts_path <- glue::glue("{home_dir}/01_Data/04_FIA/03_FullShp/FIA_US.shp")

# path to xtable or similar 
xtable_path <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt")

# set location of raster attribute table
rast_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

# path to evt_gp metadata
evt_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_200/EVT/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv")

# path to coords
coords_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv"

# libraries
#--------------------------------------------------------#

# install dev version
#remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
library(ggalluvial)

# load libraries
library(terra)
library(tidyverse)
library(magrittr)
library(ggplot2)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen"=100, "digits"=8)

#########################################################
# Run
#########################################################

# Load data
#----------------------------------------------#

# load raster 1 for comparison
r1 <- terra::rast(r1_path)
names(r1) <- "PLOTID"

# load raster 2 for comparison
r2 <- terra::rast(r2_path)
names(r2) <- "PLOTID"

# load shp / pts to extract to 
pts <- terra::vect(pts_path)

# load x.df
X.df <- read.csv(xtable_path) %>%
  select(-CN)

# load evt dat
evt_dat <- read.csv(evt_path) 

# load coords
coords <- read.csv(coords_path)

# Load raster attribute table
#-------------------------------------------------#
rat <- terra::rast(glue::glue('{rast_path}TreeMap2016.tif'))
rat <- data.frame(cats(rat))

rat %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  select(-Value)

#names(rat)

# prep evt data
#------------------------------------------------------#
evt_dat %<>%
  select(EVT_GP, EVT_GP_N, RED, GREEN, BLUE) %>%
  filter(!is.na(EVT_GP)) %>%
  distinct() %>%
  group_by(EVT_GP) %>%
  slice_head()  

# Prep pts
#----------------------------------#

# project pts 
pts %<>% terra::project(r1)

# convert to data frame
pts_df <- data.frame(pts)

# Prep coords data
#----------------------------------------------------------------#

# inspect
coords %>%
  filter(PLT_CN %in% pts_df$PREV_PLT_C) %>%
  nrow()

# Prep data to plot
#----------------------------------------------------------#

# i have CN and treemap id

# for r1_ex
#pts$CN - original CN at a point
# PLOTID - imputed plot id 


# join r1_ex$PLOTID to X.df$ID
# join r1_ex$PLOTID to rat$tm_id

# for refs
#pts$CN - original CN at a point
# rat$CN 


# extract  values to points - imputed plot ID
r1_ex <- terra::extract(r1, pts) %>%
  cbind(pts_df$CN) %>% # bind with CNs to identify plots
  rename("CN" = `pts_df$CN`) %>%
  filter(!is.na(PLOTID)) #%>% # remove NAs
  #select(-ID) 

r2_ex <- terra::extract(r2, pts) %>%
  cbind(pts_df$CN) %>%
  rename("CN" = `pts_df$CN`) %>%
  filter(!is.na(PLOTID)) #%>% # remove NAs
  #select(-ID) 

# join extracts with x table and RAT - join ri_ex$PLOTID to RAT$tm_id
# to get values of the imputed plot
r1_ex %<>% left_join(X.df,
                   by = c("PLOTID" = "ID")) %>%
  left_join(rat, by = c("PLOTID" = "tm_id")) %>%
  select(c(CN.x, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
    rename(CN = CN.x) %>%
  mutate(dataset = r1_name) 

r2_ex %<>% left_join(X.df,
                  by = c("PLOTID" = "ID")) %>%
  left_join(rat, by = c("PLOTID" = "tm_id")) %>%
  select(c(CN.x, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  rename(CN = CN.x) %>%
  mutate(dataset = r2_name) 

# prep reference values - join pts$CN with RAT$CN
refs <- 
  data.frame(pts) %>%
  filter(!is.na(CN)) %>%
  filter(CN %in% r1_ex$CN) %>%
  group_by(CN) %>%
  slice_head() %>% # get first instance of each plot cn
  #inner_join(rat, by = c("CN")) #%>% 
  #left_join(X.df, by = c("tm_id" = "ID")) %>%
  mutate(disturb_code = ShannonCla,
       ## make change classes with FIA data match those in X.df
       disturb_code = as.numeric(case_match(disturb_code, 'N' ~ '0', 'B' ~ '0', 'S' ~ '2', 'F' ~ '1'))
  ) %>%
  select(c(CN, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = "Reference")


# join
p_r <- bind_rows(r1_ex, r2_ex) %>%
  # apply any filters
  #filter(QMD_RMRS >-1) %>%
  pivot_longer(!c(CN, PLOTID, dataset, disturb_code), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         value = na_if(value, -99.00000), # update NA values from RAT
         disturb_code = factor(disturb_code, 
                               labels = c("None", "Fire", "Slow Loss"))) %>%
  group_by(disturb_code)


# Plot
#-----------------------------------------------------------#
# plot as desired


# grouped violin plots by disturbance code


# PLOT CATEGORICAL VARS
# ---------------------------------------------------#

for(i in 1:(length(eval_vars_cat)-1)) {
  
  # for testing
  #i = 1
  
  var_name <- eval_vars_cat[i]
  
p <-  p_r %>%
    filter(var == var_name) %>%
    ggplot(aes(x=as.factor(value),  fill=dataset)) + 
    geom_bar(position="dodge") + 
    facet_wrap(~disturb_code) + 
    labs(title = glue::glue('Variation in {var_name} by disturbance code, by model')) + 
    xlab(var_name)

print(p)

# save
ggsave(glue::glue('{export_fig_path}/{r1_name}_vs_{r2_name}_{var_name}.png'),
       plot = p,
       width = 7, 
       height = 4.5)    

}


# PLOT CONTINUOUS VARS
# ---------------------------------------------------#

dodge <- position_dodge(width = 0.6)

for(i in 1:(length(eval_vars_cont))) {
  
  # for testing
  #i = 3
  
  var_name <- eval_vars_cont[i]
  
  p <- p_r %>%
    filter(var == var_name) %>%
    ggplot(aes(x = disturb_code, y = value, fill = dataset))+
    geom_violin(position = dodge)+
    geom_boxplot(width=.1, outlier.colour=NA, position = dodge) + 
    labs(title = glue::glue('Variation in {var_name} by disturbance code, by model')) + 
    xlab(var_name)
  
  print(p)
  
  # save
  ggsave(glue::glue('{export_fig_path}/{r1_name}_vs_{r2_name}_{var_name}.png'),
         plot = p,
         width = 7, 
         height = 4.5)    
  
}



#############################################
# EVT-GP plotting 

# prelim data prep
p_r_evt <- p_r %>%
  ungroup() %>%
  filter(var == "EVT_GP") %>%
  left_join(evt_dat, by = c("value" = "EVT_GP")) %>%
  rename("EVT_GP" = value) %>%
  mutate(EVT_GP_N_short = str_sub(EVT_GP_N, 1, 15))

# get names of evts present
evt_names <- p_r_evt %>%
  select(c(EVT_GP, EVT_GP_N, EVT_GP_N_short)) %>%
  distinct() %>%
  mutate(EVT_GP = factor(EVT_GP)) %>%
  arrange(EVT_GP) %>%
  mutate(EVT_GP_N = factor(EVT_GP_N, levels = EVT_GP_N),
         EVT_GP_N_short = factor(EVT_GP_N_short, levels = EVT_GP_N_short))

# continue data prep
p_r_evt %<>%
  mutate(EVT_GP = factor(EVT_GP), 
         EVT_GP_N = factor(EVT_GP_N, levels = levels(evt_names$EVT_GP_N)),
         dataset = factor(dataset),
         PLOTID = as.integer(PLOTID))


# plot grouped bar chart
######################################
p_r_evt %>%
  ggplot(aes(x=EVT_GP, fill=dataset)) + 
  geom_bar(position="dodge") + 
  scale_x_discrete(labels = evt_names$EVT_GP_N_short) +
  facet_wrap(~disturb_code) + 
  labs(title = glue::glue('Variation in EVT_GP by disturbance code, by model')) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# save
ggsave(glue::glue('{export_fig_path}/{r1_name}_vs_{r2_name}_EVT_GP.png'),
       plot = last_plot(),
       width = 7, 
       height = 4.5)  

# Plot EVT GPS as sankey to see how classes change
# Aka "alluvial" plot
#----------------------------------------#
# # install dev version
# remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
# library(ggalluvial)

# nodes = dataset

# x = EVT_GP

# wrap by disturbance code

#p_r_alluvia <- 
  p_r_evt %>%
    mutate(ID = row_number()) %>%
    select(c(ID, CN, EVT_GP, disturb_code, dataset)) %>%
  pivot_wider(id_cols = ID, names_from  = dataset, values_from = EVT_GP)
    group_by(EVT_GP, dataset, disturb_code) %>%
    summarize(freq = n()) %>%
  left_join((evt_names))

is_alluvia_form(as.data.frame(p_r_alluvia))

library(RColorBrewer)
set3 <- colorRampPalette(brewer.pal('Set3',n=12))

# take 1
p_r_alluvia %>%
  as.data.frame %>%
  ggplot(aes(y = freq, axis1 = di, axis2 = dataset)) + 
  geom_alluvium(aes(fill = as.factor(EVT_GP_N_short)), width = 1/12)+
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", nudge_x = 0.05, label.size = 0.25, aes(label = after_stat(stratum)))+ 
  #scale_x_discrete(limits = c("Model", "Disturbance Code"), expand = c(.05, .05)) +
  #scale_fill_manual(values=c(brewer.pal(12,"Set3"),"#999999")) #+ 
  scale_fill_manual(values = setNames(set3(13), levels(p_r_alluvia$EVT_GP_N_short))) #+

  #ggtitle('EVT_GP transition by disturbance code, by model')

# take 2 example
data(majors)
majors$curriculum <- as.factor(majors$curriculum)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters")

# format data for take 2
p_r_alluvia_2 <- p_r_evt %>%
  select(c(PLOTID, EVT_GP, disturb_code, dataset)) %>%
  group_by(dataset) %>%
  slice_head(n = 15)

is_alluvia_form(p_r_alluvia_2)

p_r_alluvia_2 %>%
ggplot(aes(x = dataset, stratum = EVT_GP, alluvium = PLOTID,
           fill = EVT_GP, label = EVT_GP)) +
  #scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("EVT_GP across models")

# take 3 example

data(vaccinations)
vaccinations <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")


# format data for take 3
p_r_alluvia_3 <- p_r_evt %>%
  select(c(PLOTID, EVT_GP, disturb_code, dataset)) %>%
  group_by(EVT_GP, disturb_code, dataset) %>%
  #mutate(group = cur_group_id()) %>%
  group_by(EVT_GP, disturb_code, dataset) %>%
  summarize(freq = n()) %>%
  ungroup() %>%
  mutate(group = row_number())

is_alluvia_form(p_r_alluvia_3)

p_r_alluvia_3 %>%
  ggplot(aes(x = dataset, stratum = disturb_code, alluvium = EVT_GP,
      y = freq,
      fill = EVT_GP, label = EVT_GP)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("EVT Gp by model")

# format data for take 4 - ggsankey
library(ggsankey)

#pre = model 1 - with each evt group
# post = model2 with each evt group
# freq = #

library(tidyverse)
library(ggsankey)

db <- data.frame(pre = rep(c("DD", "LC", "NT",
                             "VU", "EN", "CR"), each = 6),
                 post = rep(c("DD", "LC", "NT",
                              "VU", "EN", "CR"), times = 6),
                 freq = rep(sample(seq(0:20), 6), 6))
db %>% 
  uncount(freq) #%>%
  filter(pre != "DD", post != "NT") %>%
  make_long(pre, post) %>%
  mutate(node = fct_relevel(node, "LC", "NT", "VU", "EN", "CR"), 
         next_node = fct_relevel(next_node, "DD", "LC", "VU", "EN", "CR")) %>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node))) +
  geom_alluvial() +
  scale_fill_manual(values = c("DD" = "#7C7C7C", "LC" = "#20AB5F", "NT" = "#3EFF00", "VU" = "#FBFF00", "EN" = "#FFBD00", "CR" = "#FF0C00"))
