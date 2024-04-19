# eval continuous vars - extract imputed id at actual fia id points
# then calc RMSE
# plot scatterplots w/ 1:1 line and plot RMSE / r^2 on the plot 
# save plots


# TO DO:
# for continuous scatter plots: add linear best-fit line
# add RMSE
# add MAE 

### SETUP AND RUN
######################################

# Specific inputs
#---------------------------------------------#

# list variables to evaluate
# list variables to evaluate
eval_vars_cat <- c("canopy_cover", "canopy_height", "EVT_GP", 
                   "disturb_code")

eval_vars_cont <- c("GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_D", "TPA_DEAD", "TPA_LIVE")

eval_vars <- c(eval_vars_cat, eval_vars_cont)


# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script_path <- paste(c(spl[c(1:(length(spl) - 1))],
                             "00_inputs_for_evaluation.R"),
                           collapse = "/")

source(input_script_path)

# Load data
#----------------------------------------------#

# load imputed raster for comparison
ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))
names(ras) <- "PLOTID"

# load X_df
X_df <- read.csv(xtable_path) %>%
  rename(PLOTID = ID)

# load evt dat
evt_dat <- read.csv(evt_path) 

# load coords
coords <- read.csv(coords_path)

# Load raster attribute table
#-------------------------------------------------#
rat <- terra::rast(glue::glue('{rat_path}TreeMap2016.tif'))
rat <- data.frame(cats(rat))

rat %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  select(-Value)

# prep evt data
#------------------------------------------------------#
evt_dat %<>%
  select(EVT_GP, EVT_GP_N, RED, GREEN, BLUE) %>%
  filter(!is.na(EVT_GP)) %>%
  distinct() %>%
  group_by(EVT_GP) %>%
  slice_head()  

# Prep coords data
#----------------------------------------------------------------#
coords %<>%
  filter(PLT_CN %in% rat$CN) # limit to CNs with reference values in RAT

# separate out into data frame
coords_df <- coords

# convert coords to spatial 
coords <- terra::vect(coords, geom = c("ACTUAL_LON", "ACTUAL_LAT"), crs = "epsg:4269")

# Prep data for plotting
#-----------------------------------------------------------------#
# Data Dictionary for extracts:
# ID: row number of FIA point in original coords
# PLOTID: imputed treemap plot id
# CN_pt : CN of FIA point in original coords
# CN_plot: CN of plot imputed to fia point

# reference table does not have values for PLOTID or CN_plot

# Extract  values to points - imputed plot ID at original FIA point
# -----------------------------------------------#
ras_ex <- terra::extract(ras, coords) %>%
  cbind(coords_df$PLT_CN) %>% # bind with CNs to identify plots
  rename("CN_pt" = `coords_df$PLT_CN`) %>%
  filter(!is.na(PLOTID)) # remove NAs

# join extracts with x table and RAT - join ri_ex$PLOTID to RAT$tm_id
# to get values of the imputed plot
ras_ex %<>% left_join(X_df,
                     by = c("PLOTID" = "PLOTID")) %>%
  left_join(rat, by = c("CN" = "CN", "PLOTID" = "tm_id")) %>%
  rename(CN_plot = "CN") %>%
  select(c(ID, CN_pt, CN_plot, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = "Imputed") 

# prep reference values - from "X_df" joined with RAT
refs_all <- 
  # ras_ex %>% 
  # select(ID, CN_pt) %>%
  # left_join(X_df, by = c("CN_pt" = "CN")) %>%
  X_df %>%
  left_join(rat, by = "CN") %>%
  select(c(CN, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = "Ground_FIA")%>%
  mutate_at(eval_vars_cont, ~na_if(.x , -99))

refs_zone <- 
  ras_ex %>%
  select(ID, CN_pt) %>%
  left_join(refs_all, by = c("CN_pt" = "CN")) %>%
  mutate(CN_plot = as.numeric(NA)) %>%
  mutate(PLOTID = as.numeric(NA))  %>%
  select(ID, CN_pt, CN_plot, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)), dataset)

  # join
p_r <- bind_rows(ras_ex, refs_zone) %>%
  # pivot longer
  pivot_longer(!c(ID, CN_pt, CN_plot, PLOTID, dataset), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         value = na_if(value, -99.0000),
         value = round(value, round_dig)) %>%
  arrange(ID)



# PLOT CATEGORICAL VARS
# ---------------------------------------------------#

for(i in 1:(length(eval_vars_cat)-1)) {
  
  # for testing
  #i = 2
  
  var_name <- eval_vars_cat[i]
  
  # Plot raw counts
  #----------------
  
  p <-  p_r %>%
    filter(var == var_name) %>%
    ggplot(aes(x=as.factor(value),  fill=dataset)) + 
    geom_bar(position = position_dodge2(preserve = "single")) +
    labs(title = glue::glue('Variation in {var_name}, by model')) + 
    xlab(var_name) +
    theme_bw()
  
  print(p)
  
  # save
  ggsave(glue::glue('{eval_dir}/03_FIA_Comparison/figs/Imputed_vs_FIA_{var_name}.png'),
         plot = p,
         width = 7,
         height = 4.5)
  
  # Normalized plot for categorical variables
  #--------------------
  
  # Calculate totals for datasets
  datasetTotals <- data.frame(p_r %>%
                                 filter(var == var_name) %>%
                                 group_by(dataset) %>%
                                 summarise(count = n()))
  
  # Calculate counts by dataset  for each category
  countsBy_dataset <- data.frame(p_r %>%
                                    filter(var == var_name) %>%
                                    group_by(value, dataset) %>%
                                    summarise(count = n()))
  
  # Temporary dataframe merging the categories counts and totals for normalization and  plotting
  temp_df <- merge(countsBy_dataset, datasetTotals, by = c("dataset"), all.x = TRUE)
  
  # Normalization
  temp_df$normalized_count <- temp_df$count.x/temp_df$count.y
  
  p_Norm <-  ggplot(data = temp_df, aes(x=as.factor(value), y=normalized_count, fill = dataset)) + 
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
    labs(title = glue::glue('Variation in {var_name} by model (normalized)')) + 
    xlab(var_name)
  
  print(p_Norm)
  
  ggsave(glue::glue('{eval_dir}/03_FIA_Comparison/figs/Imputed_vs_ref_{var_name}_normalized.png'),
         plot = p_Norm,
         width = 7,
         height = 4.5)
  
}

# PLOT CONTINUOUS VARS
# ---------------------------------------------------#

dodge <- position_dodge(width = 0.6)

for(i in 1:(length(eval_vars_cont))) {
  
  # for testing
  #i = 5
  
  var_name <- eval_vars_cont[i]
  
  # Violin plots
  #------------------#
  
  p <- p_r %>%
    filter(var == var_name) %>%
    ggplot(aes(x = dataset, y = value, fill = dataset))+
    geom_violin(position = dodge)+
    geom_boxplot(width=.1, outlier.colour=NA, position = dodge) + 
    labs(title = glue::glue('Variation in {var_name} by disturbance code, by model')) + 
    xlab(var_name)
  
  print(p)
  
  # Scatterplots
  #-------------------#
  p_r2 <- 
    p_r %>%
    filter(var == var_name) %>%
    select(-c(PLOTID, CN_plot)) %>%
    ungroup() %>%
    pivot_wider(names_from = dataset, values_from = value) %>%
    arrange(ID)
  
  lm <- lm(Imputed ~ Ground_FIA, data = p_r2)
  lm$coefficients
  sum <- summary(lm)
  
  sum$r.squared


  p2 <- p_r2 %>%
    ggplot() + 
    geom_abline(intercept = 0, color = "red", linewidth = 0.5 ) + 
    geom_point(aes(x = Ground_FIA, y = Imputed ), alpha = 0.25) 

  lm <- lm(p_r2$Imputed ~ p_r2$Ground_FIA)  
    
  # Parsing the information saved in the model to create the equation to be added to the scatterplot as an expression # https://r-graphics.org/recipe-scatter-fitlines-text
  eqn <- sprintf(
    "italic(y) == %.3g + %.3g * italic(x) * ',' * ~~ italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.4g",
    coef(lm)[1],
    coef(lm)[2],
    summary(lm)$r.squared,  # r-squared 
    sqrt(mean(lm$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
    mae(na.omit(p_r2$Imputed), predict(lm))
    )
  
  eqn
  
  p2 <- p_r2 %>%
    ggplot(aes(x = Ground_FIA, y = Imputed)) + 
    geom_abline(intercept = 0, color = "red", linewidth = 1, linetype = 2) + 
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", formula = y~x) +
    labs() + 
    theme_bw() + 
    ggtitle(glue::glue("Imputed vs. Ref for {var_name}"))
  
  print(p2)
  
  # calc r-squared 
  
  #lm Reference ~ LCMSDist ; extract r-squared
  # for each dataset, and for each disturbance code, and for all disturbance codes together
  
  
  # save
  ggsave(glue::glue('{eval_dir}/03_FIA_Comparison/figs/Imputed_vs_ref_{var_name}_violin.png'),
         plot = p,
         width = 7, 
         height = 4.5)    
  
  # save
  ggsave(glue::glue('{eval_dir}/03_FIA_Comparison/figs/Imputed_vs_ref_{var_name}_scatter.png'),
         plot = p2,
         width = 7, 
         height = 4.5) 
}

#########################################################
# Assemble layers- derived from imputed ids matched with X table
#########################################

eval_vars_cont %>%
  map(\(x) assembleExport(x, 
                          raster = ras, 
                          lookup = refs_all, 
                          id_field = "PLOTID",
                          export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}')
  ))

gc()


############################################################