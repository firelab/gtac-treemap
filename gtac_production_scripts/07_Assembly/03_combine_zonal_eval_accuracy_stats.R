# Combine zonal evaluation
# Get accuracy stats for each zone, for each metric
# And potentially for each year

# Combine into a single table and export
# Final outputs: 
# - Single table per year: Rows = zones, cols = attributes
# - One table: Rows = zones, cols = attributes x year

# Inputs
######################################################

#years
years <- c(2020, 2022)

# project name structure
project_name_base <- "Production_newXtable"

zones <- 'all' # options: 'all' or list of zone numbers
#zones <- c(seq(1,5,1))

# list names of vars desired for evaluation 
response_vars <-  c("evc", "evh", "evt_gp", "disturb_code_bin")

eval_vars <- c(response_vars, "disturb_code", "disturb_year")

# Types of evaluation to run and prepare reports for 
# Options: "model_eval", "CV"
eval_types <- c("model_eval", "TargetLayerComparison")

# where to export results, relative to home_dir
output_dir = '03_Outputs/08_Meta_Analysis/'

# Load library
######################################################

## load treemap library
lib_path <- glue::glue('{this.path::this.proj()}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


# Do the Work - setup
######################################################


# make zones list
if (length(zones) == 1) {
  if(zones == "all")
    zones_list = c(seq(from = 1, to = 10, by = 1), # all CONUS zones, skipping zone 11
                   seq(from = 12, to = 66, by = 1),
                   98, 99)
} else {
  zones_list = zones
}

# Do the work - looping
#############################################

# Create destination table
t = data.frame()


out_dat <- c()

# Loop over years
for (year in years){
  
  # set year for testing
  #year = 2022

  # make string for project name
  project_name <- glue::glue('{year}_{project_name_base}')
  
  # make string for project name in eval path
  project_name_path = gsub("_newXtable", "", project_name)
  
  # set evaluation directory
  eval_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_name}/03_Evaluation/")
  
  
  # Loop over zones
  for (zone_num in zones_list){
    
    # set zone
    #zone_num = 1
    
    # Set zone identifiers 
    cur_zone <- glue::glue('z{zone_num}') 
    cur_zone_zero <- if(zone_num < 10) {
      glue::glue('z0{zone_num}') } else {
        cur_zone
      }
    
    for (eval_type in eval_types) {
      
      # set eval type for testing
      #eval_type = "TargetLayerComparison"
      
      message(glue::glue("working on zone {zone_num} + {eval_type} + {year}"))
      
      # make path to RDS, specific to eval typ[e]
      if(eval_type == "model_eval"){
        eval_string = "00_Model_Evaluation"
        
        # build path to eval RDS
        eval_path <- glue::glue('{home_dir}03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/{cur_zone_zero}/model_eval/{cur_zone_zero}_{project_name_path}_CMs_ResponseVariables.RDS')
        
        # limit eval_vars to only those in the model
        eval_vars_in <- response_vars
        
      } else if(eval_type == "TargetLayerComparison"){
        eval_string = "01_Target_Layer_Comparison"
        
        # build path to eval RDS
        eval_path <- glue::glue('{eval_dir}/{cur_zone_zero}/{eval_string}/{cur_zone_zero}_{project_name_path}_CMs_{eval_type}.RDS')
        
        # keep eval vars
        eval_vars_in = eval_vars
        
      } else if(eval_type == "CV") {
        eval_string = "03_Cross_Validation"
        
        # build path to eval RDS
        eval_path <- glue::glue('{eval_dir}/{cur_zone_zero}/{eval_string}/{cur_zone_zero}_{project_name_path}_CMs_{eval_type}.RDS')
        # keep eval vars
        eval_vars_in = eval_vars
        
      } else { message("Enter a valid evaluation type")}
    
      
      
      if(!file.exists(eval_path)) {
        message(glue::glue("file does not exist: {eval_path}. moving on to next"))}
      else{
        
        # Read in RDS
        dat <- read_rds(eval_path)
    
        # Pull out accuracy for attributes of interest
        
        # make an empty data frame to append data into
        acc_df = data.frame(var = character(),
                            acc = numeric())
        
        for (var in eval_vars_in) {
          
          #var = 'evc' # for testing
          
          message(glue::glue('getting stats for {var}'))
          
          dat_var = dat[var][[1]]$overall
          row.names(dat_var) <- NULL
          
          if(!is.null(dat_var)) {
            acc = dat_var %>%
              dplyr::filter(metric == "Accuracy") %>%
              dplyr::rename("acc" = value) %>%
              dplyr::mutate(var = var) %>%
              dplyr::select(var, acc) 
              
            acc_df = rbind(acc_df, acc)
          } else { message(glue::glue('{var} does not exist; moving on to next'))}
          
          } # end loop over vars
      
      acc_df %<>%
        dplyr::mutate(zone = zone_num,
                      cur_zone_zero = cur_zone_zero,
                      eval_type = eval_type,
                      year = year)
  
  # Join with table
  out_dat = rbind(out_dat, acc_df)
  
}}}}

str(out_dat)

# Prep out table for export
out_years_long <- 
out_dat %>%
  pivot_wider(names_from = var, values_from = acc)

out_years_wide <-
out_dat %>%
  pivot_wider(names_from = c(var, year), 
              values_from = acc,
              names_sort = TRUE)


# Export tables

output_dir = glue::glue('{home_dir}/{output_dir}/')

if(!file.exists(output_dir)){
  dir.create(output_dir)
}

output_years <- paste(years, collapse = "_")

write.csv(out_years_long, glue::glue('{output_dir}/{output_years}_{project_name_base}_eval_var_accuracy_allZones_yearsLong.csv'),
          row.names = FALSE)

write.csv(out_years_wide, glue::glue('{output_dir}/{output_years}_{project_name_base}_eval_var_accuracy_allZones_yearsWide.csv'),
          row.names = FALSE)

# Summarize to national level
national_acc <- 
    out_years_long %>% 
    filter(eval_type == "TargetLayerComparison") %>%
      group_by(year) %>%
    summarise(evc = mean(evc),
              evh = mean(evh),
              evt_gp = mean(evt_gp)) 

# Get total number of unique plots imputed
##################################################

for(year in years) {
  
  message(glue::glue('calculating number of unique plots in {year} imputation'))
  
  # load tif
  ras <- terra::rast(glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/TreeMap_CONUS_{year}.tif')
  
  # load overall x table
  xtable <- read.csv(glue::glue('{home_dir}/03_Outputs/06_Reference_Data/v{year}/02_X_table_CONUS/x_table_complete_CONUS_{year}.csv')

  # Unique plots imputed in zone
  unique_plts <- length(freq(ras)$value)

  # total # plots available
  total_plts <- nrow(xtable)

  # of total available plots imputed
  pct = unique_plots / total_plts



}
