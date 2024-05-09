startTime <- Sys.time()

home_dir <- "//166.2.126.25/TreeMap"

#################################################################
# Load required packages
#################################################################

# packages required
list.of.packages <- c("this.path", "terra", "tidyverse", "magrittr", 
                      "glue", "tictoc", "caret", "yaImpute", "randomForest", "this.path")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

projectsDir <- "03_Outputs/07_Projects"
zone_num <- 16
zone_name <- glue::glue("z{zone_num}")
evalDir <- glue::glue("03_Evaluation/{zone_name}/02_Target_Layer_Comparison")
tempDir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir"

project_name <- "2016_GTAC_Test"
# project_name <- "2016_GTAC_LCMSDist"

assembled_dir <- glue::glue('{home_dir}/{projectsDir}/{project_name}/02_Assembled_model_outputs/{zone_name}')

# Variables to compare
# - Target and impute vars 1:4 are compared among datasets with each other with the reference (5 bar plots) -- as both target and imputed variables are present.
# - Impute vars 5:11 are compared among the datasets with reference (3 bar plots).


impute_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code", "CANOPYPCT", "CARBON_D", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS", "TPA_DEAD", "TPA_LIVE")

for (i in 5:length(impute_vars)){
  
    loopStartTime <- Sys.time()
  
    ImpVar <- impute_vars[i]
  
    print(paste0("Creating plot for ", ImpVar, "..."))
  
    # Load imputed rasters
    
    LFOrig_imputed <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36_{ImpVar}.tif"))
    
    LCMSDist_imputed<- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_LCMSDist/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_GTAC_LCMSDist_tilesz2000_nT36_{ImpVar}.tif"))
    
    
    # Convert rasters to df and compute frequency tables
    
    # Raster to df
    LFOrig_imputed_df <- terra::as.data.frame(LFOrig_imputed)
    names(LFOrig_imputed_df) <- ImpVar
    LFOrig_imputed_df$dataset <- "imputed_LF"
    
    # Raster to df
    LCMSDist_imputed_df <- terra::as.data.frame(LCMSDist_imputed)
    names(LCMSDist_imputed_df) <- ImpVar
    LCMSDist_imputed_df$dataset <- "imputed_LCMS"
    
    # remove rasters (free up memory)
    rm(LFOrig_imputed)
    rm(LCMSDist_imputed)
    gc()
    
    
    merge_df <- rbind(LFOrig_imputed_df, LCMSDist_imputed_df)
    
    # Change all -99 values to 0 (for continuous variables)
    merge_df[[ImpVar]][merge_df[[ImpVar]] == -99] <- 0
    
    merge_df$dataset <- factor(merge_df$dataset, levels = c("FIA", "imputed_LF", "target_LF", "imputed_LCMS", "target_LCMS"))
    
    merge_dfSubset <- merge_df[0:20,]
    
    # p <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], fill = dataset))+
    #   geom_histogram(position = position_dodge2(preserve = "single")) +
    #   scale_fill_manual(name = "Dataset",
    #                     labels = c("FIA (Ground)", "LFOrig (Imputed)", "LCMSDist (Imputed)"),
    #                     values = c("springgreen4", "darkgoldenrod1", "cyan")) +
    #   labs(title = glue::glue("Frequency of {ImpVar} in dataset"),
    #        x = ImpVar) +
    #   # scale_x_continuous(breaks = unique(merge_df[[ImpVar]])) +
    #   # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
    #   theme_bw()
    
    options(scipen=100000000)
    
    p_sc <- ggplot(data = merge_dfSubset, aes(x = .data[[ImpVar]], fill = dataset))+
      geom_histogram(aes(y = after_stat(density)), position = position_dodge2(preserve = "single")) +
      scale_fill_manual(name = "Dataset", 
                        labels = c("FIA (Ground)", "LFOrig (Imputed)", "LCMSDist (Imputed)"),
                        values = c("springgreen4", "darkgoldenrod1", "cyan")) +
      labs(title = glue::glue("Frequency density of {ImpVar} in dataset"),
           x = ImpVar, 
           y = "Density") + 
      # scale_x_continuous(breaks = unique(merge_df[[ImpVar]])) +
      # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) + 
      theme_bw()
    
    
    export_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/Ref_vs_impute_target_freq_BarPlots/"
    
    #save plots
    
    # ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_FreqPlot.png"), 
    #        plot = p, 
    #        widfh = 16, 
    #        height = 9)
    
    
    ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_NormFreqPlot.png"), 
           plot = p_n, 
           widfh = 16, 
           height = 9) 
  }
  
  print("Complete! Moving to next...")
  print(Sys.time() - loopStartTime)
}

print("Plotting complete!")
print(Sys.time() - startTime)
