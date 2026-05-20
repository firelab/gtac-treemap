# Written by Abhinav Shrestha (abhinav.shrestha@usda.gov)

# Objective: Compare output rasters - extract values to 40 m buffered FIA plots (polygons)

# Last update: 05/20/2024


startTime <- Sys.time()
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

########################################################
# Setup
########################################################

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


target_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")

impute_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code", "CANOPYPCT", "CARBON_D", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS", "TPA_DEAD", "TPA_LIVE", "")

for (i in 5:length(impute_vars)){
  
  loopStartTime <- Sys.time()
  
  print(paste0("Creating plot for ", impute_vars[i], "..."))
  
  if (i <= 4){
    
    # ===== CATAGORICAL VARIABLES =====
    
    # i = 10
    
    TargVar <- target_vars[i]
    
    ImpVar <- impute_vars[i]
    
    
    # Load imputed rasters
    
    LFOrig_imputed <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36_{ImpVar}.tif"))
    
    # LCMSDist_imputed<- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_LCMSDist/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_GTAC_LCMSDist_tilesz2000_nT36_{ImpVar}.tif"))
     
    
    
    # Load target rasters
    
    if (TargVar == "disturb_code"){
      LFOrig_target  <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/{TargVar}_LF.tif"))
      LCMSDist_target <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/{TargVar}_LFLCMS.tif"))
    } else {
      LFOrig_target  <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/{TargVar}.tif"))
      LCMSDist_target <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/{TargVar}.tif"))
    }
    
    
    
    # load X_df
    
    if (!exists(x = "X_df")){
      # Path to X table
      
      # xtable_path <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt" # ALL ref
      
      # X-df used to build model for z16
      xtable_path <- glue::glue("{home_dir}/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_GTAC_Test_GTACTarget_Data_Xdf_bin.csv")
      
      X_df <- data.frame(read.csv(xtable_path)) 
      
      # %>% rename(PLOTID = ID) # un-comment and add if all refs are considered
     
    }
    
   
    
    # Subset to just var of interest column
    X_df_subset <- data.frame(X_df[, ImpVar])
    names(X_df_subset) <- "Var"
    
    if(ImpVar == "EVT_GP"){
      
      evt_gp_remap_table_path <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/EVG_remap_table.csv"
      
      # load evt_gp remap table
      evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)
      
      
      X_df_subset <- X_df_subset %>% 
        mutate(Var = recode(Var, !!!setNames(evt_gp_remap_table$EVT_GP, evt_gp_remap_table$EVT_GP_remap)))
      
      # https://stackoverflow.com/questions/66231321/recode-values-based-on-look-up-table-with-dplyr-r
    }
    
   
    X_df_subset_FreqTable <- as.data.frame(table(X_df_subset$Var))
    
    names(X_df_subset_FreqTable) <- c(ImpVar, "Frequency")

    X_df_subset_FreqTable$dataset <- "FIA"

    X_df_subset_FreqTable$Freq_norm <- X_df_subset_FreqTable$Frequency/sum(X_df_subset_FreqTable$Frequency)
    
    
    # Convert rasters to DF and compute frequency tables
    
    
    # ----- LANDFIRE RASTERS -----
    
    
    # # Raster to df for LF imputed raster
    # LFOrig_imputed_df <- data.frame(LFOrig_imputed)
    # names(LFOrig_imputed_df) <- "Var"
    # # Change all -99 values to NA (for categorical variables)
    # LFOrig_imputed_df <- LFOrig_imputed_df %>% 
    #                         mutate(Var = ifelse(Var == -99, NA, Var))
    # 
    # # Calc freq tables
    # LFOrig_imputed_freqTable <- as.data.frame(table(LFOrig_imputed_df$Var))
    # names(LFOrig_imputed_freqTable) <- c(ImpVar, "Frequency")
    # LFOrig_imputed_freqTable$dataset <- "imputed_LF"
    # LFOrig_imputed_freqTable$Freq_norm <- LFOrig_imputed_freqTable$Frequency/sum(LFOrig_imputed_freqTable$Frequency)
    
    
    ### NEW 
    # Change raster values of -99 to NA
    LFOrig_imputed_NAs <- terra::subst(LFOrig_imputed, -99, NA)
    rm(LFOrig_imputed)
    
    # Calc freq tables
    LFOrig_imputed_freqTable_Terra <- terra::freq(LFOrig_imputed_NAs)
    LFOrig_imputed_freqTable_Terra <- LFOrig_imputed_freqTable_Terra[, -1] # remove the "layer" column -- useful for multidimentional rasters.
    names(LFOrig_imputed_freqTable_Terra) <- c(ImpVar, "Frequency")
    LFOrig_imputed_freqTable_Terra$dataset <- "imputed_LF"
    LFOrig_imputed_freqTable_Terra$Freq_norm <- LFOrig_imputed_freqTable_Terra$Frequency/sum(LFOrig_imputed_freqTable_Terra$Frequency)
    
    head(LFOrig_imputed_freqTable)
    head(LFOrig_imputed_freqTable_Terra)
    
    # Raster to df for LF target raster
    LFOrig_target_df <- data.frame(LFOrig_target)
    names(LFOrig_target_df) <- "Var"
    
    if(ImpVar == "EVT_GP"){
      
      # evt_gp_remap_table_path <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/EVG_remap_table.csv"
      # 
      # # load evt_gp remap table
      # evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)
      
      
      LFOrig_target_df <- LFOrig_target_df %>% 
                mutate(Var = recode(Var, !!!setNames(evt_gp_remap_table$EVT_GP, evt_gp_remap_table$EVT_GP_remap)))
      
      # https://stackoverflow.com/questions/66231321/recode-values-based-on-look-up-table-with-dplyr-r
    }
    
    
    # Change all -99 values to NA (for categorical variables)
    LFOrig_target_df <- LFOrig_target_df %>% 
      mutate(Var = ifelse(Var == -99, NA, Var))
    
    
    # Calc freq tables
    LFOrig_target_freqTable <- as.data.frame(table(LFOrig_target_df$Var))
    names(LFOrig_target_freqTable) <- c(ImpVar, "Frequency")
    LFOrig_target_freqTable$dataset <- "target_LF"
    LFOrig_target_freqTable$Freq_norm <- LFOrig_target_freqTable$Frequency/sum(LFOrig_target_freqTable$Frequency)
 
  
       
    
    # ----- LCMS RASTERS -----
    
    # Raster to df for LCMS imputed raster
    LCMSDist_imputed_df <- data.frame(LCMSDist_imputed)
    names(LCMSDist_imputed_df) <- "Var"
    # Change all -99 values to NA (for categorical variables)
    LCMSDist_imputed_df <- LCMSDist_imputed_df %>% 
      mutate(Var = ifelse(Var == -99, NA, Var))
   
    
    # Calc freq tables
    LCMSDist_imputed_freqTable <- as.data.frame(table(LCMSDist_imputed_df$Var))
    names(LCMSDist_imputed_freqTable) <- c(ImpVar, "Frequency")
    LCMSDist_imputed_freqTable$dataset <- "imputed_LCMS"
    LCMSDist_imputed_freqTable$Freq_norm <- LCMSDist_imputed_freqTable$Frequency/sum(LCMSDist_imputed_freqTable$Frequency)
    
    
    
    # # Raster to df for LCMS target raster 
    LCMSDist_target_df <- data.frame(LCMSDist_target)
    names(LCMSDist_target_df) <- "Var"
    
    if(ImpVar == "EVT_GP"){
      
      # evt_gp_remap_table_path <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v2016_GTAC/z16/01_final/EVG_remap_table.csv"
      # 
      # # load evt_gp remap table
      # evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)
      
      
      LCMSDist_target_df <- LCMSDist_target_df %>% 
        mutate(Var = recode(Var, !!!setNames(evt_gp_remap_table$EVT_GP, evt_gp_remap_table$EVT_GP_remap)))
      
      # https://stackoverflow.com/questions/66231321/recode-values-based-on-look-up-table-with-dplyr-r
    }
    
    
    
    # Change all -99 values to NA (for categorical variables)
    LCMSDist_target_df <- LCMSDist_target_df %>% 
      mutate(Var = ifelse(Var == -99, NA, Var))


    # Calc freq tables
    LCMSDist_target_freqTable <- as.data.frame(table(LCMSDist_target_df$Var))
    names(LCMSDist_target_freqTable) <- c(ImpVar, "Frequency")
    LCMSDist_target_freqTable$dataset <- "target_LCMS"
    LCMSDist_target_freqTable$Freq_norm <- LCMSDist_target_freqTable$Frequency/sum(LCMSDist_target_freqTable$Frequency)
    
    
    # ----- HOUSEKEEPING -----
    
    # remove rasters (free up memory)
    rm(LFOrig_imputed)
    rm(LFOrig_target)
    rm(LCMSDist_imputed)
    rm(LCMSDist_target)
    gc()
    
    
    # ----- MERGE dataframes and PLOT -----
    
    merge_df <- rbind(X_df_subset_FreqTable, LFOrig_imputed_freqTable, LFOrig_target_freqTable, LCMSDist_imputed_freqTable, LCMSDist_target_freqTable)

    merge_df[[ImpVar]] <- factor(merge_df[[ImpVar]], levels = sort(as.numeric(as.character(unique(merge_df[[ImpVar]])))))
    merge_df$dataset <- factor(merge_df$dataset, levels = c("FIA", "imputed_LF", "target_LF", "imputed_LCMS", "target_LCMS"))
    
    
    # If ALL Refs are used as X_df, un-comment below to filter plot to GPs only in target and impute datasets
    # if (ImpVar == "EVT_GP") {
    # AOI_EVT_GPs <- sort(unique(c(sort(unique(LFOrig_target_freqTable$EVT_GP)), sort(unique(LFOrig_imputed_freqTable$EVT_GP)), sort(unique(LCMSDist_target_freqTable$EVT_GP)), sort(unique(LCMSDist_imputed_freqTable$EVT_GP))))) 
    # 
    # }
    
    if (ImpVar == "disturb_code"){
      
      # p <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], y = Frequency, fill = dataset))+
      #   geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      #   scale_fill_manual(name = "Dataset",
      #                     labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
      #                     values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
      #   labs(title = glue::glue("Frequency of {ImpVar} in dataset"),
      #        x = ImpVar) +
      #   # scale_x_continuous(breaks = unique(merge_df[[ImpVar]])) +
      #   scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
      #   theme_bw()
      
      
      p_n <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], y = Freq_norm, fill = dataset))+
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
        scale_fill_manual(name = "Dataset",
                          labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
                          values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
        labs(title = glue::glue("Frequency of {ImpVar} in dataset (normalized)"),
             x = ImpVar) +
        # scale_x_continuous(breaks = unique(merge_df[[ImpVar]])) +
        scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
        theme_bw()
      
      export_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/Ref_vs_impute_target_freq_BarPlots/"
      
      #save plots
      
      # ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_FreqPlot.png"), 
      #        plot = p, 
      #        width = 16, 
      #        height = 9)
      
      
      ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_NormFreqPlot.png"), 
             plot = p_n, 
             width = 16, 
             height = 9)
      
    } else if (ImpVar == "EVT_GP") {
      
      # p <- merge_df %>%
      #         filter(EVT_GP %in% AOI_EVT_GPs) %>%
      #         ggplot(aes(x = .data[[ImpVar]], y = Frequency, fill = dataset))+
      #           geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      #           scale_fill_manual(name = "Dataset",
      #                             labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
      #                             values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
      #           labs(title = glue::glue("Frequency of {ImpVar} in dataset"),
      #                x = ImpVar) +
      #           scale_x_discrete(breaks = unique(merge_df[[ImpVar]])) +
      #           # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
      #           theme_bw()
    
    p_n <- merge_df %>% 
      # filter(EVT_GP %in% AOI_EVT_GPs) %>% # Un-comment if ALL refs are used as `X_df`
      ggplot(aes(x = .data[[ImpVar]], y = Freq_norm, fill = dataset))+
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      scale_fill_manual(name = "Dataset",
                        labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
                        values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
      labs(title = glue::glue("Frequency of {ImpVar} in dataset (normalized)"),
           x = ImpVar) +
      scale_x_discrete(breaks = unique(merge_df[[ImpVar]])) +
      # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
      theme_bw()
    
    export_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/Ref_vs_impute_target_freq_BarPlots/"
    
    #save plots
    
    # ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_FreqPlot.png"), 
    #        plot = p, 
    #        width = 16, 
    #        height = 9)
    
    
    ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_NormFreqPlot.png"), 
           plot = p_n, 
           width = 16, 
           height = 9)
    
  } else {
    
      # p <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], y = Frequency, fill = dataset))+
      #   geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      #   scale_fill_manual(name = "Dataset",
      #                     labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
      #                     values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
      #   labs(title = glue::glue("Frequency of {ImpVar} in dataset"),
      #        x = ImpVar) +
      #   scale_x_discrete(breaks = unique(merge_df[[ImpVar]])) +
      #   # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
      #   theme_bw()
      
      
      p_n <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], y = Freq_norm, fill = dataset))+
              geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
              scale_fill_manual(name = "Dataset",
                                labels = c("FIA (Ground)", "LFOrig (Imputed)", "LFOrig (Target)", "LCMSDist (Imputed)", "LCMSDist (Target)"),
                                values = c("springgreen4", "darkgoldenrod1", "darkgoldenrod", "cyan", "cyan4")) +
              labs(title = glue::glue("Frequency of {ImpVar} in dataset (normalized)"),
                   x = ImpVar) +
              scale_x_discrete(breaks = unique(merge_df[[ImpVar]])) +
              # scale_x_discrete(labels = c("None", "Fire", "Slow Loss")) +
              theme_bw()
            
      export_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/Ref_vs_impute_target_freq_BarPlots/"
      
      #save plots
      
      # ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_FreqPlot.png"), 
      #        plot = p, 
      #        width = 16, 
      #        height = 9)
      
      
      ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_NormFreqPlot.png"), 
             plot = p_n, 
             width = 16, 
             height = 9)
      
    }  
  
  } else {
    
    # ===== CONTIUOUS VARIABLES =====
    
    # Data not present as target layers (imputed only)

    ImpVar <- impute_vars[i]
    
    # Load imputed rasters
    
    LFOrig_imputed <- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36_{ImpVar}.tif"))
    
    LCMSDist_imputed<- terra::rast(glue::glue("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_LCMSDist/02_Assembled_model_outputs/z16/02_Assembled_vars/2016_GTAC_LCMSDist_tilesz2000_nT36_{ImpVar}.tif"))
    
    
    if (!exists(x = "refs")){
     
     # Path to X table
      
      # X-df used to build model for z16
      xtable_path <- glue::glue("{home_dir}/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_GTAC_Test_GTACTarget_Data_Xdf_bin.csv")
      
      X_df <- data.frame(read.csv(xtable_path))
     
      xtableALL_path <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt") # ALL ref
     
      X_df_ALL_ID_CN <- data.frame(read.csv(xtableALL_path)) %>% 
       rename(PLOTID = ID) %>% 
       select(c(PLOTID, CN))
     
      X_df <- X_df %>% 
                left_join(X_df_ALL_ID_CN, by = c("X" = "PLOTID")) %>% 
                rename(PLOTID = X)
                  
     
     
      # set location of raster attribute table
      rat_path <- glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")
      rat <- terra::rast(glue::glue('{rat_path}TreeMap2016.tif'))
      rat <- data.frame(terra::cats(rat))
      
      rat %<>%
        rename("SDIPCT_RMRS" = SDIPCT_RMR,
               "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
        mutate(CN = as.numeric(CN)) %>%
        select(-Value)
    
    
      # prep reference values - from "X_df" joined with RAT (ONLY X_df rows used to build model, and present in z16 are used)
      refs <- X_df %>%
        left_join(rat, by = c("CN" = "CN")) %>%
        select(c(CN, PLOTID, any_of(impute_vars))) %>%
        drop_na()
     
      rm(rat) 
     
    }   
    
    # Subset to just var of interest column
    X_df_subset <- data.frame(refs[, ImpVar])
    names(X_df_subset) <- ImpVar
    X_df_subset$dataset <- "FIA"
    
    # Change all -99 values to 0 (for continuous variables)
    X_df_subset$SDIPCT_RMRS[X_df_subset$SDIPCT_RMRS == -99] <- NA

       # Convert rasters to DF and compute frequency tables
    
    # Raster to df
    LFOrig_imputed_df <- data.frame(LFOrig_imputed)
    names(LFOrig_imputed_df) <- ImpVar
    LFOrig_imputed_df$dataset <- "imputed_LF"
    
     # Raster to df
    LCMSDist_imputed_df <- data.frame(LCMSDist_imputed)
    names(LCMSDist_imputed_df) <- ImpVar
    LCMSDist_imputed_df$dataset <- "imputed_LCMS"
    
    # remove rasters (free up memory)
    rm(LFOrig_imputed)
    rm(LCMSDist_imputed)
    gc()
    
    
    merge_df <- rbind(X_df_subset, LFOrig_imputed_df, LCMSDist_imputed_df)
    
    # Change all -99 values to 0 (for continuous variables)
    merge_df[[ImpVar]][merge_df[[ImpVar]] == -99] <- NA
    
    merge_df$dataset <- factor(merge_df$dataset, levels = c("FIA", "imputed_LF", "target_LF", "imputed_LCMS", "target_LCMS"))
  
    
    
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
    
    p_n <- ggplot(data = merge_df, aes(x = .data[[ImpVar]], fill = dataset))+
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
    #        width = 16, 
    #        height = 9)
    
    
    ggsave(filename = glue::glue("{export_dir}Ref_v_Imputed_v_Target_{ImpVar}_NormFreqPlot.png"), 
           plot = p_n, 
           width = 16, 
           height = 9) 
  }
  
  print("Complete! Moving to next...")
  print(Sys.time() - loopStartTime)
}

print("Plotting complete!")
print(Sys.time() - startTime)
