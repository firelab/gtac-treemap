# Exploratory data analysis for trends in accuracy across years and zones

library(tidyverse, ggplot2, glue)

home_dir <- "//166.2.126.25/TreeMap/"

dat_path <- glue::glue("{home_dir}/03_Outputs/08_Meta_Analysis/2020_2022_2023_eval_var_accuracy_allZones_yearsLong.csv")

dat <- read.csv(dat_path)

eval_vars <- c("evc", "evh", "evt_gp", "disturb_code_bin", "disturb_code", "disturb_year")

figs_dir <- glue::glue("{home_dir}/03_Outputs/08_Meta_Analysis/Figures/")
if (!dir.exists(figs_dir)) {
    dir.create(figs_dir)
}

# massage the data to make it easier to filter - make the data long
dat_long <- dat %>%
    pivot_longer(cols = all_of(eval_vars),
                 names_to = "var",
                 values_to = "acc") %>%
    mutate(zone_f = factor(zone, levels = unique(zone))) # make zone a factor for plotting, with levels in order of appearance

# Get unique zones present in the data
unique_zones <- sort(unique(dat_long$zone))
n_zones <- length(unique_zones)

# Create a color palette based on the actual zones present
zone_colors <- viridisLite::viridis(n_zones)
zonenames(zone_colors) <- as.character(unique_zones)
    
# For a single variable being evaluated, plot accuracy across years and zones
# Time series across years
# Separate lines for each zone
for(var in eval_vars) {

    #var_name = "evc"
    
    #filter data
    plot_data <- dat_long %>%
        filter(eval_type == "TargetLayerComparison", var == var_name)

    # Check data structure
    # print(head(plot_data, 20))
    # print(paste("Number of unique zones:", length(unique(plot_data$zone))))
    # print(paste("Number of unique years:", length(unique(plot_data$year))))
    # print(paste("Total rows:", nrow(plot_data)))

    p <- plot_data %>%
        #filter(zone %in% seq(1,5,1)) %>% # filter to specific zones if needed
        ggplot(aes(x = year, y = acc, color = zone_f, group = zone_f)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = zone_colors) +
        labs(title = glue::glue("Accuracy of {var_name} across Years and Zones"),
            x = "Year",
            y = "Accuracy",
            color = "Zone") +
        theme_minimal()
    print(p)

    ggsave(glue::glue("{figs_dir}/{var}_accuracy_across_years_zones.png"), width = 10, height = 6)
}
    
