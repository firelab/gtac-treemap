library(readr)

options(scipen=999)

complete_x_table<- read.csv("Y:/03_Outputs/06_Reference_Data/v2020/02_X_table_CONUS/x_table_complete_CONUS_2020.csv")
all_cns<- complete_x_table$PLT_CN

# get all fuzzed coordinates
plot_tables<- list.files("Y:/01_Data/04_FIA/05_FIA_DataMart/CSV", pattern="_PLOT", full.names = T)

columns_to_keep <- c("CN", "LAT", "LON")

# Assemble blank dataframe for storing outputs
all_states<- data.frame("CN" = "CN",
                      "LAT" = "LAT",
                      "LON" = "LON")
all_states<- all_states[-1,]


for(i in seq_along(plot_tables)){
  state<- read_csv(plot_tables[i], col_select = columns_to_keep)
  # only keep the needed csv's
  state<- state[state$CN %in% all_cns,]
  
  # bind this state to the others
  all_states<- rbind(all_states, state)
  
  #
  print(paste0("Finished state ",i))
}


write.csv(all_states,"Y:/01_Data/04_FIA/06_Coordinates/FuzzedCoordinates/fuzzed_coordinates_2020_2022_xtable_plots.csv")

