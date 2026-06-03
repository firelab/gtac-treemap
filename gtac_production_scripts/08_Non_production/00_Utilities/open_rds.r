
# Set  up libraries and functions
library(tools)
library(tidyverse)
library(magrittr)
library(waldo)
`%notin%` <- Negate('%in%')

# Which models differ in how unchanged px are treated between 2022 and 2023?
rds_path <- "C:/Users/lleatherman/Desktop/zones_with_differing_models_2022_2023.rds"

df <- readr::read_rds(rds_path)
print(df)

# List all possible input paths
path1 <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2023/01_X_tables_by_zone/v2022"
path2 <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2023/01_X_tables_by_zone/v2023"
path3 <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2022/01_X_tables_by_zone/"
path4 <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2022/01_X_tables_by_zone_incorrect_NC/"




# Select paths to compare
dir1 <- path1
dir2 <- path3

# Run
##################################

col_names <- c("Zone", "PLT_CN", "SLOPE", "ASPECT", "ELEV", "prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd", "disturb_code", "disturb_year", "canopy_cover", "canopy_height", "EVT_GP", "TM_ID")

files1 <- list.files(dir1, full.names = TRUE)
files2 <- list.files(dir2, full.names = TRUE)

# Compare file names
common_files <- intersect(basename(files1), basename(files2))

# Sort by zone number extracted from file name
zone_num <- function(filename) {
  as.numeric(sub(".*x_table_(\\d+).*", "\\1", filename))
}
files_sorted <- common_files[order(zone_num(common_files))]

not_identical_files <- c()
file_diffs <- list()

# Compare file contents
for (f in files_sorted) {
  identical <- identical(
    readLines(file.path(dir1, f)),
    readLines(file.path(dir2, f))
  )
  cat(f, "identical:", identical, "\n")

    if (!identical) {
        # add to a list of non-identical files
        not_identical_files <- c(not_identical_files, f)
        # store the differences for later review
        lines1 <- readLines(file.path(dir1, f))
        lines2 <- readLines(file.path(dir2, f))
        diff1 <- setdiff(lines1, lines2)
        diff2 <- setdiff(lines2, lines1)
        file_diffs[[f]] <- list(
            only_in_dir1 = diff1,
            only_in_dir2 = diff2
        )
    }

}

# check how many unique ids are in the non-identical files
get_ids <- function(diff_list) {
  l <- strsplit(diff_list, split = ",")
  ids <- sapply(l, function(x) x[2])
  return(ids)
}

ids_dir1 <- lapply(file_diffs, function(x) get_ids(x$only_in_dir1))
ids_dir2 <- lapply(file_diffs, function(x) get_ids(x$only_in_dir2))

to_df = function(ids_list) {
  df = data.frame(unlist(ids_list))
  names(df) <- c("CN")
  df$csv_row <- row.names(df)
  row.names(df) <- NULL
  return(df)
}

ids_dir1_df <- to_df(ids_dir1)
ids_dir2_df <- to_df(ids_dir2)

ids_dir1_df$only_in <- "dir1"
ids_dir2_df$only_in <- "dir2"

df_out <- rbind(ids_dir1_df, ids_dir2_df)
str(df_out)

# prep df
df_out %<>%
    mutate(csv = str_replace(csv_row, ".csv[0-9]+", ".csv" ),
    zone = str_extract(csv, "[0-9]+"),
    row = str_replace(csv_row, csv, "")) 

# summarize unique ids
df_out %>%
    filter(zone %in% c(8, 12)) %>%
    group_by(CN, only_in) %>%
    group_by(CN) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

# which ids appear in the same zone in both dir 1 and dir 2? 
df_out %>%
    filter(zone %in% c(8, 12)) %>%
    select(CN, only_in, zone) %>%
    group_by(CN, only_in) %>%
    filter(n() > 1) %>%
    arrange(zone, CN) %>%
    ungroup() %>%
    select(CN) %>%
    distinct()

# which diffs are due to variable differences, and in which variable? 



file_diffs

f1 <- "23,216959441020004,40,345,1222,1810.49133300781,331.672668457031,5.88176012039185,16.7982330322266,3.27557063102722,669.944946289062,1137.02758789062,0,99,15,18,625,30583"
f2 <- "23,216959441020004,40,345,1222,1810.49133300781,331.672668457031,5.88176012039185,16.7982330322266,3.27557063102722,669.944946289062,1137.02758789062,0,99,55,18,625,30583"
identical(f1, f2)
df_compare = rbind(strsplit(f1, split = ",")[[1]], strsplit(f2, split = ",")[[1]])
colnames(df_compare) <- col_names
df_compare

dlapply(file_diffs$x_table_12.csv$only_in_dir1, function(x) c(strsplit(x, split = ",")[[1]]))
