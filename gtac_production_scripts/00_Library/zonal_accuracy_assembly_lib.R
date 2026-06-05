build_conus_zones <- function(zones = "all") {
  if (length(zones) == 1 && identical(zones, "all")) {
    return(c(seq(from = 1, to = 10, by = 1), seq(from = 12, to = 66, by = 1), 98, 99))
  }
  zones
}

normalize_project_name <- function(year, project_name_or_suffix) {
  if (grepl(glue::glue("^{year}_"), project_name_or_suffix)) {
    return(project_name_or_suffix)
  }
  glue::glue("{year}_{project_name_or_suffix}")
}

get_eval_rds_path <- function(eval_type,
                              home_dir,
                              year,
                              project_name,
                              project_name_path,
                              cur_zone_zero) {
  eval_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/{year}/{project_name}/03_Evaluation")

  if (eval_type == "model_eval") {
    return(glue::glue(
      "{home_dir}03_Outputs/07_Projects/{year}/{project_name}/01_Raw_model_outputs/{cur_zone_zero}/model_eval/{cur_zone_zero}_{project_name_path}_CMs_ResponseVariables.RDS"
    ))
  }

  if (eval_type == "TargetLayerComparison") {
    return(glue::glue(
      "{eval_dir}/{cur_zone_zero}/01_Target_Layer_Comparison/{cur_zone_zero}_{project_name_path}_CMs_{eval_type}.RDS"
    ))
  }

  if (eval_type == "CV") {
    return(glue::glue(
      "{eval_dir}/{cur_zone_zero}/03_Cross_Validation/{cur_zone_zero}_{project_name_path}_CMs_{eval_type}.RDS"
    ))
  }

  stop(glue::glue("Unsupported eval_type: {eval_type}"))
}

find_existing_eval_rds_path <- function(eval_type,
                                        home_dir,
                                        year,
                                        project_names,
                                        project_name_paths,
                                        cur_zone_zero) {
  for (i in seq_along(project_names)) {
    eval_path <- get_eval_rds_path(
      eval_type = eval_type,
      home_dir = home_dir,
      year = year,
      project_name = project_names[[i]],
      project_name_path = project_name_paths[[i]],
      cur_zone_zero = cur_zone_zero
    )

    if (file.exists(eval_path)) {
      return(list(path = eval_path, project_name = project_names[[i]]))
    }
  }

  list(path = NULL, project_name = NULL)
}

extract_accuracy_rows <- function(dat, vars_to_extract) {
  acc_rows <- list()

  for (var in vars_to_extract) {
    if (!var %in% names(dat)) {
      message(glue::glue("{var} does not exist; moving on to next"))
      next
    }

    dat_var <- dat[[var]]$overall
    if (is.null(dat_var)) {
      message(glue::glue("{var} overall metrics are missing; moving on to next"))
      next
    }

    row.names(dat_var) <- NULL
    acc <- dat_var |>
      dplyr::filter(metric == "Accuracy") |>
      dplyr::rename(acc = value) |>
      dplyr::mutate(var = var) |>
      dplyr::select(var, acc)

    acc_rows[[length(acc_rows) + 1]] <- acc
  }

  if (length(acc_rows) == 0) {
    return(data.frame(var = character(), acc = numeric()))
  }

  dplyr::bind_rows(acc_rows)
}

run_single_year_accuracy <- function(year,
                                           primary_project_name_suffix,
                                           fallback_project_name_suffixes = character(0),
                                           study_area,
                                           zones,
                                           response_vars,
                                           eval_vars,
                                           eval_types,
                                           home_dir,
                                           verbose = TRUE) {

  # 1. Set up zones and dirs
  #--------------------------------------------------------------#                                          
  zones_list <- build_conus_zones(zones)
  primary_project_name <- normalize_project_name(year, primary_project_name_suffix)

  fallback_project_name_suffixes <- as.character(fallback_project_name_suffixes)
  fallback_project_names <- if (length(fallback_project_name_suffixes) > 0) {
    vapply(
      fallback_project_name_suffixes,
      FUN = function(sfx) normalize_project_name(year, sfx),
      FUN.VALUE = character(1)
    )
  } else {
    character(0)
  }

  fallback_project_names <- unique(fallback_project_names[fallback_project_names != primary_project_name])
  eval_project_names <- c(primary_project_name, fallback_project_names)
  eval_project_name_paths <- gsub("_newXtable", "", eval_project_names)

  eval_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/{year}/{primary_project_name}/03_Evaluation")
  national_dir <- glue::glue("{eval_dir}/National")
  dir.create(national_dir, recursive = TRUE, showWarnings = FALSE)

  # 2. Count unique pixels in raster vs x-table for context on imputation coverage
  #--------------------------------------------------------------#
  message(glue::glue("working on {year}; counting unique pixels"))
  
  # build paths and read in raster, RAT, and x-table for counting unique IDs in the raster vs available in the x-table
  r_path <- glue::glue("{home_dir}03_Outputs/07_Projects/{year}/{primary_project_name}/04_Mosaic_assembled_model_outputs/TreeMap{year}_{study_area}.tif")
  dbf_path <- glue::glue("{r_path}.vat.dbf")
  xtable_path <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/v{year}/02_X_table_{study_area}/x_table_complete_{study_area}_{year}.csv")

  # load in data for counting unique IDs in the raster vs available in the x-table
  xtable <- read.csv(xtable_path)
  r <- terra::rast(r_path)
  rat <- foreign::read.dbf(dbf_path)

  # check that rat has "Value" and "Count" columns
    if (!all(c("Value", "Count") %in% names(rat))) {
        stop(glue::glue("RAT is missing required columns 'Value' and 'Count'. Found columns: {paste(names(rat), collapse = ', ')}"))
    }
  
  print(glue::glue("Counting unique IDs in the raster {r_path}"))

  # count # forested pix in raster
  r_freq <- as.data.frame(terra::freq(r))
  n_forested_px <- sum(r_freq$count, na.rm = TRUE)
  n_unique <- nrow(r_freq)

  unique_ids <- data.frame(
    n_unique = nrow(rat),
    n_available = nrow(xtable),
    pct_imputed = nrow(rat) / nrow(xtable),
    n_forested_px = n_forested_px
  )

  print("Unique ID summary: ")
    print(glue::glue("n_unique: {unique_ids$n_unique}\n n_available: {unique_ids$n_available}\n pct_imputed: {unique_ids$pct_imputed}\n n_forested_px: {unique_ids$n_forested_px}"))

  unique_ids_path <- glue::glue("{national_dir}/Treemap{year}_{study_area}_uniqueIdsImputed.csv")
  write.csv(unique_ids, unique_ids_path, row.names = FALSE)

  rm(r, rat, xtable)

  # 3. Loop through zones and eval types to combine accuracy stats
  #--------------------------------------------------------------#
  out_dat_year <- data.frame(
    var = character(),
    acc = numeric(),
    zone = numeric(),
    cur_zone_zero = character(),
    eval_type = character(),
    year = numeric(),
    model_run = character(),
    stringsAsFactors = FALSE
  )

  for (zone_num in zones_list) {
    cur_zone <- glue::glue("z{zone_num}")
    cur_zone_zero <- if (zone_num < 10) glue::glue("z0{zone_num}") else cur_zone

    for (eval_type in eval_types) {
      message(glue::glue("getting eval stats for {cur_zone_zero} + {eval_type} + {year}"))

      eval_vars_in <- if (eval_type == "model_eval") response_vars else eval_vars
      eval_lookup <- find_existing_eval_rds_path(
        eval_type = eval_type,
        home_dir = home_dir,
        year = year,
        project_names = eval_project_names,
        project_name_paths = eval_project_name_paths,
        cur_zone_zero = cur_zone_zero
      )

      eval_path <- eval_lookup$path
      eval_project_name <- eval_lookup$project_name

      if (is.null(eval_path)) {
        attempted_paths <- vapply(
          seq_along(eval_project_names),
          FUN = function(i) {
            get_eval_rds_path(
              eval_type = eval_type,
              home_dir = home_dir,
              year = year,
              project_name = eval_project_names[[i]],
              project_name_path = eval_project_name_paths[[i]],
              cur_zone_zero = cur_zone_zero
            )
          },
          FUN.VALUE = character(1)
        )
        message(glue::glue("No eval file found after trying {length(attempted_paths)} project name(s) for zone {cur_zone_zero} + {eval_type}."))
        message(glue::glue("Attempted paths: {paste(attempted_paths, collapse = ' | ')}"))
        next
      }

      dat <- readRDS(eval_path)
      acc_df <- extract_accuracy_rows(dat, eval_vars_in)

      if (nrow(acc_df) == 0) {
        next
      }

      acc_df <- acc_df |>
        dplyr::mutate(
          zone = zone_num,
          cur_zone_zero = cur_zone_zero,
          eval_type = eval_type,
          year = year,
          model_run = primary_project_name_suffix,
          eval_project_name = eval_project_name
        )

      out_dat_year <- dplyr::bind_rows(out_dat_year, acc_df)
    }
  }

  if (nrow(out_dat_year) > 0) {
    out_dat_year_avg <- out_dat_year |>
      dplyr::group_by(var, eval_type) |>
      dplyr::summarize(national_avg_acc = mean(acc, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(eval_type)
  } else {
    out_dat_year_avg <- data.frame(
      var = character(),
      eval_type = character(),
      national_avg_acc = numeric(),
      stringsAsFactors = FALSE
    )
  }

  zonal_path <- glue::glue("{national_dir}/Treemap{year}_{study_area}_zonalAccuracy.csv")
  avg_path <- glue::glue("{national_dir}/Treemap{year}_{study_area}_nationalAvgAccuracy.csv")

  write.csv(out_dat_year, zonal_path, row.names = FALSE)
  write.csv(out_dat_year_avg, avg_path, row.names = FALSE)

  list(
    zonal_accuracy = out_dat_year,
    national_average = out_dat_year_avg,
    unique_ids = unique_ids,
    output_paths = list(
      zonal_accuracy = zonal_path,
      national_average = avg_path,
      unique_ids = unique_ids_path
    )
  )
}

read_single_year_accuracy <- function(year,
                                            project_name_suffix,
                                            study_area,
                                            home_dir) {
  project_name <- glue::glue("{year}_{project_name_suffix}")
  csv_path <- glue::glue(
    "{home_dir}/03_Outputs/07_Projects/{year}/{project_name}/03_Evaluation/National/Treemap{year}_{study_area}_zonalAccuracy.csv"
  )

  if (!file.exists(csv_path)) {
    warning(glue::glue("Missing zonal output for {project_name}: {csv_path}"))
    return(NULL)
  }

  dat <- read.csv(csv_path)
  dat$model_run <- project_name_suffix
  dat$project_name <- project_name
  dat
}

combine_zonal_accuracy_outputs <- function(years,
                                           project_name_suffixes,
                                           study_area,
                                           output_dir,
                                           home_dir) {
  if (length(years) != length(project_name_suffixes)) {
    stop("years and project_name_suffixes must be the same length")
  }

  year_runs <- Map(
    f = function(y, p) {
      read_single_year_accuracy(
        year = y,
        project_name_suffix = p,
        study_area = study_area,
        home_dir = home_dir
      )
    },
    y = years,
    p = project_name_suffixes
  )

  year_runs <- year_runs[!vapply(year_runs, is.null, logical(1))]
  if (length(year_runs) == 0) {
    stop("No single-year accuracy outputs were found to combine")
  }

  out_dat_all <- dplyr::bind_rows(year_runs)

  out_years_long <- out_dat_all |>
    tidyr::pivot_wider(names_from = var, values_from = acc) |>
    data.frame()

  out_years_wide <- out_dat_all |>
    tidyr::pivot_wider(
      names_from = c(model_run, var, year),
      values_from = acc,
      names_sort = TRUE
    ) |>
    data.frame()

  target_layer_dat <- out_years_long |>
    dplyr::filter(eval_type == "TargetLayerComparison")

  metric_cols <- c("evc", "evh", "evt_gp", "disturb_code")
  missing_metric_cols <- setdiff(metric_cols, names(target_layer_dat))
  if (length(missing_metric_cols) > 0) {
    for (col_name in missing_metric_cols) {
      target_layer_dat[[col_name]] <- NA_real_
    }
  }

  national_acc <- target_layer_dat |>
    dplyr::group_by(model_run, year) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(metric_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_years <- paste(years, collapse = "_")

  long_path <- glue::glue("{output_dir}/{output_years}_eval_var_accuracy_allZones_yearsLong.csv")
  wide_path <- glue::glue("{output_dir}/{output_years}_eval_var_accuracy_allZones_yearsWide.csv")
  nat_path <- glue::glue("{output_dir}/{output_years}_national_accuracy.csv")

  write.csv(out_years_long, long_path, row.names = FALSE)
  write.csv(out_years_wide, wide_path, row.names = FALSE)
  write.csv(national_acc, nat_path, row.names = FALSE)

  list(
    all_rows = out_dat_all,
    years_long = out_years_long,
    years_wide = out_years_wide,
    national_accuracy = national_acc,
    output_paths = list(long = long_path, wide = wide_path, national = nat_path)
  )
}
