# DATA QUALITY CHECK FUNCTIONS FOR WATER QUALITY AND FISHERIES DATA

# This script provides functions to run standard QC checks on environmental
# monitoring data. Functions are organized by data type and return structured
# QC results with flags and diagnostic information. These are basic functions
# and do not handle many edge cases. Data will likely need to be processed before
# running the function to ensure the right input structure.

# The invertebrate and carcass survey QC functions have been the least developed
# and tested. All functions can be improved by handling more edge cases.

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# MAIN QC DISPATCH FUNCTION

#' @title Run Data Quality Checks Based on Data Type
#' @description Dispatch function that triggers additional functions based on data type.
#' @param data Data frame containing the data to check. Requires specific column names.
#' Water quality: datetime, parameter, value, site_id; Fish observation: date, site_id, species, count (optional),
#' fork_length (optional), weight (optional), observer (optional); Carcass survey: date, site_id, survey_period,
#' carcass_id, condition, sex, fork_length, marked; RST catch: date, trap_id, species, life_stage, count,
#' hours_fished (optional); Invertebrate: date, site_id, sample_id, taxon, count; Habitat: date, site_id, transect_id,
#' depth, velocity, substrate (optional); Chemistry: analyte, result, units,
#' sample_id (optional), date (optional), site_id (optional),
#' mdl (optional), pql (optional), lab_qc_type (optional),
#' detected (optional - TRUE/FALSE for detection status)
#' @param data_type Character string specifying data type:
#'   "water_quality", "fish_observation", "carcass_survey",
#'   "rst_catch", "invertebrate", "habitat", "chemistry"
#' @param ... Additional arguments passed to specific QC functions
#' @return List containing QC results, flags, and summary statistics and plots for water quality data.
#' @examples
#' fish_data <- read_csv("data/clean/microhabitat_observations.csv")
#' fish_data_format <- fish_data |>
#'     select(date, species, micro_hab_data_tbl_id, count) |>
#'     rename(site_id = micro_hab_data_tbl_id) |>
#'     filter(count > 0)
#' qc <- run_qc_checks(fish_data_format, "fish_observation")
#' qc$flags |> View()
#'
#' @export

run_qc_checks <- function(data, data_type, ...) {

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  if (nrow(data) == 0) {
    warning("Data frame is empty")
    return(list(status = "empty", flags = NULL, summary = NULL))
  }

  # Dispatch to appropriate QC function
  qc_result <- switch(
    data_type,
    "water_quality" = qc_water_quality(data, ...),
    "fish_observation" = qc_fish_observation(data, ...),  # New combined function
    "fish_count" = qc_fish_observation(data, ...),        # Backward compatible
    "fish_measurement" = qc_fish_observation(data, ...),  # Backward compatible
    "carcass_survey" = qc_carcass_survey(data, ...),
    "rst_catch" = qc_rst_catch(data, ...),
    "invertebrate" = qc_invertebrate(data, ...),
    "habitat" = qc_habitat(data, ...),
    "chemistry" = qc_chemistry(data, ...),
    stop(paste("Unknown data type:", data_type))
  )

  # Add metadata
  qc_result$metadata <- list(
    data_type = data_type,
    n_rows = nrow(data),
    n_flagged = sum(qc_result$flags$flag != "PASS", na.rm = TRUE),
    check_date = Sys.time()
  )

  return(qc_result)
}

# WATER QUALITY QC FUNCTIONS

#' @title QC Checks for Continuous Water Quality Data
#' @description This function checks if the following parameters are within range,
#' have a reasonable rate of change, do not flat line or have sensor drift:
#' temperature, dissolved_oxygen, pH, conductivity, turbidity. The parameters
#' should be spelled as listed.
#' @param data Data frame with columns: datetime, parameter, value, site_id
#' @param temp_range Numeric vector of min/max acceptable temperature (°C)
#' @param do_range Numeric vector of min/max acceptable DO (mg/L)
#' @param ph_range Numeric vector of min/max acceptable pH
#' @param cond_range Numeric vector of min/max acceptable conductivity (μS/cm)
#' @param turb_range Numeric vector of min/max acceptable turbidity (NTU)
#' @param drift_window Integer, number of days to check for drift
#' @return List with flags, summary, and plots
qc_water_quality <- function(data,
                             temp_range = c(-1, 35),
                             do_range = c(0, 20),
                             ph_range = c(5.0, 10.0),
                             cond_range = c(0, 3000),
                             turb_range = c(0, 1000),
                             drift_window = 7) {

  # Ensure datetime column
  if (!"datetime" %in% names(data)) {
    stop("Data must contain 'datetime' column")
  }

  # Initialize flags data frame
  flags <- data.frame(
    row_id = 1:nrow(data),
    datetime = data$datetime,
    parameter = data$parameter,
    value = data$value,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Run checks by parameter
  for (param in unique(data$parameter)) {
    param_data <- data %>% filter(parameter == param)
    param_idx <- which(data$parameter == param)

    # Initialize list to collect check results
    check_results <- list()

    if (param == "temperature") {
      check_results$range <- check_range(param_data$value, temp_range, "Temperature")
      check_results$rate_of_change <- check_rate_of_change(param_data, max_change = 5)
      check_results$flat_line <- check_flat_line(param_data, threshold = 0.1, hours = 4)

    } else if (param == "dissolved_oxygen") {
      check_results$range <- check_range(param_data$value, do_range, "DO")
      check_results$saturation <- check_do_saturation(param_data, data)
      check_results$drift <- check_sensor_drift(param_data, window = drift_window)

    } else if (param == "pH") {
      check_results$range <- check_range(param_data$value, ph_range, "pH")
      check_results$rate_of_change <- check_rate_of_change(param_data, max_change = 0.5)

    } else if (param == "conductivity") {
      check_results$range <- check_range(param_data$value, cond_range, "Conductivity")
      check_results$drift <- check_sensor_drift(param_data, window = drift_window)

    } else if (param == "turbidity") {
      check_results$range <- check_range(param_data$value, turb_range, "Turbidity")
      check_results$drift <- check_sensor_drift(param_data, window = drift_window)
    }

    # Consolidate flags for this parameter
    for (i in seq_along(param_idx)) {
      idx <- param_idx[i]

      # Collect all flag types for this row
      row_flags <- sapply(check_results, function(x) x[i])

      # Determine overall flag (REJECT > SUSPECT > PASS)
      if (any(row_flags == "REJECT", na.rm = TRUE)) {
        flags$flag[idx] <- "REJECT"
      } else if (any(row_flags == "SUSPECT", na.rm = TRUE)) {
        flags$flag[idx] <- "SUSPECT"
      } else {
        flags$flag[idx] <- "PASS"
      }

      # Collect flag reasons
      failed_checks <- names(row_flags)[!is.na(row_flags) & row_flags != "PASS"]
      if (length(failed_checks) > 0) {
        flags$flag_reason[idx] <- paste(failed_checks, collapse = "; ")
      }
    }
  }

  # Calculate summary statistics
  summary_stats <- data %>%
    group_by(parameter) %>%
    summarise(
      n = n(),
      n_missing = sum(is.na(value)),
      n_flagged = sum(flags$flag[flags$parameter == first(parameter)] != "PASS"),
      pct_flagged = n_flagged / n * 100,
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Generate diagnostic plots
  plots <- list()

  # Time series with flags
  plots$timeseries <- ggplot(data %>% left_join(flags, by = c("datetime", "parameter", "value"))) +
    geom_line(aes(x = datetime, y = value), color = "gray70") +
    geom_point(aes(x = datetime, y = value, color = flag), size = 1) +
    scale_color_manual(values = c("PASS" = "darkgreen", "SUSPECT" = "orange", "REJECT" = "red")) +
    facet_wrap(~parameter, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(title = "Water Quality Time Series with QC Flags",
         x = "Date/Time", y = "Value")

  return(list(
    flags = flags,
    summary = summary_stats,
    plots = plots
  ))
}

# HELPER FUNCTIONS - RANGE CHECKS

#' Check if values are within acceptable range
check_range <- function(values, range, param_name) {
  flags <- rep("PASS", length(values))
  flags[values < range[1] | values > range[2]] <- "REJECT"
  return(flags)
}

#' Check rate of change for sudden spikes/drops
check_rate_of_change <- function(data, max_change) {
  flags <- rep("PASS", nrow(data))

  if (nrow(data) < 2) return(flags)

  # Calculate change per hour
  time_diff <- as.numeric(diff(data$datetime), units = "hours")
  value_diff <- abs(diff(data$value))
  rate <- value_diff / pmax(time_diff, 0.25)  # Minimum 15 min interval

  flags[-1][rate > max_change] <- "SUSPECT"
  return(flags)
}

#' Check for flat-line periods
check_flat_line <- function(data, threshold = 0.1, hours = 4) {
  flags <- rep("PASS", nrow(data))

  if (nrow(data) < 10) return(flags)

  # Rolling SD over time window
  data <- data %>% arrange(datetime)
  window_size <- max(which(diff(data$datetime) <= hours(hours)))

  if (window_size >= 3) {
    rolling_sd <- zoo::rollapply(data$value, width = window_size,
                                 FUN = sd, na.rm = TRUE, fill = NA)
    flags[rolling_sd < threshold] <- "SUSPECT"
  }

  return(flags)
}

#' Check DO against saturation expectations
check_do_saturation <- function(do_data, all_data) {
  flags <- rep("PASS", nrow(do_data))

  # Try to find temperature data
  temp_data <- all_data %>%
    filter(parameter == "temperature") %>%
    select(datetime, temp_value = value)

  if (nrow(temp_data) == 0) return(flags)

  # Merge DO and temperature
  merged <- do_data %>%
    left_join(temp_data, by = "datetime")

  # Calculate expected 100% saturation (simplified)
  # DO_sat (mg/L) ≈ 14.6 - 0.4 * T (°C)
  merged$do_sat <- 14.6 - 0.4 * merged$temp_value
  merged$pct_sat <- (merged$value / merged$do_sat) * 100

  # Flag if >120% or <70% saturation
  flags[merged$pct_sat > 120 | merged$pct_sat < 70] <- "SUSPECT"
  return(flags)
}

#' Check for sensor drift using moving window
check_sensor_drift <- function(data, window = 7) {
  flags <- rep("PASS", nrow(data))

  if (nrow(data) < window * 24) return(flags)  # Need at least window days of hourly data

  # Calculate rolling mean
  data <- data %>% arrange(datetime)
  data$day <- as.Date(data$datetime)

  daily_mean <- data %>%
    group_by(day) %>%
    summarise(daily_mean = mean(value, na.rm = TRUE), .groups = "drop")

  if (nrow(daily_mean) < window) return(flags)

  # Linear trend test on daily means
  daily_mean$day_num <- as.numeric(daily_mean$day - min(daily_mean$day))
  trend_model <- lm(daily_mean ~ day_num, data = daily_mean)

  # Flag if significant trend (p < 0.05) and slope > 5% of mean per week
  p_value <- summary(trend_model)$coefficients[2, 4]
  slope <- coef(trend_model)[2]
  weekly_change <- abs(slope * 7)
  mean_value <- mean(daily_mean$daily_mean, na.rm = TRUE)

  if (p_value < 0.05 && weekly_change > 0.05 * mean_value) {
    flags <- rep("SUSPECT", length(flags))
  }

  return(flags)
}

# FISH OBSERVATION QC FUNCTIONS (COMBINED COUNTS AND MEASUREMENTS)

#' QC Checks for Fish Observation Data (Counts and Measurements)
#'
#' @param data Data frame with columns: date, site_id, species, count (optional),
#'             fork_length (optional), weight (optional), observer (optional)
#' @param max_count Maximum plausible count
#' @param length_range List of species-specific length ranges
#' @param check_interobs Check inter-observer agreement if duplicate counts
#' @param check_condition Check length-weight relationship
#' @return List with flags and summary
#' @export
qc_fish_observation <- function(data,
                                max_count = 10000,
                                length_range = NULL,
                                check_interobs = TRUE,
                                check_condition = TRUE) {

  # Determine what data is present
  has_counts <- "count" %in% names(data)
  has_lengths <- "fork_length" %in% names(data)
  has_weights <- "weight" %in% names(data)

  if (!has_counts && !has_lengths) {
    stop("Data must contain either 'count' or 'fork_length' columns")
  }

  # Default length ranges by species (mm)
  if (is.null(length_range)) {
    length_range <- list(
      "chinook_salmon" = c(20, 1200),
      "steelhead" = c(20, 1000),
      "coho_salmon" = c(20, 900),
      "delta_smelt" = c(20, 120),
      "longfin_smelt" = c(20, 150),
      "striped_bass" = c(30, 1500),
      "default" = c(10, 1500)
    )
  }

  flags <- data.frame(
    row_id = 1:nrow(data),
    date = data$date,
    site_id = data$site_id,
    species = data$species,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # COUNT-SPECIFIC CHECKS

  if (has_counts) {

    # Check for missing counts
    missing_idx <- which(is.na(data$count))
    if (length(missing_idx) > 0) {
      flags$flag[missing_idx] <- "REJECT"
      flags$flag_reason[missing_idx] <- "Missing count"
    }

    # Check for negative counts (skip NAs)
    negative_idx <- which(!is.na(data$count) & data$count < 0)
    if (length(negative_idx) > 0) {
      flags$flag[negative_idx] <- "REJECT"
      flags <- append_flag_reason(flags, negative_idx, "Negative count")
    }

    # Check for implausibly high counts (skip NAs)
    high_idx <- which(!is.na(data$count) & data$count > max_count)
    if (length(high_idx) > 0) {
      flags$flag[high_idx] <- "SUSPECT"
      flags <- append_flag_reason(flags, high_idx, "Exceeds maximum plausible count")
    }

    # Check for exact duplicates (potential data entry error)
    dup_cols <- c("date", "site_id", "species", "count")
    dup_cols <- dup_cols[dup_cols %in% names(data)]
    duplicates <- duplicated(data[, dup_cols])
    dup_idx <- which(duplicates)
    if (length(dup_idx) > 0) {
      flags$flag[dup_idx] <- "SUSPECT"
      flags <- append_flag_reason(flags, dup_idx, "Exact duplicate record")
    }

    # Inter-observer checks if observer column present
    if (check_interobs && "observer" %in% names(data)) {
      interobs_results <- check_interobserver_agreement(data)

      # Merge inter-observer flags
      for (i in 1:nrow(interobs_results)) {
        if (!is.na(interobs_results$interobs_flag[i]) &&
            interobs_results$interobs_flag[i] != "PASS") {
          row_id <- interobs_results$row_id[i]
          flags$flag[row_id] <- "SUSPECT"
          flags <- append_flag_reason(flags, row_id, "Inter-observer disagreement")
        }
      }
    }

    # Statistical outlier detection for counts (>3 SD from mean by species/site)
    outliers <- data %>%
      group_by(species, site_id) %>%
      mutate(
        mean_count = mean(count, na.rm = TRUE),
        sd_count = sd(count, na.rm = TRUE),
        z_score = (count - mean_count) / sd_count,
        is_outlier = abs(z_score) > 3 & !is.na(z_score)
      ) %>%
      ungroup() %>%
      pull(is_outlier)

    outlier_idx <- which(outliers & !is.na(outliers))
    if (length(outlier_idx) > 0) {
      flags$flag[outlier_idx] <- "SUSPECT"
      flags <- append_flag_reason(flags, outlier_idx, "Count statistical outlier")
    }
  }

  # LENGTH MEASUREMENT CHECKS

  if (has_lengths) {

    # Check for missing measurements
    missing_length_idx <- which(is.na(data$fork_length))
    if (length(missing_length_idx) > 0) {
      # Only flag as REJECT if this was supposed to be measured
      # (if count > 0 and no length, it might be a count-only survey)
      if (!has_counts || all(data$count[missing_length_idx] == 1, na.rm = TRUE)) {
        flags$flag[missing_length_idx] <- "REJECT"
        flags <- append_flag_reason(flags, missing_length_idx, "Missing length")
      }
    }

    # Check length ranges by species
    for (sp in unique(data$species)) {
      sp_idx <- which(data$species == sp & !is.na(data$fork_length))
      if (length(sp_idx) == 0) next

      sp_range <- length_range[[sp]]
      if (is.null(sp_range)) sp_range <- length_range[["default"]]

      out_of_range <- data$fork_length[sp_idx] < sp_range[1] |
        data$fork_length[sp_idx] > sp_range[2]

      out_idx <- sp_idx[out_of_range]
      if (length(out_idx) > 0) {
        flags$flag[out_idx] <- "REJECT"
        flags <- append_flag_reason(flags, out_idx, "Length outside species range")
      }
    }

    # Check for digit preference (heaping on 5s and 10s)
    if (sum(!is.na(data$fork_length)) > 50) {
      last_digit <- data$fork_length %% 10
      heaping_pct <- (sum(last_digit == 0 | last_digit == 5, na.rm = TRUE) /
                        sum(!is.na(last_digit))) * 100

      if (heaping_pct > 60) {
        message("Warning: Possible digit preference detected (",
                round(heaping_pct, 1), "% of measurements end in 0 or 5)")
      }
    }

    # Statistical outlier detection for lengths
    length_outliers <- data %>%
      filter(!is.na(fork_length)) %>%
      group_by(species) %>%
      mutate(
        mean_length = mean(fork_length, na.rm = TRUE),
        sd_length = sd(fork_length, na.rm = TRUE),
        z_score = (fork_length - mean_length) / sd_length,
        is_outlier = abs(z_score) > 3
      ) %>%
      ungroup()

    if (nrow(length_outliers) > 0) {
      outlier_rows <- which(data$fork_length %in%
                              length_outliers$fork_length[length_outliers$is_outlier])
      if (length(outlier_rows) > 0) {
        flags$flag[outlier_rows] <- "SUSPECT"
        flags <- append_flag_reason(flags, outlier_rows, "Length statistical outlier")
      }
    }
  }

  # WEIGHT AND CONDITION CHECKS

  if (has_weights && has_lengths && check_condition) {
    condition_flags <- check_condition_factor(data)

    for (i in 1:nrow(condition_flags)) {
      if (!is.na(condition_flags$condition_flag[i]) &&
          condition_flags$condition_flag[i] == "SUSPECT") {
        row_id <- condition_flags$row_id[i]
        flags$flag[row_id] <- "SUSPECT"
        flags <- append_flag_reason(flags, row_id, "Poor condition factor")
      }
    }
  }

  # SUMMARY STATISTICS

  # Create summary based on what data is available
  summary_stats <- data %>%
    group_by(species, site_id)

  if (has_counts) {
    summary_stats <- summary_stats %>%
      mutate(
        n_surveys = n(),
        mean_count = mean(count, na.rm = TRUE),
        sd_count = sd(count, na.rm = TRUE),
        cv_count = sd_count / mean_count * 100
      )
  }

  if (has_lengths) {
    summary_stats <- summary_stats %>%
      mutate(
        n_measured = sum(!is.na(fork_length)),
        mean_length = mean(fork_length, na.rm = TRUE),
        sd_length = sd(fork_length, na.rm = TRUE),
        min_length = min(fork_length, na.rm = TRUE),
        max_length = max(fork_length, na.rm = TRUE)
      )
  }

  if (has_weights) {
    summary_stats <- summary_stats %>%
      mutate(
        n_weighed = sum(!is.na(weight)),
        mean_weight = mean(weight, na.rm = TRUE)
      )
  }

  summary_stats <- summary_stats %>%
    summarise(
      across(everything(), first),
      n_flagged = sum(flags$flag[flags$species == first(species) &
                                   flags$site_id == first(site_id)] != "PASS"),
      .groups = "drop"
    )

  return(list(
    flags = flags,
    summary = summary_stats
  ))
}

#' Helper function to append flag reasons
append_flag_reason <- function(flags, idx, new_reason) {
  for (i in idx) {
    if (is.na(flags$flag_reason[i])) {
      flags$flag_reason[i] <- new_reason
    } else {
      flags$flag_reason[i] <- paste0(flags$flag_reason[i], "; ", new_reason)
    }
  }
  return(flags)  # Return modified dataframe
}

#' Check condition factor (K = weight / length^3 * 10^5)
check_condition_factor <- function(data) {

  result <- data %>%
    filter(!is.na(fork_length) & !is.na(weight)) %>%
    mutate(
      condition_factor = (weight / (fork_length ^ 3)) * 100000,
      condition_flag = case_when(
        condition_factor < 0.5 | condition_factor > 2.5 ~ "SUSPECT",
        TRUE ~ "PASS"
      )
    )

  return(data.frame(
    row_id = which(!is.na(data$fork_length) & !is.na(data$weight)),
    condition_flag = result$condition_flag
  ))
}

#' Check inter-observer agreement for duplicate counts
check_interobserver_agreement <- function(data) {

  # Find duplicate surveys (same date, site, species, different observer)
  dup_surveys <- data %>%
    group_by(date, site_id, species) %>%
    filter(n_distinct(observer) > 1) %>%
    arrange(date, site_id, species, observer) %>%
    summarise(
      count_1 = first(count),
      count_2 = last(count),
      observer_1 = first(observer),
      observer_2 = last(observer),
      .groups = "drop"
    )

  if (nrow(dup_surveys) == 0) {
    return(data.frame(row_id = 1:nrow(data)))
  }

  # Calculate agreement metrics
  dup_surveys$abs_diff <- abs(dup_surveys$count_1 - dup_surveys$count_2)
  dup_surveys$mean_count <- (dup_surveys$count_1 + dup_surveys$count_2) / 2
  dup_surveys$pct_diff <- (dup_surveys$abs_diff / dup_surveys$mean_count) * 100

  # Flag if disagreement > 15%
  dup_surveys$agreement_flag <- ifelse(dup_surveys$pct_diff > 15, "SUSPECT", "PASS")

  # Merge back to original data
  result <- data %>%
    left_join(dup_surveys, by = c("date", "site_id", "species"))

  return(data.frame(
    row_id = 1:nrow(data),
    interobs_flag = result$agreement_flag
  ))
}

# CARCASS SURVEY QC FUNCTIONS


#' QC Checks for Carcass Survey Data
#'
#' @param data Data frame with columns: date, site_id, survey_period,
#'             carcass_id, condition, sex, fork_length, marked
#' @return List with flags and summary
qc_carcass_survey <- function(data) {

  flags <- data.frame(
    row_id = 1:nrow(data),
    date = data$date,
    carcass_id = data$carcass_id,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Check for duplicate carcass IDs
  dup_ids <- duplicated(data$carcass_id) & !is.na(data$carcass_id)
  flags$flag[dup_ids] <- "REJECT"
  flags$flag_reason[dup_ids] <- "Duplicate carcass ID"

  # Check for marked carcasses appearing before they were marked
  if ("marked" %in% names(data) && "survey_period" %in% names(data)) {
    marked_data <- data %>%
      filter(marked == TRUE) %>%
      arrange(carcass_id, survey_period)

    # First appearance should be the marking event
    first_mark <- marked_data %>%
      group_by(carcass_id) %>%
      slice(1) %>%
      ungroup()

    # Subsequent appearances should be after first mark
    recaptures <- marked_data %>%
      anti_join(first_mark, by = c("carcass_id", "survey_period"))

    # Check temporal logic
    temporal_issues <- recaptures %>%
      left_join(first_mark %>%
                  select(carcass_id, first_period = survey_period),
                by = "carcass_id") %>%
      filter(survey_period <= first_period)

    if (nrow(temporal_issues) > 0) {
      flags$flag[flags$carcass_id %in% temporal_issues$carcass_id] <- "REJECT"
      flags$flag_reason[flags$carcass_id %in% temporal_issues$carcass_id] <-
        "Recapture before marking"
    }
  }

  # Check sex ratio (should approximate 1:1)
  if ("sex" %in% names(data)) {
    sex_ratio <- data %>%
      filter(!is.na(sex)) %>%
      count(sex) %>%
      pivot_wider(names_from = sex, values_from = n, values_fill = 0)

    if ("M" %in% names(sex_ratio) && "F" %in% names(sex_ratio)) {
      ratio <- sex_ratio$M / sex_ratio$F
      if (ratio < 0.3 || ratio > 3.0) {
        message("Warning: Sex ratio is outside normal range (M:F = ",
                round(ratio, 2), ":1)")
      }
    }
  }

  # Summary statistics
  summary_stats <- data %>%
    summarise(
      total_carcasses = n(),
      n_marked = sum(marked == TRUE, na.rm = TRUE),
      n_recaptures = sum(duplicated(carcass_id) & marked == TRUE, na.rm = TRUE),
      mark_rate = n_marked / total_carcasses * 100,
      recapture_rate = n_recaptures / n_marked * 100,
      mean_length = mean(fork_length, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    flags = flags,
    summary = summary_stats
  ))
}

# RST CATCH QC FUNCTIONS


#' QC Checks for Rotary Screw Trap Data
#'
#' @param data Data frame with columns: date, trap_id, species, life_stage, count,
#'             hours_fished (optional)
#' @param check_effort If TRUE and hours_fished present, check effort-related issues
#' @return List with flags and summary
qc_rst_catch <- function(data, check_effort = TRUE) {

  # Check if hours_fished column exists
  has_effort <- "hours_fished" %in% names(data)

  flags <- data.frame(
    row_id = 1:nrow(data),
    date = data$date,
    trap_id = data$trap_id,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Add hours_fished to flags if present
  if (has_effort) {
    flags$hours_fished <- data$hours_fished
  }

  flags$count <- data$count

  # BASIC COUNT CHECKS (always run)

  # Check for missing counts
  missing_idx <- which(is.na(data$count))
  if (length(missing_idx) > 0) {
    flags$flag[missing_idx] <- "REJECT"
    flags$flag_reason[missing_idx] <- "Missing count"
  }

  # Check for negative counts
  negative_idx <- which(!is.na(data$count) & data$count < 0)
  if (length(negative_idx) > 0) {
    flags$flag[negative_idx] <- "REJECT"
    flags <- append_flag_reason(flags, negative_idx, "Negative count")
  }

  # EFFORT-RELATED CHECKS (only if hours_fished present)

  if (has_effort && check_effort) {

    # Check for zero hours fished with non-zero catch
    zero_effort_idx <- which(!is.na(data$hours_fished) &
                               data$hours_fished == 0 &
                               !is.na(data$count) &
                               data$count > 0)
    if (length(zero_effort_idx) > 0) {
      flags$flag[zero_effort_idx] <- "REJECT"
      flags <- append_flag_reason(flags, zero_effort_idx, "Catch with zero effort")
    }

    # Check for implausible hours fished (>24)
    high_effort_idx <- which(!is.na(data$hours_fished) & data$hours_fished > 24)
    if (length(high_effort_idx) > 0) {
      flags$flag[high_effort_idx] <- "SUSPECT"
      flags <- append_flag_reason(flags, high_effort_idx, "Hours fished > 24")
    }

    # Check for negative hours
    negative_hours_idx <- which(!is.na(data$hours_fished) & data$hours_fished < 0)
    if (length(negative_hours_idx) > 0) {
      flags$flag[negative_hours_idx] <- "REJECT"
      flags <- append_flag_reason(flags, negative_hours_idx, "Negative hours fished")
    }

    # Calculate CPUE for outlier detection (only where hours > 0)
    data$cpue <- ifelse(!is.na(data$hours_fished) & data$hours_fished > 0,
                        data$count / data$hours_fished,
                        NA)

    # Statistical outlier detection for CPUE by species/life_stage
    if (sum(!is.na(data$cpue)) > 10) {  # Need enough data for outlier detection
      outliers <- data %>%
        filter(!is.na(cpue)) %>%
        group_by(trap_id, species, life_stage) %>%
        mutate(
          mean_cpue = mean(cpue, na.rm = TRUE),
          sd_cpue = sd(cpue, na.rm = TRUE),
          z_score = (cpue - mean_cpue) / sd_cpue,
          is_outlier = abs(z_score) > 3 & !is.na(z_score)
        ) %>%
        ungroup() %>%
        pull(is_outlier)

      # Map outliers back to original row indices
      cpue_rows <- which(!is.na(data$cpue))
      outlier_idx <- cpue_rows[outliers]

      if (length(outlier_idx) > 0) {
        flags$flag[outlier_idx] <- "SUSPECT"
        flags <- append_flag_reason(flags, outlier_idx, "CPUE statistical outlier")
      }
    }

  } else if (!has_effort) {
    # Statistical outlier detection on raw counts (when no effort data)
    if (nrow(data) > 10) {
      outliers <- data %>%
        group_by(trap_id, species, life_stage) %>%
        mutate(
          mean_count = mean(count, na.rm = TRUE),
          sd_count = sd(count, na.rm = TRUE),
          z_score = (count - mean_count) / sd_count,
          is_outlier = abs(z_score) > 3 & !is.na(z_score)
        ) %>%
        ungroup() %>%
        pull(is_outlier)

      outlier_idx <- which(outliers)
      if (length(outlier_idx) > 0) {
        flags$flag[outlier_idx] <- "SUSPECT"
        flags <- append_flag_reason(flags, outlier_idx, "Count statistical outlier")
      }
    }
  }

  # TEMPORAL CHECKS (always run)

  # Check for data gaps (>7 days between samples)
  data_sorted <- data %>% arrange(trap_id, date)
  for (trap in unique(data_sorted$trap_id)) {
    trap_data <- data_sorted %>% filter(trap_id == trap)
    if (nrow(trap_data) < 2) next

    date_diffs <- diff(trap_data$date)

    if (any(date_diffs > 7, na.rm = TRUE)) {
      n_gaps <- sum(date_diffs > 7, na.rm = TRUE)
      message("Warning: ", n_gaps, " data gap(s) > 7 days detected for trap ", trap)
    }
  }

  # SUMMARY STATISTICS

  summary_stats <- data %>%
    group_by(trap_id, species, life_stage)

  if (has_effort) {
    # Calculate CPUE if not already done
    if (!"cpue" %in% names(data)) {
      data$cpue <- ifelse(!is.na(data$hours_fished) & data$hours_fished > 0,
                          data$count / data$hours_fished,
                          NA)
    }

    summary_stats <- summary_stats %>%
      summarise(
        n_samples = n(),
        n_flagged = sum(flags$flag[flags$trap_id == first(trap_id) &
                                     flags$species == first(species)] != "PASS"),
        total_catch = sum(count, na.rm = TRUE),
        total_hours = sum(hours_fished, na.rm = TRUE),
        mean_cpue = mean(cpue, na.rm = TRUE),
        sd_cpue = sd(cpue, na.rm = TRUE),
        mean_count = mean(count, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_stats <- summary_stats %>%
      summarise(
        n_samples = n(),
        n_flagged = sum(flags$flag[flags$trap_id == first(trap_id) &
                                     flags$species == first(species)] != "PASS"),
        total_catch = sum(count, na.rm = TRUE),
        mean_count = mean(count, na.rm = TRUE),
        sd_count = sd(count, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Add note about effort data
  if (!has_effort) {
    message("Note: hours_fished column not found - effort-based checks skipped")
  }

  return(list(
    flags = flags,
    summary = summary_stats,
    has_effort_data = has_effort
  ))
}

# INVERTEBRATE QC FUNCTIONS

#' QC Checks for Invertebrate Sample Data
#'
#' @param data Data frame with columns: date, site_id, sample_id, taxon, count
#' @return List with flags and summary
qc_invertebrate <- function(data) {

  flags <- data.frame(
    row_id = 1:nrow(data),
    date = data$date,
    sample_id = data$sample_id,
    taxon = data$taxon,
    count = data$count,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Check for implausibly high counts
  flags$flag[data$count > 10000] <- "SUSPECT"
  flags$flag_reason[data$count > 10000] <- "Extremely high count"

  # Check for duplicate taxon entries in same sample
  dup_taxa <- data %>%
    group_by(sample_id, taxon) %>%
    filter(n() > 1) %>%
    ungroup()

  if (nrow(dup_taxa) > 0) {
    flags$flag[flags$sample_id %in% dup_taxa$sample_id] <- "SUSPECT"
    flags$flag_reason[flags$sample_id %in% dup_taxa$sample_id] <-
      "Duplicate taxon in sample"
  }

  # Calculate diversity metrics by sample
  diversity <- data %>%
    group_by(date, site_id, sample_id) %>%
    summarise(
      taxa_richness = n_distinct(taxon),
      total_abundance = sum(count, na.rm = TRUE),
      .groups = "drop"
    )

  # Flag samples with unusually low richness
  low_richness <- diversity$taxa_richness < 3
  if (any(low_richness)) {
    message("Warning: ", sum(low_richness), " samples have < 3 taxa")
  }

  # Summary statistics
  summary_stats <- data %>%
    group_by(site_id) %>%
    summarise(
      n_samples = n_distinct(sample_id),
      n_taxa = n_distinct(taxon),
      mean_abundance = mean(count, na.rm = TRUE),
      total_abundance = sum(count, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    flags = flags,
    summary = summary_stats,
    diversity = diversity
  ))
}

# HABITAT QC FUNCTIONS

#' QC Checks for Habitat Measurement Data
#'
#' @param data Data frame with columns: date, site_id, transect_id,
#'             depth, velocity, substrate (optional)
#' @return List with flags and summary
qc_habitat <- function(data) {

  flags <- data.frame(
    row_id = 1:nrow(data),
    date = data$date,
    site_id = data$site_id,
    depth = data$depth,
    velocity = data$velocity,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Check depth range (0-10 m)
  depth_out_idx <- which(!is.na(data$depth) & (data$depth < 0 | data$depth > 10))
  if (length(depth_out_idx) > 0) {
    flags$flag[depth_out_idx] <- "REJECT"
    flags$flag_reason[depth_out_idx] <- "Depth out of range"
  }

  # Check for missing depth
  depth_missing_idx <- which(is.na(data$depth))
  if (length(depth_missing_idx) > 0) {
    flags$flag[depth_missing_idx] <- "REJECT"
    flags <- append_flag_reason(flags, depth_missing_idx, "Missing depth")
  }

  # Check velocity range (0-3 m/s for most streams)
  velocity_out_idx <- which(!is.na(data$velocity) & (data$velocity < 0 | data$velocity > 3))
  if (length(velocity_out_idx) > 0) {
    flags$flag[velocity_out_idx] <- "SUSPECT"
    flags <- append_flag_reason(flags, velocity_out_idx, "Velocity unusual")
  }

  # Check for missing velocity
  velocity_missing_idx <- which(is.na(data$velocity))
  if (length(velocity_missing_idx) > 0) {
    flags$flag[velocity_missing_idx] <- "SUSPECT"
    flags <- append_flag_reason(flags, velocity_missing_idx, "Missing velocity")
  }

  # Check depth-velocity relationship (high velocity in shallow water is unusual)
  unusual_combo_idx <- which(!is.na(data$depth) & !is.na(data$velocity) &
                               data$depth < 0.3 & data$velocity > 1.5)
  if (length(unusual_combo_idx) > 0) {
    flags$flag[unusual_combo_idx] <- "SUSPECT"
    flags <- append_flag_reason(flags, unusual_combo_idx, "Unusual depth-velocity combo")
  }

  # Check substrate percentages sum to 100 if substrate composition provided
  if ("substrate_pct" %in% names(data)) {
    substrate_sums <- data %>%
      group_by(date, site_id, transect_id) %>%
      summarise(total_pct = sum(substrate_pct, na.rm = TRUE), .groups = "drop")

    bad_sums <- substrate_sums %>%
      filter(abs(total_pct - 100) > 2)  # Allow 2% tolerance

    if (nrow(bad_sums) > 0) {
      message("Warning: ", nrow(bad_sums),
              " transects have substrate % not summing to ~100")
    }
  }

  # Summary statistics
  summary_stats <- data %>%
    group_by(site_id) %>%
    summarise(
      n_measurements = n(),
      n_flagged = sum(flags$flag[flags$site_id == first(site_id)] != "PASS"),
      mean_depth = mean(depth, na.rm = TRUE),
      sd_depth = sd(depth, na.rm = TRUE),
      mean_velocity = mean(velocity, na.rm = TRUE),
      sd_velocity = sd(velocity, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    flags = flags,
    summary = summary_stats
  ))
}

# CHEMISTRY/LAB QC FUNCTIONS

#' QC Checks for Laboratory Chemistry Data
#'
#' @param data Data frame with columns: analyte, result, units,
#'             sample_id (optional), date (optional), site_id (optional),
#'             mdl (optional), pql (optional), lab_qc_type (optional),
#'             detected (optional - TRUE/FALSE for detection status)
#' @return List with flags and summary
qc_chemistry <- function(data) {

  # Check if sample_id exists, if not create one
  has_sample_id <- "sample_id" %in% names(data)
  if (!has_sample_id) {
    # Create sample ID from available columns
    if ("date" %in% names(data) && "site_id" %in% names(data)) {
      data$sample_id <- paste0(data$site_id, "_", data$date)
    } else {
      data$sample_id <- paste0("S", seq_len(nrow(data)))
    }
    message("Note: sample_id column not found - generated IDs from available data")
  }

  # Check if detected column exists
  has_detected <- "detected" %in% names(data)
  if (!has_detected) {
    message("Note: 'detected' column not found - will infer non-detects from result < mdl")
  }

  # IDENTIFY AND REMOVE ANALYTES WITH NON-NUMERIC VALUES

  # Check which analytes have non-numeric values in the result column
  if (!is.numeric(data$result)) {
    # Test conversion to numeric
    result_test <- suppressWarnings(as.numeric(data$result))

    # Find analytes with any non-numeric values
    non_numeric_by_analyte <- data %>%
      mutate(test_result = suppressWarnings(as.numeric(result))) %>%
      group_by(analyte) %>%
      summarise(
        has_non_numeric = any(is.na(test_result) & !is.na(result)),
        n_non_numeric = sum(is.na(test_result) & !is.na(result)),
        n_total = n(),
        .groups = "drop"
      ) %>%
      filter(has_non_numeric)

    if (nrow(non_numeric_by_analyte) > 0) {
      message("Removing ", nrow(non_numeric_by_analyte),
              " analyte(s) with non-numeric result values:")

      for (i in 1:nrow(non_numeric_by_analyte)) {
        message("  - ", non_numeric_by_analyte$analyte[i], ": ",
                non_numeric_by_analyte$n_non_numeric[i], " of ",
                non_numeric_by_analyte$n_total[i], " values non-numeric")
      }

      # Remove these analytes from the dataset
      data <- data %>%
        filter(!analyte %in% non_numeric_by_analyte$analyte)

      message("Remaining analytes: ", n_distinct(data$analyte))
    }

    # Now convert result to numeric (should be clean)
    data$result <- as.numeric(data$result)
  }

  # Check if we have any data left
  if (nrow(data) == 0) {
    stop("No analytes with numeric data remaining after filtering. ",
         "All analytes contained non-numeric values.")
  }

  # INFER NON-DETECTS IF DETECTED COLUMN NOT PROVIDED

  if (!has_detected) {
    # Infer non-detects from result and MDL
    if ("mdl" %in% names(data)) {
      # Ensure MDL is numeric
      if (!is.numeric(data$mdl)) {
        data$mdl <- suppressWarnings(as.numeric(data$mdl))
      }

      # Assume non-detect if result is NA and MDL is present
      # OR if result < MDL
      data$detected <- ifelse(
        is.na(data$result) & !is.na(data$mdl), FALSE,
        ifelse(!is.na(data$result) & !is.na(data$mdl) & data$result < data$mdl, FALSE,
               ifelse(!is.na(data$result), TRUE, NA))
      )

      n_inferred_nd <- sum(!data$detected, na.rm = TRUE)
      if (n_inferred_nd > 0) {
        message("Inferred ", n_inferred_nd, " non-detects from result < mdl or result = NA with mdl present")
      }
    } else {
      # No MDL available, assume all NA results are truly missing
      data$detected <- ifelse(is.na(data$result), NA, TRUE)
    }
  }

  flags <- data.frame(
    row_id = 1:nrow(data),
    sample_id = data$sample_id,
    analyte = data$analyte,
    result = data$result,
    detected = data$detected,
    flag = "PASS",
    flag_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # CHECK FOR MISSING vs NON-DETECT

  # Non-detects (detected = FALSE) - flag as "U" (undetected)
  non_detect_idx <- which(!is.na(data$detected) & data$detected == FALSE)
  if (length(non_detect_idx) > 0) {
    flags$flag[non_detect_idx] <- "U"  # U = Undetected/Non-detect
    flags$flag_reason[non_detect_idx] <- "Non-detect"
  }

  # Truly missing results (detected = NA, result = NA)
  missing_idx <- which(is.na(data$result) & is.na(data$detected))
  if (length(missing_idx) > 0) {
    flags$flag[missing_idx] <- "REJECT"
    flags$flag_reason[missing_idx] <- "Missing result"
  }

  # Check for negative results (usually invalid, but some parameters can be negative)
  # Skip if known to allow negatives (e.g., Redox potential)
  # Also skip non-detects
  negative_idx <- which(!is.na(data$result) &
                          data$result < 0 &
                          (is.na(data$detected) | data$detected == TRUE))
  if (length(negative_idx) > 0) {
    # Check if analyte typically allows negatives
    allow_negative <- data$analyte[negative_idx] %in%
      c("redox_potential", "ORP", "charge_balance", "delta_13C", "delta_15N", "delta_34S")

    reject_idx <- negative_idx[!allow_negative]
    if (length(reject_idx) > 0) {
      flags$flag[reject_idx] <- "REJECT"
      flags <- append_flag_reason(flags, reject_idx, "Negative result")
    }
  }

  # Check results below MDL (but not already flagged as non-detect)
  if ("mdl" %in% names(data)) {
    # Ensure MDL is numeric
    if (!is.numeric(data$mdl)) {
      data$mdl <- suppressWarnings(as.numeric(data$mdl))
    }

    below_mdl_idx <- which(!is.na(data$result) &
                             !is.na(data$mdl) &
                             data$result < data$mdl &
                             (is.na(data$detected) | data$detected == TRUE))
    if (length(below_mdl_idx) > 0) {
      flags$flag[below_mdl_idx] <- "J"  # Qualified/estimated
      flags <- append_flag_reason(flags, below_mdl_idx, "Below MDL")
    }
  }

  # Check for extremely high results (potential data entry errors)
  # Only check detected values
  numeric_results <- sum(!is.na(data$result) &
                           (is.na(data$detected) | data$detected == TRUE))
  if (numeric_results > 10) {

    # Group by analyte and check for outliers (detected values only)
    outlier_check <- data %>%
      filter(!is.na(result) & (is.na(detected) | detected == TRUE)) %>%
      group_by(analyte) %>%
      filter(n() >= 3) %>%  # Need at least 3 values to calculate meaningful median
      mutate(
        median_result = median(result, na.rm = TRUE),
        is_extreme = result > 1000 * median_result & median_result > 0
      ) %>%
      ungroup()

    if (nrow(outlier_check) > 0) {
      extreme_rows <- which(data$result %in% outlier_check$result[outlier_check$is_extreme])
      if (length(extreme_rows) > 0) {
        flags$flag[extreme_rows] <- "SUSPECT"
        flags <- append_flag_reason(flags, extreme_rows, "Extremely high result")
      }
    }
  }

  # SUMMARY STATISTICS

  # Determine grouping for summary
  group_vars <- "analyte"

  if ("site_id" %in% names(data)) {
    group_vars <- c("site_id", "analyte")
  }

  # Filter out QC samples for summary
  field_samples <- data
  if ("lab_qc_type" %in% names(data)) {
    field_samples <- data %>%
      filter(is.na(lab_qc_type) | lab_qc_type == "sample")
  }

  if (nrow(field_samples) == 0) {
    # No field samples
    summary_stats <- data.frame(
      analyte = unique(data$analyte),
      n = 0,
      n_flagged = 0
    )
  } else {
    summary_stats <- field_samples %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        n = n(),
        n_flagged = sum(flags$flag[flags$analyte == first(analyte)] != "PASS" &
                          flags$flag[flags$analyte == first(analyte)] != "U"),
        n_detected = sum(detected == TRUE, na.rm = TRUE),
        n_non_detect = sum(detected == FALSE, na.rm = TRUE),
        detection_freq = n_detected / (n_detected + n_non_detect) * 100,
        mean_detected = mean(result[detected == TRUE], na.rm = TRUE),
        median_detected = median(result[detected == TRUE], na.rm = TRUE),
        min_detected = min(result[detected == TRUE], na.rm = TRUE),
        max_detected = max(result[detected == TRUE], na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(list(
    flags = flags,
    summary = summary_stats,
    has_sample_id = has_sample_id,
    has_detected = has_detected
  ))
}

# UTILITY FUNCTIONS

#' Generate QC Report
#'
#' @param qc_results List output from run_qc_checks()
#' @param output_file Path to save report (optional)
#' @examples
#' report <- generate_qc_report(qc)
#'
#' @export
generate_qc_report <- function(qc_results, output_file = NULL) {

  report <- list(
    metadata = qc_results$metadata,
    summary = list(
      total_records = qc_results$metadata$n_rows,
      flagged_records = qc_results$metadata$n_flagged,
      percent_flagged = round(qc_results$metadata$n_flagged /
                                qc_results$metadata$n_rows * 100, 2),
      pass = sum(qc_results$flags$flag == "PASS", na.rm = TRUE),
      suspect = sum(qc_results$flags$flag == "SUSPECT", na.rm = TRUE),
      reject = sum(qc_results$flags$flag == "REJECT", na.rm = TRUE)
    ),
    flag_reasons = qc_results$flags %>%
      filter(flag != "PASS") %>%
      count(flag, flag_reason) %>%
      arrange(desc(n)),
    data_summary = qc_results$summary
  )

  if (!is.null(output_file)) {
    saveRDS(report, output_file)
    message("QC report saved to ", output_file)
  }

  return(report)
}

#' Print QC Summary
#'
#' @param qc_results List output from run_qc_checks()
#' @examples
#' print_qc_summary(qc)
#'
#' @export
print_qc_summary <- function(qc_results) {

  cat("\n==================================================\n")
  cat("DATA QUALITY CHECK SUMMARY\n")
  cat("==================================================\n\n")

  cat("Data type:", qc_results$metadata$data_type, "\n")
  cat("Check date:", format(qc_results$metadata$check_date, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total records:", qc_results$metadata$n_rows, "\n\n")

  cat("QC Results:\n")
  cat("  PASS:", sum(qc_results$flags$flag == "PASS", na.rm = TRUE), "\n")
  cat("  SUSPECT:", sum(qc_results$flags$flag == "SUSPECT", na.rm = TRUE), "\n")
  cat("  REJECT:", sum(qc_results$flags$flag == "REJECT", na.rm = TRUE), "\n\n")

  if (sum(qc_results$flags$flag != "PASS", na.rm = TRUE) > 0) {
    cat("Most common flag reasons:\n")
    flag_summary <- qc_results$flags %>%
      filter(flag != "PASS") %>%
      count(flag_reason) %>%
      arrange(desc(n)) %>%
      head(5)

    for (i in 1:nrow(flag_summary)) {
      cat("  ", flag_summary$flag_reason[i], ":", flag_summary$n[i], "\n")
    }
  }

  cat("\n==================================================\n")
}

