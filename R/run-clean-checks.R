# Main dispatch function -------------------------------------------------------

#' @title Run basic data cleaning checks
#' @description Main dispatch function to run multiple cleaning operations on a dataset
#' currently including a function to check for negative values and percent ranges.
#' The functionality to add other checks exists by using a custom function.
#' @param data Data frame to clean
#' @param dataset_name Name of dataset (for logging and file naming)
#' @param diagnostics_dir Directory to save diagnostic CSV files (created if doesn't exist)
#' @param negative_cols Character vector of columns to check for negative values (optional)
#' @param percent_cols Character vector of columns to check for percent range (optional)
#' @param custom_checks List of custom cleaning functions (optional)
#' @param verbose Logical, print detailed progress messages (default: FALSE)
#' @return List with cleaned data, issue log, and all diagnostic details
#' @export
#'
#' @examples
#' # Basic usage
#' result <- run_clean_checks(
#'   data = my_data,
#'   dataset_name = "water_quality_2024",
#'   negative_cols = c("temperature", "do_mgl"),
#'   percent_cols = c("do_pct_sat", "turbidity_pct")
#' )
#'
#' # Access cleaned data
#' clean_data <- result$data
#'
#' # View issue log
#' print(result$issue_log)
#'
#' # Save issue log
#' write.csv(result$issue_log, "cleaning_log.csv", row.names = FALSE)

run_clean_checks <- function(data,
                             dataset_name,
                             diagnostics_dir = "diagnostics",
                             negative_cols = NULL,
                             percent_cols = NULL,
                             custom_checks = NULL,
                             verbose = FALSE) {

  # Start timer and logging
  start_time <- Sys.time()

  message("================================================================================")
  message("DATA CLEANING CHECKS")
  message("================================================================================")
  message("")
  message("Dataset: ", dataset_name)
  message("Start time: ", format(start_time, "%Y-%m-%d %H:%M:%S"))
  message("Rows: ", nrow(data))
  message("Columns: ", ncol(data))
  message("")

  # Create diagnostics directory if needed
  if (!dir.exists(diagnostics_dir)) {
    dir.create(diagnostics_dir, recursive = TRUE)
    message("Created diagnostics directory: ", diagnostics_dir)
  }

  # Initialize issue log
  issue_log <- tibble::tibble(
    issue = character(),
    rows_affected = integer(),
    prevalence = numeric(),
    action = character(),
    details_path = character()
  )

  # Set n_total for prevalence calculations (used by log_issue)
  n_total <- nrow(data)

  # Initialize details lists
  all_negative_details <- list()
  all_percent_details <- list()
  all_custom_details <- list()

  # Track cleaning results
  cleaning_log <- list()

  # ============================================================================
  # 1. NEGATIVE VALUE CHECKS
  # ============================================================================

  if (!is.null(negative_cols) && length(negative_cols) > 0) {
    message("--- Checking for negative values ---")
    log_info("Columns to check: ", paste(negative_cols, collapse = ", "), verbose = verbose)

    negative_start <- Sys.time()
    result <- clean_negative_values(
      data = data,
      cols = negative_cols,
      dataset_name = dataset_name,
      diagnostics_dir = diagnostics_dir,
      issue_log = issue_log,
      n_total = n_total,
      verbose = verbose
    )

    data <- result$raw
    issue_log <- result$issue_log
    all_negative_details <- result$negative_details

    n_issues <- sum(vapply(all_negative_details, nrow, integer(1)))
    message("  Found ", n_issues, " negative values across ",
            length(all_negative_details), " column(s)")
    message("  Time elapsed: ", format_elapsed(negative_start))
    message("")

    cleaning_log$negative <- list(
      n_columns_checked = length(negative_cols),
      n_columns_with_issues = length(all_negative_details),
      n_issues = n_issues
    )
  }

  # ============================================================================
  # 2. PERCENT RANGE CHECKS
  # ============================================================================

  if (!is.null(percent_cols) && length(percent_cols) > 0) {
    message("--- Checking percent ranges (0-100) ---")
    log_info("Columns to check: ", paste(percent_cols, collapse = ", "), verbose = verbose)

    percent_start <- Sys.time()
    result <- clean_percent_ranges(
      data = data,
      cols = percent_cols,
      dataset_name = dataset_name,
      diagnostics_dir = diagnostics_dir,
      issue_log = issue_log,
      n_total = n_total,
      verbose = verbose
    )

    data <- result$raw
    issue_log <- result$issue_log
    all_percent_details <- result$percent_details

    n_issues <- sum(vapply(all_percent_details, nrow, integer(1)))
    message("  Found ", n_issues, " out-of-range values across ",
            length(all_percent_details), " column(s)")
    message("  Time elapsed: ", format_elapsed(percent_start))
    message("")

    cleaning_log$percent <- list(
      n_columns_checked = length(percent_cols),
      n_columns_with_issues = length(all_percent_details),
      n_issues = n_issues
    )
  }

  # ============================================================================
  # 3. CUSTOM CHECKS
  # ============================================================================

  if (!is.null(custom_checks) && length(custom_checks) > 0) {
    message("--- Running custom checks ---")
    message("  Number of custom checks: ", length(custom_checks))

    for (i in seq_along(custom_checks)) {
      check_name <- names(custom_checks)[i]
      if (is.null(check_name) || check_name == "") {
        check_name <- paste0("custom_check_", i)
      }

      message("  Running: ", check_name)
      custom_start <- Sys.time()

      # Custom check should be a function that takes (data, dataset_name, diagnostics_dir, issue_log)
      # and returns list(raw = data, issue_log = issue_log, details = details)
      result <- custom_checks[[i]](
        data = data,
        dataset_name = dataset_name,
        diagnostics_dir = diagnostics_dir,
        issue_log = issue_log,
        n_total = n_total,
        verbose = verbose
      )

      data <- result$raw
      issue_log <- result$issue_log
      if (!is.null(result$details)) {
        all_custom_details[[check_name]] <- result$details
      }

      message("    Time elapsed: ", format_elapsed(custom_start))
    }
    message("")
  }

  # ============================================================================
  # EXPORT DIAGNOSTIC DETAILS
  # ============================================================================

  message("--- Exporting diagnostic details ---")

  n_files_exported <- 0

  # Export negative value details
  if (length(all_negative_details) > 0) {
    for (col in names(all_negative_details)) {
      filepath <- file.path(diagnostics_dir,
                            paste0(dataset_name, "_negative_", col, ".csv"))
      write.csv(all_negative_details[[col]], filepath, row.names = FALSE)
      log_info("  Exported: ", filepath, verbose = verbose)
      n_files_exported <- n_files_exported + 1
    }
  }

  # Export percent range details
  if (length(all_percent_details) > 0) {
    for (col in names(all_percent_details)) {
      filepath <- file.path(diagnostics_dir,
                            paste0(dataset_name, "_percent_out_of_range_", col, ".csv"))
      write.csv(all_percent_details[[col]], filepath, row.names = FALSE)
      log_info("  Exported: ", filepath, verbose = verbose)
      n_files_exported <- n_files_exported + 1
    }
  }

  # Export custom check details
  if (length(all_custom_details) > 0) {
    for (check_name in names(all_custom_details)) {
      filepath <- file.path(diagnostics_dir,
                            paste0(dataset_name, "_", check_name, ".csv"))
      write.csv(all_custom_details[[check_name]], filepath, row.names = FALSE)
      log_info("  Exported: ", filepath, verbose = verbose)
      n_files_exported <- n_files_exported + 1
    }
  }

  message("  Exported ", n_files_exported, " diagnostic file(s)")
  message("")

  # ============================================================================
  # EXPORT ISSUE LOG
  # ============================================================================

  issue_log_path <- file.path(diagnostics_dir, paste0(dataset_name, "_issue_log.csv"))
  write.csv(issue_log, issue_log_path, row.names = FALSE)
  message("--- Issue log exported ---")
  message("  File: ", issue_log_path)
  message("  Issues logged: ", nrow(issue_log))
  message("")

  # ============================================================================
  # SUMMARY
  # ============================================================================

  total_issues <- nrow(issue_log)
  total_rows_affected <- sum(issue_log$rows_affected)

  message("================================================================================")
  message("CLEANING SUMMARY")
  message("================================================================================")
  message("")
  message("Dataset: ", dataset_name)
  message("Original rows: ", n_total)
  message("Final rows: ", nrow(data))
  message("Rows removed: ", n_total - nrow(data))
  message("")
  message("Issues detected: ", total_issues)
  message("Rows affected: ", total_rows_affected, " (",
          round(total_rows_affected / n_total * 100, 2), "%)")
  message("")

  if (nrow(issue_log) > 0) {
    message("Top issues:")
    top_issues <- issue_log %>%
      dplyr::arrange(desc(rows_affected)) %>%
      dplyr::slice(1:min(5, nrow(issue_log)))

    for (i in 1:nrow(top_issues)) {
      message("  ", i, ". ", top_issues$issue[i], ": ",
              top_issues$rows_affected[i], " rows (",
              round(top_issues$prevalence[i] * 100, 2), "%)")
    }
    message("")
  }

  end_time <- Sys.time()
  message("End time: ", format(end_time, "%Y-%m-%d %H:%M:%S"))
  message("Total time elapsed: ", format_elapsed(start_time))
  message("")
  message("================================================================================")

  # Return results
  invisible(list(
    data = data,
    issue_log = issue_log,
    negative_details = all_negative_details,
    percent_details = all_percent_details,
    custom_details = all_custom_details,
    cleaning_log = cleaning_log,
    summary = list(
      dataset_name = dataset_name,
      original_rows = n_total,
      final_rows = nrow(data),
      rows_removed = n_total - nrow(data),
      n_issues = total_issues,
      rows_affected = total_rows_affected,
      start_time = start_time,
      end_time = end_time,
      elapsed_time = format_elapsed(start_time)
    )
  ))
}


# Helper functions -------------------------------------------------------------
# Print a message only when verbose is TRUE
log_info <- function(..., verbose = F) {
  if (isTRUE(verbose)) {
    message(...)
  }
}

# Print a label and a table only when verbose is TRUE
print_table <- function(label, x, verbose = F) {
  if (isTRUE(verbose)) {
    message(label)
    print(x)
  }
}

# Log an issue row with prevalence calculated from total dataframe rows (n_total) in this script
log_issue <- function(issue, rows_affected, action, n_total, details_path = NA_character_) {
  tibble::tibble(
    issue = issue,
    rows_affected = rows_affected,
    prevalence = if (n_total > 0) round(rows_affected / n_total, 4) else NA_real_,
    action = action,
    details_path = details_path
  )
}

#' Format elapsed time for logging
format_elapsed <- function(start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (elapsed < 60) {
    sprintf("%.1f seconds", elapsed)
  } else if (elapsed < 3600) {
    sprintf("%.1f minutes", elapsed / 60)
  } else {
    sprintf("%.1f hours", elapsed / 3600)
  }
}

#' @title Clean negative values
#' @description Clean negative numeric values, log issues, and capture diagnostics
#' @param data Data frame to clean
#' @param cols Character vector of column names to check for negative values
#' @param dataset_name Name of dataset (for diagnostic file naming)
#' @param diagnostics_dir Directory to save diagnostic CSV files
#' @param issue_log Existing issue log to append to
#' @param n_total This is passed from the dispatch function.
#' @param verbose Logical, print detailed progress messages (default: FALSE).
#' Passed from the dispatch function.
#' @return List with cleaned data, updated issue log, and negative value details
#' @export

# Clean negative numeric values, log issues, and capture diagnostics
# dataframe is expected to have an "id" column. if it does not exist an id
# will be created from the date and site_id, if exists; otherwise, the row number.
clean_negative_values <- function(data, cols, dataset_name, diagnostics_dir, issue_log, n_total, verbose = F) {
  negative_details <- list()

  # Check if id exists, if not create one
  has_id <- "id" %in% names(data)
  if (!has_id) {
    # Create ID from available columns
    if ("date" %in% names(data) && "site_id" %in% names(data)) {
      data$id <- paste0(data$site_id, "_", data$date)
    } else {
      data$id <- paste0("S", seq_len(nrow(data)))
    }
    message("Note: id column not found - generated IDs from available data")
  }

  for (col in cols) {
    if (!col %in% names(data)) {
      log_info(paste0("Warning: Column '", col, "' not found in data - skipping"), verbose = verbose)
      next
    }

    negative_rows <- which(!is.na(data[[col]]) & data[[col]] < 0)
    n_bad <- length(negative_rows)

    if (n_bad > 0) {
      negative_details[[col]] <- tibble::tibble(
        id = data$id[negative_rows],
        variable = col,
        value = data[[col]][negative_rows]
      )

    data[[col]] <- ifelse(data[[col]] < 0, NA, data[[col]])

      issue_log <- dplyr::bind_rows(
        issue_log,
        log_issue(
          paste0(col, "_negative"),
          n_bad,
          "Negative values set to NA",
          n_total = n_total,
          details_path = file.path(diagnostics_dir, paste0(dataset_name, "_negative_", col, ".csv"))
        )
      )
    }
  }

  if (isTRUE(verbose)) {
    if (length(negative_details) > 0) {
      negative_summary <- data.frame(
        variable = names(negative_details),
        rows = vapply(negative_details, nrow, integer(1)),
        row.names = NULL
      )
      print_table("Negative value counts by variable:", negative_summary)
    } else {
      log_info("Negative value counts by variable: 0", verbose = verbose)
    }
  }

  list(raw = data, issue_log = issue_log, negative_details = negative_details)
}

#' @title Clean percent values
#' @description Clean percent columns to the 0-100 range, log issues, and capture diagnostics
#' @param data Data frame to clean
#' @param cols Character vector of column names to check for percent range
#' @param dataset_name Name of dataset (for diagnostic file naming)
#' @param diagnostics_dir Directory to save diagnostic CSV files
#' @param issue_log Existing issue log to append to
#' @param n_total This is passed from the dispatch function.
#' @param verbose Logical, print detailed progress messages (default: FALSE).
#' Passed from the dispatch function.
#' @return List with cleaned data, updated issue log, and percent range details
#' @export

# Clean percent columns to the 0-100 range, log issues, and capture diagnostics
clean_percent_ranges <- function(data, cols, dataset_name, diagnostics_dir, issue_log, n_total, verbose = F) {
  percent_details <- list()

  # Check if id exists, if not create one
  has_id <- "id" %in% names(data)
  if (!has_id) {
    # Create ID from available columns
    if ("date" %in% names(data) && "site_id" %in% names(data)) {
      data$id <- paste0(data$site_id, "_", data$date)
    } else {
      data$id <- paste0("S", seq_len(nrow(data)))
    }
    message("Note: id column not found - generated IDs from available data")
  }

  for (col in cols) {
    if (!col %in% names(data)) {
      log_info(paste0("Warning: Column '", col, "' not found in data - skipping"), verbose = verbose)
      next
    }

    out_of_range <- which(!is.na(data[[col]]) & (data[[col]] < 0 | data[[col]] > 100))
    n_bad <- length(out_of_range)

    if (n_bad > 0) {
      percent_details[[col]] <- tibble::tibble(
        id = data$id[out_of_range],
        variable = col,
        value = data[[col]][out_of_range]
      )

    data[[col]] <- ifelse(!is.na(data[[col]]) & (data[[col]] < 0 | data[[col]] > 100), NA, data[[col]])

      issue_log <- dplyr::bind_rows(
        issue_log,
        log_issue(
          paste0(col, "_out_of_range"),
          n_bad,
          "Percent values outside 0-100 set to NA",
          n_total = n_total,
          details_path = file.path(diagnostics_dir, paste0(dataset_name, "_percent_out_of_range_", col, ".csv"))
        )
      )
    }
  }

  if (isTRUE(verbose)) {
    if (length(percent_details) > 0) {
      percent_summary <- data.frame(
        variable = names(percent_details),
        rows = vapply(percent_details, nrow, integer(1)),
        row.names = NULL
      )
      print_table("Percent out-of-range counts by variable:", percent_summary)
    } else {
      log_info("Percent out-of-range counts by variable: 0", verbose = verbose)
    }
  }

  list(raw = data, issue_log = issue_log, percent_details = percent_details)
}
