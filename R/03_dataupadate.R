#' Update hourly extracted data
#'
#' Cleans extracted hourly data, merges with existing historical parquet
#' records, removes duplicates, and returns a unified, validated dataset.
#' Enforces UTC timezone and preserves time-series continuity.
#'
#' @param hourly_table A `data.table` from the extraction stage. Must contain
#'   columns: `date_time` (POSIXct, UTC), `open`, `high`, `low`, `close`,
#'   `timeframe`, `ticker`, `asset_class`.
#' @param config Validated pipeline configuration list from `load_config()`.
#'   Must contain `transformation`, `paths$forexdataset$output_base`,
#'   `paths$frequency_folders`, `paths$subfolder`, and related keys.
#'
#' @return A `data.table` with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `timeframe`, `ticker`,
#'   `asset_class`. Deduplicated and merged with historical parquet data.
#'
#' @import data.table
#' @importFrom logger log_info log_warn
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' hourly_table <- extract_hourly_data(row, row$source_path, config)
#' updated_h1 <- update_h1_data(hourly_table, config)
#' }

# Update_Hourly --------------------------------------------------------

update_h1_data <- function(
  hourly_table,
  config
) {
  log_info(
    "Starting hourly transformation for {n} rows across {n_tickers} tickers",
    n = nrow(hourly_table),
    n_tickers = length(unique(hourly_table$ticker))
  )

  # ============================================================================
  # STEP 1: INPUT VALIDATION
  # ============================================================================
  # Check class FIRST before evaluating data frame properties like nrow()
  if (!is.data.table(hourly_table)) {
    stop(
      "hourly_table must be a data.table"
    )
  }

  if (nrow(hourly_table) == 0) {
    log_warn(
      "Input hourly_table is empty. Returning an empty data.table."
    )
    return(data.table())
  }

  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "timeframe",
    "ticker",
    "asset_class"
  )
  missing_cols <- setdiff(
    required_cols,
    names(hourly_table)
  )
  if (length(missing_cols) > 0) {
    stop(
      "hourly_table missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }

  if (
    is.null(config$transformation) ||
      is.null(
        config$paths$forexdataset$output_base
      )
  ) {
    stop(
      "config missing required sections: 'transformation' or
      'paths$forexdataset$output_base'"
    )
  }

  log_info(
    paste0(
      "Input validation passed for hourly_table with ",
      nrow(hourly_table),
      " rows and config check successful"
    )
  )

  # ============================================================================
  # STEP 2: GLOBAL CLEAN (PRE-CONVERSION)
  # ============================================================================
  # Remove duplicates by extraction key (keep first occurrence)
  log_info(paste0(
    "Starting deduplication for hourly_table with ",
    nrow(hourly_table),
    " rows before deduplication"
  ))

  hourly_table <- unique(
    hourly_table,
    by = c("ticker", "date_time")
  )

  log_info(
    "Hourly global clean: {n} rows after deduplication",
    n = nrow(hourly_table)
  )

  # ============================================================================
  # STEP 3: PRELOAD ALL EXISTING DATA (vectorized I/O where possible)
  # ============================================================================
  # Identify unique processing groups
  groups <- setorder(
    unique(hourly_table[, .(
      asset_class,
      ticker,
      timeframe
    )]),
    asset_class
  )

  # --------------------------------------------------------------------------
  # BUILD EXISTING DATA PATH (CONFIG-DRIVEN)
  # --------------------------------------------------------------------------
  # Path components with config validation
  freq_folder <- config$paths$frequency_folders$h1 %||%
    "h1"

  # Build paths & load existing data in ONE batch
  existing_paths <- groups[,
    file_path := build_data_path(config, unique(timeframe), asset_class, ticker)
  ]

  # ------------------------------------------------------------------------
  # LOAD EXISTING DATA (IF FILE EXISTS - VECTTORISED CHECK)
  # ------------------------------------------------------------------------
  log_info(
    "Preloading existing data for {n} tickers with pip_val mapping applied",
    n = nrow(groups)
  )

  existing_data_list <- existing_paths[,
    {
      # ---- Read the file (with error handling) ----
      dt <- tryCatch(
        {
          # CHECK IF FILE EXISTS ───────────────────────────
          if (!file.exists(file_path)) {
            log_warn(
              "File '{file}' does NOT exist. Skipping.",
              file = basename(file_path)
            )
            NULL
          } else {
            # READ THE FILE (only if it exists) ───────────────
            dt <- data.table::as.data.table(
              arrow::read_parquet(file_path)
            )

            dt
          }
        },
        error = function(e) {
          log_warn(
            "FAILED to read '{file}': {err}",
            file = basename(file_path),
            err = e$message
          )
          stop(sprintf(
            "could not use existing file '%s': %s",
            basename(file_path),
            e$message
          ))
        }
      )

      # If file didn't exist, return an empty structure
      if (is.null(dt)) {
        dt <- data.table(
          date_time = as.POSIXct(character()),
          open = numeric(),
          high = numeric(),
          low = numeric(),
          close = numeric(),
          timeframe = character(),
          ticker = character(),
          asset_class = character()
        )
      }

      # 1. STRICT CHECK: Core price & time columns must exist
      core_cols <- c("date_time", "open", "high", "low", "close")
      missing_core <- setdiff(core_cols, names(dt))
      if (length(missing_core) > 0) {
        cols_str <- if (length(missing_core) > 1) {
          paste(missing_core, collapse = ", ")
        } else {
          missing_core
        }
        stop(sprintf(
          "could not use existing file '%s' - missing columns: %s",
          basename(file_path),
          cols_str
        ))
      }

      # 2. STRICT DATE_TIME VALIDATION & CONVERSION
      if (any(is.na(dt$date_time))) {
        stop(sprintf(
          "could not use existing file '%s' - date_time contains NA values",
          basename(file_path)
        ))
      }

      if (inherits(dt$date_time, "POSIXct")) {
        # Already correct
      } else if (inherits(dt$date_time, "Date")) {
        dt[, date_time := as.POSIXct(date_time, tz = "UTC")]
      } else if (is.character(dt$date_time) || is.factor(dt$date_time)) {
        parsed_dates <- tryCatch(
          as.POSIXct(as.character(dt$date_time), tz = "UTC"),
          error = function(e) NULL
        )
        if (is.null(parsed_dates) || any(is.na(parsed_dates))) {
          stop(sprintf(
            "could not use existing file '%s' - date_time is not a date-time",
            basename(file_path)
          ))
        }
        dt[, date_time := parsed_dates]
      } else {
        stop(sprintf(
          "could not use existing file '%s' - date_time is not a date-time",
          basename(file_path)
        ))
      }

      # 3. LENIENT CHECK: Auto-fill missing metadata columns safely
      if (!"timeframe" %in% names(dt)) {
        dt[, timeframe := freq_folder]
      }
      if (!"ticker" %in% names(dt)) {
        dt[, ticker := .BY$ticker]
      }
      if (!"asset_class" %in% names(dt)) {
        dt[, asset_class := .BY$asset_class]
      }

      # Return as a list-column named "data"
      .(data = list(dt))
    },
    by = .(asset_class, ticker)
  ] # <<< GROUP BY HERE

  # COMBINE ALL FILES INTO ONE data.table
  existing_all <- rbindlist(
    existing_data_list$data,
    fill = TRUE
  )

  log_info(
    "Completed loading existing data: {n} total rows across {n_tickers} tickers",
    n = nrow(existing_all),
    n_tickers = length(unique(existing_all$ticker))
  )

  # ------------------------------------------------------------------------
  # ENSURE EXISTING DATA HAS ALL REQUIRED COLS
  # ------------------------------------------------------------------------
  existing_all <- existing_all[, ..required_cols]

  data.table::setkeyv(
    existing_all,
    c(
      "asset_class",
      "ticker",
      "date_time"
    )
  )
  data.table::setkeyv(
    hourly_table,
    c(
      "asset_class",
      "ticker",
      "date_time"
    )
  )

  # =========================================================================
  # STEP 4: GLOBAL ANTI-JOIN: Flag NEW records in ONE step
  # =========================================================================
  log_info(
    "Performing global anti-join to identify new records across {n} tickers",
    n = nrow(groups)
  )

  # Extract ONLY new records (still grouped implicitly)
  new_records <- hourly_table[
    !existing_all,
    on = .(
      asset_class,
      ticker,
      date_time
    )
  ]
  log_info(
    "Found {n_new} new records | Tickers: {tickers}",
    n_new = nrow(new_records),
    tickers = paste(
      unique(new_records$ticker),
      collapse = ", "
    )
  )

  if (nrow(new_records) == 0) {
    log_info(
      "No new records found across {n} tickers",
      n = nrow(groups)
    )
    return(existing_all) # or return existing_all (if non-empty)
  }

  # ============================================================================
  # STEP 5: MERGE AND FINAL CLEAN (SINGLE COMPREHENSIVE CLEAN)
  # ============================================================================
  log_info(
    paste0(
      "Merging new records with existing data and performing final ",
      "deduplication"
    )
  )

  # Ensure new_records & existing_all have the same columns order
  existing_all <- data.table::setcolorder(
    existing_all,
    required_cols
  )

  new_records <- data.table::setcolorder(
    new_records,
    required_cols
  )

  # Merge new_records to existing_all
  full_data <- rbindlist(
    list(existing_all, new_records),
    use.names = TRUE
  )[!(format(date_time, "%H:%M:%S") == "00:00:00")]

  # Use unique.data.table with 'by' argument for explicit key-based
  # deduplication
  full_data <- unique(
    full_data,
    by = c("ticker", "date_time")
  )

  setorderv(
    full_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  # Force UTC timezone attribute preservation
  data.table::setattr(full_data$date_time, "tzone", "UTC")

  return(full_data)
}


#' Update daily extracted data
#'
#' Cleans extracted daily data, merges with existing historical parquet
#' records, removes duplicates, and returns a unified, validated dataset.
#' Handles both local and web data formats and enforces UTC timezone.
#'
#' @param daily_table A `data.table` from the extraction stage with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`, `open_interest`,
#'   `timeframe`, `ticker`, `asset_class`.
#' @param config A validated configuration list from `load_config()`.
#'   Must contain `paths$forexdataset$output_base`, `paths$frequency_folders`,
#'   `paths$subfolder`, and related keys.
#'
#' @return A `data.table` with columns:
#'   `date_time` (POSIXct, UTC), `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`. Deduplicated and
#'   merged with historical parquet data.
#'
#' @import data.table
#' @importFrom logger log_info log_warn
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' daily_table <- extract_daily_data(row, row$source_path, config)
#' updated_daily <- update_daily_data(daily_table, config)
#' }

# Update_Daily ---------------------------------------------------------

update_daily_data <- function(
  daily_table,
  config
) {
  log_info(
    "Starting daily transformation for {n} rows across {n_tickers} tickers",
    n = nrow(daily_table),
    n_tickers = length(unique(daily_table$ticker))
  )

  # ============================================================================
  # STEP 0: INPUT VALIDATION
  # ============================================================================
  # Check class FIRST before evaluating data frame properties like nrow()
  if (!is.data.table(daily_table)) {
    stop(
      "daily_table must be a data.table"
    )
  }

  if (nrow(daily_table) == 0) {
    log_warn(
      "Input daily_table is empty. Returning an empty data.table."
    )
    return(data.table())
  }

  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "open_interest",
    "timeframe",
    "ticker",
    "asset_class"
  )
  missing_cols <- setdiff(
    required_cols,
    names(daily_table)
  )
  if (length(missing_cols) > 0) {
    stop(
      "daily_table missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }

  if (
    is.null(
      config$paths$forexdataset$output_base
    )
  ) {
    stop(
      "config missing required sections: 'paths$forexdataset$output_base'"
    )
  }

  # ============================================================================
  # STEP 1: GLOBAL CLEAN (PRE-CONVERSION)
  # ============================================================================
  # Remove duplicates by extraction key (keep first occurrence)
  # daily_table <- daily_table[!duplicated(daily_table)]
  log_info(paste0(
    "Starting deduplication for daily_table with ",
    nrow(daily_table),
    " rows before deduplication"
  ))

  daily_table <- unique(
    daily_table,
    by = c("ticker", "date_time")
  )

  log_info(
    "Daily global clean: {n} rows after deduplication",
    n = nrow(daily_table)
  )

  # ============================================================================
  # STEP 2: PRELOAD ALL EXISTING DATA (vectorized I/O where possible)
  # ============================================================================
  # Identify unique processing groups
  groups <- setorder(
    unique(daily_table[, .(
      asset_class,
      ticker,
      timeframe
    )]),
    asset_class
  )

  # --------------------------------------------------------------------------
  # BUILD EXISTING DATA PATH (CONFIG-DRIVEN)
  # --------------------------------------------------------------------------
  # Path components with config validation
  freq_folder <- config$paths$frequency_folders$daily %||%
    "daily"

  # Build paths & load existing data in ONE batch
  existing_paths <- groups[,
    file_path := build_data_path(config, unique(timeframe), asset_class, ticker)
  ]
  # ------------------------------------------------------------------------
  # LOAD EXISTING DATA (IF FILE EXISTS - VECTTORISED CHECK)
  # ------------------------------------------------------------------------
  log_info(
    "Preloading existing data for {n} tickers based on config-driven paths",
    n = nrow(groups)
  )

  existing_data_list <- existing_paths[,
    {
      # ---- Read the file (with error handling) ----
      dt <- tryCatch(
        {
          # CHECK IF FILE EXISTS ───────────────────────────
          if (!file.exists(file_path)) {
            log_warn(
              "File '{file}' does NOT exist. Skipping.",
              file = basename(file_path)
            )
            NULL
          } else {
            # READ THE FILE (only if it exists) ───────────────
            dt <- data.table::as.data.table(
              arrow::read_parquet(file_path)
            )

            dt
          }
        },
        error = function(e) {
          log_warn(
            "FAILED to read '{file}': {err}",
            file = basename(file_path),
            err = e$message
          )
          stop(sprintf(
            "could not use existing file '%s': %s",
            basename(file_path),
            e$message
          ))
        }
      )

      # If file didn't exist, return an empty structure
      if (is.null(dt)) {
        dt <- data.table(
          date_time = as.POSIXct(character()),
          open = numeric(),
          high = numeric(),
          low = numeric(),
          close = numeric(),
          volume = as.integer(numeric()),
          open_interest = as.integer(numeric()),
          timeframe = character(),
          ticker = character(),
          asset_class = character()
        )
      }

      # 1. STRICT CHECK: Core price & time columns must exist
      core_cols <- c(
        "date_time",
        "open",
        "high",
        "low",
        "close",
        "volume",
        "open_interest"
      )
      missing_core <- setdiff(core_cols, names(dt))
      if (length(missing_core) > 0) {
        cols_str <- if (length(missing_core) > 1) {
          paste0(missing_core, collapse = ", ")
        } else {
          missing_core
        }
        stop(sprintf(
          "could not use existing file '%s' - missing columns: %s",
          basename(file_path),
          cols_str
        ))
      }

      # 2. STRICT DATE_TIME VALIDATION & CONVERSION
      if (any(is.na(dt$date_time))) {
        stop(sprintf(
          "could not use existing file '%s' - date_time contains NA values",
          basename(file_path)
        ))
      }

      if (inherits(dt$date_time, "POSIXct")) {
        # Already correct
      } else if (inherits(dt$date_time, "Date")) {
        dt[, date_time := as.POSIXct(date_time, tz = "UTC")]
      } else if (is.character(dt$date_time) || is.factor(dt$date_time)) {
        parsed_dates <- tryCatch(
          as.POSIXct(as.character(dt$date_time), tz = "UTC"),
          error = function(e) NULL
        )
        if (is.null(parsed_dates) || any(is.na(parsed_dates))) {
          stop(sprintf(
            "could not use existing file '%s' - date_time is not a date-time",
            basename(file_path)
          ))
        }
        dt[, date_time := parsed_dates]
      } else {
        stop(sprintf(
          "could not use existing file '%s' - date_time is not a date-time",
          basename(file_path)
        ))
      }

      # 3. LENIENT CHECK: Auto-fill missing metadata columns safely
      if (!"timeframe" %in% names(dt)) {
        dt[, timeframe := freq_folder]
      }
      if (!"ticker" %in% names(dt)) {
        dt[, ticker := .BY$ticker]
      }
      if (!"asset_class" %in% names(dt)) {
        dt[, asset_class := .BY$asset_class]
      }

      # Return as a list-column named "data"
      .(data = list(dt))
    },
    by = .(asset_class, ticker)
  ] # <<< GROUP BY HERE

  # COMBINE ALL FILES INTO ONE data.table
  existing_all <- rbindlist(
    existing_data_list$data,
    fill = TRUE
  )

  log_info(
    "Completed loading existing data: {n} total rows across {n_tickers} tickers",
    n = nrow(existing_all),
    n_tickers = length(unique(existing_all$ticker))
  )

  # ----------------------------------------------------------------------------
  # ENSURE EXISTING DATA HAS ALL REQUIRED COLS
  # ----------------------------------------------------------------------------
  existing_all <- existing_all[, ..required_cols]

  data.table::setkeyv(
    existing_all,
    c(
      "asset_class",
      "ticker",
      "date_time"
    )
  )
  data.table::setkeyv(
    daily_table,
    c(
      "asset_class",
      "ticker",
      "date_time"
    )
  )

  # =========================================================================
  # STEP 3: GLOBAL ANTI-JOIN: Flag NEW records in ONE step
  # =========================================================================
  log_info(
    "Performing global anti-join to identify new records across {n} tickers",
    n = nrow(groups)
  )

  # Extract ONLY new recds (still grouped implicitly)
  new_records <- daily_table[
    !existing_all,
    on = .(
      asset_class,
      ticker,
      date_time
    )
  ]
  log_info(
    "Found {n_new} new records | Tickers: {tickers}",
    n_new = nrow(new_records),
    tickers = paste(
      unique(new_records$ticker),
      collapse = ", "
    )
  )

  if (nrow(new_records) == 0) {
    log_info(
      "No new records found across {n} tickers",
      n = nrow(groups)
    )
    return(existing_all) # or return existing_all (if non-empty)
  }

  # ============================================================================
  # STEP 4: MERGE AND FINAL CLEAN (SINGLE COMPREHENSIVE CLEAN)
  # ============================================================================
  log_info(
    paste0(
      "Merging new records with existing data and performing final ",
      "deduplication"
    )
  )

  # Ensure that existing_all & new_records have the same columns order
  existing_all <- data.table::setcolorder(
    existing_all,
    required_cols
  )

  new_records <- data.table::setcolorder(
    new_records,
    required_cols
  )

  # Merging new_records to existing_all
  full_data <- rbindlist(
    list(existing_all, new_records),
    use.names = TRUE
  )

  # Use unique.data.table with 'by' argument for explicit key-based
  # deduplication
  full_data <- unique(
    full_data,
    by = c("ticker", "date_time")
  )

  setorderv(
    full_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  # Force UTC timezone attribute preservation
  data.table::setattr(full_data$date_time, "tzone", "UTC")

  return(full_data)
}


#' Aggregate hourly data to 4-hour OHLC
#'
#' Aggregates hourly data into 4-hour candles. Preserves ticker and asset
#' class grouping and aligns candle timestamps to 4-hour boundaries.
#'
#' @param hourly_data A `data.table` with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `timeframe`, `ticker`,
#'   `asset_class`.
#' @param config A validated configuration list with
#'   `transformation$aggregation$h1_to_h4` and
#'   `paths$frequency_folders$h4`.
#'
#' @return A `data.table` with 4-hour candles and columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `timeframe`, `ticker`,
#'   `asset_class`.
#'
#' @import data.table
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom lubridate ceiling_date minutes
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' h4_data <- aggregate_hourly_to_4hour(updated_h1, config)
#' }

# Aggregate_to_4Hour ------------------------------------------------------

aggregate_hourly_to_4hour <- function(
  hourly_data,
  config
) {
  # ----------------------------------------------------------------------
  # STEP 0: INPUT VALIDATION & LOGGING
  # ----------------------------------------------------------------------
  log_info(
    "Function entry - starting 4-hour summarization"
  )

  if (!inherits(hourly_data, "data.table")) {
    msg <- sprintf(
      "Invalid input type: %s. Expected data.table",
      class(hourly_data)[1]
    )
    log_error(msg)
    stop(
      "aggregate_hourly_to_4hour: hourly_data must be a data.table"
    )
  }

  if (is.null(config) || !is.list(config)) {
    log_error(
      "Config is NULL or not a list"
    )
    stop(
      "aggregate_hourly_to_4hour: config must be a non-null list"
    )
  }

  # Validate required config sections
  if (is.null(config$transformation)) {
    log_error(
      "Config missing required sections: 'transformation'"
    )
    stop(
      "config missing required sections: 'transformation'"
    )
  }

  # Validate required data columns
  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "timeframe",
    "ticker",
    "asset_class"
  )
  missing_cols <- setdiff(
    required_cols,
    names(hourly_data)
  )
  if (length(missing_cols) > 0) {
    msg <- sprintf(
      "Missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
    log_error(msg)
    stop(sprintf(
      "aggregate_hourly_to_4hour: hourly_data missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    ))
  }

  # Validate date column type
  if (
    !inherits(
      hourly_data$date_time[1],
      "POSIXct"
    )
  ) {
    log_error(
      "Date column must be POSIXct (hourly timestamps)"
    )
    stop(
      "aggregate_hourly_to_4hour: date column must be POSIXct
      (hourly timestamps)"
    )
  }

  # Check for empty data
  if (nrow(hourly_data) == 0) {
    log_warn(
      "Input data is empty - returning empty data.table"
    )
    return(data.table())
  }

  # Log input statistics
  log_info(sprintf(
    "Processing %d rows for %d unique tickers",
    nrow(hourly_data),
    uniqueN(hourly_data$ticker)
  ))

  # ----------------------------------------------------------------------
  # STEP 1: DATA QUALITY CHECKS
  # ----------------------------------------------------------------------
  log_debug(
    "Performing data quality checks"
  )

  # Check for NA values in critical columns
  critical_cols <- c(
    "open",
    "high",
    "low",
    "close"
  )
  na_counts <- sapply(
    critical_cols,
    function(col) {
      sum(is.na(hourly_data[[col]]))
    }
  )

  if (any(na_counts > 0)) {
    # 2. Find which tickers actually have NAs
    na_tickers <- unique(
      hourly_data$ticker[rowSums(is.na(hourly_data[, ..critical_cols])) > 0]
    )

    # Build a readable summary of column counts and affected tickers
    col_summary <- paste(
      names(na_counts)[na_counts > 0],
      na_counts[na_counts > 0],
      sep = "=",
      collapse = ", "
    )

    ticker_summary <- paste(na_tickers, collapse = ", ")

    log_error(sprintf(
      "Found NAs in critical columns: %s. Affected tickers: %s",
      col_summary,
      ticker_summary
    ))
    stop(sprintf(
      paste0(
        "aggregate_hourly_to_4hour: Input data contains NAs in: %s. ",
        "Problematic ticker(s): %s"
      ),
      paste(names(na_counts)[na_counts > 0], collapse = ", "),
      ticker_summary
    ))
  }

  # Check for negative prices
  negative_prices <- sapply(
    critical_cols,
    function(col) {
      sum(
        hourly_data[[col]] < 0,
        na.rm = TRUE
      )
    }
  )
  if (any(negative_prices > 0)) {
    neg_summary <- paste(
      names(negative_prices)[
        negative_prices > 0
      ],
      negative_prices[
        negative_prices > 0
      ],
      sep = "=",
      collapse = ", "
    )
    log_warn(sprintf(
      "Found negative prices: %s",
      neg_summary
    ))
  }

  # ----------------------------------------------------------------------
  # STEP 2: AGGREGATE TO 4-HOUR OHLC (with tryCatch)
  # ----------------------------------------------------------------------
  log_info(
    "Aggregating hourly data to 4-hour OHLC"
  )

  h4 <- config$transformation$aggregation$h1_to_h4

  freq_folder <- config$paths$frequency_folders$h4

  four_hour_data <- tryCatch(
    {
      hourly_data[,
        .(
          # Standard OHLC aggregation
          open = first(open),
          high = max(high),
          low = min(low),
          close = last(close)
        ),
        by = .(
          ticker,
          asset_class,
          date_time = fifelse(
            format(
              ceiling_date(date_time, unit = h4),
              "%H:%M:%S"
            ) ==
              "00:00:00",
            ceiling_date(date_time, unit = h4) - minutes(1),
            ceiling_date(date_time, unit = h4)
          )
        )
      ][, timeframe := freq_folder]
    },
    error = function(e) {
      log_error(sprintf(
        "Aggregation failed: %s",
        e$message
      ))
      stop(sprintf(
        "aggregate_hourly_to_4hour: Data aggregation failed - %s",
        e$message
      ))
    },
    warning = function(w) {
      log_warn(sprintf(
        "Warning during aggregation: %s",
        w$message
      ))
      NULL
    }
  )

  if (is.null(four_hour_data)) {
    log_error(
      "Aggregation returned NULL - check warnings above"
    )
    stop(
      "aggregate_hourly_to_4hour: Aggregation failed with warnings"
    )
  }

  log_info(sprintf(
    "Successfully aggregated to %d 4-hour candles",
    nrow(four_hour_data)
  ))

  # ----------------------------------------------------------------------
  # STEP 3: FINAL CLEANUP & VALIDATION
  # ----------------------------------------------------------------------
  log_info(
    "Performing final cleanup and validation"
  )

  # More memory-efficient:
  data.table::setcolorder(
    four_hour_data,
    required_cols
  )

  # Final validation
  final_row_count <- nrow(
    four_hour_data
  )
  final_ticker_count <- data.table::uniqueN(
    four_hour_data$ticker
  )

  log_info(sprintf(
    "Function complete - Output: %d rows, %d tickers, date range: %s to %s",
    final_row_count,
    final_ticker_count,
    format(
      min(four_hour_data$date_time),
      "%Y-%m-%d %H:%M:%S"
    ),
    format(
      max(four_hour_data$date_time),
      "%Y-%m-%d %H:%M:%S"
    )
  ))

  setorderv(
    four_hour_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  return(four_hour_data[])
}


#' Aggregate daily data to weekly OHLC
#'
#' Aggregates daily data into weekly candles. Preserves ticker and asset
#' class grouping and aligns week start to Monday.
#'
#' @param daily_data A `data.table` with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#' @param config A validated configuration list with
#'   `transformation$aggregation$daily_to_weekly` and
#'   `paths$frequency_folders$weekly`.
#'
#' @return A `data.table` with weekly candles and columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#'
#' @import data.table
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom lubridate floor_date
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' weekly_data <- aggregate_daily_to_weekly(updated_daily, config)
#' }

# Aggregate_to_Weekly -----------------------------------------------------

aggregate_daily_to_weekly <- function(
  daily_data,
  config
) {
  # ----------------------------------------------------------------------
  # STEP 0: INPUT VALIDATION & LOGGING
  # ----------------------------------------------------------------------
  log_info(
    "Function entry - starting weekly summarisation"
  )

  if (!inherits(daily_data, "data.table")) {
    msg <- sprintf(
      "Invalid input type: %s. Expected data.table",
      class(daily_data)[1]
    )
    log_error(msg)
    stop(
      "aggregate_daily_to_weekly: hourly_data must be a data.table"
    )
  }

  if (is.null(config) || !is.list(config)) {
    log_error(
      "Config is NULL or not a list"
    )
    stop(
      "aggregate_daily_to_weekly: config must be a non-null list"
    )
  }

  if (is.null(config$transformation)) {
    log_error(
      "Config missing required sections: 'transformation'"
    )
    stop(
      "config missing required sections: 'transformation'"
    )
  }

  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "open_interest",
    "timeframe",
    "ticker",
    "asset_class"
  )

  missing_cols <- setdiff(
    required_cols,
    names(daily_data)
  )
  if (length(missing_cols) > 0) {
    msg <- sprintf(
      "Missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
    log_error(msg)
    stop(sprintf(
      "aggregate_daily_to_weekly: daily_data missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    ))
  }

  if (nrow(daily_data) == 0) {
    log_warn(
      "Input data is empty - returning empty data.table"
    )
    return(data.table())
  }

  log_info(sprintf(
    "Processing %d rows for %d unique tickers",
    nrow(daily_data),
    uniqueN(daily_data$ticker)
  ))

  # ----------------------------------------------------------------------
  # STEP 1: DATA QUALITY CHECKS
  # ----------------------------------------------------------------------
  log_debug(
    "Performing data quality checks"
  )

  critical_cols <- c(
    "open",
    "high",
    "low",
    "close"
  )
  na_counts <- vapply(
    critical_cols,
    function(col) {
      sum(is.na(daily_data[[col]]))
    },
    integer(1)
  )
  if (any(na_counts > 0)) {
    na_summary <- paste(
      names(na_counts)[na_counts > 0],
      na_counts[na_counts > 0],
      sep = "=",
      collapse = ", "
    )
    log_error(sprintf(
      "Found NAs in critical columns: %s",
      na_summary
    ))
    stop(sprintf(
      "aggregate_daily_to_weekly: Input data contains NAs in: %s",
      paste(
        names(na_counts)[na_counts > 0],
        collapse = ", "
      )
    ))
  }

  neg_prices <- vapply(
    critical_cols,
    function(col) {
      sum(
        daily_data[[col]] < 0,
        na.rm = TRUE
      )
    },
    integer(1)
  )
  if (any(neg_prices > 0)) {
    neg_summary <- paste(
      names(neg_prices)[neg_prices > 0],
      neg_prices[neg_prices > 0],
      sep = "=",
      collapse = ", "
    )
    log_warn(sprintf(
      "Found negative prices: %s",
      neg_summary
    ))
  }

  # ----------------------------------------------------------------------
  # STEP 2: AGGREGATE TO WEEKLY OHLC (with tryCatch)
  # ----------------------------------------------------------------------
  log_info(
    "Aggregating daily data to weekly OHLC"
  )

  wk <- config$transformation$aggregation$daily_to_weekly
  freq_folder <- config$paths$frequency_folders$weekly

  weekly_data <- tryCatch(
    {
      daily_data[,
        .(
          open = first(open),
          high = max(high),
          low = min(low),
          close = last(close),
          volume = sum(volume),
          open_interest = last(
            open_interest
          )
        ),
        by = .(
          ticker,
          asset_class,
          date_time = floor_date(
            date_time,
            unit = wk,
            week_start = 1
          )
        )
      ][, timeframe := freq_folder]
    },
    error = function(e) {
      log_error(sprintf(
        "Aggregation failed: %s",
        e$message
      ))
      stop(sprintf(
        "aggregate_daily_to_weekly: Data aggregation failed - %s",
        e$message
      ))
    },
    warning = function(w) {
      log_warn(sprintf(
        "Warning during aggregation: %s",
        w$message
      ))
      NULL
    }
  )

  if (is.null(weekly_data)) {
    log_error(
      "Aggregation returned NULL - check warnings above"
    )
    stop(
      "aggregate_daily_to_weekly: Aggregation failed with warnings"
    )
  }

  log_info(sprintf(
    "Successfully aggregated to %d weekly candles",
    nrow(weekly_data)
  ))

  # ----------------------------------------------------------------------
  # STEP 3: FINAL CLEANUP & VALIDATION
  # ----------------------------------------------------------------------
  log_info(
    "Performing final cleanup and validation"
  )

  data.table::setcolorder(
    weekly_data,
    required_cols
  )

  final_row_count <- nrow(weekly_data)
  final_ticker_count <- data.table::uniqueN(
    weekly_data$ticker
  )

  log_info(sprintf(
    "Function complete - Output: %d rows, %d tickers, date range: %s to %s",
    final_row_count,
    final_ticker_count,
    format(
      min(weekly_data$date_time),
      "%Y-%m-%d"
    ),
    format(
      max(weekly_data$date_time),
      "%Y-%m-%d"
    )
  ))

  setorderv(
    weekly_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  return(weekly_data[])
}

#' Aggregate daily data to monthly OHLC
#'
#' Aggregates daily data into monthly candles. Preserves ticker and asset
#' class grouping and adjusts month-start dates that fall on weekends.
#'
#' @param daily_data A `data.table` with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#' @param config A validated configuration list with
#'   `transformation$aggregation$daily_to_monthly` and
#'   `paths$frequency_folders$monthly`.
#'
#' @return A `data.table` with monthly candles and columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#'
#' @import data.table
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom lubridate floor_date days
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' monthly_data <- aggregate_daily_to_monthly(updated_daily, config)
#' }

# Aggregate_to_Monthly ----------------------------------------------------

aggregate_daily_to_monthly <- function(
  daily_data,
  config
) {
  # ----------------------------------------------------------------------
  # STEP 0: INPUT VALIDATION & LOGGING
  # ----------------------------------------------------------------------
  log_info(
    "Function entry - starting monthly summarisation"
  )

  if (!inherits(daily_data, "data.table")) {
    msg <- sprintf(
      "Invalid input type: %s. Expected data.table",
      class(daily_data)[1]
    )
    log_error(msg)
    stop(
      "aggregate_daily_to_monthly: hourly_data must be a data.table"
    )
  }

  if (is.null(config) || !is.list(config)) {
    log_error(
      "Config is NULL or not a list"
    )
    stop(
      "aggregate_daily_to_monthly: config must be a non-null list"
    )
  }

  if (is.null(config$transformation)) {
    log_error(
      "Config missing required sections: 'transformation'"
    )
    stop(
      "config missing required sections: 'transformation'"
    )
  }

  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "open_interest",
    "timeframe",
    "ticker",
    "asset_class"
  )

  missing_cols <- setdiff(
    required_cols,
    names(daily_data)
  )
  if (length(missing_cols) > 0) {
    msg <- sprintf(
      "Missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
    log_error(msg)
    stop(sprintf(
      "aggregate_daily_to_monthly: daily_data missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    ))
  }

  if (nrow(daily_data) == 0) {
    log_warn(
      "Input data is empty - returning empty data.table"
    )
    return(data.table())
  }

  log_info(sprintf(
    "Processing %d rows for %d unique tickers",
    nrow(daily_data),
    uniqueN(daily_data$ticker)
  ))

  # ----------------------------------------------------------------------
  # STEP 1: DATA QUALITY CHECKS
  # ----------------------------------------------------------------------
  log_debug(
    "Performing data quality checks"
  )

  critical_cols <- c(
    "open",
    "high",
    "low",
    "close"
  )
  na_counts <- vapply(
    critical_cols,
    function(col) {
      sum(is.na(daily_data[[col]]))
    },
    integer(1)
  )
  if (any(na_counts > 0)) {
    na_summary <- paste(
      names(na_counts)[na_counts > 0],
      na_counts[na_counts > 0],
      sep = "=",
      collapse = ", "
    )
    log_error(sprintf(
      "Found NAs in critical columns: %s",
      na_summary
    ))
    stop(sprintf(
      "aggregate_daily_to_monthly: Input data contains NAs in: %s",
      paste(
        names(na_counts)[na_counts > 0],
        collapse = ", "
      )
    ))
  }

  neg_prices <- vapply(
    critical_cols,
    function(col) {
      sum(
        daily_data[[col]] < 0,
        na.rm = TRUE
      )
    },
    integer(1)
  )
  if (any(neg_prices > 0)) {
    neg_summary <- paste(
      names(neg_prices)[neg_prices > 0],
      neg_prices[neg_prices > 0],
      sep = "=",
      collapse = ", "
    )
    log_warn(sprintf(
      "Found negative prices: %s",
      neg_summary
    ))
  }

  # ----------------------------------------------------------------------
  # STEP 2: AGGREGATE TO MONTHLY OHLC (with tryCatch)
  # ----------------------------------------------------------------------
  log_info(
    "Aggregating daily data to monthly OHLC"
  )

  mnt <- config$transformation$aggregation$daily_to_monthly
  freq_folder <- config$paths$frequency_folders$monthly

  monthly_data <- tryCatch(
    {
      daily_data[,
        .(
          open = first(open),
          high = max(high),
          low = min(low),
          close = last(close),
          volume = sum(volume),
          open_interest = last(open_interest)
        ),
        by = .(
          ticker,
          asset_class,
          date_time = fcase(
            lubridate::wday(floor_date(date_time, unit = mnt)) == 1,
            floor_date(date_time, unit = mnt) + days(1),
            lubridate::wday(floor_date(date_time, unit = mnt)) == 7,
            floor_date(date_time, unit = mnt) + days(2),
            default = floor_date(date_time)
          )
        )
      ][, timeframe := freq_folder]
    },
    error = function(e) {
      log_error(sprintf(
        "Aggregation failed: %s",
        e$message
      ))
      stop(sprintf(
        "aggregate_daily_to_monthly: Data aggregation failed - %s",
        e$message
      ))
    },
    warning = function(w) {
      log_warn(sprintf(
        "Warning during aggregation: %s",
        w$message
      ))
      NULL
    }
  )

  if (is.null(monthly_data)) {
    log_error(
      "Aggregation returned NULL - check warnings above"
    )
    stop(
      "aggregate_daily_to_monthly: Aggregation failed with warnings"
    )
  }

  log_info(sprintf(
    "Successfully aggregated to %d monthly candles",
    nrow(monthly_data)
  ))

  # ----------------------------------------------------------------------
  # STEP 3: FINAL CLEANUP & VALIDATION
  # ----------------------------------------------------------------------
  log_info(
    "Performing final cleanup and validation"
  )

  data.table::setcolorder(
    monthly_data,
    required_cols
  )

  final_row_count <- nrow(monthly_data)
  final_ticker_count <- data.table::uniqueN(
    monthly_data$ticker
  )

  log_info(sprintf(
    "Function complete - Output: %d rows, %d tickers, date range: %s to %s",
    final_row_count,
    final_ticker_count,
    format(
      min(monthly_data$date_time),
      "%Y-%m-%d"
    ),
    format(
      max(monthly_data$date_time),
      "%Y-%m-%d"
    )
  ))

  setorderv(
    monthly_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  return(monthly_data[])
}


#' Aggregate daily data to quarterly OHLC
#'
#' Aggregates daily data into quarterly candles. Preserves ticker and asset
#' class grouping and adjusts quarter-start dates that fall on weekends.
#'
#' @param daily_data A `data.table` with columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#' @param config A validated configuration list with
#'   `transformation$aggregation$daily_to_quarterly` and
#'   `paths$frequency_folders$quarterly`.
#'
#' @return A `data.table` with quarterly candles and columns:
#'   `date_time`, `open`, `high`, `low`, `close`, `volume`,
#'   `open_interest`, `timeframe`, `ticker`, `asset_class`.
#'
#' @import data.table
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom lubridate floor_date days
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' quarterly_data <- aggregate_daily_to_quarterly(updated_daily, config)
#' }

# Aggregate_to_Quarterly --------------------------------------------------

aggregate_daily_to_quarterly <- function(
  daily_data,
  config
) {
  # ----------------------------------------------------------------------
  # STEP 0: INPUT VALIDATION & LOGGING
  # ----------------------------------------------------------------------
  log_info(
    "Function entry - starting quarterly summarisation"
  )

  if (!inherits(daily_data, "data.table")) {
    msg <- sprintf(
      "Invalid input type: %s. Expected data.table",
      class(daily_data)[1]
    )
    log_error(msg)
    stop(
      "aggregate_daily_to_quarterly: hourly_data must be a data.table"
    )
  }

  if (is.null(config) || !is.list(config)) {
    log_error(
      "Config is NULL or not a list"
    )
    stop(
      "aggregate_daily_to_quarterly: config must be a non-null list"
    )
  }

  if (is.null(config$transformation)) {
    log_error(
      "Config missing required sections: 'transformation'"
    )
    stop(
      "config missing required sections: 'transformation'"
    )
  }

  required_cols <- c(
    "date_time",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "open_interest",
    "timeframe",
    "ticker",
    "asset_class"
  )

  missing_cols <- setdiff(
    required_cols,
    names(daily_data)
  )
  if (length(missing_cols) > 0) {
    msg <- sprintf(
      "Missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
    log_error(msg)
    stop(sprintf(
      "aggregate_daily_to_quarterly: daily_data missing required columns: %s",
      paste(
        missing_cols,
        collapse = ", "
      )
    ))
  }

  if (nrow(daily_data) == 0) {
    log_warn(
      "Input data is empty - returning empty data.table"
    )
    return(data.table())
  }

  log_info(sprintf(
    "Processing %d rows for %d unique tickers",
    nrow(daily_data),
    uniqueN(daily_data$ticker)
  ))

  # ----------------------------------------------------------------------
  # STEP 1: DATA QUALITY CHECKS
  # ----------------------------------------------------------------------
  log_debug(
    "Performing data quality checks"
  )

  critical_cols <- c(
    "open",
    "high",
    "low",
    "close"
  )
  na_counts <- vapply(
    critical_cols,
    function(col) {
      sum(is.na(daily_data[[col]]))
    },
    integer(1)
  )
  if (any(na_counts > 0)) {
    na_summary <- paste(
      names(na_counts)[na_counts > 0],
      na_counts[na_counts > 0],
      sep = "=",
      collapse = ", "
    )
    log_error(sprintf(
      "Found NAs in critical columns: %s",
      na_summary
    ))
    stop(sprintf(
      "aggregate_daily_to_quarterly: Input data contains NAs in: %s",
      paste(
        names(na_counts)[na_counts > 0],
        collapse = ", "
      )
    ))
  }

  neg_prices <- vapply(
    critical_cols,
    function(col) {
      sum(
        daily_data[[col]] < 0,
        na.rm = TRUE
      )
    },
    integer(1)
  )
  if (any(neg_prices > 0)) {
    neg_summary <- paste(
      names(neg_prices)[neg_prices > 0],
      neg_prices[neg_prices > 0],
      sep = "=",
      collapse = ", "
    )
    log_warn(sprintf(
      "Found negative prices: %s",
      neg_summary
    ))
  }

  # ----------------------------------------------------------------------
  # STEP 2: AGGREGATE TO QUARTERLY OHLC (with tryCatch)
  # ----------------------------------------------------------------------
  log_info(
    "Aggregating daily data to quarterly OHLC"
  )

  qrt <- config$transformation$aggregation$daily_to_quarterly
  freq_folder <- config$paths$frequency_folders$quarterly

  quarterly_data <- tryCatch(
    {
      daily_data[,
        .(
          open = first(open),
          high = max(high),
          low = min(low),
          close = last(close),
          volume = sum(volume),
          open_interest = last(open_interest)
        ),
        by = .(
          ticker,
          asset_class,
          date_time = fcase(
            lubridate::wday(floor_date(date_time, unit = qrt)) == 1,
            floor_date(date_time, unit = qrt) + days(1),
            lubridate::wday(floor_date(date_time, unit = qrt)) == 7,
            floor_date(date_time, unit = qrt) + days(2),
            default = floor_date(date_time)
          )
        )
      ][, timeframe := freq_folder]
    },
    error = function(e) {
      log_error(sprintf(
        "Aggregation failed: %s",
        e$message
      ))
      stop(sprintf(
        "aggregate_daily_to_quarterly: Data aggregation failed - %s",
        e$message
      ))
    },
    warning = function(w) {
      log_warn(sprintf(
        "Warning during aggregation: %s",
        w$message
      ))
      NULL
    }
  )

  if (is.null(quarterly_data)) {
    log_error(
      "Aggregation returned NULL - check warnings above"
    )
    stop(
      "aggregate_daily_to_quarterly: Aggregation failed with warnings"
    )
  }

  log_info(sprintf(
    "Successfully aggregated to %d quarterly candles",
    nrow(quarterly_data)
  ))

  # ----------------------------------------------------------------------
  # STEP 3: FINAL CLEANUP & VALIDATION
  # ----------------------------------------------------------------------
  log_info(
    "Performing final cleanup and validation"
  )

  data.table::setcolorder(
    quarterly_data,
    required_cols
  )

  final_row_count <- nrow(
    quarterly_data
  )
  final_ticker_count <- data.table::uniqueN(
    quarterly_data$ticker
  )

  log_info(sprintf(
    "Function complete - Output: %d rows, %d tickers, date range: %s to %s",
    final_row_count,
    final_ticker_count,
    format(
      min(quarterly_data$date_time),
      "%Y-%m-%d"
    ),
    format(
      max(quarterly_data$date_time),
      "%Y-%m-%d"
    )
  ))

  setorderv(
    quarterly_data,
    c(
      "ticker",
      "asset_class",
      "date_time"
    )
  )

  return(quarterly_data[])
}
