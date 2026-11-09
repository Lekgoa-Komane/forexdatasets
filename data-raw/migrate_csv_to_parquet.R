# =============================================================================
# ONE-TIME MOVE: old CSV files -> new parquet files
# =============================================================================
# Put this script in `data-raw/` inside the data package. It is NOT part of the
# package (not exported, not loaded) and NOT part of the targets pipeline.
#
# OLD layout:  old_base / asset_class / folder / subfolder / ticker_suffix.csv
#               (for you: .../Dataset/Python/Commodity_Futures/daily/csv/cb.f_d.csv)
# NEW layout:  output_base / Folder / asset_class / ticker_suffix.parquet
#
# The old CSV files are only READ. They are never changed or deleted.
#
# ONLY h1 AND daily ARE MOVED. H4, weekly, monthly and quarterly are rebuilt
# from them by the aggregate_* functions (those functions take the full table
# in memory and never read old files), so they are not moved from CSV.
#
# HOW TO USE (start with daily, it is the smaller one):
#
#   devtools::load_all()
#   config <- load_config()
#
#   # 1. Dry run: lists what it found, writes nothing
#   old_base <- file.path(config$paths$output_base, "Python")
#   migrate_csv_to_parquet(config, old_base, timeframes = "daily")
#
#   # 2. The real move
#   migrate_csv_to_parquet(config, old_base, timeframes = "daily", dry_run = FALSE)
#
#   # 3. Look at the report it returns. Every row should say ok = TRUE.
#   # 4. Repeat for "h1" (or run both at once: leave `timeframes` out).
# =============================================================================

# ---- Old column names that the transform functions used to rename ----------
# These are the two timeframes that are moved, and the old names are the ones
# 04_transform.R used to rename. If your old CSV files have other old names,
# add them here.
legacy_renames <- list(
  h1 = c(
    candle_pips = "pip_movement",
    candle_return = "volatility",
    log_return = "h1_return"
  ),
  daily = c(
    pct_change = "body_return",
    candle_return = "volatility",
    log_return = "daily_return"
  )
)


# ---- Helper: make any date column a POSIXct in UTC -------------------------
to_utc_posixct <- function(x) {
  if (inherits(x, "POSIXct")) {
    attr(x, "tzone") <- "UTC" # same moment in time, shown in UTC
    return(x)
  }
  out <- as.POSIXct(as.character(x), tz = "UTC")
  if (anyNA(out) && !anyNA(x)) {
    stop(
      "Some dates could not be read. Check the date format in the CSV.",
      call. = FALSE
    )
  }
  out
}


# ---- Helper: find the old CSV files of one timeframe -----------------------
find_old_csv_files <- function(config, tf_key, old_base) {
  base <- old_base
  folder <- config$paths$frequency_folders[[tf_key]]
  suffix <- config$paths$file_suffixes[[tf_key]]
  subdir <- config$paths$subfolder
  if (is.null(subdir)) {
    subdir <- "CSV"
  }

  missing_what <- c(
    if (is.null(base)) "old_base",
    if (is.null(folder)) paste0("paths$frequency_folders$", tf_key),
    if (is.null(suffix)) paste0("paths$file_suffixes$", tf_key)
  )
  if (length(missing_what) > 0L) {
    stop(
      "Config is missing: ",
      paste(missing_what, collapse = ", "),
      call. = FALSE
    )
  }

  # The new timeframe folders (H1, Daily, ...) live in the same base folder,
  # so they are left out when looking for the old asset_class folders.
  new_shelves <- unlist(config$paths$frequency_folders)
  candidates <- list.dirs(base, full.names = FALSE, recursive = FALSE)
  candidates <- setdiff(candidates[nzchar(candidates)], new_shelves)

  pattern <- paste0("_", suffix, "\\.csv$")

  found <- lapply(candidates, function(ac) {
    dir <- file.path(base, ac, folder, subdir)
    if (!dir.exists(dir)) {
      return(NULL)
    }
    files <- list.files(dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0L) {
      return(NULL)
    }
    data.table::data.table(
      asset_class = ac,
      ticker = sub(pattern, "", basename(files)),
      csv_path = files
    )
  })

  data.table::rbindlist(found)
}


# ---- Helper: read and tidy ONE old CSV file --------------------------------
# NOTE: the arguments are called ac / tk / tf (not asset_class / ticker /
# timeframe) because inside data.table a COLUMN with the same name would win.
read_old_csv <- function(csv_path, ac, tk, tf) {
  dt <- data.table::fread(csv_path, stringsAsFactors = FALSE)

  dt <- if (tf == "h1") {
    dt[,
      c(
        "volume",
        "body_return",
        "volatility",
        "body_ratio",
        "h1_return",
        "pip_conv"
      ) := NULL
    ]
  } else {
    dt[, c("body_return", "volatility", "body_ratio", "daily_return") := NULL]
  }

  # 1. Old column names -> new names (only if the new name is not there yet)
  map <- legacy_renames[[tf]]
  if (!is.null(map)) {
    hit <- intersect(names(map), names(dt))
    hit <- hit[!(unname(map[hit]) %in% names(dt))]
    if (length(hit) > 0L) {
      data.table::setnames(dt, hit, unname(map[hit]))
    }
  }

  # 2. One name for the date column: date_time
  if ("trade_date" %in% names(dt) && !"date_time" %in% names(dt)) {
    data.table::setnames(dt, "trade_date", "date_time")
  }
  if (!"date_time" %in% names(dt)) {
    stop("No date column found in: ", csv_path, call. = FALSE)
  }
  dt[, date_time := to_utc_posixct(date_time)]

  # 3. The file's location is the source of truth for ticker / asset_class
  if ("ticker" %in% names(dt) && any(dt$ticker != tk, na.rm = TRUE)) {
    logger::log_warn(paste0(
      "Ticker column differs from file name in: ",
      basename(csv_path),
      ". Using the file name."
    ))
  }
  dt[, `:=`(asset_class = ac, ticker = tk, timeframe = tf)]

  dt
}


# ---- The main function ------------------------------------------------------
migrate_csv_to_parquet <- function(
  config,
  old_base,
  timeframes = c("h1", "daily"),
  dry_run = TRUE,
  overwrite = FALSE
) {
  reports <- list()

  for (tf in timeframes) {
    logger::log_info(paste0("---- Timeframe: ", tf, " ----"))

    # The table's `timeframe` value must match the config key, because
    # build_data_path() looks the folder up by that name.
    folder <- config$paths$frequency_folders[[tf]]
    if (!is.null(folder) && tolower(folder) != tf) {
      logger::log_warn(paste0(
        "Folder name '",
        folder,
        "' in lower case is not '",
        tf,
        "'. The transform functions use tolower(folder) as the timeframe value, ",
        "so check that it matches the config key."
      ))
    }

    # 1. Find the old files
    logger::log_info(paste0("Looking for old CSV files in: ", old_base))
    files <- find_old_csv_files(config, tf, old_base)
    if (nrow(files) == 0L) {
      logger::log_warn(paste0(
        "No old CSV files found for '",
        tf,
        "'. Skipping."
      ))
      next
    }
    logger::log_info(paste0(
      "Found ",
      nrow(files),
      " CSV file(s) in ",
      data.table::uniqueN(files$asset_class),
      " asset class(es)."
    ))

    # 2. Protect newer parquet data from being replaced by older CSV data
    target <- build_data_path(config, tf, files$asset_class, files$ticker)
    already <- file.exists(target)
    if (any(already) && !overwrite) {
      stop(
        sum(already),
        " parquet file(s) already exist for '",
        tf,
        "', for example:\n",
        target[already][1L],
        "\nRunning this move again could replace newer data with older CSV data. ",
        "Use overwrite = TRUE only if you are sure.",
        call. = FALSE
      )
    }

    # 3. Dry run stops here
    if (dry_run) {
      logger::log_info("DRY RUN: nothing was read or written.")
      print(files[, .(csv_files = .N), by = asset_class])
      next
    }

    # 4. Read and tidy every file. If ANY file fails, stop before saving.
    problems <- character()
    tables <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch(
        read_old_csv(
          files$csv_path[i],
          files$asset_class[i],
          files$ticker[i],
          tf
        ),
        error = function(e) {
          problems <<- c(
            problems,
            paste0(basename(files$csv_path[i]), ": ", conditionMessage(e))
          )
          NULL
        }
      )
    })
    if (length(problems) > 0L) {
      stop(
        "Some files could not be read. Nothing was saved.\n",
        paste(problems, collapse = "\n"),
        call. = FALSE
      )
    }

    combined <- data.table::rbindlist(tables, fill = TRUE)
    if (nrow(combined) == 0L) {
      logger::log_warn(paste0(
        "All CSV files for '",
        tf,
        "' were empty. Skipping."
      ))
      next
    }

    raw_counts <- combined[, .(csv_rows = .N), by = .(asset_class, ticker)]

    # 5. Save with the normal packer (same checks, same address map)
    save_dataset(combined, config)

    # 6. Check the result: compare what went in with what is on the shelf
    expected <- unique(combined, by = c("asset_class", "ticker", "date_time"))[,
      .(unique_rows = .N, first = min(date_time), last = max(date_time)),
      by = .(asset_class, ticker)
    ]
    report <- merge(raw_counts, expected, by = c("asset_class", "ticker"))
    report[, path := build_data_path(config, tf, asset_class, ticker)]

    on_shelf <- report[,
      {
        d <- arrow::read_parquet(path, col_select = "date_time")$date_time
        .(
          parquet_rows = length(d),
          parquet_first = min(d),
          parquet_last = max(d)
        )
      },
      by = .(asset_class, ticker)
    ]

    report <- merge(report, on_shelf, by = c("asset_class", "ticker"))
    report[,
      ok := unique_rows == parquet_rows &
        first == parquet_first &
        last == parquet_last
    ]
    report[, timeframe := tf]

    n_bad <- sum(!report$ok)
    if (n_bad == 0L) {
      logger::log_info(paste0(
        "CHECK PASSED for '",
        tf,
        "': all ",
        nrow(report),
        " files match."
      ))
    } else {
      logger::log_error(paste0(
        "CHECK FAILED for '",
        tf,
        "': ",
        n_bad,
        " file(s) do not match. See the report."
      ))
    }

    reports[[tf]] <- report
  }

  invisible(data.table::rbindlist(reports, fill = TRUE))
}
