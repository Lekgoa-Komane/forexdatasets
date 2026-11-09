# cot_save ----------------------------------------------------------------

#' Build the file path of a saved COT report (the COT "one map")
#'
#' Every function that saves or reads a COT report should use this helper, so
#' the saver and the readers can never disagree about where a file lives:
#'
#'   output_base / cot / report_key.parquet
#'
#' The folder name is `cot` unless `config$cot_report$output_folder` says
#' otherwise.
#'
#' @param config The config list from `load_config()`. Needs
#'   `paths$forexdataset$output_base`.
#' @param report_key One or more report keys, for example "legacy". These are
#'   the names of the list returned by [cot_pipeline()].
#'
#' @return A character vector of file paths, one per report key.
#' @export
build_cot_path <- function(config, report_key) {
  if (
    !is.character(report_key) ||
      length(report_key) == 0L ||
      anyNA(report_key) ||
      any(!nzchar(report_key))
  ) {
    stop(
      "`report_key` must be one or more non-empty text values.",
      call. = FALSE
    )
  }

  output_base <- config$paths$forexdataset$output_base
  if (is.null(output_base)) {
    stop("Config is missing `paths$forexdataset$output_base`.", call. = FALSE)
  }

  folder <- config$cot_report$output_folder
  if (is.null(folder)) {
    folder <- "cot"
  }

  file.path(output_base, folder, paste0(report_key, ".parquet"))
}


#' Save the COT reports to parquet, one file per report type
#'
#' The "packer" for COT data. It takes the named list returned by
#' [cot_pipeline()] and stores each report in its own parquet file, using the
#' same safe pattern as [save_dataset()]: write to a temporary file first,
#' then swap it into place, so a crash never leaves a half-written file.
#'
#' Two safety rules:
#' * An EMPTY report (for example when every download failed) is skipped, and
#'   any file saved earlier is left untouched. It is never overwritten with
#'   nothing.
#' * Each report must have a `Report_Date_as_YYYY_MM_DD` column of class Date.
#'
#' @param cot_list A named list of data.tables, as returned by
#'   [cot_pipeline()].
#' @param config The config list from `load_config()`.
#'
#' @return Invisibly, the file paths that were written.
#' @export
save_cot_dataset <- function(cot_list, config) {
  logger::log_info("Starting to save COT reports")

  # ---- 1. Safety checks ----
  if (!is.list(cot_list) || is.data.frame(cot_list) || length(cot_list) == 0L) {
    stop(
      "`cot_list` must be a non-empty named list of data.tables.",
      call. = FALSE
    )
  }

  keys <- names(cot_list)
  if (
    is.null(keys) ||
      anyNA(keys) ||
      any(!nzchar(keys)) ||
      anyDuplicated(keys) > 0L
  ) {
    stop(
      "`cot_list` must have a unique, non-empty name for every report.",
      call. = FALSE
    )
  }

  not_dt <- keys[!vapply(cot_list, data.table::is.data.table, logical(1L))]
  if (length(not_dt) > 0L) {
    stop(
      "These reports are not data.tables: ",
      paste(not_dt, collapse = ", "),
      call. = FALSE
    )
  }

  date_col <- "Report_Date_as_YYYY_MM_DD"

  # ---- 2. Work out every address ONCE, before writing anything ----
  paths <- build_cot_path(config, keys)
  names(paths) <- keys

  # ---- 3. Pack each report safely ----
  saved <- character()

  for (key in keys) {
    dt <- cot_list[[key]]

    if (nrow(dt) == 0L) {
      logger::log_warn(paste0(
        "COT report '",
        key,
        "' is empty. Skipped; any saved file is left as it is."
      ))
      next
    }

    if (!date_col %in% names(dt)) {
      stop(
        "COT report '",
        key,
        "' is missing column `",
        date_col,
        "`.",
        call. = FALSE
      )
    }
    if (!inherits(dt[[date_col]], "Date")) {
      stop(
        "Column `",
        date_col,
        "` of COT report '",
        key,
        "' must be a Date.",
        call. = FALSE
      )
    }

    # Sort by date on a copy, so the caller's table is never changed
    dt <- data.table::copy(dt)
    data.table::setorderv(dt, date_col)

    final <- paths[[key]]
    dir.create(dirname(final), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(final, ".tmp")

    tryCatch(
      {
        arrow::write_parquet(dt, tmp, compression = "zstd")
        if (!file.rename(tmp, final)) {
          stop("Could not move the temporary file into place: ", final)
        }
      },
      error = function(e) {
        unlink(tmp)
        logger::log_error(paste0(
          "Saving COT report '",
          key,
          "' failed: ",
          conditionMessage(e)
        ))
        stop(e)
      }
    )

    saved <- c(saved, final)
  }

  logger::log_info(paste0("Saved ", length(saved), " COT report(s)"))
  invisible(unname(saved))
}


# build_paths -------------------------------------------------------------

#' Build the file path of a stored dataset (the "one map")
#'
#' Every function that saves or reads a ticker's parquet file should use this
#' helper, so the saver and the readers can never disagree about where a file
#' lives. The path is:
#'
#'   output_base / folder / asset_class / ticker_suffix.parquet
#'
#' The base folder, shelf name (folder) and file ending (suffix) all come from
#' the config.
#'
#' @param config The config list from `load_config()`. Needs
#'   `paths$forexdataset$output_base`, `paths$frequency_folders` and
#'   `paths$file_suffixes`.
#' @param timeframe ONE timeframe key, as used in the config, for example
#'   "h1", "h4", "daily", "weekly", "monthly" or "quarterly".
#' @param asset_class Asset class name(s), for example "Currency_Pairs".
#'   Can be a vector.
#' @param ticker Ticker name(s). Can be a vector the same length as
#'   `asset_class`, so a whole table of groups can be done in one step.
#'
#' @return A character vector of file paths (one per ticker).
#' @export
#'
#' @examples
#' \dontrun{
#' build_data_path(config, "h1", "Currency_Pairs", "EURUSD")
#'
#' # For a whole table of groups at once:
#' groups[, file_path := build_data_path(config, "h1", asset_class, ticker)]
#' }
build_data_path <- function(config, timeframe, asset_class, ticker) {
  timeframe <- as.character(timeframe)
  if (length(timeframe) != 1L || is.na(timeframe)) {
    stop(
      "`timeframe` must be exactly one value, for example \"h1\".",
      call. = FALSE
    )
  }

  if (anyNA(asset_class) || anyNA(ticker)) {
    stop("`asset_class` and `ticker` must not contain NA.", call. = FALSE)
  }

  output_base <- config$paths$forexdataset$output_base
  folder <- config$paths$frequency_folders[[timeframe]]
  suffix <- config$paths$file_suffixes[[timeframe]]

  # No quiet fallbacks: a wrong or missing setting should stop loudly,
  # not send data to a surprise folder.
  if (is.null(output_base)) {
    stop("Config is missing `paths$forexdataset$output_base`.", call. = FALSE)
  }
  if (is.null(folder)) {
    stop(
      "Config has no `paths$frequency_folders` entry for '",
      timeframe,
      "'.",
      call. = FALSE
    )
  }
  if (is.null(suffix)) {
    stop(
      "Config has no `paths$file_suffixes` entry for '",
      timeframe,
      "'.",
      call. = FALSE
    )
  }

  file.path(
    output_base,
    folder,
    asset_class,
    paste0(ticker, "_", suffix, ".parquet")
  )
}


# ssva_data ---------------------------------------------------------------

#' Save a big data.table to parquet, one file per ticker
#'
#' The "packer" of the data package. It takes one big table (all tickers for
#' one timeframe) and stores each ticker in its own parquet file. Every file
#' path comes from `build_data_path()`, the same "one map" the readers use:
#'
#'   output_base / folder / asset_class / ticker_suffix.parquet
#'
#' @param dt A data.table with columns `date_time` (POSIXct, UTC),
#'   `timeframe`, `ticker` and `asset_class`. It must hold ONE timeframe, and
#'   its value must match a key in the config (for example "h1" or "daily").
#' @param config The config list from `load_config()`.
#'
#' @return Invisibly, the file paths that were written.
#' @export
save_dataset <- function(dt, config) {
  logger::log_info("Starting to save data")

  # ---- 1. Safety checks (stop early, before bad data lands on the shelf) ----
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.", call. = FALSE)
  }
  if (nrow(dt) == 0L) {
    stop("`dt` is empty. Nothing to save.", call. = FALSE)
  }

  needed <- c("date_time", "timeframe", "ticker", "asset_class")
  missing <- setdiff(needed, names(dt))
  if (length(missing) > 0L) {
    stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!inherits(dt$date_time, "POSIXct")) {
    stop("`date_time` must be POSIXct.", call. = FALSE)
  }
  if (!identical(attr(dt$date_time, "tzone"), "UTC")) {
    stop("`date_time` must be in the UTC time zone.", call. = FALSE)
  }

  # NOTE: the table has a COLUMN called `timeframe`, and inside data.table's
  # j the column wins over a variable with the same name. So the single value
  # is kept under a different name: `tf_key`.
  tf_key <- as.character(unique(dt$timeframe))
  if (length(tf_key) != 1L) {
    stop(
      "`dt` must hold exactly one timeframe. Found: ",
      paste(tf_key, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 2. Tidy: remove duplicate candles (newest copy wins), then sort ----
  # unique() makes a new table, so the caller's table is never changed.
  dt <- unique(
    dt,
    by = c("asset_class", "ticker", "date_time"),
    fromLast = TRUE
  )
  data.table::setorderv(dt, c("asset_class", "ticker", "date_time"))

  # ---- 3. Work out every address ONCE, using the one map ----
  # If the config is missing a setting, this stops here, before any writing.
  groups <- unique(dt[, .(asset_class, ticker)])
  target_paths <- build_data_path(
    config,
    tf_key,
    groups$asset_class,
    groups$ticker
  )

  # Build the drawers (asset_class folders) once, before saving.
  for (d in unique(dirname(target_paths))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- 4. Pack each ticker safely, one group at a time ----
  logger::log_info("Saving data by asset_class and ticker")

  paths <- tryCatch(
    {
      dt[,
        {
          final <- build_data_path(config, tf_key, asset_class, ticker)
          tmp <- paste0(final, ".tmp")

          # Write to a temporary box first, then swap it in.
          tryCatch(
            {
              # .SD does not hold the group columns, so .BY adds them back.
              arrow::write_parquet(
                data.table::as.data.table(c(.BY, .SD)),
                tmp,
                compression = "zstd"
              )
              if (!file.rename(tmp, final)) {
                stop("Could not move the temporary file into place: ", final)
              }
            },
            error = function(e) {
              unlink(tmp)
              stop(e)
            }
          )

          .(path = final)
        },
        by = .(asset_class, ticker)
      ]$path
    },
    error = function(e) {
      # Never swallow the error: say what went wrong, then stop.
      logger::log_error(paste0("Saving failed: ", conditionMessage(e)))
      stop(e)
    }
  )

  logger::log_info(paste0(
    "Saved ",
    length(paths),
    " file(s) for timeframe '",
    tf_key,
    "'"
  ))
  invisible(paths)
}


# get_data ----------------------------------------------------------------

#' Get saved data back out of the pantry (the "serving window")
#'
#' Reads the parquet files that [save_dataset()] wrote and gives you one
#' data.table. Nothing is stored inside the package, so the result always
#' reflects the files on your drive right now.
#'
#' @param timeframe One timeframe key from the config: "h1", "h4", "daily",
#'   "weekly", "monthly" or "quarterly".
#' @param ticker Optional. One or more tickers, spelled exactly as in the file
#'   names (for example "audcad" or "GC.F"). Default: all tickers.
#' @param asset_class Optional. One or more asset classes, for example
#'   "Currency_Pairs". Default: all asset classes. Not every asset class has
#'   every timeframe (for example only some have "h1"). Asking for one that has
#'   no data for the timeframe stops with a message that lists the asset
#'   classes that do.
#' @param from,to Optional date limits. Each can be a Date, a POSIXct, or text
#'   such as "2024-01-31". Both ends are INCLUDED. A date without a time means
#'   the start of that day (UTC).
#' @param columns Optional. Names of the columns you want. `date_time`,
#'   `ticker` and `asset_class` are always returned first, so every row can be
#'   identified. Default: all columns.
#' @param config The config list from [load_config()]. Pass it in when you
#'   call this many times, so the config file is not read again each time.
#'
#' @return A data.table sorted by `asset_class`, `ticker`, `date_time`. It has
#'   zero rows (not an error) when the filters leave nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataset("daily")
#' get_dataset("h1", ticker = "audcad", from = "2024-01-01")
#' get_dataset("daily", asset_class = "Metals", columns = c("close", "volume"))
#' }
get_dataset <- function(
  timeframe,
  ticker = NULL,
  asset_class = NULL,
  from = NULL,
  to = NULL,
  columns = NULL,
  config = load_config()
) {
  # ---- 1. Check the arguments ----
  check_text(timeframe, "timeframe", one = TRUE, allow_null = FALSE)
  check_text(ticker, "ticker")
  check_text(asset_class, "asset_class")
  check_text(columns, "columns")

  from_utc <- as_utc_bound(from, "from")
  to_utc <- as_utc_bound(to, "to")
  if (!is.null(from_utc) && !is.null(to_utc) && from_utc > to_utc) {
    stop("`from` is later than `to`.", call. = FALSE)
  }

  # Different names from the data.table columns, so they cannot be mixed up
  wanted_tickers <- ticker
  wanted_classes <- asset_class

  # ---- 2. Find the files ----
  # Layout: output_base / folder / asset_class / ticker_suffix.parquet
  layout <- timeframe_layout(config, timeframe)

  if (!dir.exists(layout$dir)) {
    stop(
      "No data folder for timeframe '",
      timeframe,
      "' at: ",
      layout$dir,
      ". Is the drive connected?",
      call. = FALSE
    )
  }

  # The asset-class folders sit directly inside the timeframe folder
  all_classes <- list.dirs(layout$dir, full.names = FALSE, recursive = FALSE)
  if (length(all_classes) == 0L) {
    stop(
      "No parquet files found for timeframe '",
      timeframe,
      "' in: ",
      layout$dir,
      call. = FALSE
    )
  }

  # What is on the drive decides which asset classes have data for this
  # timeframe. The config's list only tells a typo from a real asset class.
  classes <- all_classes
  if (!is.null(wanted_classes)) {
    not_on_drive <- setdiff(wanted_classes, all_classes)

    if (length(not_on_drive) > 0L) {
      config_classes <- config$asset_classes$names
      typos <- if (is.null(config_classes)) {
        character(0)
      } else {
        setdiff(not_on_drive, config_classes)
      }

      if (length(typos) > 0L) {
        stop(
          "Unknown asset_class '",
          paste(typos, collapse = "', '"),
          "'. Valid asset classes: ",
          paste(sort(union(config_classes, all_classes)), collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      stop(
        "'",
        paste(not_on_drive, collapse = "', '"),
        "' ",
        if (length(not_on_drive) > 1L) "have" else "has",
        " no ",
        timeframe,
        " data. Asset classes with ",
        timeframe,
        " data: ",
        paste(sort(all_classes), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    classes <- wanted_classes
  }

  # Each asset-class folder comes from the one map (build_data_path), and only
  # THAT folder is listed: every file that ends in _<suffix>.parquet.
  class_dirs <- dirname(build_data_path(config, timeframe, classes, "x"))
  ending <- paste0("_", escape_regex(layout$suffix), "\\.parquet$")

  found <- data.table::rbindlist(lapply(seq_along(classes), function(i) {
    files <- list.files(class_dirs[i], pattern = ending, full.names = TRUE)
    data.table::data.table(
      file = files,
      ticker = sub(ending, "", basename(files)),
      asset_class = rep(classes[i], length(files))
    )
  }))

  if (!is.null(wanted_classes)) {
    no_files <- setdiff(wanted_classes, found$asset_class)
    if (length(no_files) > 0L) {
      stop(
        "'",
        paste(no_files, collapse = "', '"),
        "' ",
        if (length(no_files) > 1L) "have" else "has",
        " no ",
        timeframe,
        " files saved yet.",
        call. = FALSE
      )
    }
  }

  if (nrow(found) == 0L) {
    stop(
      "No parquet files found for timeframe '",
      timeframe,
      "' in: ",
      layout$dir,
      call. = FALSE
    )
  }

  if (!is.null(wanted_tickers)) {
    unknown <- setdiff(wanted_tickers, found$ticker)
    if (length(unknown) > 0L) {
      available <- sort(unique(found$ticker))
      shown <- utils::head(available, 20L)
      stop(
        "No data for ticker(s): ",
        paste(unknown, collapse = ", "),
        " in timeframe '",
        timeframe,
        "'. Available: ",
        paste(shown, collapse = ", "),
        if (length(available) > length(shown)) ", ..." else "",
        call. = FALSE
      )
    }
    found <- found[ticker %in% wanted_tickers]
  }

  # ---- 3. Read them (any unreadable file stops the call) ----
  # Note: col_select must get either a literal choice or nothing at all.
  # Passing a variable that holds NULL makes arrow select zero columns.
  read_cols <- if (!is.null(columns)) {
    unique(c("date_time", "ticker", "asset_class", columns))
  } else {
    NULL
  }

  tables <- lapply(found$file, function(f) {
    tryCatch(
      if (is.null(read_cols)) {
        data.table::as.data.table(arrow::read_parquet(f))
      } else {
        data.table::as.data.table(arrow::read_parquet(
          f,
          col_select = tidyselect::all_of(read_cols)
        ))
      },
      error = function(e) {
        stop(
          "get_dataset: could not read '",
          basename(f),
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  })

  result <- data.table::rbindlist(tables, use.names = TRUE)

  # ---- 4. Date limits and order ----
  if (!is.null(from_utc)) {
    result <- result[date_time >= from_utc]
  }
  if (!is.null(to_utc)) {
    result <- result[date_time <= to_utc]
  }

  data.table::setorderv(result, c("asset_class", "ticker", "date_time"))

  logger::log_info(paste0(
    "get_dataset: ",
    nrow(result),
    " row(s) from ",
    nrow(found),
    " file(s), timeframe '",
    timeframe,
    "'"
  ))
  result[]
}


# ---- internal helpers (not exported) --------------------------------------

# Folder and file ending of one timeframe, from the config
timeframe_layout <- function(config, timeframe) {
  output_base <- config$paths$forexdataset$output_base
  folder <- config$paths$frequency_folders[[timeframe]]
  suffix <- config$paths$file_suffixes[[timeframe]]

  if (is.null(output_base)) {
    stop("Config is missing `paths$forexdataset$output_base`.", call. = FALSE)
  }
  if (is.null(folder)) {
    stop(
      "Config has no `paths$frequency_folders` entry for '",
      timeframe,
      "'.",
      call. = FALSE
    )
  }
  if (is.null(suffix)) {
    stop(
      "Config has no `paths$file_suffixes` entry for '",
      timeframe,
      "'.",
      call. = FALSE
    )
  }

  list(dir = file.path(output_base, folder), suffix = suffix)
}

check_text <- function(x, name, one = FALSE, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop("`", name, "` must be one non-empty text value.", call. = FALSE)
  }
  bad <- !is.character(x) ||
    length(x) == 0L ||
    anyNA(x) ||
    any(!nzchar(x)) ||
    (one && length(x) != 1L)
  if (bad) {
    if (one) {
      stop("`", name, "` must be one non-empty text value.", call. = FALSE)
    }
    stop(
      "`",
      name,
      "` must be text (one or more non-empty values).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Turn a Date, POSIXct or text into a POSIXct in UTC (or NULL)
as_utc_bound <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- if (length(x) == 1L && !is.na(x)) {
    tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) NA)
  } else {
    NA
  }
  if (length(out) != 1L || is.na(out)) {
    stop(
      "`",
      name,
      "` must be one date or date-time, for example \"2024-01-31\".",
      call. = FALSE
    )
  }
  out
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}
