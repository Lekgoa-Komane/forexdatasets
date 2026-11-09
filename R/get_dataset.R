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
  output_base <- config$paths$output_base
  folder <- config$paths$frequency_folders[[timeframe]]
  suffix <- config$paths$file_suffixes[[timeframe]]

  if (is.null(output_base)) {
    stop("Config is missing `paths$output_base`.", call. = FALSE)
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
