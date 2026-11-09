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
