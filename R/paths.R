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
#'   `paths$output_base`, `paths$frequency_folders` and
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

  output_base <- config$paths$output_base
  folder <- config$paths$frequency_folders[[timeframe]]
  suffix <- config$paths$file_suffixes[[timeframe]]

  # No quiet fallbacks: a wrong or missing setting should stop loudly,
  # not send data to a surprise folder.
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

  file.path(
    output_base,
    folder,
    asset_class,
    paste0(ticker, "_", suffix, ".parquet")
  )
}
