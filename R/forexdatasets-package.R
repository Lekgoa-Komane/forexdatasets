#' forexengine: Fetch, Store and Serve Forex Price Data
#'
#' The "pantry" of the trading system. It fetches raw price data, stores it as
#' parquet files (one file per ticker, one folder per timeframe), and serves it
#' to the other packages.
#'
#' @keywords internal
#' @import data.table
#' @importFrom logger log_debug log_info log_warn log_error
"_PACKAGE"

# Column names used inside data.table expressions. This only stops
# R CMD check from printing notes about "no visible binding".
utils::globalVariables(c(
  ".",
  "asset_class",
  "ticker",
  "timeframe",
  "date_time",
  "path",
  "first",
  "last",
  "file_path",
  "..critical_cols",
  "..required_cols",
  "body_ratio",
  "body_return",
  "calculated_pip_val",
  "daily_return",
  "data_source",
  "extract_flag",
  "file_ext",
  "fill_val",
  "frequency",
  "h1_return",
  "high",
  "i.calculated_pip_val",
  "i.last_close",
  "i.subdir",
  "investing_ticker",
  "last_close",
  "low",
  "mnt_date",
  "open_interest",
  "pip_conv",
  "qrt_date",
  "quart_time",
  "source_path",
  "subdir",
  "time",
  "type",
  "volatility",
  "volume",
  "Source_Year"
))
