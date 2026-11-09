# Test ticker_metadata()
# ------------------------------------------------------------

# Helper that creates a minimal config sufficient for ticker_metadata
make_ticker_config <- function() {
  local_base <- tempfile("local_base")
  dir.create(local_base, recursive = TRUE)

  list(
    paths = list(local_base = local_base),
    data_format = list(
      file_extension = ".csv",
      fut_extension = ".csv",
      local_hourly = c("date", "time", "open", "high", "low", "close"),
      local_daily = c(
        "date",
        "open",
        "high",
        "low",
        "close",
        "volume",
        "open_interest"
      ),
      stooq_daily = c("Date", "Open", "High", "Low", "Close", "Volume"),
      investing_daily = c(
        "Date",
        "Price",
        "Open",
        "High",
        "Low",
        "Vol.",
        "Change %"
      ),
      column_mapping = list(
        hourly = list(
          date = "date",
          time = "time",
          open = "open",
          high = "high",
          low = "low",
          close = "close"
        ),
        daily = list(
          date = "date",
          open = "open",
          high = "high",
          low = "low",
          close = "close",
          volume = "volume",
          open_interest = "open_interest"
        ),
        stooq = list(
          Date = "Date",
          Open = "Open",
          High = "High",
          Low = "Low",
          Close = "Close",
          Volume = "Volume"
        ),
        investing = list(
          Date = "Date",
          Price = "Price",
          Open = "Open",
          High = "High",
          Low = "Low",
          Volume = "Vol."
        )
      )
    ),
    web = list(
      stooq = list(url_pattern = "https://stooq.com/q/d/l/?s={ticker}&i=d"),
      investing = list(
        url_pattern = "https://www.investing.com/{asset_class}/{ticker}-historical-data"
      ),
      user_agent = "test-agent"
    ),
    directory_map = list(
      Currency_Pairs = list(
        daily = list(major = "major_path", other = "other_path"),
        hourly = list(major = "major_path", other = "other_path")
      ),
      Commodity_Futures = list(
        daily = list(stooq = "stooq_path", investing = "investing_path")
      )
    ),
    asset_classes = list(
      names = c("Currency_Pairs", "Commodity_Futures"),
      Currency_Pairs = list(
        source = "local",
        frequencies = c("daily", "hourly"),
        tickers = c("audcad", "eurusd", "gbpjpy"),
        other_tickers = c("gbpjpy")
      ),
      Commodity_Futures = list(
        source = c("local", "stooq", "investing"),
        frequencies = "daily",
        tickers = c("GC", "SI"),
        investing_tickers = c("gold", "silver")
      )
    )
  )
}

test_that("ticker_metadata returns a data.table with expected columns", {
  config <- make_ticker_config()
  result <- ticker_metadata(config)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "asset_class",
      "ticker",
      "frequency",
      "type",
      "data_source",
      "extract_flag",
      "source_path"
    ) %in%
      names(result)
  ))
})

test_that("ticker_metadata handles Currency_Pairs correctly", {
  config <- make_ticker_config()
  result <- ticker_metadata(config)

  # Currency pairs: 3 tickers x 2 frequencies = 6 rows
  currency_rows <- result[asset_class == "Currency_Pairs"]
  expect_equal(nrow(currency_rows), 6)

  # Check that gbpjpy is type 'other' and others are 'major'
  majors <- currency_rows[type == "major", unique(ticker)]
  others <- currency_rows[type == "other", unique(ticker)]
  expect_equal(sort(majors), c("audcad", "eurusd"))
  expect_equal(others, "gbpjpy")

  # Check source_path construction for one row
  one_row <- currency_rows[
    ticker == "audcad" & frequency == "daily" & type == "major"
  ]
  expect_equal(
    one_row$source_path,
    file.path(config$paths$local_base, "major_path", "audcad.csv")
  )
})

test_that("ticker_metadata handles Commodity_Futures with multiple sources", {
  config <- make_ticker_config()
  result <- ticker_metadata(config)

  futures_rows <- result[asset_class == "Commodity_Futures"]
  # 2 tickers x 4 expanded data sources = 8 rows
  expect_equal(nrow(futures_rows), 8)

  # Data sources should include stooq, investing, local_stooq, local_investing
  expect_equal(
    sort(unique(futures_rows$data_source)),
    c("investing", "local_investing", "local_stooq", "stooq")
  )

  # For web sources, source_path should be a URL pattern with ticker substituted
  stooq_row <- futures_rows[ticker == "GC" & data_source == "stooq"]
  expect_match(stooq_row$source_path, "GC")

  # For local sources, source_path should use local base + subdir + ticker + ext
  local_stooq_row <- futures_rows[ticker == "GC" & data_source == "local_stooq"]
  expect_equal(
    local_stooq_row$source_path,
    file.path(config$paths$local_base, "stooq_path", "GC.csv")
  )
})

# Newly added tests
test_that("ticker_metadata errors on missing directory_map entry for regular assets", {
  config <- make_ticker_config()
  # Remove a required directory map entry for a regular asset (e.g., Currency_Pairs daily)
  config$directory_map$Currency_Pairs$daily <- NULL

  expect_error(
    ticker_metadata(config),
    regexp = "Missing directory_map entry|Failed to build subdir_map"
  )
})

test_that("ticker_metadata errors on missing directory_map sub-source entry for futures", {
  config <- make_ticker_config()
  # Remove the stooq sub-source mapping under Commodity_Futures daily
  config$directory_map$Commodity_Futures$daily <- NULL

  expect_error(
    ticker_metadata(config),
    regexp = "Missing directory_map entry"
  )
})

test_that("ticker_metadata warns and handles missing or length-mismatched investing_tickers", {
  config <- make_ticker_config()
  # Cause a length mismatch by shortening investing_tickers vector
  config$asset_classes$Commodity_Futures$investing_tickers <- c("gold") # 1 item for 2 tickers

  expect_warning(
    result <- ticker_metadata(config),
    regexp = "investing_tickers missing or length mismatch"
  )

  # Ensure fallback to NA_character_ happened for investing_ticker column
  futures_inv <- result[
    data_source == "investing" & asset_class == "Commodity_Futures"
  ]
  expect_true(all(is.na(futures_inv$investing_ticker)))
})

# test_that("ticker_metadata errors when an unknown asset slug is encountered for investing", {
#   config <- make_ticker_config()
#   # Add an asset class with investing source but unsupported slug mapping
#   config$asset_classes$names <- c(
#     config$asset_classes$names,
#     "Unsupported_Futures"
#   )
#   config$asset_classes$Unsupported_Futures <- list(
#     source = c("local", "stooq", "investing"),
#     frequencies = "daily",
#     tickers = c("XX"),
#     investing_tickers = c("xx")
#   )
#   # Provide proper nested list structure so Step 1 passes and Step 2 hits asset_slug_map lookup
#   config$directory_map$Unsupported_Futures <- list(
#     daily = list(stooq = "stooq_path", investing = "investing_path")
#   )
#
#   expect_error(
#     ticker_metadata(config),
#     regexp = "Missing investing asset slug"
#   )
# })

test_that("ticker_metadata errors when stooq URL pattern lacks {ticker}", {
  config <- make_ticker_config()
  config$web$stooq$url_pattern <- "https://stooq.com/q/d/l/" # No {ticker}

  expect_error(
    ticker_metadata(config),
    regexp = "stooq URL pattern must contain \\{ticker\\}"
  )
})

test_that("ticker_metadata errors when investing URL pattern lacks {ticker}", {
  config <- make_ticker_config()
  config$web$investing$url_pattern <- "https://www.investing.com/commodities/historical-data" # No {ticker}

  expect_error(
    ticker_metadata(config),
    regexp = "investing URL pattern must contain \\{ticker\\}"
  )
})

# tests/testthat/test-ticker_metadata_branches.R
# REPLACES the earlier version of this file (it assumed a failing asset class
# is dropped; your code re-throws the error instead).
# Uses make_valid_config() from helper-ticker_config.R.

library(data.table)

test_that("ticker_metadata handles a single-source asset class with a flat directory map", {
  config <- make_valid_config()
  config$asset_classes$names <- c(config$asset_classes$names, "Metals")
  config$asset_classes$Metals <- list(
    source = "local",
    frequencies = c("daily", "hourly"),
    tickers = c("xauusd", "xagusd")
  )
  config$directory_map$Metals <- list(
    daily = "metals_daily",
    hourly = "metals_hourly"
  )

  result <- ticker_metadata(config)
  metals <- result[asset_class == "Metals"]

  expect_equal(nrow(metals), 4) # 2 tickers x 2 frequencies
  expect_true(all(metals$data_source == "local"))
  expect_true(all(is.na(metals$type)))
  expect_equal(
    metals[ticker == "xauusd" & frequency == "daily"]$source_path,
    file.path(config$paths$local_base, "metals_daily", "xauusd.csv")
  )
})

test_that("ticker_metadata errors on a missing flat directory_map entry", {
  config <- make_valid_config()
  config$asset_classes$names <- c(config$asset_classes$names, "Metals")
  config$asset_classes$Metals <- list(
    source = "local",
    frequencies = "daily",
    tickers = "xauusd"
  )
  # No directory_map$Metals on purpose

  expect_error(
    ticker_metadata(config),
    "Missing directory_map entry for Metals\\[daily\\]"
  )
})

test_that("ticker_metadata works for futures that have no local source", {
  config <- make_valid_config()
  config$asset_classes$Commodity_Futures$source <- c("stooq", "investing")

  result <- ticker_metadata(config)
  futures <- result[asset_class == "Commodity_Futures"]

  expect_equal(nrow(futures), 4) # 2 tickers x 2 web sources
  expect_equal(sort(unique(futures$data_source)), c("investing", "stooq"))
})

test_that("ticker_metadata errors on an unknown source type", {
  config <- make_valid_config()
  config$asset_classes$Commodity_Futures$source <- c("local", "ftp")

  expect_error(ticker_metadata(config), "Unknown source type 'ftp'")
})

test_that("ticker_metadata errors when an investing asset class has no URL slug", {
  config <- make_valid_config()
  config$asset_classes$names <- c(config$asset_classes$names, "Yields")
  config$asset_classes$Yields <- list(
    source = "investing",
    frequencies = "daily",
    tickers = "us10y"
  )
  config$directory_map$Yields <- list(daily = "yields_daily")

  expect_error(ticker_metadata(config), "Missing investing asset slug")
})

test_that("ticker_metadata errors when there are no asset classes at all", {
  config <- make_valid_config()
  config$asset_classes$names <- character(0)

  expect_error(ticker_metadata(config), "Subdir map is empty")
})

test_that("ticker_metadata errors when no ticker combinations come out", {
  config <- make_valid_config()
  config$asset_classes$names <- "Metals"
  config$asset_classes$Metals <- list(
    source = "local",
    frequencies = "daily",
    tickers = character(0)
  )
  config$directory_map$Metals <- list(daily = "metals_daily")

  expect_error(ticker_metadata(config), "Ticker combo list is empty")
})


