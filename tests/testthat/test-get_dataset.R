# tests/testthat/test-get_dataset.R
# get_dataset(): reading saved data back out.
# Uses make_valid_config() (a temporary pantry) and write_fake_parquet()
# from helper-parquet.R. Never touches your real data.

library(data.table)

make_day_rows <- function(ticker, asset_class, dates, closes) {
  data.table(
    date_time = as.POSIXct(dates, tz = "UTC"),
    open = closes - 0.001,
    high = closes + 0.002,
    low = closes - 0.002,
    close = closes,
    volume = 100,
    open_interest = 10,
    timeframe = "daily",
    ticker = ticker,
    asset_class = asset_class
  )
}

# audcad: 3 days, eurusd: 2 days (both Currency_Pairs), xauusd: 2 days (Metals)
make_pantry <- function(config) {
  write_fake_parquet(
    config,
    "daily",
    "Currency_Pairs",
    "audcad",
    make_day_rows(
      "audcad",
      "Currency_Pairs",
      c("2020-01-06", "2020-01-07", "2020-01-08"),
      c(1.20, 1.21, 1.22)
    )
  )
  write_fake_parquet(
    config,
    "daily",
    "Currency_Pairs",
    "eurusd",
    make_day_rows(
      "eurusd",
      "Currency_Pairs",
      c("2020-01-07", "2020-01-08"),
      c(1.10, 1.11)
    )
  )
  write_fake_parquet(
    config,
    "daily",
    "Metals",
    "xauusd",
    make_day_rows(
      "xauusd",
      "Metals",
      c("2020-01-06", "2020-01-07"),
      c(1500, 1510)
    )
  )
  invisible(config)
}

test_that("get_dataset returns every ticker of a timeframe, sorted", {
  config <- make_pantry(make_valid_config())

  result <- get_dataset("daily", config = config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 7)
  expect_equal(
    result$ticker,
    c(rep("audcad", 3), rep("eurusd", 2), rep("xauusd", 2))
  )
  expect_false(is.unsorted(result[ticker == "audcad"]$date_time))
  expect_s3_class(result$date_time, "POSIXct")
})

test_that("get_dataset filters by ticker and by asset class", {
  config <- make_pantry(make_valid_config())

  two <- get_dataset("daily", ticker = c("eurusd", "xauusd"), config = config)
  expect_equal(nrow(two), 4)
  expect_equal(sort(unique(two$ticker)), c("eurusd", "xauusd"))

  metals <- get_dataset("daily", asset_class = "Metals", config = config)
  expect_equal(nrow(metals), 2)
  expect_equal(unique(metals$ticker), "xauusd")
})

test_that("get_dataset limits dates with both ends included, whatever type they are", {
  config <- make_pantry(make_valid_config())

  # text and Date
  both <- get_dataset(
    "daily",
    from = "2020-01-07",
    to = as.Date("2020-01-08"),
    config = config
  )
  expect_equal(nrow(both), 5) # audcad 2, eurusd 2, xauusd 1

  # POSIXct
  one_day <- get_dataset(
    "daily",
    from = as.POSIXct("2020-01-08", tz = "UTC"),
    config = config
  )
  expect_equal(nrow(one_day), 2) # audcad and eurusd on 01-08
})

test_that("get_dataset returns the identity columns first, then only the columns asked for", {
  config <- make_pantry(make_valid_config())

  result <- get_dataset("daily", columns = "close", config = config)

  expect_equal(names(result), c("date_time", "ticker", "asset_class", "close"))
  expect_equal(nrow(result), 7)
})

test_that("get_dataset returns an empty table, not an error, when the filters leave nothing", {
  config <- make_pantry(make_valid_config())

  result <- get_dataset("daily", from = "2021-01-01", config = config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_true("close" %in% names(result))
})

test_that("get_dataset explains an unknown ticker or asset class", {
  config <- make_pantry(make_valid_config())

  expect_error(
    get_dataset("daily", ticker = "zzzusd", config = config),
    "No data for ticker\\(s\\): zzzusd.*Available: audcad, eurusd, xauusd"
  )
  expect_error(
    get_dataset("daily", asset_class = "Yields", config = config),
    "Unknown asset_class 'Yields'. Valid asset classes: Commodity_Futures, Currency_Pairs, Metals."
  )
  # The ticker exists, but not inside the asset class that was asked for
  expect_error(
    get_dataset(
      "daily",
      ticker = "audcad",
      asset_class = "Metals",
      config = config
    ),
    "No data for ticker"
  )
})

test_that("get_dataset stops and names the file when one file cannot be read", {
  config <- make_pantry(make_valid_config())
  path <- build_data_path(config, "daily", "Currency_Pairs", "audcad")
  writeLines("this is not a parquet file", path)

  expect_error(
    get_dataset("daily", config = config),
    "could not read 'audcad_d.parquet'"
  )
})

test_that("get_dataset stops when a requested column does not exist", {
  config <- make_pantry(make_valid_config())

  expect_error(
    get_dataset("daily", columns = "no_such_column", config = config),
    "could not read"
  )
})

test_that("get_dataset explains a missing folder, an empty folder and an unknown timeframe", {
  config <- make_valid_config()

  # the weekly folder was never created (for example, the drive is not connected)
  expect_error(get_dataset("weekly", config = config), "No data folder")

  # the folder exists but holds no parquet files
  dir.create(
    file.path(
      config$paths$forexdataset$output_base,
      config$paths$frequency_folders$weekly
    ),
    recursive = TRUE
  )
  expect_error(get_dataset("weekly", config = config), "No parquet files found")

  # the config has no such timeframe
  expect_error(get_dataset("hourly", config = config), "frequency_folders")
})

test_that("get_dataset refuses bad arguments", {
  config <- make_pantry(make_valid_config())

  expect_error(
    get_dataset(c("daily", "h1"), config = config),
    "must be one non-empty text value"
  )
  expect_error(
    get_dataset(1, config = config),
    "must be one non-empty text value"
  )
  expect_error(
    get_dataset("daily", ticker = 1, config = config),
    "must be text"
  )
  expect_error(
    get_dataset("daily", columns = "", config = config),
    "must be text"
  )
  expect_error(
    get_dataset("daily", from = "garbage", config = config),
    "must be one date or date-time"
  )
  expect_error(
    get_dataset(
      "daily",
      from = "2020-02-01",
      to = "2020-01-01",
      config = config
    ),
    "later than"
  )
})

test_that("get_dataset stops when the config has no output folder", {
  config <- make_pantry(make_valid_config())
  config$paths$forexdataset$output_base <- NULL

  expect_error(get_dataset("daily", config = config), "paths\\$forexdataset\\$output_base")
})

test_that("get_dataset gives back what save_dataset saved", {
  config <- make_valid_config()
  rows <- make_day_rows(
    "audcad",
    "Currency_Pairs",
    c("2020-01-06", "2020-01-07"),
    c(1.20, 1.21)
  )

  save_dataset(rows, config)
  result <- get_dataset("daily", config = config)

  expect_equal(nrow(result), 2)
  expect_equal(result$close, c(1.20, 1.21))
  expect_equal(result$date_time, rows$date_time)
})

test_that("get_dataset lists only the asset-class folders, one level down", {
  config <- make_pantry(make_valid_config())
  class_dir <- dirname(build_data_path(config, "daily", "Currency_Pairs", "x"))

  # A loose file in the timeframe folder, and one in a deeper folder, are ignored
  timeframe_dir <- dirname(class_dir)
  writeLines("junk", file.path(timeframe_dir, "loose_d.parquet"))
  dir.create(file.path(class_dir, "archive"))
  writeLines("junk", file.path(class_dir, "archive", "audcad_d.parquet"))

  result <- get_dataset("daily", config = config)

  expect_equal(nrow(result), 7)
})

test_that("get_dataset reads only the asset class that was asked for", {
  config <- make_pantry(make_valid_config())
  # Break the Metals file. Asking for Currency_Pairs must never open it.
  writeLines("junk", build_data_path(config, "daily", "Metals", "xauusd"))

  result <- get_dataset(
    "daily",
    asset_class = "Currency_Pairs",
    config = config
  )

  expect_equal(nrow(result), 5)
})

test_that("get_dataset returns every saved column when `columns` is not given", {
  config <- make_pantry(make_valid_config())

  result <- get_dataset("daily", config = config)

  expect_equal(
    names(result),
    c(
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
  )
})

test_that("get_dataset does not repeat a column that is asked for twice or is always included", {
  config <- make_pantry(make_valid_config())

  result <- get_dataset(
    "daily",
    columns = c("close", "date_time", "close", "ticker"),
    config = config
  )

  expect_equal(names(result), c("date_time", "ticker", "asset_class", "close"))
})

# ---------------------------------------------------------------------------
# Asset classes that have no data for the timeframe
# ---------------------------------------------------------------------------

test_that("get_dataset tells a typo apart from an asset class that has no data", {
  config <- make_pantry(make_valid_config())

  # a typo: not a real asset class at all
  expect_error(
    get_dataset("daily", asset_class = "Currency_Pair", config = config),
    "Unknown asset_class 'Currency_Pair'. Valid asset classes: Commodity_Futures, Currency_Pairs, Metals."
  )

  # a real asset class (it is in the config) with nothing saved for this timeframe
  expect_error(
    get_dataset("daily", asset_class = "Commodity_Futures", config = config),
    "'Commodity_Futures' has no daily data. Asset classes with daily data: Currency_Pairs, Metals."
  )
})

test_that("get_dataset stops the whole call when only one of several asset classes has no data", {
  config <- make_pantry(make_valid_config())

  expect_error(
    get_dataset(
      "daily",
      asset_class = c("Currency_Pairs", "Commodity_Futures"),
      config = config
    ),
    "'Commodity_Futures' has no daily data"
  )
})

test_that("get_dataset uses 'have' when several asset classes have no data", {
  config <- make_pantry(make_valid_config())
  config$asset_classes$names <- c(config$asset_classes$names, "Yields")

  expect_error(
    get_dataset(
      "daily",
      asset_class = c("Commodity_Futures", "Yields"),
      config = config
    ),
    "'Commodity_Futures', 'Yields' have no daily data"
  )
})

test_that("get_dataset reports an asset-class folder that exists but holds no files", {
  config <- make_pantry(make_valid_config())
  dir.create(file.path(
    config$paths$forexdataset$output_base,
    config$paths$frequency_folders$daily,
    "Commodity_Futures"
  ))

  expect_error(
    get_dataset("daily", asset_class = "Commodity_Futures", config = config),
    "'Commodity_Futures' has no daily files saved yet"
  )

  # Asking for everything just skips the empty folder
  expect_equal(nrow(get_dataset("daily", config = config)), 7)
})

test_that("get_dataset still explains itself when the config has no asset-class list", {
  config <- make_pantry(make_valid_config())
  config$asset_classes$names <- NULL

  expect_error(
    get_dataset("daily", asset_class = "Yields", config = config),
    "'Yields' has no daily data. Asset classes with daily data: Currency_Pairs, Metals."
  )
})

test_that("the list of asset classes with data comes from the drive, for h1 and for h4 alike", {
  config <- make_valid_config()
  config$asset_classes$names <- c(
    "Currency_Indices",
    "Currency_Pairs",
    "Metals",
    "Currency_Futures"
  )
  one_row <- function(timeframe, ticker, asset_class) {
    data.table(
      date_time = as.POSIXct("2020-01-06 01:00:00", tz = "UTC"),
      open = 1,
      high = 2,
      low = 0.5,
      close = 1.5,
      timeframe = timeframe,
      ticker = ticker,
      asset_class = asset_class
    )
  }

  # h1 exists for three asset classes, h4 for only one
  for (ac in c("Currency_Indices", "Currency_Pairs", "Metals")) {
    write_fake_parquet(config, "h1", ac, "t1", one_row("h1", "t1", ac))
  }
  write_fake_parquet(
    config,
    "h4",
    "Metals",
    "t1",
    one_row("h4", "t1", "Metals")
  )

  expect_error(
    get_dataset("h1", asset_class = "Currency_Futures", config = config),
    "'Currency_Futures' has no h1 data. Asset classes with h1 data: Currency_Indices, Currency_Pairs, Metals."
  )
  expect_error(
    get_dataset("h4", asset_class = "Currency_Pairs", config = config),
    "'Currency_Pairs' has no h4 data. Asset classes with h4 data: Metals."
  )
  expect_equal(
    nrow(get_dataset("h1", asset_class = "Metals", config = config)),
    1
  )
})

# Newly added tests
test_that("get_dataset stops when asset-class folders exist but hold no parquet files", {
  config <- make_valid_config()

  # Asset-class folder exists for the timeframe, but holds no *_d.parquet
  # files, so `found` is empty even though `all_classes` is not.
  class_dir <- file.path(
    config$paths$forexdataset$output_base,
    config$paths$frequency_folders$daily,
    "Currency_Pairs"
  )
  dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("not a parquet file", file.path(class_dir, "notes.txt"))

  expect_error(
    get_dataset("daily", config = config),
    "No parquet files found for timeframe 'daily'"
  )
})

test_that("get_dataset stops when the config has no file-suffix entry for the timeframe", {
  config <- make_pantry(make_valid_config())
  config$paths$file_suffixes$daily <- NULL

  expect_error(
    get_dataset("daily", config = config),
    "file_suffixes"
  )
})

test_that("get_dataset refuses a NULL timeframe", {
  config <- make_pantry(make_valid_config())

  expect_error(
    get_dataset(NULL, config = config),
    "`timeframe` must be one non-empty text value"
  )
})
