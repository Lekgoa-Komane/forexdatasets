# Test extract_daily_data() and helpers
# ------------------------------------------------------------

test_that("extract_daily_data routes local non-futures correctly (empty CSV)", {
  config <- make_valid_config()

  # Create empty CSV with only header row (zero data rows)
  file_path <- tempfile(fileext = ".csv")
  writeLines("date,time,open,high,low,close", file_path)

  ticker_row <- data.table(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    data_source = "local"
  )

  result <- extract_daily_data(ticker_row, file_path, config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "open_interest", "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      names(result)
  ))
})

test_that("extract_daily_data routes local futures correctly", {
  config <- make_valid_config()

  # Create CSV with futures columns, including Open Interest
  file_path <- tempfile(fileext = ".csv")
  dt <- data.table(
    Date = c("01 Jan 2024", "02 Jan 2024"),
    Open = c(100.0, 101.0),
    High = c(102.0, 103.0),
    Low = c(99.0, 100.0),
    Close = c(101.0, 102.0),
    Volume = c(1000, 1100),
    `Open Interest` = c(500, 600)
  )
  fwrite(dt, file_path)

  ticker_row <- data.table(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    data_source = "local_stooq"
  )

  result <- extract_daily_data(ticker_row, file_path, config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_true(all(
    c(
      "date_time",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "open_interest", "timeframe",
      "ticker",
      "asset_class"
    ) %in%
      names(result)
  ))
  expect_equal(result$ticker[1], "GC.F")
  expect_equal(result$asset_class[1], "Commodity_Futures")
  expect_true(inherits(result$date_time, "POSIXct"))
})

test_that("extract_daily_data returns NULL for unknown data_source", {
  config <- make_valid_config()

  ticker_row <- data.table(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    data_source = "unknown"
  )

  expect_null(extract_daily_data(ticker_row, "some/path", config))
})

test_that("extract_local_daily returns NULL when file does not exist", {
  config <- make_valid_config()

  result <- extract_local_daily(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    file_path = tempfile(fileext = ".csv"),
    config = config
  )

  expect_null(result)
})

test_that("web_scraper returns NULL when robots.txt disallows path", {
  config <- make_valid_config()

  # Mock polite::bow()
  local_mocked_bindings(
    bow = function(url, user_agent, force, delay) {
      list(url = "https://stooq.com", user_agent = user_agent)
    },
    .package = "polite"
  )

  # Mock robotstxt::paths_allowed()
  local_mocked_bindings(
    paths_allowed = function(paths, domain, bot, user_agent) {
      FALSE
    },
    .package = "robotstxt"
  )

  result <- web_scraper(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    data_source = "stooq",
    url_path = "https://stooq.com/q/d/l/?s=GC.F&i=d",
    config = config
  )

  expect_null(result)
})

# Newly added tests
test_that("extract_daily_data errors when ticker_row is not a data.table", {
  config <- make_valid_config()

  # Pass a regular data.frame or list instead of data.table
  not_dt <- data.frame(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    data_source = "local"
  )

  expect_error(
    extract_daily_data(not_dt, "some/path", config),
    regexp = "daily_table must be a data\\.table"
  )
})

test_that("extract_daily_data errors when sources is not a vector", {
  config <- make_valid_config()

  ticker_row <- data.table(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    data_source = "local"
  )

  # Pass a non-vector (e.g., a function or invalid environment)
  expect_error(
    extract_daily_data(ticker_row, sources = function() {}, config),
    regexp = "sources must be a vector"
  )
})

test_that("extract_daily_data returns an empty data.table when ticker_row has zero rows", {
  config <- make_valid_config()
  empty_row <- data.table(
    ticker = character(),
    asset_class = character(),
    data_source = character()
  )

  result <- extract_daily_data(empty_row, "some/path", config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("extract_daily_data returns NULL for an unknown data_source", {
  config <- make_valid_config()
  ticker_row <- data.table(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    data_source = "invalid_source"
  )

  result <- extract_daily_data(ticker_row, "some/path", config)

  expect_null(result)
})

# Unit Tests for extract_local_daily
test_that("extract_local_daily returns NULL when local futures file is missing required columns", {
  config <- make_valid_config()
  file_path <- tempfile(fileext = ".csv")

  # Write CSV lacking 'Volume' and 'Open Interest'
  dt <- data.table(
    Date = "01 Jan 2024",
    Open = 100.0,
    High = 102.0,
    Low = 99.0,
    Close = 101.0
  )
  fwrite(dt, file_path)

  result <- suppressWarnings(extract_local_daily(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    file_path = file_path,
    config = config
  ))

  expect_null(result)
})

test_that("extract_local_daily returns NULL when local non-futures file is missing required columns", {
  config <- make_valid_config()
  file_path <- tempfile(fileext = ".csv")

  # Write CSV lacking 'time' and 'close'
  dt <- data.table(
    date = 20200101L,
    open = 1.2000,
    high = 1.2100,
    low = 1.1900
  )
  fwrite(dt, file_path)

  result <- suppressWarnings(extract_local_daily(
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    file_path = file_path,
    config = config
  ))

  expect_null(result)
})

# Unit Tests for web_scraper
test_that("web_scraper returns NULL when scraping encounters an error (e.g. invalid URL or session failure)", {
  config <- make_valid_config()

  # Mock polite::bow() and robotstxt::paths_allowed() to pass the initial guards
  local_mocked_bindings(
    bow = function(url, user_agent, force, delay) {
      list(url = "https://stooq.com", user_agent = user_agent)
    },
    .package = "polite"
  )

  local_mocked_bindings(
    paths_allowed = function(paths, domain, bot, user_agent) {
      TRUE
    },
    .package = "robotstxt"
  )

  # Pass an invalid URL path to trigger the tryCatch error block in web_scraper
  result <- web_scraper(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    data_source = "stooq",
    url_path = "invalid-malformed-url",
    config = config
  )

  expect_null(result)
})

# Additional Test for extract_daily_data (Web Routing)
test_that("extract_daily_data routes web sources (stooq/investing) to web_scraper correctly", {
  config <- make_valid_config()

  # Mock polite::bow() and robotstxt::paths_allowed() to pass guards
  local_mocked_bindings(
    bow = function(url, user_agent, force, delay) {
      list(url = "https://stooq.com", user_agent = user_agent)
    },
    .package = "polite"
  )

  local_mocked_bindings(
    paths_allowed = function(paths, domain, bot, user_agent) {
      TRUE
    },
    .package = "robotstxt"
  )

  ticker_row <- data.table(
    ticker = "GC.F",
    asset_class = "Commodity_Futures",
    data_source = "stooq"
  )

  # This invokes the routing block in extract_daily_data for web sources
  result <- extract_daily_data(ticker_row, "invalid-url", config)
  expect_null(result)
})

# =============================================================================
# WEB SCRAPER COMPREHENSIVE TESTS (FULLY ISOLATED & MOCKED)
# =============================================================================

