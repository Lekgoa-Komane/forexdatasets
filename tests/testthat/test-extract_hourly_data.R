# Test extract_hourly_data()
# ------------------------------------------------------------

test_that("extract_hourly_data returns correct data.table for valid file", {
  config <- make_valid_config()

  # Create temporary CSV with required local_hourly columns
  file_path <- tempfile(fileext = ".csv")
  dt <- data.table(
    date = c(20200101L, 20200101L),
    time = c(80000L, 90000L),
    open = c(1.2000, 1.2010),
    high = c(1.2020, 1.2030),
    low = c(1.1990, 1.2000),
    close = c(1.2010, 1.2020)
  )
  fwrite(dt, file_path)

  ticker_row <- data.table(
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )

  result <- extract_hourly_data(ticker_row, file_path, config)

  expect_s3_class(result, "data.table")
  expect_true(all(
    c("date_time", "open", "high", "low", "close", "timeframe", "ticker", "asset_class") %in%
      names(result)
  ))
  expect_equal(nrow(result), 2)
  expect_equal(result$ticker[1], "audcad")
  expect_equal(result$asset_class[1], "Currency_Pairs")
  expect_true(inherits(result$date_time, "POSIXct"))
})

test_that("extract_hourly_data returns NULL when file does not exist", {
  config <- make_valid_config()
  ticker_row <- data.table(ticker = "audcad", asset_class = "Currency_Pairs")

  result <- extract_hourly_data(ticker_row, tempfile(fileext = ".csv"), config)

  expect_null(result)
})

test_that("extract_hourly_data returns NULL when required column is missing", {
  config <- make_valid_config()

  # File with missing 'close' column
  file_path <- tempfile(fileext = ".csv")
  dt <- data.table(
    date = c(20200101L),
    time = c(80000L),
    open = c(1.2),
    high = c(1.21),
    low = c(1.19)
  )
  fwrite(dt, file_path)

  ticker_row <- data.table(ticker = "audcad", asset_class = "Currency_Pairs")

  # Suppress the fread warning about missing 'close'
  result <- suppressWarnings(extract_hourly_data(ticker_row, file_path, config))

  expect_null(result)
})

# Newly added tests
test_that("extract_hourly_data returns NULL when ticker_row does not have exactly 1 row", {
  config <- make_valid_config()

  # Zero rows
  zero_rows <- data.table(ticker = character(), asset_class = character())
  expect_null(extract_hourly_data(zero_rows, tempfile(), config))

  # Two rows
  two_rows <- data.table(
    ticker = c("audcad", "eurusd"),
    asset_class = c("Currency_Pairs", "Currency_Pairs")
  )
  expect_null(extract_hourly_data(two_rows, tempfile(), config))
})
