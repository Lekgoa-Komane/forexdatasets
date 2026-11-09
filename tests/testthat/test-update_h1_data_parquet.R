# tests/testthat/test-update_h1_data_parquet.R
# Step 2: the "old data exists" part of update_h1_data(), plus its input checks.
# Candle times avoid 00:00:00 on purpose (see the note about the midnight rule).

library(data.table)

make_hourly_input <- function(times, closes, ticker = "audcad") {
  data.table(
    date_time = as.POSIXct(times, tz = "UTC"),
    open = closes - 0.0005,
    high = closes + 0.0010,
    low = closes - 0.0010,
    close = closes,
    timeframe = "h1",
    ticker = ticker,
    asset_class = "Currency_Pairs"
  )
}

# An old file exactly as the pipeline saves it (all columns present),
# plus one legacy column (pip_movement) that must be dropped.
make_old_hourly_full <- function() {
  data.table(
    date_time = as.POSIXct(
      c(
        "2020-01-01 01:00:00",
        "2020-01-01 02:00:00",
        "2020-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    open = c(1.1995, 1.2005, 1.2015),
    high = c(1.2020, 1.2030, 1.2040),
    low = c(1.1990, 1.2000, 1.2010),
    close = c(1.2010, 1.2020, 1.2030),
    timeframe = "h1",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

# ---------------------------------------------------------------------------
# Merging with an existing parquet file
# ---------------------------------------------------------------------------

test_that("update_h1_data keeps old candles, adds only new ones, and links the first new candle to the last old close", {
  config <- make_valid_config()
  write_fake_parquet(
    config,
    "h1",
    "Currency_Pairs",
    "audcad",
    make_old_hourly_full()
  )

  # 02:00 and 03:00 already exist (02:00 comes with a DIFFERENT close).
  # 04:00 and 05:00 are new.
  input <- make_hourly_input(
    c(
      "2020-01-01 02:00:00",
      "2020-01-01 03:00:00",
      "2020-01-01 04:00:00",
      "2020-01-01 05:00:00"
    ),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  result <- update_h1_data(input, config)

  expect_equal(nrow(result), 5)
  expect_equal(
    result$date_time,
    as.POSIXct(
      c(
        "2020-01-01 01:00:00",
        "2020-01-01 02:00:00",
        "2020-01-01 03:00:00",
        "2020-01-01 04:00:00",
        "2020-01-01 05:00:00"
      ),
      tz = "UTC"
    )
  )

  # The old candle wins: its close and stored body_return are untouched
  old_row <- result[date_time == as.POSIXct("2020-01-01 02:00:00", tz = "UTC")]
  expect_equal(old_row$close, 1.2020)

  # The new 04:00 candle: body return uses its own open,
  # h1_return links to the last OLD close (03:00 close = 1.2030)
  new_row <- result[date_time == as.POSIXct("2020-01-01 04:00:00", tz = "UTC")]
  expect_equal(new_row$timeframe, "h1")
})

test_that("update_h1_data finds no new records after its own result has been saved", {
  config <- make_valid_config()
  input <- make_hourly_input(
    c("2020-01-01 01:00:00", "2020-01-01 02:00:00", "2020-01-01 03:00:00"),
    c(1.2010, 1.2020, 1.2030)
  )

  first <- update_h1_data(data.table::copy(input), config)
  save_dataset(first, config)
  second <- update_h1_data(data.table::copy(input), config)

  expect_s3_class(second, "data.table")
  expect_equal(nrow(second), nrow(first))
  expect_equal(second$date_time, first$date_time)
  expect_equal(second$close, first$close)
})

test_that("update_h1_data fills in the columns an old file does not have", {
  config <- make_valid_config()

  # Old file: plain Date column, and no timeframe, ticker or asset_class
  old_partial <- make_old_hourly_full()[, .(
    date_time = as.POSIXct(date_time),
    open,
    high,
    low,
    close
  )]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old_partial)

  input <- make_hourly_input(
    c("2020-01-01 03:00:00", "2020-01-01 04:00:00"),
    c(1.2030, 1.2040)
  )

  result <- update_h1_data(input, config)

  expect_equal(nrow(result), 4)
  expect_s3_class(result$date_time, "POSIXct")
  expect_true(all(result$ticker == "audcad"))
  expect_true(all(result$asset_class == "Currency_Pairs"))
  expect_true(all(result$timeframe == "h1"))
})

test_that("update_h1_data stops and names the missing columns when an old file lacks them", {
  config <- make_valid_config()

  old_partial <- make_old_hourly_full()[, .(date_time, open, high, low)]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old_partial)

  input <- make_hourly_input(
    c("2020-01-01 03:00:00", "2020-01-01 04:00:00"),
    c(1.2030, 1.2040)
  )

  expect_error(
    update_h1_data(input, config),
    "could not use existing file.*missing columns: close"
  )
})

test_that("update_h1_data names multiple missing columns when an old file lacks them", {
  config <- make_valid_config()

  # Missing two core columns: close and volume is not in h1 core, so drop
  # close and low to get length(missing_core) == 2.
  old_partial <- make_old_hourly_full()[, .(date_time, open, high)]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old_partial)

  input <- make_hourly_input(
    c("2020-01-01 03:00:00", "2020-01-01 04:00:00"),
    c(1.2030, 1.2040)
  )

  expect_error(
    update_h1_data(input, config),
    "could not use existing file.*missing columns: low, close"
  )
})

test_that("update_h1_data stops when the date column of an old file is not a date-time", {
  config <- make_valid_config()

  old_bad <- make_old_hourly_full()
  old_bad[, date_time := as.integer(date_time)]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old_bad)

  input <- make_hourly_input("2020-01-01 04:00:00", 1.2040)

  expect_error(
    update_h1_data(input, config),
    "could not use existing file.*not a date-time"
  )
})

test_that("update_h1_data stops when the date column of an old file has NA values", {
  config <- make_valid_config()

  old_bad <- make_old_hourly_full()
  old_bad[1, date_time := NA]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old_bad)

  input <- make_hourly_input("2020-01-01 04:00:00", 1.2040)

  expect_error(
    update_h1_data(input, config),
    "could not use existing file.*date_time contains NA values"
  )
})

test_that("update_h1_data stops instead of replacing history when an old file cannot be read", {
  config <- make_valid_config()
  path <- build_data_path(config, "h1", "Currency_Pairs", "audcad")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("this is not a parquet file", path)

  input <- make_hourly_input(
    c("2020-01-01 01:00:00", "2020-01-01 02:00:00", "2020-01-01 03:00:00"),
    c(1.2010, 1.2020, 1.2030)
  )

  expect_error(
    update_h1_data(input, config),
    "could not use existing file 'audcad_h1.parquet'"
  )
})

# ---------------------------------------------------------------------------
# New date checks
# ---------------------------------------------------------------------------
test_that("update_h1_data accepts an old file whose date_time is readable text", {
  config <- make_valid_config()
  old <- make_old_hourly_full()
  old[, date_time := format(date_time, "%Y-%m-%d %H:%M:%S")]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old)

  input <- make_hourly_input(
    c(
      "2020-01-01 02:00:00",
      "2020-01-01 03:00:00",
      "2020-01-01 04:00:00",
      "2020-01-01 05:00:00"
    ),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  result <- update_h1_data(input, config)

  expect_equal(nrow(result), 5) # 3 old + 2 new (02:00 - 03:00 already existed)
  expect_s3_class(result$date_time, "POSIXct")
})

test_that("update_h1_data stops when an old file's date_time text cannot be read as dates", {
  config <- make_valid_config()
  old <- make_old_hourly_full()
  old[, date_time := c("not a date", "still not a date", "no date")]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old)

  input <- make_hourly_input(
    c("2020-01-01 01:00:00", "2020-01-01 02:00:00", "2020-01-01 03:00:00"),
    c(1.2010, 1.2020, 1.2030)
  )

  expect_error(
    update_h1_data(input, config),
    "date_time is not a date-time"
  )
})

test_that("update_h1_data accepts an old file whose date_time is a plain Date", {
  config <- make_valid_config()
  old <- make_old_hourly_full()
  old[, date_time := as.Date(date_time)]
  write_fake_parquet(config, "h1", "Currency_Pairs", "audcad", old)

  input <- make_hourly_input(
    c(
      "2020-01-01 02:00:00",
      "2020-01-01 03:00:00",
      "2020-01-01 04:00:00",
      "2020-01-01 05:00:00"
    ),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  result <- update_h1_data(input, config)

  expect_equal(nrow(result), 4) # 3 old + 2 new
  expect_s3_class(result$date_time, "POSIXct")
})

# ---------------------------------------------------------------------------
# Input checks
# ---------------------------------------------------------------------------

test_that("update_h1_data rejects bad input", {
  config <- make_valid_config()
  input <- make_hourly_input("2020-01-01 01:00:00", 1.2010)

  expect_error(
    update_h1_data(list(a = 1), config),
    "hourly_table must be a data.table"
  )

  no_close <- data.table::copy(input)[, close := NULL]
  expect_error(
    update_h1_data(no_close, config),
    "hourly_table missing required columns: close"
  )
})

test_that("update_h1_data rejects a config with missing sections", {
  input <- make_hourly_input("2020-01-01 01:00:00", 1.2010)

  config_no_transformation <- make_valid_config()
  config_no_transformation$transformation <- NULL
  expect_error(
    update_h1_data(input, config_no_transformation),
    "config missing required sections"
  )

  config_no_output <- make_valid_config()
  config_no_output$paths$forexdataset$output_base <- NULL
  expect_error(
    update_h1_data(input, config_no_output),
    "config missing required sections"
  )
})

# ---------------------------------------------------------------------------
# Empty input
# ---------------------------------------------------------------------------

test_that("update_h1_data returns an empty data.table for empty input", {
  config <- make_valid_config()
  empty_input <- make_hourly_input("2020-01-01 01:00:00", 1.2010)[0]

  result <- update_h1_data(empty_input, config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})
