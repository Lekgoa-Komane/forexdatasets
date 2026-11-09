# tests/testthat/test-transform_daily_parquet.R
# Step 2: the "old data exists" part of update_daily_data(), plus its input checks.
# Dates are Monday to Friday of one week.

library(data.table)

make_daily_input <- function(dates, closes, ticker = "audcad") {
  data.table(
    date_time = as.POSIXct(dates, tz = "UTC"),
    open = closes - 0.0005,
    high = closes + 0.0010,
    low = closes - 0.0010,
    close = closes,
    volume = 100,
    open_interest = 50,
    timeframe = "daily",
    ticker = ticker,
    asset_class = "Currency_Pairs"
  )
}

# An old file exactly as the pipeline saves it
make_old_daily_full <- function() {
  data.table(
    date_time = as.POSIXct(
      c("2020-01-06", "2020-01-07", "2020-01-08"),
      tz = "UTC"
    ),
    open = c(1.1995, 1.2005, 1.2015),
    high = c(1.2020, 1.2030, 1.2040),
    low = c(1.1990, 1.2000, 1.2010),
    close = c(1.2010, 1.2020, 1.2030),
    volume = c(100, 200, 300),
    open_interest = c(10, 20, 30),
    timeframe = "daily",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

# ---------------------------------------------------------------------------
# Merging with an existing parquet file
# ---------------------------------------------------------------------------

test_that(
  paste0(
    "update_daily_data keeps old rows, adds only new ones, ",
    "and links the first new row to the last old close"
  ),
  {
    config <- make_valid_config()
    write_fake_parquet(
      config,
      "daily",
      "Currency_Pairs",
      "audcad",
      make_old_daily_full()
    )

    # 01-07 and 01-08 already exist (01-07 comes with a DIFFERENT close).
    # 01-09 and 01-10 are new.
    input <- make_daily_input(
      c("2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10"),
      c(9.9999, 9.9999, 1.2040, 1.2050)
    )

    result <- update_daily_data(input, config)

    expect_equal(nrow(result), 5)
    expect_equal(
      result$date_time,
      as.POSIXct(
        c(
          "2020-01-06",
          "2020-01-07",
          "2020-01-08",
          "2020-01-09",
          "2020-01-10"
        ),
        tz = "UTC"
      )
    )

    old_row <- result[date_time == as.POSIXct("2020-01-07", tz = "UTC")]
    expect_equal(old_row$close, 1.2020)

    new_row <- result[date_time == as.POSIXct("2020-01-09", tz = "UTC")]
    expect_equal(new_row$timeframe, "daily")
  }
)

test_that(
  paste0(
    "update_daily_data finds no new records after its own result ",
    "has been saved"
  ),
  {
    config <- make_valid_config()
    input <- make_daily_input(
      c("2020-01-06", "2020-01-07", "2020-01-08"),
      c(1.2010, 1.2020, 1.2030)
    )

    first <- update_daily_data(data.table::copy(input), config)
    save_dataset(first, config)
    second <- update_daily_data(data.table::copy(input), config)

    expect_s3_class(second, "data.table")
    expect_equal(nrow(second), nrow(first))
    expect_equal(second$date_time, first$date_time)
    expect_equal(second$close, first$close)
  }
)

test_that("update_daily_data fills in the columns an old file does not have", {
  config <- make_valid_config()

  # Old file: plain Date column, and no timeframe, ticker or asset_class
  old_partial <- make_old_daily_full()[, .(
    date_time = as.Date(date_time),
    open,
    high,
    low,
    close,
    volume,
    open_interest
  )]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old_partial)

  input <- make_daily_input(
    c("2020-01-08", "2020-01-09"),
    c(1.2030, 1.2040)
  )

  result <- update_daily_data(input, config)

  expect_equal(nrow(result), 4)
  expect_s3_class(result$date_time, "POSIXct")
  expect_true(all(result$ticker == "audcad"))
  expect_true(all(result$asset_class == "Currency_Pairs"))
  expect_true(all(result$timeframe == "daily"))
})

test_that("update_daily_data stops and names the missing columns when an old file lacks them", {
  config <- make_valid_config()

  old_partial <- make_old_daily_full()[, .(date_time, open, high, low, close)]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old_partial)

  input <- make_daily_input(
    c("2020-01-08", "2020-01-09"),
    c(1.2030, 1.2040)
  )

  expect_error(
    update_daily_data(input, config),
    "could not use existing file.*missing columns: volume, open_interest"
  )
})

test_that("update_daily_data names the single missing column when only one is absent", {
  config <- make_valid_config()

  # Keep every core column except open_interest, so exactly one is missing.
  old_partial <- make_old_daily_full()[, .(
    date_time,
    open,
    high,
    low,
    close,
    volume
  )]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old_partial)

  input <- make_daily_input(
    c("2020-01-08", "2020-01-09"),
    c(1.2030, 1.2040)
  )

  expect_error(
    update_daily_data(input, config),
    "could not use existing file.*missing columns: open_interest"
  )
})

test_that(
  paste0(
    "update_daily_data stops instead of replacing history when an old file is corrupted"
  ),
  {
    config <- make_valid_config()
    path <- build_data_path(config, "daily", "Currency_Pairs", "audcad")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("this is not a parquet file", path)

    input <- make_daily_input(
      c("2020-01-06", "2020-01-07", "2020-01-08"),
      c(1.2010, 1.2020, 1.2030)
    )

    expect_error(
      update_daily_data(input, config),
      "could not use existing file 'audcad_d.parquet'"
    )
  }
)

test_that("update_daily_data stops when the date column of an old file is not a date-time", {
  config <- make_valid_config()

  old_bad <- make_old_daily_full()
  old_bad[, date_time := as.integer(date_time)]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old_bad)

  input <- make_daily_input("2020-01-09", 1.2040)

  expect_error(
    update_daily_data(input, config),
    "could not use existing file.*not a date-time"
  )
})

test_that("update_daily_data stops when the date column of an old file has NA values", {
  config <- make_valid_config()

  old_bad <- make_old_daily_full()
  old_bad[1, date_time := NA]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old_bad)

  input <- make_daily_input("2020-01-09", 1.2040)

  expect_error(
    update_daily_data(input, config),
    "could not use existing file.*date_time contains NA values"
  )
})

test_that("update_daily_data returns an empty data.table for empty input", {
  config <- make_valid_config()
  empty_input <- make_daily_input("2020-01-06", 1.2010)[0]

  result <- update_daily_data(empty_input, config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# ---------------------------------------------------------------------------
# New date checks
# ---------------------------------------------------------------------------
test_that("update_daily_data accepts an old file whose date_time is a plain Date", {
  config <- make_valid_config()
  old <- make_old_daily_full()
  old[, date_time := as.Date(date_time)]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old)

  input <- make_daily_input(
    c("2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10"),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  result <- update_daily_data(input, config)

  expect_equal(nrow(result), 5) # 3 old + 2 new (01-07 - 01-08 already existed)
  expect_s3_class(result$date_time, "POSIXct")
})

test_that("update_daily_data accepts an old file whose date_time is readable text", {
  config <- make_valid_config()
  old <- make_old_daily_full()
  old[, date_time := format(date_time, "%Y-%m-%d")]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old)

  input <- make_daily_input(
    c("2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10"),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  result <- update_daily_data(input, config)

  expect_equal(nrow(result), 5)
  expect_s3_class(result$date_time, "POSIXct")
})

test_that("update_daily_data stops when an old file's date_time text cannot be read as dates", {
  config <- make_valid_config()
  old <- make_old_daily_full()
  old[, date_time := c("not a date", "still not a date", "no date")]
  write_fake_parquet(config, "daily", "Currency_Pairs", "audcad", old)

  input <- make_daily_input(
    c("2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10"),
    c(9.9999, 9.9999, 1.2040, 1.2050)
  )

  expect_error(
    update_daily_data(input, config),
    "date_time is not a date-time"
  )
})

# ---------------------------------------------------------------------------
# Input checks
# ---------------------------------------------------------------------------

test_that("update_daily_data rejects bad input", {
  config <- make_valid_config()
  input <- make_daily_input("2020-01-06", 1.2010)

  expect_error(
    update_daily_data(list(a = 1), config),
    "daily_table must be a data.table"
  )

  no_volume <- data.table::copy(input)[, volume := NULL]
  expect_error(
    update_daily_data(no_volume, config),
    "daily_table missing required columns: volume"
  )
})

test_that("transform_daily rejects a config with missing sections", {
  input <- make_daily_input("2020-01-06", 1.2010)

  config <- make_valid_config()
  config$paths$output_base <- NULL

  expect_error(
    update_daily_data(input, config),
    "config missing required sections"
  )
})
