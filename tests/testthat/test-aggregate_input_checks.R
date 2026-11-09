# tests/testthat/test-aggregate_input_checks.R
# Step 3: the input checks that all four aggregate functions share.
# One loop runs the same checks against each function.

# ---- valid inputs (same shapes as the existing aggregate tests) ----------

make_checks_hourly <- function() {
  data.table(
    date_time = as.POSIXct(
      c(
        "2020-01-01 00:00:00",
        "2020-01-01 01:00:00",
        "2020-01-01 02:00:00",
        "2020-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    open = c(1.2000, 1.2010, 1.2020, 1.2030),
    high = c(1.2020, 1.2030, 1.2040, 1.2050),
    low = c(1.1990, 1.2000, 1.2010, 1.2020),
    close = c(1.2010, 1.2020, 1.2030, 1.2040),
    timeframe = "h1",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

make_checks_daily <- function() {
  data.table(
    date_time = as.POSIXct(
      c(
        "2020-01-06", "2020-01-07", "2020-01-08", "2020-01-09",
        "2020-01-10", "2020-01-11", "2020-01-12"
      ),
      tz = "UTC"
    ),
    open = c(1.2000, 1.2010, 1.2020, 1.2030, 1.2040, 1.2050, 1.2060),
    high = c(1.2020, 1.2030, 1.2040, 1.2050, 1.2060, 1.2070, 1.2080),
    low = c(1.1990, 1.2000, 1.2010, 1.2020, 1.2030, 1.2040, 1.2050),
    close = c(1.2010, 1.2020, 1.2030, 1.2040, 1.2050, 1.2060, 1.2070),
    volume = c(100, 150, 200, 250, 300, 350, 400),
    open_interest = c(50, 55, 60, 65, 70, 75, 80),
    timeframe = "daily",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

aggregates <- list(
  list(name = "aggregate_hourly_to_4hour",
       fn = aggregate_hourly_to_4hour, make = make_checks_hourly),
  list(name = "aggregate_daily_to_weekly",
       fn = aggregate_daily_to_weekly, make = make_checks_daily),
  list(name = "aggregate_daily_to_monthly",
       fn = aggregate_daily_to_monthly, make = make_checks_daily),
  list(name = "aggregate_daily_to_quarterly",
       fn = aggregate_daily_to_quarterly, make = make_checks_daily)
)

# ---- the same checks for every function ----------------------------------

for (spec in aggregates) {

  test_that(paste(spec$name, "rejects input that is not a data.table"), {
    config <- make_valid_config()
    expect_error(spec$fn(list(a = 1), config), "must be a data.table")
  })

  test_that(paste(spec$name, "rejects a NULL or non-list config"), {
    data <- spec$make()
    expect_error(spec$fn(data, NULL), "config must be a non-null list")
    expect_error(spec$fn(data, "not a list"), "config must be a non-null list")
  })

  test_that(paste(spec$name, "rejects a config without a transformation section"), {
    config <- make_valid_config()
    config$transformation <- NULL
    expect_error(
      spec$fn(spec$make(), config),
      "config missing required sections: 'transformation'"
    )
  })

  test_that(paste(spec$name, "names the missing column"), {
    config <- make_valid_config()
    data <- spec$make()
    data[, close := NULL]
    expect_error(spec$fn(data, config), "missing required columns: close")
  })

  test_that(paste(spec$name, "refuses NAs in the price columns"), {
    config <- make_valid_config()
    data <- spec$make()
    data[1, close := NA_real_]
    expect_error(spec$fn(data, config), "contains NAs in: close")
  })

  test_that(paste(spec$name, "only logs a warning for negative prices and still returns data"), {
    config <- make_valid_config()
    data <- spec$make()
    data[1, low := -1]

    result <- spec$fn(data, config)

    expect_s3_class(result, "data.table")
    expect_gt(nrow(result), 0)
  })

  test_that(paste(spec$name, "returns an empty data.table for empty input"), {
    config <- make_valid_config()
    result <- spec$fn(spec$make()[0], config)

    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 0)
  })
}

# ---- the 4-hour NA message also names the ticker ------------------------

test_that("aggregate_hourly_to_4hour names the column and the ticker when prices have NAs", {
  config <- make_valid_config()
  data <- make_checks_hourly()
  data[1, close := NA_real_]

  expect_error(
    aggregate_hourly_to_4hour(data, config),
    "NAs in: close\\. Problematic ticker\\(s\\): audcad"
  )
})

# ---- only the 4-hour function checks the date type -----------------------

test_that("aggregate_hourly_to_4hour rejects a date column that is not POSIXct", {
  config <- make_valid_config()
  data <- make_checks_hourly()
  data[, date_time := as.character(date_time)]

  expect_error(
    aggregate_hourly_to_4hour(data, config),
    "date column must be POSIXct"
  )
})

# tests/testthat/test-aggregate_failures.R
# Step 4: what the four aggregate functions do when the aggregation step
# or the metrics step fails.
#
# To force a failure we either give a nonsense time unit in the config,
# or replace one helper function with a fake one that raises an error or a
# warning (testthat::local_mocked_bindings). The fakes only live inside the
# one test that creates them.

make_fail_hourly <- function() {
  data.table(
    date_time = as.POSIXct(
      c(
        "2020-01-01 00:00:00",
        "2020-01-01 01:00:00",
        "2020-01-01 02:00:00",
        "2020-01-01 03:00:00"
      ),
      tz = "UTC"
    ),
    open = c(1.2000, 1.2010, 1.2020, 1.2030),
    high = c(1.2020, 1.2030, 1.2040, 1.2050),
    low = c(1.1990, 1.2000, 1.2010, 1.2020),
    close = c(1.2010, 1.2020, 1.2030, 1.2040),
    timeframe = "h1",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

make_fail_daily <- function() {
  data.table(
    date_time = as.POSIXct(
      c(
        "2020-01-06", "2020-01-07", "2020-01-08", "2020-01-09",
        "2020-01-10", "2020-01-11", "2020-01-12"
      ),
      tz = "UTC"
    ),
    open = c(1.2000, 1.2010, 1.2020, 1.2030, 1.2040, 1.2050, 1.2060),
    high = c(1.2020, 1.2030, 1.2040, 1.2050, 1.2060, 1.2070, 1.2080),
    low = c(1.1990, 1.2000, 1.2010, 1.2020, 1.2030, 1.2040, 1.2050),
    close = c(1.2010, 1.2020, 1.2030, 1.2040, 1.2050, 1.2060, 1.2070),
    volume = c(100, 150, 200, 250, 300, 350, 400),
    open_interest = c(50, 55, 60, 65, 70, 75, 80),
    timeframe = "daily",
    ticker = "audcad",
    asset_class = "Currency_Pairs"
  )
}

# name        : the function being tested
# unit_key    : the config setting that holds its time unit
# date_fn     : the lubridate function it uses to group the dates
specs <- list(
  list(name = "aggregate_hourly_to_4hour", fn = aggregate_hourly_to_4hour,
       make = make_fail_hourly, unit_key = "h1_to_h4",
       date_fn = "ceiling_date"),
  list(name = "aggregate_daily_to_weekly", fn = aggregate_daily_to_weekly,
       make = make_fail_daily, unit_key = "daily_to_weekly",
       date_fn = "floor_date"),
  list(name = "aggregate_daily_to_monthly", fn = aggregate_daily_to_monthly,
       make = make_fail_daily, unit_key = "daily_to_monthly",
       date_fn = "floor_date"),
  list(name = "aggregate_daily_to_quarterly", fn = aggregate_daily_to_quarterly,
       make = make_fail_daily, unit_key = "daily_to_quarterly",
       date_fn = "floor_date")
)

for (spec in specs) {

  test_that(paste(spec$name, "stops with a clear message when the time unit is nonsense"), {
    config <- make_valid_config()
    config$transformation$aggregation[[spec$unit_key]] <- "not a time unit"

    expect_error(
      spec$fn(spec$make(), config),
      "Data aggregation failed"
    )
  })

  test_that(paste(spec$name, "stops when the aggregation step raises a warning"), {
    config <- make_valid_config()

    if (spec$date_fn == "ceiling_date") {
      testthat::local_mocked_bindings(
        ceiling_date = function(...) warning("forced warning"),
        .package = "forexdatasets"
      )
    } else {
      testthat::local_mocked_bindings(
        floor_date = function(...) {
  warning("forced warning")
  as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
},
        .package = "forexdatasets"
      )
    }

    expect_error(
      spec$fn(spec$make(), config),
      "Aggregation failed with warnings"
    )
  })
  }

  
