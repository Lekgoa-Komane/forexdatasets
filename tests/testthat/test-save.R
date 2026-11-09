# --- Test Helper Data Generators ---
make_valid_save_dt <- function() {
  data.table(
    date_time = as.POSIXct(
      c("2026-10-26 12:00:00", "2026-10-26 13:00:00"),
      tz = "UTC"
    ),
    timeframe = c("daily", "daily"),
    ticker = c("audcad", "audcad"),
    asset_class = c("Currency_Pairs", "Currency_Pairs"),
    open = c(1.10, 1.11),
    high = c(1.15, 1.16),
    low = c(1.09, 1.10),
    close = c(1.12, 1.13),
    volume = c(1000, 1100),
    open_interest = c(0, 0)
  )
}

# ==============================================================================
# 1. GUARDRAIL & VALIDATION TESTS
# ==============================================================================

test_that("save_dataset asserts class and emptiness correctly", {
  config <- make_valid_config()

  # Must be a data.table
  expect_error(save_dataset(list(a = 1), config), "`dt` must be a data.table.")

  # Cannot be empty
  expect_error(
    save_dataset(data.table(), config),
    "`dt` is empty. Nothing to save."
  )
})

test_that("save_dataset catches missing tracking columns", {
  config <- make_valid_config()
  bad_dt <- data.table(date_time = as.POSIXct("2026-10-26", tz = "UTC"))

  expect_error(save_dataset(bad_dt, config), "Missing column\\(s\\):")
})

test_that("save_dataset enforces POSIXct and UTC constraints", {
  config <- make_valid_config()

  # Wrong type (Date instead of POSIXct)
  dt_wrong_type <- make_valid_save_dt()
  dt_wrong_type[, date_time := as.Date(date_time)]
  expect_error(
    save_dataset(dt_wrong_type, config),
    "`date_time` must be POSIXct."
  )

  # Wrong Time Zone (EST instead of UTC)
  dt_wrong_tz <- make_valid_save_dt()
  dt_wrong_tz[, date_time := as.POSIXct(date_time, tz = "EST")]
  expect_error(
    save_dataset(dt_wrong_tz, config),
    "`date_time` must be in the UTC time zone."
  )
})

test_that("save_dataset rejects entries containing mixed timeframes", {
  config <- make_valid_config()
  mixed_dt <- make_valid_save_dt()
  mixed_dt$timeframe <- c("daily", "h1") # Mix timeframes to force error

  expect_error(
    save_dataset(mixed_dt, config),
    "`dt` must hold exactly one timeframe."
  )
})

# ==============================================================================
# 2. SUCCESSFUL EXECUTION TESTS (THE HAPPY PATH)
# ==============================================================================

test_that("save_dataset removes duplicates, sorts, and successfully writes parquet", {
  config <- make_valid_config()
  dt_with_dupe <- make_valid_save_dt()

  # Inject an intentional duplicate row (same asset, ticker, and timestamp)
  # The second row has a distinct close value to confirm the "newest copy wins" rule
  duplicate_row <- data.table(
    date_time = as.POSIXct("2026-10-26 12:00:00", tz = "UTC"),
    timeframe = "daily",
    ticker = "audcad",
    asset_class = "Currency_Pairs",
    open = 1.10,
    high = 1.15,
    low = 1.09,
    close = 1.99, # 1.99 should win because fromLast = TRUE
    volume = 1000,
    open_interest = 0
  )

  dt_with_dupe <- rbind(dt_with_dupe, duplicate_row)

  # Execute the package saving pipeline using your real build_data_path layout
  written_paths <- expect_silent(save_dataset(dt_with_dupe, config))

  # Calculate where your package actually wrote the file inside the temp layout
  expected_file_path <- build_data_path(
    config,
    "daily",
    "Currency_Pairs",
    "audcad"
  )

  # Verify tracking arrays and file footprint exist on disk
  expect_equal(written_paths, expected_file_path)
  expect_true(file.exists(expected_file_path))

  # Read back the saved parquet file using arrow to confirm data integrity
  saved_data <- data.table::as.data.table(arrow::read_parquet(
    expected_file_path
  ))

  # We input 3 rows (2 originals + 1 duplicate timestamp).
  # Deduplication should thin this out to exactly 2 rows.
  expect_equal(nrow(saved_data), 2)

  # Confirm 'fromLast = TRUE' works: the duplicate timestamp (12:00:00) should now have close = 1.99
  expect_equal(
    saved_data[
      date_time == as.POSIXct("2026-10-26 12:00:00", tz = "UTC")
    ]$close,
    1.99
  )
})

test_that("save_dataset stops with a clear message and leaves no .tmp file when the final move fails", {
  config <- make_valid_config()
  dt <- make_valid_save_dt()

  final <- build_data_path(config, "daily", "Currency_Pairs", "audcad")

  # A non-empty FOLDER sitting where the file should go makes the final
  # rename fail, even though the temporary file was written fine.
  dir.create(final, recursive = TRUE)
  file.create(file.path(final, "blocker.txt"))

  expect_error(
    suppressWarnings(save_dataset(dt, config)),
    "Could not move the temporary file into place"
  )

  # The error handler must clean up its temporary file
  expect_false(file.exists(paste0(final, ".tmp")))

  # The blocker was not touched
  expect_true(file.exists(file.path(final, "blocker.txt")))
})
