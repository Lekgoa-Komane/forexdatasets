# tests/testthat/test-cot_save.R
# Tests for build_cot_path() and save_cot_dataset().
# Uses make_valid_config() from helper.R (a temporary pantry, never real data).

library(data.table)

make_cot_dt <- function() {
  # Deliberately NOT sorted by date
  data.table(
    Market_and_Exchange_Names = c("EURO FX (EUR)", "EURO FX (EUR)", "GOLD (XAU)"),
    Report_Date_as_YYYY_MM_DD = as.Date(c("2024-01-09", "2024-01-02", "2024-01-02")),
    Source_Year = 2024L,
    Open_Interest = c(100, 110, 120)
  )
}

# ---------------------------------------------------------------------------
# build_cot_path
# ---------------------------------------------------------------------------

test_that("build_cot_path uses the cot folder by default", {
  config <- make_valid_config()

  expect_equal(
    build_cot_path(config, "legacy"),
    file.path(config$paths$output_base, "cot", "legacy.parquet")
  )
})

test_that("build_cot_path honours a custom folder and handles several reports", {
  config <- make_valid_config()
  config$cot_report <- list(output_folder = "positioning")

  expect_equal(
    build_cot_path(config, c("legacy", "tff")),
    file.path(config$paths$output_base, "positioning", c("legacy.parquet", "tff.parquet"))
  )
})

test_that("build_cot_path refuses bad input", {
  config <- make_valid_config()

  expect_error(build_cot_path(config, NA_character_), "non-empty text")
  expect_error(build_cot_path(config, ""), "non-empty text")
  expect_error(build_cot_path(config, character()), "non-empty text")
  expect_error(build_cot_path(config, 1), "non-empty text")

  config$paths$output_base <- NULL
  expect_error(build_cot_path(config, "legacy"), "paths\\$output_base")
})

# ---------------------------------------------------------------------------
# save_cot_dataset
# ---------------------------------------------------------------------------

test_that("save_cot_dataset writes one parquet file per report and the data survives the round trip", {
  config <- make_valid_config()
  cot <- list(legacy = make_cot_dt(), tff = make_cot_dt())

  paths <- save_cot_dataset(cot, config)

  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))
  expect_equal(paths, build_cot_path(config, c("legacy", "tff")))

  back <- data.table::as.data.table(arrow::read_parquet(paths[1]))
  expect_equal(nrow(back), 3)
  expect_equal(sort(names(back)), sort(names(make_cot_dt())))
  expect_s3_class(back$Report_Date_as_YYYY_MM_DD, "Date")
  expect_false(is.unsorted(back$Report_Date_as_YYYY_MM_DD))
  expect_equal(back$Open_Interest[1], 110) # the earliest date comes first
})

test_that("save_cot_dataset does not change the table it is given and leaves no .tmp file", {
  config <- make_valid_config()
  cot <- list(legacy = make_cot_dt())

  paths <- save_cot_dataset(cot, config)

  # The caller's table is still in its original (unsorted) order
  expect_equal(cot$legacy$Report_Date_as_YYYY_MM_DD[1], as.Date("2024-01-09"))
  expect_length(list.files(dirname(paths), pattern = "\\.tmp$"), 0)
})

test_that("save_cot_dataset gives the same file when run twice", {
  config <- make_valid_config()
  cot <- list(legacy = make_cot_dt())

  path <- save_cot_dataset(cot, config)
  first <- data.table::as.data.table(arrow::read_parquet(path))
  save_cot_dataset(cot, config)
  second <- data.table::as.data.table(arrow::read_parquet(path))

  expect_equal(second, first)
})

test_that("save_cot_dataset skips an empty report and never overwrites a saved file with nothing", {
  config <- make_valid_config()

  # A good file already exists
  save_cot_dataset(list(legacy = make_cot_dt()), config)
  legacy_path <- build_cot_path(config, "legacy")

  # Now every download "failed": legacy is empty, tff has data, other is empty
  save_cot_dataset(
    list(legacy = data.table(), tff = make_cot_dt(), other = data.table()),
    config
  )

  kept <- data.table::as.data.table(arrow::read_parquet(legacy_path))
  expect_equal(nrow(kept), 3)                                  # untouched
  expect_true(file.exists(build_cot_path(config, "tff")))      # written
  expect_false(file.exists(build_cot_path(config, "other")))   # never created
})

test_that("save_cot_dataset refuses bad input", {
  config <- make_valid_config()

  expect_error(save_cot_dataset(make_cot_dt(), config), "non-empty named list")
  expect_error(save_cot_dataset(list(), config), "non-empty named list")
  expect_error(save_cot_dataset(list(make_cot_dt()), config), "unique, non-empty name")
  expect_error(
    save_cot_dataset(list(a = make_cot_dt(), a = make_cot_dt()), config),
    "unique, non-empty name"
  )
  expect_error(
    save_cot_dataset(list(legacy = data.frame(x = 1)), config),
    "not data.tables: legacy"
  )

  no_date <- make_cot_dt()[, Report_Date_as_YYYY_MM_DD := NULL]
  expect_error(
    save_cot_dataset(list(legacy = no_date), config),
    "missing column `Report_Date_as_YYYY_MM_DD`"
  )

  text_date <- make_cot_dt()[, Report_Date_as_YYYY_MM_DD := as.character(Report_Date_as_YYYY_MM_DD)]
  expect_error(
    save_cot_dataset(list(legacy = text_date), config),
    "must be a Date"
  )
})

test_that("save_cot_dataset stops before writing anything when the config is incomplete", {
  config <- make_valid_config()
  base <- config$paths$output_base
  config$paths$output_base <- NULL

  expect_error(save_cot_dataset(list(legacy = make_cot_dt()), config), "paths\\$output_base")
  expect_false(dir.exists(file.path(base, "cot")))
})

test_that("save_cot_dataset stops with a clear message and leaves no .tmp file when the final move fails", {
  config <- make_valid_config()
  final <- build_cot_path(config, "legacy")

  # A non-empty FOLDER where the file should go makes the final rename fail
  dir.create(final, recursive = TRUE)
  file.create(file.path(final, "blocker.txt"))

  expect_error(
    suppressWarnings(save_cot_dataset(list(legacy = make_cot_dt()), config)),
    "Could not move the temporary file into place"
  )
  expect_false(file.exists(paste0(final, ".tmp")))
})
