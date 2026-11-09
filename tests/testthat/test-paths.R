# Helper function to generate a fully complete mock config for build_data_path
make_mock_path_config <- function() {
  list(
    paths = list(
      output_base = "/mock/base",
      frequency_folders = list(
        daily = "daily_folder",
        h1 = "hourly_folder"
      ),
      file_suffixes = list(
        daily = "d",
        h1 = "h1"
      )
    )
  )
}

# ==============================================================================
# INPUT VALIDATION GUARDRAILS (LINES 34-43)
# ==============================================================================

test_that("build_data_path catches invalid timeframe values", {
  config <- make_mock_path_config()

  # Trigger NA timeframe check
  expect_error(
    build_data_path(config, timeframe = NA, "Currency_Pairs", "audcad"),
    regexp = "`timeframe` must be exactly one value"
  )

  # Trigger multiple timeframe values check
  expect_error(
    build_data_path(
      config,
      timeframe = c("daily", "h1"),
      "Currency_Pairs",
      "audcad"
    ),
    regexp = "`timeframe` must be exactly one value"
  )

  # Trigger zero-length timeframe check
  expect_error(
    build_data_path(
      config,
      timeframe = character(0),
      "Currency_Pairs",
      "audcad"
    ),
    regexp = "`timeframe` must be exactly one value"
  )
})

test_that("build_data_path rejects NA values in asset_class or ticker", {
  config <- make_mock_path_config()

  # NA in asset_class
  expect_error(
    build_data_path(config, "daily", asset_class = NA_character_, "audcad"),
    regexp = "`asset_class` and `ticker` must not contain NA"
  )

  # NA in ticker
  expect_error(
    build_data_path(config, "daily", "Currency_Pairs", ticker = NA_character_),
    regexp = "`asset_class` and `ticker` must not contain NA"
  )
})

# ==============================================================================
# CONFIGURATION SCHEMA GUARDRAILS (LINES 51-69)
# ==============================================================================

test_that("build_data_path errors when config lacks output_base", {
  config <- make_mock_path_config()
  config$paths$output_base <- NULL # Strip out base path

  expect_error(
    build_data_path(config, "daily", "Currency_Pairs", "audcad"),
    regexp = "Config is missing `paths\\$output_base`"
  )
})

test_that("build_data_path errors when frequency_folders missing timeframe key", {
  config <- make_mock_path_config()

  # Request a structural timeframe not mapped in our mock frequency_folders list
  expect_error(
    build_data_path(config, "m15", "Currency_Pairs", "audcad"),
    regexp = "Config has no `paths\\$frequency_folders` entry for 'm15'"
  )
})

test_that("build_data_path errors when file_suffixes missing timeframe key", {
  config <- make_mock_path_config()

  # Request a timeframe not mapped in file_suffixes list
  expect_error(
    build_data_path(config, "m15", "Currency_Pairs", "audcad"),
    regexp = "Config has no `paths\\$frequency_folders` entry for 'm15'"
  )

  # Explicitly isolate missing file_suffix while folder exists
  config$paths$file_suffixes$daily <- NULL
  expect_error(
    build_data_path(config, "daily", "Currency_Pairs", "audcad"),
    regexp = "Config has no `paths\\$file_suffixes` entry for 'daily'"
  )
})

# ==============================================================================
# HAPPY PATH SANITY CHECK (LINES 71-76)
# ==============================================================================

test_that("build_data_path resolves correct file footprint structure on success", {
  config <- make_mock_path_config()

  result <- build_data_path(config, "daily", "Currency_Pairs", "audcad")
  expected <- file.path(
    "/mock/base",
    "daily_folder",
    "Currency_Pairs",
    "audcad_d.parquet"
  )

  expect_equal(result, expected)
})
