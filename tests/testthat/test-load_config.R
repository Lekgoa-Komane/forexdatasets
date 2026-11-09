test_that("load_config returns valid list from YAML file", {
  config <- make_valid_config()
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  result <- load_config(config_file)

  expect_type(result, "list")
  expect_true(all(
    c(
      "paths",
      "web",
      "asset_classes",
      "directory_map",
      "data_format",
      "transformation",
      "analysis",
      "database",
      "pipeline"
    ) %in%
      names(result)
  ))
})

test_that("load_config errors when file does not exist", {
  expect_error(
    load_config(tempfile(fileext = ".yml")),
    regexp = "Configuration file not found"
  )
})

test_that("load_config errors when YAML is syntactically invalid", {
  # Create a syntactically invalid YAML file (unbalanced bracket)
  invalid_file <- tempfile(fileext = ".yml")
  writeLines("key: [unclosed", invalid_file)

  expect_error(
    load_config(invalid_file),
    regexp = "Failed to parse configuration"
  )
})

test_that("load_config errors when required top-level key is missing", {
  config <- make_valid_config()
  config$web <- NULL # remove required section
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "Configuration missing critical sections"
  )
})

# Newly added tests
test_that("load_config errors when required paths keys are missing", {
  config <- make_valid_config()
  config$paths$local_base <- NULL # Remove required path key
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "paths section missing required keys"
  )
})

test_that("load_config errors when local base directory does not exist on disk", {
  config <- make_valid_config()
  # Point local_base to a non-existent path
  config$paths$local_base <- tempfile("non_existent_dir")
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "Local base directory does not exist"
  )
})

test_that("load_config errors when required web keys are missing", {
  config <- make_valid_config()
  config$web$stooq <- NULL # Remove required web subkey
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "web section missing required keys"
  )
})

test_that("load_config errors when web URL pattern is missing {ticker} placeholder", {
  config <- make_valid_config()
  config$web$stooq$url_pattern <- "https://stooq.com/q/d/l/" # Missing {ticker}
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "Invalid web URL pattern: missing \\{ticker\\} placeholder"
  )
})

test_that("load_config errors when required data_format keys are missing", {
  config <- make_valid_config()
  config$data_format$file_extension <- NULL
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "data_format section missing required keys"
  )
})

test_that("load_config errors when required column mappings are missing", {
  config <- make_valid_config()
  config$data_format$column_mapping$stooq <- NULL
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "column_mapping section missing required keys"
  )
})

test_that("load_config errors when asset_classes is missing 'names' key", {
  config <- make_valid_config()
  config$asset_classes$names <- NULL
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "asset_classes must include a 'names' list"
  )
})

test_that("load_config errors when an asset class in names is not defined", {
  config <- make_valid_config()
  config$asset_classes$names <- c(config$asset_classes$names, "NonExistentAsset")
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "is listed in names but not defined in asset_classes"
  )
})

test_that("load_config errors when an asset class is missing required keys", {
  config <- make_valid_config()
  config$asset_classes$Currency_Pairs$tickers <- NULL # Remove required asset key
  config_file <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, config_file)

  expect_error(
    load_config(config_file),
    regexp = "missing required keys"
  )
})
