# tests/testthat/test-cot_pipeline_branches.R
# The failure and edge-case paths of cot_pipeline().
# download.file() and unzip() are replaced by fakes, so there is no network
# and no real zip file. The cache lives in a temporary folder.

library(data.table)

current_year <- as.integer(format(Sys.Date(), "%Y"))

default_cot_rows <- function() {
  data.table(
    Market_and_Exchange_Names = "GOLD - COMMODITY EXCHANGE INC.",
    Report_Date_as_MM_DD_YYYY = "2023-01-03",
    Noncommercial_Positions_Long_All = 15000,
    Unused_Col_Old = 1
  )
}

make_cot_pipeline_config <- function(
  report_names = list(deacot = "deacot"),
  start_year = current_year
) {
  config <- make_valid_config()
  config$cot_report <- list(
    cot_regex = "old", # columns whose name contains "old" are dropped
    cache_dir = tempfile("cot_cache"),
    start_year = start_year,
    report_names = report_names
  )
  config$web$cftc <- list(url = "https://example.com/cftc/")
  config
}

# Replaces download.file() and unzip() for the length of ONE test.
# Returns an environment that records the URLs that were requested.
mock_cftc <- function(
  status_for = function(url) 0L,
  csv_for = NULL,
  archive_has_no_data_file = FALSE,
  .env = parent.frame()
) {
  state <- new.env()
  state$urls <- character()
  state$last_url <- NA_character_

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, quiet, mode) {
      state$urls <- c(state$urls, url)
      state$last_url <- url
      status <- status_for(url)
      if (identical(status, 0L)) {
        file.create(destfile)
      }
      status
    },
    unzip = function(zipfile, exdir) {
      if (archive_has_no_data_file) {
        return(file.path(exdir, "readme.pdf"))
      }
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      year <- as.integer(sub(".*([0-9]{4})\\.zip$", "\\1", state$last_url))
      rows <- if (is.null(csv_for)) default_cot_rows() else csv_for(year)
      path <- file.path(exdir, "data.txt")
      data.table::fwrite(rows, path)
      path
    },
    .package = "forexdatasets",
    .env = .env
  )
  state
}

cache_file <- function(config, report_code, year) {
  file.path(config$cot_report$cache_dir, paste0(report_code, "_", year, ".rds"))
}

# ---------------------------------------------------------------------------
# Download problems
# ---------------------------------------------------------------------------

test_that("cot_pipeline returns an empty table when the download fails and nothing is cached", {
  config <- make_cot_pipeline_config()
  mock_cftc(status_for = function(url) 1L)

  output <- cot_pipeline(config)

  expect_s3_class(output$deacot, "data.table")
  expect_equal(nrow(output$deacot), 0)
})

test_that("cot_pipeline falls back to the cached copy when the download fails", {
  config <- make_cot_pipeline_config()
  dir.create(config$cot_report$cache_dir, recursive = TRUE)
  cached <- data.table(
    Market_and_Exchange_Names = "GOLD (XAU)",
    Report_Date_as_YYYY_MM_DD = as.Date("2023-01-03"),
    Source_Year = current_year
  )
  saveRDS(cached, cache_file(config, "deacot", current_year))
  mock_cftc(status_for = function(url) 1L)

  output <- cot_pipeline(config)

  expect_equal(nrow(output$deacot), 1)
  expect_equal(output$deacot$Market_and_Exchange_Names, "GOLD (XAU)")
})

test_that("cot_pipeline survives a cached file that cannot be read", {
  config <- make_cot_pipeline_config()
  dir.create(config$cot_report$cache_dir, recursive = TRUE)
  writeLines(
    "this is not an rds file",
    cache_file(config, "deacot", current_year)
  )
  mock_cftc(status_for = function(url) 1L)

  output <- cot_pipeline(config)

  expect_equal(nrow(output$deacot), 0)
})

test_that("cot_pipeline survives an archive that has no .txt or .csv file", {
  config <- make_cot_pipeline_config()
  mock_cftc(archive_has_no_data_file = TRUE)

  output <- cot_pipeline(config)

  expect_equal(nrow(output$deacot), 0)
})

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------

test_that("cot_pipeline builds the URL from the report code for non-deacot reports", {
  config <- make_cot_pipeline_config(report_names = list(tff = "fut_fin_txt"))
  state <- mock_cftc()

  output <- cot_pipeline(config)

  expect_true(any(grepl("fut_fin_txt_", state$urls, fixed = TRUE)))
  expect_gt(nrow(output$tff), 0)
})

test_that("cot_pipeline honours a start-year override for one report", {
  config <- make_cot_pipeline_config(start_year = current_year - 5L)
  config$cot_report$start_year_overrides <- list(deacot = current_year)
  state <- mock_cftc()

  cot_pipeline(config)

  expect_length(state$urls, 1) # only this year, not six years
})

# ---------------------------------------------------------------------------
# Several years, and the previous-year refresh
# ---------------------------------------------------------------------------

test_that("cot_pipeline still returns data when a column changes type between years", {
  config <- make_cot_pipeline_config(start_year = current_year - 1L)
  mock_cftc(csv_for = function(year) {
    rows <- default_cot_rows()
    if (year == current_year) {
      rows[, Noncommercial_Positions_Long_All := "n/a"]
    }
    rows
  })

  output <- suppressWarnings(cot_pipeline(config))

  expect_equal(nrow(output$deacot), 2)
})

test_that("cot_pipeline records the year after a successful run", {
  config <- make_cot_pipeline_config(start_year = current_year - 1L)
  mock_cftc()

  cot_pipeline(config)

  meta <- file.path(config$cot_report$cache_dir, ".last_run_year")
  expect_true(file.exists(meta))
  expect_equal(readLines(meta, warn = FALSE), as.character(current_year))
})

test_that("cot_pipeline survives a cache directory that cannot record the run year", {
  config <- make_cot_pipeline_config()
  mock_cftc()

  testthat::local_mocked_bindings(
    writeLines = function(text, con, ...) {
      if (is.character(con) && grepl("\\.last_run_year$", con)) {
        stop("simulated unwritable metadata path")
      }
      base::writeLines(text, con, ...)
    },
    .package = "base"
  )

  output <- cot_pipeline(config)

  expect_gt(nrow(output$deacot), 0)
})

test_that("cot_pipeline does not record the year when the previous-year refresh fails", {
  config <- make_cot_pipeline_config(start_year = current_year - 1L)
  mock_cftc(status_for = function(url) {
    if (grepl(as.character(current_year - 1L), url, fixed = TRUE)) 1L else 0L
  })

  output <- cot_pipeline(config)

  expect_equal(nrow(output$deacot), 1) # this year's data only
  expect_false(file.exists(file.path(
    config$cot_report$cache_dir,
    ".last_run_year"
  )))
})

# Newly added tests
test_that("clean_cot_dataset cleans columns, handles patterns, and preserves data.table reference", {
  dt <- data.table(
    `Market and Exchange Names` = c("GOLD - COMMODITY EXCHANGE INC."),
    `Report_Date_as_MM_DD_YYYY` = c("2020-01-01"),
    `Concentration_Gross_LE_4` = c(10),
    `Unused_Col_Old` = c(1),
    `Pct_Open_Interest` = c(50.5),
    `Col (With Parens)` = c(100),
    `Col-With-Hyphen` = c(200)
  )

  result <- clean_cot_dataset(dt, drop_pattern = "old")

  expect_s3_class(result, "data.table")
  expect_false("Unused_Col_Old" %in% names(result))
  expect_true("Market_and_Exchange_Names" %in% names(result))
  expect_true("Report_Date_as_YYYY_MM_DD" %in% names(result))
  expect_true("Pct_Open_Interest" %in% names(result))
  expect_true("Col_With_Parens" %in% names(result))
  expect_true("Col_With_Hyphen" %in% names(result))
})

test_that("standardise_market_names correctly maps market names", {
  dt <- data.table(
    Market_and_Exchange_Names = c(
      "GOLD - COMMODITY EXCHANGE INC.",
      "UNKNOWN MARKET - EXCHANGE"
    ),
    Value = c(100, 200)
  )

  name_map <- c(
    "GOLD - COMMODITY EXCHANGE INC." = "GOLD (XAU)"
  )

  result <- standardise_market_names(dt, name_map)

  expect_equal(
    result$Market_and_Exchange_Names,
    c("GOLD (XAU)", "UNKNOWN MARKET - EXCHANGE")
  )
})

test_that("cot_pipeline processes data correctly with mocked downloads and caches", {
  config <- make_valid_config()
  cache_dir <- tempfile("cot_cache")
  config$cot_report <- list(
    cot_regex = "old",
    cache_dir = cache_dir,
    start_year = 2023,
    report_names = list(
      deacot = "deacot"
    )
  )
  config$web <- list(
    cftc = list(
      url = "https://example.com/cftc/"
    )
  )

  # Create mock CSV using Report_Date_as_MM_DD_YYYY with underscores
  mock_dir <- tempdir()
  mock_csv_filename <- "deacot_2023.txt"
  mock_csv_path <- file.path(mock_dir, mock_csv_filename)

  mock_dt <- data.table(
    `Market_and_Exchange_Names` = c("GOLD - COMMODITY EXCHANGE INC."),
    `Report_Date_as_MM_DD_YYYY` = c("2023-01-03"),
    `Noncommercial_Positions_Long_All` = c(15000),
    `Unused_Col_Old` = c(1)
  )
  data.table::fwrite(mock_dt, mock_csv_path)

  mock_zip_path <- tempfile(fileext = ".zip")

  # Zip using relative paths to avoid absolute path warnings
  old_wd <- setwd(mock_dir)
  zip::zip(mock_zip_path, files = mock_csv_filename)
  setwd(old_wd)

  local_mocked_bindings(
    download.file = function(url, destfile, quiet, mode) {
      file.copy(mock_zip_path, destfile, overwrite = TRUE)
      return(0L)
    },
    .package = "forexdatasets"
  )

  output <- cot_pipeline(config)

  expect_type(output, "list")
  expect_true("deacot" %in% names(output))

  res_dt <- output$deacot
  expect_s3_class(res_dt, "data.table")
  expect_gt(nrow(res_dt), 0)
  expect_true("GOLD (XAU)" %in% res_dt$Market_and_Exchange_Names)

  output_cached <- cot_pipeline(config)
  expect_s3_class(output_cached$deacot, "data.table")
  expect_equal(nrow(output_cached$deacot), nrow(res_dt))
})
