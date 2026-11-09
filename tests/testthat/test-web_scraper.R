# tests/testthat/test-web_scraper.R
# web_scraper(), fully isolated: no Chrome, no network, no real robots.txt.
# Uses mock_scraper_guards() and fake_chromote() from helper-web_scraper.R.
#
# The two settings set below (scrape_timeout, poll_interval) keep these tests
# fast. They only take effect once web_scraper() reads them (see the small code
# change suggested with this file). Without the change the tests still pass,
# but the "table never appears" test waits the full 15 seconds.

library(data.table)

scraper_config <- function(folder = "stooq") {
  config <- make_valid_config()
  config$web$scrape_timeout <- 0.3
  config$web$poll_interval <- 0.01
  if (!is.null(folder)) {
    dir.create(
      file.path(config$paths$local_base, folder),
      recursive = TRUE, showWarnings = FALSE
    )
  }
  config
}

run_scraper <- function(config, data_source = "stooq", ticker = "GC.F") {
  web_scraper(
    ticker = ticker,
    asset_class = "Commodity_Futures",
    data_source = data_source,
    url_path = "https://example.com/data",
    config = config
  )
}

test_that("web_scraper returns the scraped table, saves it, and closes the browser", {
  config <- scraper_config()
  mock_scraper_guards(allowed = TRUE)
  state <- fake_chromote()

  result <- run_scraper(config)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("Date", "Close"))

  saved <- file.path(config$paths$local_base, "stooq", "GC.F.csv")
  expect_true(file.exists(saved))
  expect_equal(nrow(data.table::fread(saved)), 2)

  expect_equal(state$navigated, "https://example.com/data")
  expect_equal(state$closed, 1L)
})

test_that("web_scraper works for the investing source too", {
  config <- scraper_config(folder = "investing")
  # web_scraper picks the 1st (stooq) and 3rd (investing) settings by POSITION,
  # so put a cftc entry in 2nd place, as your real config has.
  config$web <- append(
    config$web,
    list(cftc = list(url = "https://example.com/cftc/")),
    after = 1
  )
  mock_scraper_guards(allowed = TRUE)
  fake_chromote()

  result <- run_scraper(config, data_source = "investing", ticker = "gold")

  expect_equal(nrow(result), 2)
  expect_true(file.exists(
    file.path(config$paths$local_base, "investing", "gold.csv")
  ))
})

test_that("web_scraper does not start the browser when robots.txt disallows the site", {
  config <- scraper_config()
  mock_scraper_guards(allowed = FALSE)
  state <- fake_chromote()

  result <- run_scraper(config)

  expect_null(result)
  expect_length(state$navigated, 0)
  expect_equal(state$closed, 0L)
})

test_that("web_scraper gives up and closes the browser when the table never appears", {
  config <- scraper_config()
  mock_scraper_guards(allowed = TRUE)
  state <- fake_chromote(table_ready = FALSE)

  result <- run_scraper(config)

  expect_null(result)
  expect_equal(state$closed, 1L)
})

test_that("web_scraper returns NULL for a table that has no rows", {
  config <- scraper_config()
  mock_scraper_guards(allowed = TRUE)
  empty_table <- paste0(
    '<table id="f13">',
    "<thead><tr><th>Date</th><th>Close</th></tr></thead>",
    "<tbody></tbody></table>"
  )
  state <- fake_chromote(table_html = empty_table)

  result <- run_scraper(config)

  expect_null(result)
  expect_equal(state$closed, 1L)
})

test_that("web_scraper returns NULL when the file cannot be saved", {
  config <- scraper_config(folder = NULL) # the target folder does not exist
  mock_scraper_guards(allowed = TRUE)
  state <- fake_chromote()

  result <- run_scraper(config)

  expect_null(result)
  expect_equal(state$closed, 1L)
})

test_that("web_scraper returns NULL when the browser cannot start", {
  config <- scraper_config()
  mock_scraper_guards(allowed = TRUE)
  state <- fake_chromote(start_fails = TRUE)

  result <- run_scraper(config)

  expect_null(result)
  expect_equal(state$closed, 0L)
})
