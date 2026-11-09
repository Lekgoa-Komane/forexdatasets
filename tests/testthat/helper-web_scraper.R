# tests/testthat/helper-web_scraper.R
# Fakes for the three outside things web_scraper() talks to:
#   1. polite::bow()             (robots.txt session)
#   2. robotstxt::paths_allowed() (robots.txt answer)
#   3. chromote::ChromoteSession  (a real Chrome browser)
# With these, a scraper test never starts Chrome and never touches the network.
# Each fake lives only for the length of the test that creates it.

default_scraped_table_html <- function() {
  paste0(
    '<table id="f13">',
    "<thead><tr><th>Date</th><th>Close</th></tr></thead>",
    "<tbody>",
    "<tr><td>2024-01-02</td><td>100</td></tr>",
    "<tr><td>2024-01-03</td><td>101</td></tr>",
    "</tbody>",
    "</table>"
  )
}

# Robots.txt guards: allowed = TRUE lets the scraper through.
mock_scraper_guards <- function(allowed = TRUE, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    bow = function(url, user_agent, force, delay) {
      list(url = url, user_agent = user_agent)
    },
    .package = "polite",
    .env = .env
  )
  testthat::local_mocked_bindings(
    paths_allowed = function(paths, domain, bot, user_agent) allowed,
    .package = "robotstxt",
    .env = .env
  )
  invisible(NULL)
}

# A fake Chrome. Returns an environment that records what happened:
#   state$closed    how many times the browser was closed
#   state$navigated the pages it was sent to
fake_chromote <- function(
  table_ready = TRUE,
  table_html = default_scraped_table_html(),
  start_fails = FALSE,
  .env = parent.frame()
) {
  state <- new.env()
  state$closed <- 0L
  state$navigated <- character()

  browser <- list(
    Emulation = list(
      setUserAgentOverride = function(...) NULL,
      setDeviceMetricsOverride = function(...) NULL
    ),
    Page = list(
      addScriptToEvaluateOnNewDocument = function(...) NULL,
      navigate = function(url) {
        state$navigated <- c(state$navigated, url)
        NULL
      }
    ),
    Runtime = list(
      evaluate = function(expression) {
        if (grepl("tbody tr", expression, fixed = TRUE)) {
          list(result = list(value = table_ready))
        } else {
          list(result = list(value = table_html))
        }
      }
    ),
    close = function() {
      state$closed <- state$closed + 1L
      invisible(NULL)
    }
  )

  generator <- list(
    new = function() {
      if (start_fails) {
        stop("Chrome is not available")
      }
      browser
    }
  )

  testthat::local_mocked_bindings(
    ChromoteSession = generator,
    .package = "forexdatasets",
    .env = .env
  )
  state
}
