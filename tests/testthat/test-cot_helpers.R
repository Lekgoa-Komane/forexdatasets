# tests/testthat/test-cot_helpers.R
# Tests for clean_cot_dataset() and standardise_market_names().

library(data.table)

# ---------------------------------------------------------------------------
# clean_cot_dataset
# ---------------------------------------------------------------------------

test_that("clean_cot_dataset tidies the column names", {
  dt <- data.table(
    1, 2, 3, 4, 5
  )
  setnames(dt, c(
    "Market and Exchange Names",
    "Open Interest (All)",
    "% of OI",
    "Report_Date_as_MM_DD_YYYY",
    "Long-Short"
  ))

  result <- clean_cot_dataset(dt, drop_pattern = "^$never_matches$")

  expect_equal(names(result), c(
    "Market_and_Exchange_Names",
    "Open_Interest",
    "Pct_of_OI",
    "Report_Date_as_YYYY_MM_DD",
    "Long_Short"
  ))
})

test_that("clean_cot_dataset drops the columns that match the drop pattern, ignoring case", {
  dt <- data.table(a = 1, b = 2, c = 3)
  setnames(dt, c("Conc_Net_Long", "Open Interest", "CONC_Short"))

  result <- clean_cot_dataset(dt, drop_pattern = "conc")

  expect_equal(names(result), "Open_Interest")
})

test_that("clean_cot_dataset makes names unique when two columns collapse to one", {
  dt <- data.table(a = 1, b = 2)
  setnames(dt, c("A B", "A_B"))

  result <- clean_cot_dataset(dt, drop_pattern = "^$never_matches$")

  expect_equal(names(result), c("A_B", "A_B.1"))
})

test_that("clean_cot_dataset changes the table in place", {
  dt <- data.table(a = 1)
  setnames(dt, "Open Interest")

  clean_cot_dataset(dt, drop_pattern = "^$never_matches$")

  expect_equal(names(dt), "Open_Interest")
})

# ---------------------------------------------------------------------------
# standardise_market_names
# ---------------------------------------------------------------------------

test_that("standardise_market_names maps known names and leaves the rest alone", {
  dt <- data.table(
    Market_and_Exchange_Names = c(
      "EURO FX - CHICAGO MERCANTILE EXCHANGE",
      "  GOLD - COMMODITY EXCHANGE INC.  ",
      "SOMETHING ELSE"
    ),
    x = 1:3
  )
  name_map <- c(
    "EURO FX - CHICAGO MERCANTILE EXCHANGE" = "EURO FX (EUR)",
    "GOLD - COMMODITY EXCHANGE INC." = "GOLD (XAU)"
  )

  result <- standardise_market_names(dt, name_map)

  expect_equal(
    result$Market_and_Exchange_Names,
    c("EURO FX (EUR)", "GOLD (XAU)", "SOMETHING ELSE")
  )
})

test_that("standardise_market_names does nothing when there is nothing to do", {
  dt <- data.table(Market_and_Exchange_Names = "EURO FX - CHICAGO MERCANTILE EXCHANGE")
  name_map <- c("EURO FX - CHICAGO MERCANTILE EXCHANGE" = "EURO FX (EUR)")

  # No map
  expect_equal(
    standardise_market_names(data.table::copy(dt), NULL)$Market_and_Exchange_Names,
    "EURO FX - CHICAGO MERCANTILE EXCHANGE"
  )

  # Empty table
  empty <- dt[0]
  expect_equal(nrow(standardise_market_names(empty, name_map)), 0)

  # No market column
  other <- data.table(x = 1)
  expect_equal(standardise_market_names(other, name_map), data.table(x = 1))
})
