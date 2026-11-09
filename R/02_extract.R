#' Extract hourly data from a local source
#'
#' Reads a single hourly data file for one ticker, renames columns using the
#' configuration mapping, adds a UTC timestamp column, and returns a clean
#' data.table. Designed to be called in parallel over many ticker/file pairs.
#'
#' @param ticker_row A single-row \code{data.table} containing at least the
#'   columns \code{asset_class} and \code{ticker}. The function expects
#'   exactly one row.
#' @param source_paths A character vector of file paths. For hourly data this
#'   is typically a single file path.
#' @param config A validated configuration list, as returned by
#'   \code{\link{load_config}}. Must include \code{data_format$local_hourly}
#'   (vector of column names to read) and
#'   \code{data_format$column_mapping$hourly} (named list for renaming).
#'
#' @return A \code{data.table} with columns:
#'   \code{date_time} (POSIXct, UTC), \code{open}, \code{high}, \code{low},
#'   \code{close}, \code{timeframe}, \code{ticker}, \code{asset_class}.
#'   Returns \code{NULL} if the file cannot be read or processing fails.
#'
#' @import data.table
#' @importFrom logger log_debug log_info log_error
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' meta <- ticker_metadata(config)
#' row <- meta[1]
#' hourly_data <- extract_hourly_data(row, row$source_path, config)
#' head(hourly_data)
#' }

# Hourly data extraction --------------------------------------------------

extract_hourly_data <- function(
  ticker_row,
  source_paths,
  config
) {
  if (nrow(ticker_row) != 1) {
    log_error(
      "extract_hourly_data expects 1 row, got {n}",
      n = nrow(ticker_row)
    )

    return(NULL)
  }

  ticker <- ticker_row$ticker
  file_path <- source_paths
  asset_class <- ticker_row$asset_class

  # Debug: Per-ticker start (only shows if DEBUG logging enabled)
  log_debug(
    "Loading hourly: {ticker} from {file}",
    ticker = ticker,
    file = basename(file_path)
  )

  hourly_data <- tryCatch(
    {
      # Read all files + add file_id in bulk
      dt <- fread(
        file_path,
        header = TRUE,
        select = config$data_format$local_hourly
      )

      # Validate columns exist
      expected_cols <- config$data_format$local_hourly
      missing_cols <- setdiff(
        expected_cols,
        names(dt)
      )

      if (length(missing_cols) > 0) {
        stop(sprintf(
          "Missing columns: %s",
          paste(
            missing_cols,
            collapse = ", "
          )
        ))
      }

      # Rename columns using mapping
      setnames(
        dt,
        old = names(
          config$data_format$column_mapping$hourly
        ),
        new = unname(unlist(
          config$data_format$column_mapping$hourly
        ))
      )

      # Clean up
      dt[, `:=`(
        date_time = as.POSIXct(
          sprintf("%s%06d", date, time),
          format = "%Y%m%d%H%M%S",
          tz = "UTC"
        ),
        timeframe = config$paths$frequency_folders$h1,
        ticker = ticker,
        asset_class = asset_class
      )]

      dt <- dt[, .(
        date_time,
        open,
        high,
        low,
        close,
        timeframe,
        ticker,
        asset_class
      )]

      log_info(
        "Loaded {n} rows for {ticker}",
        n = nrow(dt),
        ticker = ticker
      )

      return(dt)
    },
    error = function(e) {
      log_error(
        "Failed to load {ticker} from {path}: {error}",
        ticker == ticker,
        path = basename(file_path),
        error = e$message
      )

      return(NULL)
    }
  )

  return(hourly_data)
}


# =============================================================================
# LOAD DAILY DATA (LOCAL + WEB VIA SOURCE PATH)
# =============================================================================

#' Extract daily data from local or web source
#'
#' Routes data extraction to the appropriate handler based on the
#' `data_source` field in the input row. Local sources (`local`,
#' `local_stooq`) use `extract_local_daily()`; web sources (`investing`,
#' `stooq`) use `web_scraper()`.
#'
#' @param ticker_row A single-row `data.table` with at least columns
#'   `asset_class`, `ticker`, and `data_source`.
#' @param sources A character vector of file paths or URLs for the ticker.
#' @param config A validated configuration list from `load_config()`.
#'
#' @return A `data.table` of daily data, or `NULL` if the source is unknown
#'   or extraction fails.
#'
#' @import data.table
#' @importFrom logger log_warn log_error
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' row <- ticker_metadata[1]
#' daily_data <- extract_daily_data(row, row$source_path, config)
#' head(daily_data)
#' }

# Daily data extraction ---------------------------------------------------

extract_daily_data <- function(
  ticker_row,
  sources,
  config
) {
  # Validate input
  if (nrow(ticker_row) == 0) {
    log_warn(
      "Input ticker_metadat is empty. Returning an empty data.table."
    )
    return(data.table())
  }

  if (!is.data.table(ticker_row)) {
    stop(
      "daily_table must be a data.table"
    )
  }

  if (!is.vector(sources)) {
    stop("sources must be a vector")
  }

  # Extract metadata
  asset_class <- ticker_row$asset_class
  ticker <- ticker_row$ticker
  data_source <- ticker_row$data_source
  source_path <- sources

  # Route to appropriate extractor based on data_source
  if (data_source %in% c("local", "local_stooq")) {
    return(extract_local_daily(
      ticker,
      asset_class,
      source_path,
      config
    ))
  } else if (data_source %in% c("investing", "stooq")) {
    return(web_scraper(
      ticker,
      asset_class,
      data_source,
      source_path,
      config
    ))
  } else {
    log_error(
      "Unknown data_source '{src}' for {ticker}",
      src = data_source,
      ticker = ticker
    )
    return(NULL)
  }
}


#' Extract daily data from a local file
#'
#' Reads a local CSV file for a single ticker, validates columns, renames
#' them according to the configuration mapping, adds metadata columns, and
#' returns a clean data.table. Handles futures and non futures formats.
#'
#' @param ticker Character string. Ticker symbol.
#' @param asset_class Character string. Asset class (e.g., `"Currency_Pairs"`,
#'   `"Commodity_Futures"`).
#' @param file_path Character string. Path to the local data file.
#' @param config A validated configuration list.
#'
#' @return A `data.table` with columns: `trade_date` (Date), `open`, `high`,
#'   `low`, `close`, `volume`, `open_interest`, `ticker`, `asset_class`.
#'   Returns `NULL` on error.
#'
#' @import data.table
#' @importFrom logger log_debug log_info log_error
#' @noRd

extract_local_daily <- function(
  ticker,
  asset_class,
  file_path,
  config
) {
  log_debug(
    "Loading daily (local): {ticker} from {file}",
    ticker = ticker,
    file = basename(file_path)
  )

  tryCatch(
    {
      if (grepl("Futures", asset_class)) {
        dt <- fread(
          file_path,
          header = TRUE,
          select = config$data_format$stooq_daily
        )

        # Validate columns
        expected_cols <- config$data_format$stooq_daily
        missing_cols <- setdiff(
          expected_cols,
          names(dt)
        )

        if (length(missing_cols) > 0) {
          stop(sprintf(
            "Missing columns: %s",
            paste(
              missing_cols,
              collapse = ", "
            )
          ))
        }

        # Rename columns
        setnames(
          dt,
          old = names(
            config$data_format$column_mapping$stooq
          ),
          new = unname(unlist(
            config$data_format$column_mapping$stooq
          ))
        )

        # Add metadata
        dt[, `:=`(
          date_time = as.POSIXct(
            date_time,
            format = "%d %b %Y",
            tz = "UTC"
          ),
          volume = as.integer(gsub(
            ",",
            "",
            volume
          )),
          open_interest = as.integer(gsub(
            ",",
            "",
            open_interest
          )),
          timeframe = config$paths$frequency_folders$daily,
          ticker = ticker,
          asset_class = asset_class
        )]

        setorder(dt, date_time)

        dt <- dt[date_time >= as.POSIXct("1985-01-01", tz = "UTC")]

        log_info(
          "Loaded {ticker} (local): {n} rows",
          ticker = ticker,
          n = format(
            nrow(dt),
            big.mark = ","
          )
        )

        return(dt)
      } else {
        # Read file
        dt <- fread(
          file_path,
          header = TRUE,
          select = config$data_format$local_daily
        )

        # Validate columns
        expected_cols <- config$data_format$local_daily
        missing_cols <- setdiff(
          expected_cols,
          names(dt)
        )

        if (length(missing_cols) > 0) {
          stop(sprintf(
            "Missing columns: %s",
            paste(
              missing_cols,
              collapse = ", "
            )
          ))
        }

        # Rename columns
        setnames(
          dt,
          old = names(
            config$data_format$column_mapping$daily
          ),
          new = unname(unlist(
            config$data_format$column_mapping$daily
          ))
        )

        # Add metadata
        dt[, `:=`(
          date_time = as.POSIXct(
            sprintf("%s%06d", date, time),
            format = "%Y%m%d%H%M%S",
            tz = "UTC"
          ),
          volume = as.integer(numeric()),
          open_interest = as.integer(numeric()),
          timeframe = config$paths$frequency_folders$daily,
          ticker = ticker,
          asset_class = asset_class
        )]

        setorder(dt, date_time)

        dt <- dt[date_time >= as.POSIXct("1985-01-01", tz = "UTC")]

        dt <- dt[, .(
        date_time,
        open,
        high,
        low,
        close,
        timeframe, 
        volume,
        open_interest,
        ticker,
        asset_class
      )]

      log_info(
          "Loaded {ticker} (local): {n} rows",
          ticker = ticker,
          n = format(
            nrow(dt),
            big.mark = ","
          )
        )

        return(dt)
      }
    },
    error = function(e) {
      log_error(
        "Failed to load {ticker} (local) from {file}: {error}",
        ticker = ticker,
        file = basename(file_path),
        error = e$message
      )
      return(NULL)
    }
  )
}


#' Scrape daily data from a web source
#'
#' Uses a headless Chromote session with a polite user agent to retrieve
#' daily data from stooq or investing.com. Respects robots.txt, extracts the
#' HTML table, and saves the raw data to a local CSV file for future use.
#'
#' @param ticker Character string. Ticker symbol.
#' @param asset_class Character string. Asset class (e.g., `"Commodity_Futures"`).
#' @param data_source Character string. Either `"stooq"` or `"investing"`.
#' @param url_path Character string. The full URL to scrape.
#' @param config A validated configuration list.
#'
#' @return A `data.table` with the scraped data, or `NULL` if the scrape
#'   fails or is disallowed by robots.txt.
#'
#' @import data.table
#' @importFrom logger log_info log_error
#' @importFrom polite bow
#' @importFrom robotstxt paths_allowed
#' @importFrom rvest read_html html_element html_table
#' @importFrom chromote ChromoteSession
#' @noRd

web_scraper <- function(ticker, asset_class, data_source, url_path, config) {
  log_info(
    "Loading daily (web): {ticker} from {url}",
    ticker = ticker,
    url = url_path
  )

  url_name <- switch(
    data_source,
    "stooq" = names(config$web)[1],
    "investing" = names(config$web)[3]
  )

  css <- switch(
    data_source,
    "stooq" = config$web[[url_name]]$table_id,
    "investing" = config$web[[url_name]]$table_id
  )

  # 1. Prepare File Path
  file_path <- file.path(
    config$paths$local_base,
    config$directory_map$Commodity_Futures$daily[[url_name]],
    paste0(ticker, config$data_format$fut_extension)
  )

  # ---------- 1. Use polite session for robots.txt ----------
  session <- polite::bow(
    url = config$web[[url_name]]$base_url,
    user_agent = config$web$user_agent[1], # e.g. "Googlebot/2.1"
    force = FALSE,
    delay = 10
  )

  # Check if the path is allowed by robots.txt
  log_info(
    "Checking robots.txt for {url} with user-agent '{ua}'",
    url = session$url,
    ua = session$user_agent
  )

  if (
    !robotstxt::paths_allowed(
      paths = "/",
      domain = session$url,
      bot = session$user_agent,
      user_agent = session$user_agent
    )
  ) {
    log_error("Path disallowed by robots.txt: {path}", path = session$url)
    return(NULL)
  } else {
    log_info("Path allowed by robots.txt: {path}", path = session$url)
  }

  # ---------- 2. Use Chromote with the same user-agent ----------
  b <- NULL
  tryCatch(
    {
      b <- ChromoteSession$new()

      # Apply the same user-agent from the polite session
      b$Emulation$setUserAgentOverride(userAgent = session$user_agent)
      # Stealth tweaks (optional but recommended)
      b$Emulation$setDeviceMetricsOverride(
        width = 1920,
        height = 1080,
        deviceScaleFactor = 1,
        mobile = FALSE
      )
      b$Page$addScriptToEvaluateOnNewDocument(
        source = paste(
          "Object.defineProperty(navigator,",
          "'webdriver',",
          "{get: () => undefined});"
        )
      )

      # Navigate to the daily page
      b$Page$navigate(url_path)

      # Wait for the data table to appear
      log_info(paste0("Waiting for table ", css, " to load..."))
      timeout <- 15
      start <- Sys.time()
      table_ready <- FALSE
      while (difftime(Sys.time(), start, units = "secs") < timeout) {
        Sys.sleep(0.5)
        ready <- b$Runtime$evaluate(
          paste0("document.querySelector('", css, " tbody tr') !== null")
        )$result$value
        if (isTRUE(ready)) {
          table_ready <- TRUE
          break
        }
      }

      if (!table_ready) {
        stop("Table ", css, " did not load within ", timeout, " seconds")
      }

      # Extract the table HTML fragment
      table_html <- b$Runtime$evaluate(
        paste0("document.querySelector('", css, "').outerHTML")
      )$result$value

      b$close()
      b <- NULL

      # ---------- 3. Parse exactly as before ----------
      raw_data <- data.table::setDT(
        rvest::read_html(table_html) |>
          rvest::html_element(xpath = config$web[[url_name]]$x_path) |>
          rvest::html_table(header = TRUE)
      )

      if (nrow(raw_data) == 0) {
        stop("HTML table is empty")
      }

      data.table::fwrite(raw_data, file_path, nThread = 4)

      log_info(
        "Loaded {ticker} (web): {n} rows",
        ticker = ticker,
        n = nrow(raw_data)
      )
      return(raw_data)
    },
    error = function(e) {
      log_error(
        "Failed to scrape {ticker}: {error}",
        ticker = ticker,
        error = e$message
      )
      return(NULL)
    },
    finally = {
      if (!is.null(b)) try(b$close(), silent = TRUE)
    }
  )
}

################################################################################
