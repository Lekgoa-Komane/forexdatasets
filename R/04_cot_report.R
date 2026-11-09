# ------------------------------------------------------------------------------
# Helper Function: Clean Columns
# ------------------------------------------------------------------------------

#' Clean COT Dataset Column Names
#'
#' Standardises the column names of a raw CFTC Commitments of Traders (COT)
#' dataset. Columns whose names match `drop_pattern` are dropped, and the
#' remaining names are normalised by replacing `%` with `Pct`, removing
#' parentheses, converting hyphens and whitespace to underscores, and
#' collapsing repeated underscores.
#'
#' @param dt A `data.table` containing raw COT data as returned by
#'   [data.table::fread()]. The object is modified by reference.
#' @param drop_pattern A case-insensitive regex pattern. Columns whose names
#'   match are dropped before renaming.
#'
#' @return The same `data.table`, with cleaned and unique column names.
#'   The input object is modified in place.
#'
#' @details
#' The `drop_pattern` is intentionally permissive (case-insensitive substring
#' match) because the three COT report types use slightly different column
#' naming conventions (for example `conc` versus `concentration`). No raw
#' column in any of the current report types is literally named `Gold`, so
#' tokens such as `old` are safe to match as substrings.
#'
#' @keywords internal

# Cot_helper --------------------------------------------------------------

clean_cot_dataset <- function(dt, drop_pattern) {
  # Intentionally permissive: report types differ in column naming
  # (e.g. `conc` vs `concentration`), and no raw column named `Gold`
  # exists, so substring matching is safe here.
  cols_to_drop <- names(dt)[grepl(drop_pattern, names(dt), ignore.case = TRUE)]
  if (length(cols_to_drop) > 0) {
    dt[, (cols_to_drop) := NULL]
  }

  new_names <- names(dt)
  # print(new_names)
  new_names <- gsub(
    "Report_Date_as_MM_DD_YYYY",
    "As of Date in Form_YYYY_MM_DD",
    new_names
  )
  new_names <- gsub(
    "As of Date in Form",
    "Report_Date_as",
    new_names,
    fixed = TRUE
  )
  new_names <- gsub("_All|All", "", new_names)
  new_names <- gsub("%", "Pct", new_names, fixed = TRUE)
  new_names <- gsub("[()]", "", new_names)
  new_names <- gsub("-", "_", new_names, fixed = TRUE)
  new_names <- trimws(new_names)
  new_names <- gsub("\\s+", "_", new_names)
  new_names <- gsub("_+", "_", new_names)

  # Guard against two raw columns collapsing to the same cleaned name.
  if (anyDuplicated(new_names)) {
    new_names <- make.unique(new_names)
  }

  setnames(dt, new_names)
  return(dt)
}

# ------------------------------------------------------------------------------
# Market Name Standardisation
# ------------------------------------------------------------------------------
# Old and new CFTC files spell the same market in different ways (for example
# "NZ DOLLAR - ..." and "NEW ZEALAND DOLLAR - ..."). These lookup tables turn
# all of them into one short, clean name.
#
# The list names (traders_in_financial_futures_fut, disaggregated_fut,
# legacy_fut) must match either the KEY or the CODE used in
# config$cot_report$report_names. If yours are called something else, just
# rename the three list names below.
#
# Each entry reads:  "name as it appears in CFTC file" = "clean name"

cot_market_name_map <- list(
  fut_fin_txt = c(
    "BRITISH POUND - CHICAGO MERCANTILE EXCHANGE" = "BRITISH POUND (GBP)",
    "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE" = "BRITISH POUND (GBP)",
    "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE" = "JAPANESE YEN (JPY)",
    "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "NEW ZEALAND DOLLAR (NZD)",
    "U.S. DOLLAR INDEX - ICE FUTURES U.S." = "U.S. DOLLAR INDEX (USDX)",
    "NZ DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "NEW ZEALAND DOLLAR (NZD)",
    "USD INDEX - ICE FUTURES U.S." = "U.S. DOLLAR INDEX (USDX)",
    "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "AUSTRALIAN DOLLAR (AUD)",
    "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "CANADIAN DOLLAR (CAD)",
    "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE" = "SWISS FRANC (CHF)",
    "EURO FX - CHICAGO MERCANTILE EXCHANGE" = "EURO FX (EUR)"
  ),

  fut_disagg_txt = c(
    "GOLD - COMMODITY EXCHANGE INC." = "GOLD (XAU)",
    "PALLADIUM - NEW YORK MERCANTILE EXCHANGE" = "PALLADIUM (XPD)",
    "PLATINUM - NEW YORK MERCANTILE EXCHANGE" = "PLATINUM (XPT)",
    "SILVER - COMMODITY EXCHANGE INC." = "SILVER (XAG)"
  ),

  deacot = c(
    "AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "AUSTRALIAN DOLLAR (AUD)",
    "AUSTRALIAN DOLLARS - CHICAGO MERCANTILE EXCHANGE" = "AUSTRALIAN DOLLAR (AUD)",
    "AUSTRALIAN DOLLARS - INTERNATIONAL MONETARY MARKET" = "AUSTRALIAN DOLLAR (AUD)",
    "CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "CANADIAN DOLLAR (CAD)",
    "CANADIAN DOLLAR - INTERNATIONAL MONETARY MARKET" = "CANADIAN DOLLAR (CAD)",
    "SWISS FRANC - CHICAGO MERCANTILE EXCHANGE" = "SWISS FRANC (CHF)",
    "SWISS FRANC - INTERNATIONAL MONETARY MARKET" = "SWISS FRANC (CHF)",
    "EURO FX - CHICAGO MERCANTILE EXCHANGE" = "EURO FX (EUR)",
    "EURO FX - INTERNATIONAL MONETARY MARKET" = "EURO FX (EUR)",
    "BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE" = "BRITISH POUND (GBP)",
    "BRITISH POUND STERLING - INTERNATIONAL MONETARY MARKET" = "BRITISH POUND (GBP)",
    "BRITISH POUND - CHICAGO MERCANTILE EXCHANGE" = "BRITISH POUND (GBP)",
    "POUND STERLING - CHICAGO MERCANTILE EXCHANGE" = "BRITISH POUND (GBP)",
    "POUND STERLING - INTERNATIONAL MONETARY MARKET" = "BRITISH POUND (GBP)",
    "JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE" = "JAPANESE YEN (JPY)",
    "JAPANESE YEN - INTERNATIONAL MONETARY MARKET" = "JAPANESE YEN (JPY)",
    "NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "NEW ZEALAND DOLLAR (NZD)",
    "NEW ZEALAND DOLLARS - CHICAGO MERCANTILE EXCHANGE" = "NEW ZEALAND DOLLAR (NZD)",
    "NEW ZEALAND DOLLARS - INTERNATIONAL MONETARY MARKET" = "NEW ZEALAND DOLLAR (NZD)",
    "NZ DOLLAR - CHICAGO MERCANTILE EXCHANGE" = "NEW ZEALAND DOLLAR (NZD)",
    "U.S. DOLLAR INDEX - ICE FUTURES U.S." = "U.S. DOLLAR INDEX (USDX)",
    "U.S. DOLLAR INDEX - NEW YORK BOARD OF TRADE" = "U.S. DOLLAR INDEX (USDX)",
    "U.S. DOLLAR INDEX - NEW YORK COTTON EXCHANGE" = "U.S. DOLLAR INDEX (USDX)",
    "USD INDEX - ICE FUTURES U.S." = "U.S. DOLLAR INDEX (USDX)",
    "GOLD - COMMODITY EXCHANGE INC." = "GOLD (XAU)",
    "GOLD - INTERNATIONAL MONETARY MARKET" = "GOLD (XAU)",
    "PALLADIUM - NEW YORK MERCANTILE EXCHANGE" = "PALLADIUM (XPD)",
    "PLATINUM - NEW YORK MERCANTILE EXCHANGE" = "PLATINUM (XPT)",
    "SILVER - COMMODITY EXCHANGE INC." = "SILVER (XAG)"
  )
)

#' Standardise Market_and_Exchange_Names
#'
#' Replaces the values in `Market_and_Exchange_Names` using a named lookup
#' vector. Names that are not in the lookup are left unchanged. The input
#' is modified by reference.
#'
#' @param dt A `data.table` (modified by reference).
#' @param name_map Named character vector: old name = new name. `NULL` means
#'   "do nothing".
#' @return The same `data.table` (returned visibly), with the column updated
#'   in place.
#' @keywords internal
standardise_market_names <- function(dt, name_map) {
  col <- "Market_and_Exchange_Names"
  if (is.null(name_map) || nrow(dt) == 0 || !col %in% names(dt)) {
    return(dt)
  }

  old_names <- as.character(dt[[col]])
  new_names <- unname(name_map[trimws(old_names)]) # NA when not in the lookup
  data.table::set(
    dt,
    j = col,
    value = fifelse(is.na(new_names), old_names, new_names)
  )
  dt
}

# ------------------------------------------------------------------------------
# Single-Argument Master Function
# ------------------------------------------------------------------------------

#' Download and Assemble CFTC Commitments of Traders Reports
#'
#' Downloads, extracts, cleans, and caches CFTC Commitments of Traders (COT)
#' reports for a configurable set of report types and years, returning them
#' as a named list of `data.table`s.
#'
#' @param config A validated configuration list. Must contain:
#'   \describe{
#'     \item{`web$cftc$url`}{Base URL to the CFTC historical archive. Must
#'       end with a single `/`.}
#'     \item{`cot_report$cache_dir`}{Directory in which downloaded and parsed
#'       reports are cached as `.rds` files. Created if missing.}
#'     \item{`cot_report$start_year`}{First year to include in the pipeline.}
#'     \item{`cot_report$report_names`}{Named list mapping report keys
#'       (e.g. `legacy`, `disaggregated`, `tff`) to their CFTC filename
#'       prefixes.}
#'   }
#'
#' @return A named list of `data.table`s, one per entry in
#'   `config$cot_report$report_names`. The names of the list match the report
#'   keys. If no year could be retrieved for a given report type, an empty
#'   `data.table` is returned for that key.
#'
#' @details
#' For each report type, the pipeline iterates over every year from
#' `start_year` to the current calendar year. Historical years are loaded
#' from the cache when a matching `.rds` file exists; the current year is
#' always re-downloaded.
#'
#' A small metadata file (`.last_run_year`) is written inside the cache
#' directory after a successful run. When the calendar year changes between
#' runs, the pipeline forces a refresh of the previous year's cache, since
#' that year may have been cached before the final CFTC release. The
#' metadata is only advanced if the forced refresh succeeded for every
#' report type.
#'
#' Network or parsing failures are caught per report/year. If a cached
#' version of the failing report/year exists, it is returned instead; if not,
#' `NULL` is returned for that year and excluded from the final binding.
#' A warning is logged if column types differ across years within the same
#' report type.
#'
#' @importFrom data.table data.table rbindlist fread setnames set fifelse
#' @importFrom logger log_info log_warn log_error log_success
#' @importFrom fs path file_exists
#' @importFrom utils download.file unzip
#' @export

# COT REPORT ---------------------------------------------------------------

cot_pipeline <- function(config) {
  # Step 1: Dynamically derive years and paths directly from config
  drop_pattern <- config$cot_report$cot_regex
  cftc_url <- config$web$cftc$url
  cache_dir <- config$cot_report$cache_dir
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # Default start year and per-report overrides from config
  default_start_year <- config$cot_report$start_year
  start_year_overrides <- config$cot_report$start_year_overrides %||% list()

  # Helper: return the effective start year for a given report code
  get_start_year <- function(report_code) {
    if (!is.null(start_year_overrides[[report_code]])) {
      as.integer(start_year_overrides[[report_code]])
    } else {
      as.integer(default_start_year)
    }
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Step 1b: Determine whether the previous year needs a forced refresh
  meta_file <- fs::path(cache_dir, ".last_run_year")
  last_run_year <- if (fs::file_exists(meta_file)) {
    tryCatch(
      as.integer(readLines(meta_file, warn = FALSE)[1]),
      error = function(e) NA_integer_
    )
  } else {
    NA_integer_
  }

  year_rolled_over <- is.na(last_run_year) || last_run_year != current_year
  prev_year_refresh_failed <- FALSE

  log_info(paste0(
    "Pipeline initiated. Cache: ",
    cache_dir
  ))

  output_list <- list()

  # Step 2: Loop through all reports defined in config$cot_report$report_names
  for (report_key in names(config$cot_report$report_names)) {
    report_code <- config$cot_report$report_names[[report_key]]
    start_year <- get_start_year(report_code)

    # Per-report historical year range
    hist_years <- if (start_year <= current_year - 1) {
      seq.int(start_year, current_year - 1)
    } else {
      integer(0)
    }
    all_years <- c(hist_years, current_year)

    # Per-report forced refresh of previous year
    force_refresh_year <- if (
      year_rolled_over &&
        current_year - 1 >= start_year
    ) {
      current_year - 1L
    } else {
      NA_integer_
    }

    cot_year_list <- list()

    log_info(paste0(
      "--- Starting report type: '",
      report_key,
      "' (Code: ",
      report_code,
      ") | Year range: ",
      start_year,
      "-",
      current_year,
      " ---"
    ))

    for (yr in all_years) {
      local_file <- file.path(cache_dir, paste0(report_code, "_", yr, ".rds"))
      is_current <- (yr == current_year)
      is_force_refresh <- !is.na(force_refresh_year) && yr == force_refresh_year

      # Read from cache if historical file exists and no forced refresh
      if (file.exists(local_file) && !is_current && !is_force_refresh) {
        log_info(paste0(
          "Loading cached file for ",
          report_code,
          " (Year: ",
          yr,
          ")..."
        ))
        cot_year_list[[as.character(yr)]] <- readRDS(local_file)
        next
      }

      # Determine URL
      cftc_prefix <- if (report_code == "deacot") {
        "deacot"
      } else {
        paste0(report_code, "_")
      }
      url <- paste0(cftc_url, cftc_prefix, yr, ".zip")

      if (is_force_refresh) {
        log_info(paste0(
          "Year rolled over; forcing refresh of previous year for ",
          report_code,
          " (Year: ",
          yr,
          ")..."
        ))
      } else {
        log_info(paste0(
          "Downloading live CFTC archive: ",
          report_code,
          " (Year: ",
          yr,
          ")..."
        ))
      }

      tmp_zip <- tempfile(fileext = ".zip")
      ex_dir <- tempfile(pattern = "cot_dir_")

      dt <- tryCatch(
        {
          dl_status <- download.file(url, tmp_zip, quiet = TRUE, mode = "wb")
          if (!identical(as.integer(dl_status), 0L)) {
            stop("download.file() returned non-zero status: ", dl_status)
          }

          extracted_files <- unzip(tmp_zip, exdir = ex_dir)
          target_file <- extracted_files[grepl(
            "\\.(txt|csv)$",
            extracted_files,
            ignore.case = TRUE
          )][1]

          if (is.na(target_file)) {
            stop(
              "No .txt or .csv file found in archive for ",
              report_code,
              " ",
              yr
            )
          }

          data_out <- fread(target_file, fill = TRUE)
          data_out <- clean_cot_dataset(data_out, drop_pattern)
          data_out[, Source_Year := yr]

          setorderv(data_out, "Report_Date_as_YYYY_MM_DD")

          saveRDS(data_out, local_file)
          log_success(paste0(
            "Successfully processed & cached ",
            report_code,
            " for ",
            yr,
            " (",
            nrow(data_out),
            " rows)."
          ))

          data_out
        },
        error = function(e) {
          log_error(paste0(
            "Failed to download/parse ",
            report_code,
            " for ",
            yr,
            ": ",
            e$message
          ))

          if (file.exists(local_file)) {
            log_warn(paste0(
              "Returning last cached version for ",
              report_code,
              " year ",
              yr
            ))
            return(tryCatch(
              readRDS(local_file),
              error = function(e2) {
                log_warn(paste0(
                  "Cached file for ",
                  report_code,
                  " year ",
                  yr,
                  " is unreadable: ",
                  e2$message
                ))
                NULL
              }
            ))
          }
          return(NULL)
        },
        finally = {
          if (file.exists(tmp_zip)) {
            unlink(tmp_zip)
          }
          if (dir.exists(ex_dir)) unlink(ex_dir, recursive = TRUE)
        }
      )

      if (is_force_refresh && is.null(dt)) {
        prev_year_refresh_failed <- TRUE
      }

      cot_year_list[[as.character(yr)]] <- dt
    }

    valid_tables <- cot_year_list[!sapply(cot_year_list, is.null)]
    if (length(valid_tables) > 0) {
      if (length(valid_tables) > 1) {
        ref_types <- vapply(
          valid_tables[[1]],
          function(x) class(x)[1],
          character(1)
        )
        for (i in seq.int(2L, length(valid_tables))) {
          curr_types <- vapply(
            valid_tables[[i]],
            function(x) class(x)[1],
            character(1)
          )
          common <- intersect(names(ref_types), names(curr_types))
          mismatched <- common[ref_types[common] != curr_types[common]]
          if (length(mismatched) > 0) {
            log_warn(paste0(
              "Column type drift detected in report '",
              report_key,
              "' across years: ",
              paste(mismatched, collapse = ", ")
            ))
            break
          }
        }
      }
      combined <- rbindlist(valid_tables, fill = TRUE)

      # Clean the market names. Done on the combined table (not before caching)
      # so it also fixes years that are already saved in the cache.
      name_map <- cot_market_name_map[[report_key]] %||%
        cot_market_name_map[[report_code]]
      output_list[[report_key]] <- standardise_market_names(combined, name_map)
    } else {
      output_list[[report_key]] <- data.table()
    }
  }

  # Step 3: Advance cache metadata if the forced refresh succeeded
  if (!prev_year_refresh_failed) {
    tryCatch(
      writeLines(as.character(current_year), meta_file),
      error = function(e) {
        log_warn(paste0("Could not write cache metadata: ", e$message))
      }
    )
  } else {
    log_warn(paste0(
      "Previous-year refresh failed; cache metadata not updated so that ",
      "the next run will retry."
    ))
  }

  log_info("Pipeline execution completed.")
  return(output_list)
}

################################################################################
