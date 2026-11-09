# =============================================================================
# LOAD CONFIGURATION FILE
# =============================================================================

#' Load and validate pipeline configuration
#'
#' Reads a YAML configuration file, validates all critical sections and
#' nested keys, and returns the configuration as a list. The function is
#' designed to catch configuration errors early, before the pipeline runs,
#' to prevent downstream failures.
#'
#' @param config_path Character string. Path to the configuration file.
#'   Defaults to `system.file("config", "config.yml", package = "forexdatasets")`.
#'
#' @return A named list containing the validated configuration. The list
#'   includes sections for `paths`, `web`, `asset_classes`, `directory_map`,
#'   `data_format`, `transformation`, `analysis`, `database`, and `pipeline`.
#'
#' @importFrom fs file_exists dir_exists
#' @importFrom yaml read_yaml
#' @importFrom logger log_error log_info skip_formatter
#
#' @examples
#' \dontrun{
#'   # Load configuration from the default path
#'   config <- load_config()
#'
#'   # Access a critical config value
#'   local_data_dir <- config$paths$local_base
#'   cat(paste("Local data directory:", local_data_dir, "\n"))
#'
#'   # Load configuration from a custom path
#'   custom_path <- "path/to/your/custom_config.yml"
#'   if (fs::file_exists(custom_path)) {
#'     config_custom <- load_config(custom_path)
#'   }
#' }
#'
#' @seealso \code{\link[yaml]{read_yaml}}, \code{\link[fs]{file_exists}},
#' \code{\link[fs]{dir_exists}}, \code{\link[logger]{log_error}}
#'
#' @note This function is designed to be called at the start of your ETL
#' pipeline. By validating the configuration upfront, it prevents downstream
#' failures due to missing or invalid config values.
#'
#' @export

load_config <- function(
  config_path = system.file("config", "config.yml", package = "forexdatasets")
) {
  # Validate input
  stopifnot(
    is.character(config_path),
    length(config_path) == 1
  )

  # Check if file exists
  if (!file_exists(config_path)) {
    log_error(
      "Configuration file not found: {path}",
      path = config_path
    )
    stop(
      "Configuration file not found at: ",
      config_path
    )
  }

  # Try to read with better error handling
  config <- tryCatch(
    read_yaml(config_path),
    error = function(e) {
      log_error(
        "Failed to parse configuration: {error}",
        error = e$message
      )
      stop(
        "Failed to parse configuration file: ",
        e$message
      )
    }
  )

  # ==============================================================
  # START OF EXPANDED CONFIG VALIDATION
  # Purpose: Catch configuration errors early to prevent downstream pipeline
  # failures Validates all critical config sections and nested keys
  # ==============================================================
  # 1. Validate all critical top-level config sections exist
  required_top_level <- c(
    "paths",
    "web",
    "asset_classes",
    "directory_map",
    "data_format",
    "transformation",
    "analysis",
    "database",
    "pipeline"
  )

  missing_top_level <- setdiff(
    required_top_level,
    names(config)
  )

  if (length(missing_top_level) > 0) {
    log_error(
      "Missing required top-level config keys: {keys}",
      keys = paste(
        missing_top_level,
        collapse = ", "
      )
    )
    stop(sprintf(
      "Configuration missing critical sections: %s",
      paste(
        missing_top_level,
        collapse = ", "
      )
    ))
  }

  # 2. Validate paths section (critical for local data access)
  required_paths <- c(
    "local_base",
    "forexengine",
    "forexdataset",
    "frequency_folders",
    "file_suffixes",
    "subfolder"
  )

  missing_paths <- setdiff(
    required_paths,
    names(config$paths)
  )

  if (length(missing_paths) > 0) {
    log_error(
      "Missing required paths keys: {keys}",
      keys = paste(
        missing_paths,
        collapse = ", "
      )
    )
    stop(sprintf(
      "paths section missing required keys: %s",
      paste(
        missing_paths,
        collapse = ", "
      )
    ))
  }

  # Ensure local data directory actually exists on disk
  if (!dir_exists(config$paths$local_base)) {
    log_error(
      "Local base directory not found: {path}",
      path = config$paths$local_base
    )
    stop(sprintf(
      "Local base directory does not exist: %s",
      config$paths$local_base
    ))
  }

  # 3. Validate web section (critical for web scraping)
  required_web <- c(
    "stooq",
    "investing",
    "delay_between_requests",
    "timeout"
  )

  missing_web <- setdiff(
    required_web,
    names(config$web)
  )

  if (length(missing_web) > 0) {
    log_error(
      "Missing required web keys: {keys}",
      keys = paste(
        missing_web,
        collapse = ", "
      )
    )
    stop(sprintf(
      "web section missing required keys: %s",
      paste(
        missing_web,
        collapse = ", "
      )
    ))
  }

  # Ensure URL pattern includes {ticker} placeholder
  # (required for dynamic URL generation)
  if (
    !grepl(
      "{ticker}",
      config$web$stooq$url_pattern,
      fixed = TRUE
    )
  ) {
    log_error(
      skip_formatter("web$stooq$url_pattern must contain {ticker} placeholder")
    )
    stop(
      "Invalid web URL pattern: missing {ticker} placeholder"
    )
  }

  # 4. Validate data_format section (critical for column selection/renaming)
  required_data_format <- c(
    "local_hourly",
    "local_daily",
    "stooq_daily",
    "investing_daily",
    "file_extension",
    "date_format",
    "date_time_format",
    "column_mapping"
  )

  missing_data_format <- setdiff(
    required_data_format,
    names(config$data_format)
  )

  if (length(missing_data_format) > 0) {
    log_error(
      "Missing required data_format keys: {keys}",
      keys = paste(
        missing_data_format,
        collapse = ", "
      )
    )
    stop(sprintf(
      "data_format section missing required keys: %s",
      paste(
        missing_data_format,
        collapse = ", "
      )
    ))
  }

  # Ensure all necessary column mappings exist
  required_column_mappings <- c(
    "hourly",
    "daily",
    "stooq",
    "investing"
  )

  missing_mappings <- setdiff(
    required_column_mappings,
    names(
      config$data_format$column_mapping
    )
  )

  if (length(missing_mappings) > 0) {
    log_error(
      "Missing required column mappings: {keys}",
      keys = paste(
        missing_mappings,
        collapse = ", "
      )
    )
    stop(sprintf(
      "column_mapping section missing required keys: %s",
      paste(
        missing_mappings,
        collapse = ", "
      )
    ))
  }

  # 5. Validate asset_classes structure (critical for ticker metadata generation)
  if (
    !"names" %in%
      names(config$asset_classes)
  ) {
    log_error(
      "asset_classes section missing 'names' key"
    )
    stop(
      "asset_classes must include a 'names' list of asset types"
    )
  }

  # Validate each asset class has required configuration
  for (asset in config$asset_classes$names) {
    if (
      !asset %in%
        names(config$asset_classes)
    ) {
      log_error(
        "Asset class '{asset}' listed in names but not defined"
      )
      stop(sprintf(
        "Asset class '%s' is listed in names but not defined in asset_classes",
        asset
      ))
    }

    required_asset_keys <- c(
      "source",
      "frequencies",
      "tickers"
    )

    missing_asset_keys <- setdiff(
      required_asset_keys,
      names(config$asset_classes[[
        asset
      ]])
    )

    if (length(missing_asset_keys) > 0) {
      log_error(
        "Missing keys for asset class '{asset}': {keys}",
        asset = asset,
        keys = paste(
          missing_asset_keys,
          collapse = ", "
        )
      )
      stop(sprintf(
        "Asset class '%s' missing required keys: %s",
        asset,
        paste(
          missing_asset_keys,
          collapse = ", "
        )
      ))
    }
  }

  # 6. Calculate total tickers across all asset classes for success log
  total_tickers <- sum(
    sapply(
      config$asset_classes$names,
      function(asset) {
        length(
          config$asset_classes[[
            asset
          ]]$tickers
        )
      }
    )
  )

  # Success log with detailed metrics
  log_info(
    "Configuration loaded: {n} total tickers across {k} asset classes from {path}",
    n = total_tickers,
    k = length(
      config$asset_classes$names
    ),
    path = basename(config_path)
  )

  return(config)
}


# =============================================================================
# CREATE TICKER METADATA
# =============================================================================

#' Build ticker metadata from configuration
#'
#' Constructs a data.table of ticker combinations for every asset class
#' defined in the configuration. The function expands frequencies, sources
#' (including local, stooq, and investing), subdirectory mappings, and
#' generates source paths for each record.
#'
#' The resulting data.table contains one row per unique
#' (asset_class, ticker, frequency, data_source) combination, with columns:
#' \code{asset_class}, \code{ticker}, \code{frequency}, \code{type},
#' \code{data_source}, \code{extract_flag}, and \code{source_path}.
#'
#' Special handling includes:
#' \itemize{
#'   \item Currency pairs are split into major/other types using the
#'     \code{other_tickers} field from config.
#'   \item Futures assets can have multiple sources (local, stooq, investing)
#'     and are expanded accordingly.
#'   \item URL patterns are validated to contain a \code{{ticker}} placeholder.
#' }
#'
#' @param config A validated configuration list, as returned by
#'   \code{\link{load_config}}.
#'
#' @return A \code{data.table} with columns:
#'   \code{asset_class}, \code{ticker}, \code{frequency}, \code{type},
#'   \code{data_source}, \code{extract_flag}, and \code{source_path}.
#'
#' @import data.table
#' @importFrom logger log_info log_warn log_error skip_formatter
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config()
#' ticker_metadata <- ticker_metadata(config)
#' head(ticker_metadata)
#' }

ticker_metadata <- function(config) {
  log_info("Building ticker metadata from configuration")

  # ===========================================================
  # STEP 1: PRE-COMPUTE SUBDIRECTORY MAP
  # ===========================================================
  log_info(
    "Constructing subdirectory mapping for all asset classes and frequencies"
  )

  subdir_map <- rbindlist(
    lapply(
      config$asset_classes$names,
      function(asset) {
        result <- tryCatch(
          {
            # -------------------------------------------------
            # NEW: HANDLE FUTURES WITH DUAL SOURCES (stooq + investing)
            # -------------------------------------------------
            if (asset %in% c("Commodity_Futures", "Currency_Futures")) {
              freqs <- config$asset_classes[[asset]]$frequencies
              sources <- config$asset_classes[[asset]]$source # e.g. c("web","investing","local")

              # Determine the local sub‑source names from the directory_map.
              local_sub_sources <- c()
              for (freq in freqs) {
                entry <- config$directory_map[[asset]][[freq]]
                if (is.list(entry)) {
                  local_sub_sources <- unique(c(
                    local_sub_sources,
                    names(entry)
                  ))
                } else {
                  # Fallback if the user kept a flat string (backward compatibility)
                  local_sub_sources <- unique(c(local_sub_sources, "stooq"))
                }
              }

              # Expand the generic "local" into one row per local sub‑source
              expanded_sources <- if ("local" %in% sources) {
                c(
                  setdiff(sources, "local"),
                  paste0("local_", local_sub_sources)
                )
              } else {
                sources
              }

              grid <- CJ(
                frequency = freqs,
                type = expanded_sources,
                sorted = FALSE
              )

              subdirs <- vapply(
                seq_len(nrow(grid)),
                function(i) {
                  freq <- grid$frequency[i]
                  typ <- grid$type[i]

                  if (grepl("^local_", typ)) {
                    sub_source <- sub("^local_", "", typ)
                    path <- config$directory_map[[asset]][[freq]][[sub_source]]
                    if (is.null(path)) {
                      stop(sprintf(
                        "Missing directory_map entry for %s[%s][%s]",
                        asset,
                        freq,
                        sub_source
                      ))
                    }
                    return(path)
                  } else if (typ %in% c("stooq", "investing")) {
                    return(NA_character_)
                  } else {
                    stop(sprintf("Unknown source type '%s'", typ))
                  }
                },
                FUN.VALUE = character(1)
              )

              dt_dir <- data.table(
                asset_class = asset,
                frequency = grid$frequency,
                type = grid$type,
                subdir = subdirs
              )

              log_info(
                paste(
                  "Constructed subdir mapping for",
                  asset,
                  "with multiple sources:",
                  nrow(dt_dir),
                  "rows"
                )
              )

              return(dt_dir)
            }

            # -------------------------------------------------
            # EXISTING: CURRENCY PAIRS SPECIAL CASE
            # -------------------------------------------------
            if (asset == "Currency_Pairs") {
              freq_types <- expand.grid(
                frequency = c("daily", "hourly"),
                type = c("major", "other"),
                stringsAsFactors = FALSE
              )

              subdirs <- vapply(
                seq_len(nrow(freq_types)),
                function(i) {
                  freq <- freq_types$frequency[i]
                  typ <- freq_types$type[i]
                  path <- config$directory_map$Currency_Pairs[[freq]][[typ]]
                  if (is.null(path)) {
                    stop(sprintf(
                      "Missing directory_map entry for Currency_Pairs[%s][%s]",
                      freq,
                      typ
                    ))
                  }
                  path
                },
                FUN.VALUE = character(1)
              )

              dt_dir <- data.table(
                asset_class = "Currency_Pairs",
                frequency = freq_types$frequency,
                type = freq_types$type,
                subdir = subdirs
              )

              log_info(
                paste(
                  "Constructed subdir mapping for 'Currency_Pairs'",
                  "with type distinction:",
                  nrow(dt_dir),
                  "rows"
                )
              )

              return(dt_dir)
            } else {
              # All other assets (single source, flat directory_map)
              freqs <- config$asset_classes[[asset]]$frequencies
              subdirs <- vapply(
                freqs,
                function(f) {
                  path <- config$directory_map[[asset]][[f]]
                  if (is.null(path)) {
                    stop(sprintf(
                      "Missing directory_map entry for %s[%s]",
                      asset,
                      f
                    ))
                  }
                  path
                },
                FUN.VALUE = character(1)
              )

              dt_dir <- data.table(
                asset_class = asset,
                frequency = freqs,
                type = NA_character_,
                subdir = subdirs
              )

              log_info(
                "Constructed subdir mapping for '{asset}': {n} rows",
                asset = asset,
                n = nrow(dt_dir)
              )

              return(dt_dir)
            }
          },
          error = function(e) {
            log_error(
              "Failed to build subdir_map for '{asset}': {error}",
              asset = asset,
              error = conditionMessage(e)
            )
            # NULL
            # FIX 1: Re-throw structural configuration errors up the stack
            stop(e)
          }
        )
        # return(result)
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(subdir_map) == 0) {
    log_error("No valid subdir mappings generated")
    stop("Subdir map is empty")
  }

  setkeyv(subdir_map, c("asset_class", "frequency", "type"))

  # ===========================================================
  # STEP 2: BUILD TICKER COMBINATIONS
  # ===========================================================
  log_info(
    "Generating ticker combinations for {k} asset classes",
    k = length(config$asset_classes$names)
  )

  combos_list <- lapply(
    config$asset_classes$names,
    function(asset) {
      result <- tryCatch(
        {
          tickers <- config$asset_classes[[asset]]$tickers
          freqs <- config$asset_classes[[asset]]$frequencies
          # For futures the only frequency is "daily"
          if (!asset %in% c("Currency_Indices", "Currency_Pairs", "Metals")) {
            freqs <- "daily"
          }

          dt <- CJ(
            asset_class = asset,
            ticker = tickers,
            frequency = freqs,
            sorted = FALSE
          )

          sources <- config$asset_classes[[asset]]$source

          # -------------------------------------------------------
          # NEW: HANDLE MULTIPLE SOURCES (web, investing, local)
          # -------------------------------------------------------
          if (
            length(sources) > 1 &&
              asset %in% c("Commodity_Futures", "Currency_Futures")
          ) {
            # Expand the generic "local" into sub‑source types
            local_sub_sources <- names(config$directory_map[[asset]][[freqs[
              1
            ]]]) # safe, already validated
            expanded_sources <- if ("local" %in% sources) {
              c(setdiff(sources, "local"), paste0("local_", local_sub_sources))
            } else {
              sources
            }

            dt <- dt[
              CJ(
                ticker = unique(dt$ticker),
                frequency = unique(dt$frequency),
                type = expanded_sources,
                sorted = FALSE
              ),
              on = .(ticker, frequency),
              allow.cartesian = TRUE
            ]

            dt[,
              data_source := fcase(
                type == "stooq",
                "stooq",
                type == "investing",
                "investing",
                type == "local_stooq",
                "local_stooq",
                type == "local_investing",
                "local_investing",
                default = NA_character_
              )
            ]

            # Add investing ticker mapping (parallel vector)
            inv_tickers <- config$asset_classes[[asset]]$investing_tickers
            if (
              !is.null(inv_tickers) && length(inv_tickers) == length(tickers)
            ) {
              map_dt <- data.table(
                ticker = tickers,
                investing_ticker = inv_tickers
              )
              dt <- merge(dt, map_dt, by = "ticker", all.x = TRUE)
            } else {
              # log_warn(
              #   "investing_tickers missing or length mismatch for '{asset}'",
              #   asset = asset
              # )
              msg <- sprintf(
                "investing_tickers missing or length mismatch for '%s'",
                asset
              )
              log_warn(logger::skip_formatter(msg))
              # FIX 2: Signal native warning so testthat can record it
              warning(msg, call. = FALSE)
              dt[, investing_ticker := NA_character_]
            }
          } else {
            dt[, `:=`(
              data_source = sources[1],
              type = NA_character_,
              # add this to avoid missing column issues later, even if it's
              # not used for non‑futures
              investing_ticker = NA_character_
            )]
          }

          dt[, extract_flag := TRUE]

          # ===========================================================
          # EXISTING: CURRENCY PAIRS TYPE OVERRIDE
          # ===========================================================
          if (asset == "Currency_Pairs") {
            other <- config$asset_classes$Currency_Pairs$other_tickers
            dt[, type := fifelse(ticker %in% other, "other", "major")]
          }

          # Merge subdir information
          dt[
            subdir_map,
            on = .(asset_class, frequency, type),
            subdir := i.subdir
          ]

          # -------------------------------------------------------
          # URL PATTERN VALIDATION (stooq always, investing only if used)
          # -------------------------------------------------------
          pattern_stooq <- config$web$stooq$url_pattern
          if (!grepl("{ticker}", pattern_stooq, fixed = TRUE)) {
            log_error(skip_formatter(
              "Invalid stooq URL pattern: missing {ticker}"
            ))
            stop("stooq URL pattern must contain {ticker}")
          }
          url_pattern_stooq <- gsub(
            "{ticker}",
            "%s",
            pattern_stooq,
            fixed = TRUE
          )

          # Initialize investing pattern to NA (safe for non‑futures)
          url_pattern_investing <- NA_character_

          if ("investing" %in% sources) {
            pattern_investing <- config$web$investing$url_pattern
            if (!grepl("{ticker}", pattern_investing, fixed = TRUE)) {
              log_error(skip_formatter(
                "Invalid investing URL pattern: missing {ticker}"
              ))
              stop("investing URL pattern must contain {ticker}")
            }

            asset_slug_map <- c(
              Commodity_Futures = "commodities",
              Currency_Futures = "currencies"
            )
            asset_slug <- asset_slug_map[asset]
            if (is.na(asset_slug)) {
              log_error("No asset slug defined for '{asset}' in investing URL")
              stop("Missing investing asset slug")
            }

            url_pattern_investing <- gsub(
              "{asset_class}",
              asset_slug,
              gsub("{ticker}", "%s", pattern_investing, fixed = TRUE),
              fixed = TRUE
            )
          }

          # -------------------------------------------------------
          # FILE EXTENSION PER ASSET CLASS
          # -------------------------------------------------------
          futures_assets <- c("Commodity_Futures", "Currency_Futures")
          dt[,
            file_ext := fifelse(
              asset_class %in% futures_assets,
              config$data_format$fut_extension,
              config$data_format$file_extension
            )
          ]

          # -------------------------------------------------------
          # SOURCE PATH LOGIC (NOW HANDLES stooq / investing / local)
          # -------------------------------------------------------
          dt[, source_path := NA_character_]

          # stooq web source
          dt[
            data_source == "stooq",
            source_path := sprintf(url_pattern_stooq, ticker)
          ]

          # investing web source (only when available)
          if ("investing" %in% sources) {
            dt[
              data_source == "investing",
              source_path := sprintf(url_pattern_investing, investing_ticker)
            ]
          }

          # ALL local sources – "local", "local_stooq", "local_investing"
          dt[
            grepl("^local", data_source),
            source_path := file.path(
              config$paths$local_base,
              subdir,
              paste0(ticker, file_ext)
            )
          ]

          # Clean up helper columns
          dt[, c("subdir", "file_ext", "investing_ticker") := NULL]

          log_info(
            "Generated {n} ticker combinations for '{asset}'",
            n = nrow(dt),
            asset = asset
          )

          return(dt)
        },
        error = function(e) {
          log_error(
            "Failed to generate ticker combos for '{asset}': {error}",
            asset = asset,
            error = conditionMessage(e)
          )
          # NULL
          # Re-throw the error so that validation errors are not swallowed into a NULL
          stop(e)
        }
      )
      # return(result)
    }
  )

  ticker_dt <- rbindlist(combos_list, use.names = TRUE, fill = TRUE)

  if (nrow(ticker_dt) == 0) {
    log_error("No ticker combinations generated")
    stop("Ticker combo list is empty")
  }

  setkeyv(ticker_dt, c("asset_class", "ticker", "frequency", "type"))

  log_info(
    paste(
      "Ticker metadata complete:",
      nrow(ticker_dt),
      "records,",
      length(unique(ticker_dt$asset_class)),
      "asset classes,",
      length(unique(ticker_dt$ticker)),
      "unique tickers"
    )
  )

  return(ticker_dt)
}
