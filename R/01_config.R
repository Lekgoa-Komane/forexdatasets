# load_config -------------------------------------------------------------

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# LOAD CONFIGURATION FILE
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
#'   includes sections for `defaults`, `paths`, `web`, `asset_classes`,
#'   `directory_map`, `data_format`, `cot_report`, `transformation`, `analysis`,
#'    `paper_trading`, `database`, and `pipeline`.
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
  stopifnot(is.character(config_path), length(config_path) == 1)

  if (!file_exists(config_path)) {
    log_error("Configuration file not found: {path}", path = config_path)
    stop("Configuration file not found at: ", config_path)
  }

  config <- tryCatch(
    read_yaml(config_path),
    error = function(e) {
      log_error("Failed to parse configuration: {error}", error = e$message)
      stop("Failed to parse configuration file: ", e$message)
    }
  )

  # ------------------------------------------------------------------
  # Small internal helpers to keep validation DRY and messages uniform
  # ------------------------------------------------------------------
  .require_keys <- function(obj, keys, label) {
    missing <- setdiff(keys, names(obj))
    if (length(missing) > 0) {
      log_error(
        "Missing required {label} keys: {keys}",
        label = label,
        keys = paste(missing, collapse = ", ")
      )
      stop(sprintf(
        "%s section missing required keys: %s",
        label,
        paste(missing, collapse = ", ")
      ))
    }
  }

  .is_scalar_chr <- function(x) is.character(x) && length(x) == 1 && nzchar(x)

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 1. Top-level sections
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  required_top_level <- c(
    "default",
    "paths",
    "web",
    "asset_classes",
    "directory_map",
    "data_format",
    "cot_report",
    "transformation",
    "analysis",
    "paper_trading",
    "database",
    "pipeline"
  )
  # NOTE: "pipeline" is optional to preserve compatibility with minimal test fixtures,
  #       but validated when present.

  missing_top_level <- setdiff(required_top_level, names(config))
  if (length(missing_top_level) > 0) {
    log_error(
      "Missing required top-level config keys: {keys}",
      keys = paste(missing_top_level, collapse = ", ")
    )
    stop(sprintf(
      "Configuration missing critical sections: %s",
      paste(missing_top_level, collapse = ", ")
    ))
  }

  .require_keys(config$default, c("project_name", "environment"), "default")
  if (!.is_scalar_chr(config$default$project_name)) {
    log_error("default$project_name must be a non-empty character string")
    stop("default$project_name must be a non-empty character string")
  }
  valid_envs <- c("dev", "test", "prod")
  if (
    !.is_scalar_chr(config$default$environment) ||
      !config$default$environment %in% valid_envs
  ) {
    log_error(
      "default$environment must be one of: {envs}",
      envs = paste(valid_envs, collapse = ", ")
    )
    stop(sprintf(
      "default$environment must be one of: %s",
      paste(valid_envs, collapse = ", ")
    ))
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 2. paths
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$paths,
    c(
      "local_base",
      "forexengine",
      "forexdataset",
      "frequency_folders",
      "file_suffixes",
      "subfolder"
    ),
    "paths"
  )

  if (!.is_scalar_chr(config$paths$subfolder)) {
    log_error("paths$subfolder must be a non-empty character string")
    stop("paths$subfolder must be a non-empty character string")
  }

  .require_keys(
    config$paths$forexengine,
    c("output_base", "table_names", "db_dir"),
    "paths$forexengine"
  )

  if (length(config$paths$forexengine$table_names) == 0) {
    log_error("paths$forexengine$table_names must not be empty")
    stop("paths$forexengine$table_names must not be empty")
  }

  .require_keys(
    config$paths$forexdataset,
    c("output_base"),
    "paths$forexdataset"
  )

  # frequency_folders and file_suffixes must share keys
  ff <- names(config$paths$frequency_folders)
  fs <- names(config$paths$file_suffixes)
  if (!setequal(ff, fs)) {
    log_error("frequency_folders and file_suffixes must have identical keys")
    stop(sprintf(
      paste0(
        "frequency_folders/file_suffixes key mismatch. ",
        "Only in folders: [%s]; only in suffixes: [%s]"
      ),
      paste(setdiff(ff, fs), collapse = ", "),
      paste(setdiff(fs, ff), collapse = ", ")
    ))
  }

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

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 3. web
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$web,
    c("stooq", "cftc", "delay_between_requests", "timeout", "poll_interval"),
    "web"
  )

  .require_keys(
    config$web$stooq,
    c("base_url", "polite_path", "url_pattern", "x_path", "table_id"),
    "web$stooq"
  )

  if (!grepl("{ticker}", config$web$stooq$url_pattern, fixed = TRUE)) {
    log_error(skip_formatter(
      "web$stooq$url_pattern must contain {ticker} placeholder"
    ))
    stop("Invalid web URL pattern: missing {ticker} placeholder")
  }

  if (
    !is.null(config$web$user_agent) &&
      (!is.character(config$web$user_agent) ||
        length(config$web$user_agent) < 1)
  ) {
    log_error(
      "web$user_agent must be a non-empty character vector when provided"
    )
    stop("web$user_agent must be a non-empty character vector")
  }

  .require_keys(config$web$cftc, c("url"), "web$cftc")

  if (!is.numeric(config$web$timeout) || config$web$timeout <= 0) {
    log_error("web$timeout must be a positive number")
    stop("web$timeout must be a positive number")
  }
  if (
    !is.numeric(config$web$delay_between_requests) ||
      config$web$delay_between_requests < 0
  ) {
    log_error("web$delay_between_requests must be a non-negative number")
    stop("web$delay_between_requests must be a non-negative number")
  }
  if (!is.numeric(config$web$poll_interval) || config$web$poll_interval <= 0) {
    log_error("web$poll_interval must be a positive number")
    stop("web$poll_interval must be a positive number")
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 4. data_format
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$data_format,
    c(
      "local_hourly",
      "local_daily",
      "stooq_daily",
      "file_extension",
      "fut_extension",
      "date_format",
      "date_time_format",
      "column_mapping"
    ),
    "data_format"
  )

  required_column_mappings <- c("hourly", "daily", "stooq")
  .require_keys(
    config$data_format$column_mapping,
    required_column_mappings,
    "column_mapping"
  )

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 5. asset_classes
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if (!"names" %in% names(config$asset_classes)) {
    log_error("asset_classes section missing 'names' key")
    stop("asset_classes must include a 'names' list of asset types")
  }

  for (asset in config$asset_classes$names) {
    if (!asset %in% names(config$asset_classes)) {
      log_error(
        "Asset class '{asset}' listed in names but not defined",
        asset = asset
      )
      stop(sprintf(
        "Asset class '%s' is listed in names but not defined in asset_classes",
        asset
      ))
    }

    .require_keys(
      config$asset_classes[[asset]],
      c("source", "frequencies", "tickers"),
      sprintf("asset_classes.%s", asset)
    )

    src <- config$asset_classes[[asset]]$source
    freq <- config$asset_classes[[asset]]$frequencies
    tk <- config$asset_classes[[asset]]$tickers

    if (!is.character(src) || length(src) < 1) {
      log_error(
        "asset_classes.{asset}.source must be a non-empty character vector",
        asset = asset
      )
      stop(sprintf(
        "asset_classes.%s.source must be a non-empty character vector",
        asset
      ))
    }
    if (!is.character(freq) || length(freq) < 1) {
      log_error(
        "asset_classes.{asset}.frequencies must be a non-empty character vector",
        asset = asset
      )
      stop(sprintf(
        "asset_classes.%s.frequencies must be a non-empty character vector",
        asset
      ))
    }
    if (!is.character(tk) || length(tk) < 1) {
      log_error(
        "asset_classes.{asset}.tickers must be a non-empty character vector",
        asset = asset
      )
      stop(sprintf(
        "asset_classes.%s.tickers must be a non-empty character vector",
        asset
      ))
    }

    # Recognised sources only
    known_sources <- c("stooq", "local")
    bad_src <- setdiff(src, known_sources)
    if (length(bad_src) > 0) {
      log_error(
        "Unknown source(s) for asset '{asset}': {bad}",
        asset = asset,
        bad = paste(bad_src, collapse = ", ")
      )
      stop(sprintf(
        "asset_classes.%s.source contains unknown value(s): %s",
        asset,
        paste(bad_src, collapse = ", ")
      ))
    }
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 6. directory_map completeness
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  for (asset in config$asset_classes$names) {
    freqs <- config$asset_classes[[asset]]$frequencies
    dm <- config$directory_map[[asset]]

    if (is.null(dm)) {
      log_error(
        "directory_map missing entry for asset '{asset}'",
        asset = asset
      )
      stop(sprintf("directory_map has no entry for asset class '%s'", asset))
    }

    for (f in freqs) {
      entry <- dm[[f]]
      if (is.null(entry)) {
        log_error(
          "directory_map.{asset}.{freq} missing",
          asset = asset,
          freq = f
        )
        stop(sprintf("directory_map.%s.%s is missing", asset, f))
      }
      if (is.list(entry)) {
        # Nested: must contain at least one named sub-path (e.g. stooq/major/other)
        if (length(entry) == 0 || is.null(names(entry))) {
          log_error(
            "directory_map.{asset}.{freq} must be a named list",
            asset = asset,
            freq = f
          )
          stop(sprintf("directory_map.%s.%s must be a named list", asset, f))
        }
      } else if (!.is_scalar_chr(entry)) {
        log_error(
          "directory_map.{asset}.{freq} must be a string or named list",
          asset = asset,
          freq = f
        )
        stop(sprintf(
          "directory_map.%s.%s must be a string or a named list",
          asset,
          f
        ))
      }
    }

    # Currency_Pairs special case: other_tickers must exist and be subset of tickers
    if (asset == "Currency_Pairs") {
      ot <- config$asset_classes$Currency_Pairs$other_tickers
      if (is.null(ot)) {
        log_error("Currency_Pairs must declare other_tickers")
        stop("Currency_Pairs must declare other_tickers")
      }
      bad <- setdiff(ot, config$asset_classes$Currency_Pairs$tickers)
      if (length(bad) > 0) {
        log_error(
          "other_tickers not in tickers: {bad}",
          bad = paste(bad, collapse = ", ")
        )
        stop(sprintf(
          "other_tickers contains tickers not in tickers: %s",
          paste(bad, collapse = ", ")
        ))
      }
      # Currency_Pairs requires both major/other sub-keys per frequency
      for (f in config$asset_classes$Currency_Pairs$frequencies) {
        sub <- config$directory_map$Currency_Pairs[[f]]
        if (!is.list(sub) || !all(c("major", "other") %in% names(sub))) {
          log_error(
            "directory_map.Currency_Pairs.{freq} must contain 'major' and 'other'",
            freq = f
          )
          stop(sprintf(
            "directory_map.Currency_Pairs.%s must contain both 'major' and 'other' sub-paths",
            f
          ))
        }
      }
    }
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 7. cot_report
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$cot_report,
    c(
      "report_names",
      "output_folder",
      "cache_dir",
      "cot_regex",
      "start_year",
      "start_year_overrides"
    ),
    "cot_report"
  )

  .require_keys(
    config$cot_report$report_names,
    c("fut_disagg", "fut_fin", "legacy"),
    "cot_report$report_names"
  )

  bad_overrides <- setdiff(
    names(config$cot_report$start_year_overrides),
    names(config$cot_report$report_names)
  )
  if (length(bad_overrides) > 0) {
    log_error(
      "cot_report.start_year_overrides contains unknown report keys: {bad}",
      bad = paste(bad_overrides, collapse = ", ")
    )
    stop(sprintf(
      "cot_report.start_year_overrides has unknown keys: %s",
      paste(bad_overrides, collapse = ", ")
    ))
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 8. transformation
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$transformation,
    c("aggregation", "pip_factor"),
    "transformation"
  )
  .require_keys(
    config$transformation$aggregation,
    c(
      "daily_to_quarterly",
      "daily_to_monthly",
      "daily_to_weekly",
      "h1_to_daily",
      "h1_to_h4"
    ),
    "transformation$aggregation"
  )
  .require_keys(
    config$transformation$pip_factor,
    c("regex_pattern", "value_for_match", "default_value"),
    "transformation$pip_factor"
  )

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 9. analysis
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$analysis,
    c(
      "sessions",
      "trend_filter",
      "swing_filter",
      "choch_filter",
      "signal_filter",
      "backtest"
    ),
    "analysis"
  )

  .require_keys(
    config$analysis$sessions,
    c(
      "timezone_standard",
      "kill_zone_buffer_minutes",
      "asian",
      "london",
      "newyork",
      "overlaps"
    ),
    "analysis$sessions"
  )

  .require_keys(
    config$analysis$sessions$asian,
    c("name", "start", "end"),
    "analysis$sessions$asian"
  )

  .require_keys(
    config$analysis$sessions$overlaps,
    c("london_ny", "asian_london"),
    "analysis$sessions$overlaps"
  )

  for (sess in c("london", "newyork")) {
    s <- config$analysis$sessions[[sess]]
    .require_keys(
      s,
      c(
        "name",
        "standard_time",
        "daylight_time",
        "dst_transition_dates",
        "kill_zones"
      ),
      sprintf("analysis$sessions$%s", sess)
    )
    .require_keys(
      s$standard_time,
      c("start", "end"),
      sprintf("analysis$sessions$%s$standard_time", sess)
    )
    .require_keys(
      s$daylight_time,
      c("start", "end"),
      sprintf("analysis$sessions$%s$daylight_time", sess)
    )
    .require_keys(
      s$dst_transition_dates,
      c("start_dst", "end_dst"),
      sprintf("analysis$sessions$%s$dst_transition_dates", sess)
    )

    if (length(s$kill_zones) == 0) {
      log_error(
        "analysis.sessions.{sess}.kill_zones must not be empty",
        sess = sess
      )
      stop(sprintf("analysis$sessions$%s$kill_zones must not be empty", sess))
    }
    for (kz_name in names(s$kill_zones)) {
      .require_keys(
        s$kill_zones[[kz_name]],
        c("start", "end"),
        sprintf("analysis$sessions$%s$kill_zones$%s", sess, kz_name)
      )
    }
  }

  # trend_filter: lookback_period per timeframe
  for (tf in c("weekly", "daily", "h4", "h1")) {
    .require_keys(
      config$analysis$trend_filter[[tf]],
      c("lookback_period"),
      sprintf("analysis$trend_filter$%s", tf)
    )
    lb <- config$analysis$trend_filter[[tf]]$lookback_period
    if (!is.numeric(lb) || lb < 1) {
      log_error(
        "analysis.trend_filter.{tf}.lookback_period must be >= 1",
        tf = tf
      )
      stop(sprintf(
        "analysis$trend_filter$%s$lookback_period must be a positive number",
        tf
      ))
    }
  }

  .require_keys(
    config$analysis$swing_filter,
    c("noise_filter", "sw_gap_bot", "sw_gap_top", "ob_filter", "obs"),
    "analysis$swing_filter"
  )
  if (
    !is.numeric(config$analysis$swing_filter$obs) ||
      config$analysis$swing_filter$obs < 1
  ) {
    log_error("analysis.swing_filter.obs must be a positive number")
    stop("analysis$swing_filter$obs must be a positive number")
  }

  .require_keys(
    config$analysis$choch_filter,
    c("choch_quant"),
    "analysis$choch_filter"
  )
  .require_keys(
    config$analysis$signal_filter,
    c(
      "buffer_pipette",
      "min_confluence",
      "risk_pct",
      "upper_gap",
      "lower_gap",
      "allowed_alignments",
      "min_risk_reward",
      "score_daily_overlap"
    ),
    "analysis$signal_filter"
  )

  sf <- config$analysis$signal_filter
  if (!(sf$lower_gap < sf$upper_gap)) {
    log_error("analysis.signal_filter: lower_gap must be < upper_gap")
    stop("analysis$signal_filter: lower_gap must be less than upper_gap")
  }
  if (!(is.numeric(sf$risk_pct) && sf$risk_pct > 0 && sf$risk_pct <= 1)) {
    log_error("analysis.signal_filter.risk_pct must be in (0, 1]")
    stop("analysis$signal_filter$risk_pct must be in (0, 1]")
  }
  if (length(sf$allowed_alignments) == 0) {
    log_error("analysis.signal_filter.allowed_alignments must not be empty")
    stop("analysis$signal_filter$allowed_alignments must not be empty")
  }
  if (
    !is.logical(sf$score_daily_overlap) || length(sf$score_daily_overlap) != 1
  ) {
    log_error(
      "analysis.signal_filter.score_daily_overlap must be a scalar logical"
    )
    stop("analysis$signal_filter$score_daily_overlap must be a scalar logical")
  }

  .require_keys(
    config$analysis$backtest,
    c("max_holding_candles", "trailing_stop"),
    "analysis$backtest"
  )
  .require_keys(
    config$analysis$backtest$trailing_stop,
    c("trigger_pips", "step_pips"),
    "analysis$backtest$trailing_stop"
  )

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 10. paper_trading
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$paper_trading,
    c(
      "currency",
      "starting_balance",
      "min_risk_pct",
      "max_risk_pct",
      "min_win_rate",
      "minimum_months",
      "start_date"
    ),
    "paper_trading"
  )

  pt <- config$paper_trading
  if (!.is_scalar_chr(pt$currency)) {
    log_error("paper_trading.currency must be a non-empty character string")
    stop("paper_trading$currency must be a non-empty character string")
  }
  if (!is.numeric(pt$starting_balance) || pt$starting_balance <= 0) {
    log_error("paper_trading.starting_balance must be positive")
    stop("paper_trading$starting_balance must be positive")
  }
  if (!is.numeric(pt$minimum_months) || pt$minimum_months < 1) {
    log_error("paper_trading.minimum_months must be >= 1")
    stop("paper_trading$minimum_months must be >= 1")
  }
  if (pt$min_risk_pct > pt$max_risk_pct) {
    log_error(
      "paper_trading: min_risk_pct ({a}) must be <= max_risk_pct ({b})",
      a = pt$min_risk_pct,
      b = pt$max_risk_pct
    )
    stop("paper_trading: min_risk_pct must be <= max_risk_pct")
  }
  if (
    !(is.numeric(pt$min_win_rate) &&
      pt$min_win_rate >= 0 &&
      pt$min_win_rate <= 1)
  ) {
    log_error("paper_trading.min_win_rate must be in [0, 1]")
    stop("paper_trading$min_win_rate must be in [0, 1]")
  }
  if (is.na(as.Date(pt$start_date, optional = TRUE))) {
    log_error(
      "paper_trading.start_date is not a parseable date: {d}",
      d = pt$start_date
    )
    stop(sprintf(
      "paper_trading$start_date is not a valid date: %s",
      pt$start_date
    ))
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 11. database
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  .require_keys(
    config$database,
    c(
      "db_suffix",
      "table_prefixes",
      "index_column",
      "pragma",
      "batch_size",
      "create_if_not_exists"
    ),
    "database"
  )
  .require_keys(
    config$database$table_prefixes,
    c("journal", "paper_trade"),
    "database$table_prefixes"
  )
  .require_keys(
    config$database$pragma,
    c("journal_mode", "synchronous"),
    "database$pragma"
  )

  if (
    !is.character(config$database$index_column) ||
      length(config$database$index_column) < 1
  ) {
    log_error("database$index_column must be a non-empty character vector")
    stop("database$index_column must be a non-empty character vector")
  }
  if (
    !is.numeric(config$database$batch_size) || config$database$batch_size < 1
  ) {
    log_error("database$batch_size must be a positive number")
    stop("database$batch_size must be a positive number")
  }
  if (
    !is.logical(config$database$create_if_not_exists) ||
      length(config$database$create_if_not_exists) != 1
  ) {
    log_error("database$create_if_not_exists must be a scalar logical")
    stop("database$create_if_not_exists must be a scalar logical")
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Optional: pipeline (when present in config)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if (!is.null(config$pipeline) && length(config$pipeline) > 0) {
    if (!is.null(config$pipeline$parallel)) {
      .require_keys(
        config$pipeline$parallel,
        c("enabled", "max_workers", "chunk_size"),
        "pipeline$parallel"
      )
    }
    if (!is.null(config$pipeline$cache)) {
      .require_keys(
        config$pipeline$cache,
        c("enabled", "directory", "ttl_hours"),
        "pipeline$cache"
      )
    }
    if (!is.null(config$pipeline$retry)) {
      .require_keys(
        config$pipeline$retry,
        c("max_attempts", "backoff_seconds"),
        "pipeline$retry"
      )
    }
  }

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Success
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  total_tickers <- sum(vapply(
    config$asset_classes$names,
    function(a) length(config$asset_classes[[a]]$tickers),
    integer(1)
  ))

  log_info(
    "Configuration loaded: {n} total tickers across {k} asset classes from {path}",
    n = total_tickers,
    k = length(config$asset_classes$names),
    path = basename(config_path)
  )

  return(config)
}


# ticker_metadata ---------------------------------------------------------

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# CREATE TICKER METADATA
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#' Build ticker metadata from configuration
#'
#' Constructs a data.table of ticker combinations for every asset class
#' defined in the configuration. The function expands frequencies,
#' sources (including local and stooq), subdirectory mappings, and generates
#' source paths for each record.
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
#'   \item Futures assets can have multiple sources (local, stooq)
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

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # STEP 1: PRE-COMPUTE SUBDIRECTORY MAP
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
            # FUTURES SOURCES (stooq + local)
            # -------------------------------------------------
            if (asset %in% c("Commodity_Futures", "Currency_Futures")) {
              freqs <- config$asset_classes[[asset]]$frequencies
              sources <- config$asset_classes[[asset]]$source # e.g. c("stooq", "local")

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
                  } else if (typ == "stooq") {
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
            # Re-throw structural configuration errors up the stack
            stop(e)
          }
        )
        result
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

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # STEP 2: BUILD TICKER COMBINATIONS
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
          # HANDLE MULTIPLE SOURCES (stooq, local)
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
                type == "local_stooq",
                "local_stooq",
                default = NA_character_
              )
            ]
          } else {
            dt[, `:=`(
              data_source = sources[1],
              type = NA_character_
            )]
          }

          dt[, extract_flag := TRUE]

          # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          # EXISTING: CURRENCY PAIRS TYPE OVERRIDE
          # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
          # URL PATTERN VALIDATION (stooq)
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
          # SOURCE PATH LOGIC (stooq / local)
          # -------------------------------------------------------
          dt[, source_path := NA_character_]

          # stooq web source
          dt[
            data_source == "stooq",
            source_path := sprintf(url_pattern_stooq, ticker)
          ]

          # ALL local sources – "local", "local_stooq"
          dt[
            grepl("^local", data_source),
            source_path := file.path(
              config$paths$local_base,
              subdir,
              paste0(ticker, file_ext)
            )
          ]

          # Clean up helper columns
          dt[, c("subdir", "file_ext") := NULL]

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
          # Re-throw the error so that validation errors are not swallowed into a NULL
          stop(e)
        }
      )
      result
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
