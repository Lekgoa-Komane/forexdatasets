# tests/testthat/helper.R

make_valid_config <- function() {
  local_base <- tempfile("local_base")
  forexdataset_base <- tempfile("forexdatasets")
  forexengine_base <- tempfile("forexengine")
  db_dir <- file.path(forexengine_base, "Databases")

  dir.create(local_base, recursive = TRUE)
  dir.create(forexdataset_base, recursive = TRUE)
  dir.create(forexengine_base, recursive = TRUE)
  dir.create(db_dir, recursive = TRUE)

  list(
    default = list(
      project_name = "Data ETL Pipeline",
      environment = "dev"
    ),
    paths = list(
      local_base = local_base,
      forexengine = list(
        output_base = forexengine_base,
        table_names = c("registry", "bos_choch", "reversal_watch"),
        db_dir = db_dir
      ),
      forexdataset = list(output_base = forexdataset_base),
      frequency_folders = list(
        h1 = "h1",
        daily = "daily",
        h4 = "h4",
        weekly = "weekly",
        monthly = "monthly",
        quarterly = "quarterly"
      ),
      file_suffixes = list(
        h1 = "h1",
        daily = "d",
        h4 = "h4",
        weekly = "w",
        monthly = "m",
        quarterly = "q"
      ),
      subfolder = "csv"
    ),
    web = list(
      stooq = list(
        base_url = "https://stooq.com",
        polite_path = "/q/d/?s=",
        url_pattern = "https://stooq.com/q/d/?s={ticker}",
        table_id = "#f13",
        x_path = "//table"
      ),
      cftc = list(
        url = "https://www.cftc.gov/files/dea/history/"
      ),
      user_agent = c("Googlebot", "Bingbot"),
      delay_between_requests = 2,
      timeout = 30,
      poll_interval = 0.5
    ),
    asset_classes = list(
      names = c("Currency_Pairs", "Commodity_Futures"),
      Currency_Pairs = list(
        source = "local",
        frequencies = c("daily", "hourly"),
        tickers = c("audcad", "eurusd", "gbpjpy"),
        other_tickers = c("gbpjpy")
      ),
      Commodity_Futures = list(
        source = c("local", "stooq"),
        frequencies = "daily",
        tickers = c("gc.f", "si.f")
      )
    ),
    directory_map = list(
      Currency_Pairs = list(
        daily = list(major = "major", other = "other"),
        hourly = list(major = "major", other = "other")
      ),
      Commodity_Futures = list(
        daily = list(stooq = "Scraped/Commodity_Futures")
      )
    ),
    data_format = list(
      local_hourly = c("date", "time", "open", "high", "low", "close"),
      local_daily = c("date", "time", "open", "high", "low", "close"),
      stooq_daily = c(
        "Date",
        "Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Open Interest"
      ),
      file_extension = ".csv",
      fut_extension = "_d.csv",
      date_format = "%Y%m%d",
      date_time_format = "%Y%m%d%H%M%S",
      column_mapping = list(
        hourly = list(
          date = "date",
          time = "time",
          open = "open",
          high = "high",
          low = "low",
          close = "close"
        ),
        daily = list(
          date = "date",
          time = "time",
          open = "open",
          high = "high",
          low = "low",
          close = "close"
        ),
        stooq = list(
          Date = "date_time",
          Open = "open",
          High = "high",
          Low = "low",
          Close = "close",
          Volume = "volume",
          `Open Interest` = "open_interest"
        )
      )
    ),
    cot_report = list(
      report_names = list(
        fut_disagg = "fut_disagg_txt",
        fut_fin = "fut_fin_txt",
        legacy = "deacot"
      ),
      output_folder = "cot",
      cache_dir = "cot_cache",
      cot_regex = "CFTC|conc|concentration|FutOnly|old|other|YYMMDD",
      start_year = 2000,
      start_year_overrides = list(
        fut_disagg = 2010,
        fut_fin = 2010
      )
    ),
    transformation = list(
      missing_value_fill = 0,
      pip_factor = list(
        regex_pattern = "jpy|_i|xau|xag|xpd|xpt|dx.f|cb.f|gc.f|pa.f|pl.f|si.f|\\.b$",
        value_for_match = 0.001,
        default_value = 0.00001
      ),
      aggregation = list(
        h1_to_h4 = "4 hours",
        h1_to_daily = "day",
        daily_to_weekly = "week",
        daily_to_monthly = "month",
        daily_to_quarterly = "quarter"
      )
    ),
    analysis = list(
      sessions = list(
        timezone_standard = "UTC",
        kill_zone_buffer_minutes = 30L,
        asian = list(
          name = "Asian Session",
          start = "00:00:00",
          end = "09:00:00"
        ),
        london = list(
          name = "London Session",
          standard_time = list(start = "08:00:00", end = "17:00:00"),
          daylight_time = list(start = "07:00:00", end = "16:00:00"),
          dst_transition_dates = list(
            start_dst = "last sunday of march",
            end_dst = "last sunday of october"
          ),
          kill_zones = list(
            london_open = list(
              start = "08:00:00",
              end = "11:00:00",
              priority = 1L
            )
          )
        ),
        newyork = list(
          name = "New York Session",
          standard_time = list(start = "13:00:00", end = "22:00:00"),
          daylight_time = list(start = "12:00:00", end = "21:00:00"),
          dst_transition_dates = list(
            start_dst = "second sunday of march",
            end_dst = "first sunday of november"
          ),
          kill_zones = list(
            ny_open = list(start = "13:00:00", end = "16:00:00", priority = 1L)
          )
        ),
        overlaps = list(
          london_ny = list(name = "London-NY Overlap", priority = 3L),
          asian_london = list(name = "Asian-London Overlap", priority = 4L)
        )
      ),
      trend_filter = list(
        weekly = list(lookback_period = 13L),
        daily = list(lookback_period = 21L),
        h4 = list(lookback_period = 12L),
        h1 = list(lookback_period = 12L)
      ),
      swing_filter = list(
        noise_filter = 0.92,
        sw_gap_bot = 0.20,
        sw_gap_top = 0.80,
        ob_filter = 0.618,
        obs = 4L
      ),
      choch_filter = list(choch_quant = 0.618),
      signal_filter = list(
        buffer_pipette = 50L,
        min_confluence = 2L,
        risk_pct = 0.02,
        lower_gap = 0.500,
        upper_gap = 0.999,
        allowed_alignments = c("bull_candidate", "bear_candidate"),
        min_risk_reward = 0.75,
        score_daily_overlap = TRUE
      ),
      backtest = list(
        max_holding_candles = "",
        trailing_stop = list(
          trigger_pips = 900L,
          step_pips = 300L
        )
      )
    ),
    paper_trading = list(
      currency = "GBP",
      starting_balance = 20.00,
      min_risk_pct = 1,
      max_risk_pct = 2,
      min_win_rate = 0.65,
      minimum_months = 4L,
      start_date = "2026-08-31"
    ),
    database = list(
      db_suffix = ".sqlite",
      table_prefixes = list(
        journal = "trading_journal",
        paper_trade = "paper_trading"
      ),
      index_column = c("date", "ticker"),
      pragma = list(
        journal_mode = "WAL",
        synchronous = "NORMAL"
      ),
      batch_size = 5000L,
      create_if_not_exists = TRUE
    ),
    pipeline = list(
      parallel = list(enabled = TRUE, max_workers = 4L, chunk_size = 100L),
      cache = list(enabled = TRUE, directory = "cache", ttl_hours = 24L),
      retry = list(max_attempts = 3L, backoff_seconds = c(1L, 5L, 10L))
    )
  )
}
