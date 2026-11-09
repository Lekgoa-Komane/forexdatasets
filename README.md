# ForexDatasets: Financial Data Ingestion, Transformation, and Storage Pipeline

`forexdatasets` is a robust, production-grade R pipeline designed to fetch, 
transform, clean, and store multi-frequency financial market data (Foreign 
Exchange, Commodities, Indices, Metals, and Futures) into high-performance 
Apache Parquet formats. Operating as the foundational "pantry" of a quantitative
 trading system, the package ensures rigorous type safety, UTC alignment, 
deduplication, and schema validation across local disk partitions and live 
web sources.

---

## Architecture Overview

The pipeline follows a structured, modular ETL (Extract, Transform, Load) 
design pattern divided into independent functional stages:

```
[Configuration & Metadata] ──> [Extraction (Local / Web Scraper)] ──> 
[Transformation & Aggregation] ──> [Storage (Parquet / Pantry)]
```

1. **Configuration (`01_config.R`)**: Validates environment parameters, 
2. directory structures, asset definitions, and mapping formats upfront to 
3. prevent downstream pipeline crashes. It constructs a centralized ticker 
4. metadata matrix.
2. **Extraction (`02_extract.R`)**: Handles multi-source data acquisition. 
3. Hourly records are loaded locally, while daily records dynamically route 
4. between local partitions or web scrapers (`chromote`/`polite`) depending on 
5. the designated data source.
3. **Data Transformation & Update (`03_dataupadate.R`)**: Performs strict UTC 
4. date conversion, deduplication, anti-joins against existing parquet 
5. histories, and time-series aggregation (generating 4-hour, weekly, monthly, 
6. and quarterly candles).
4. **CFTC COT Pipeline (`04_cot_report.R`)**: Automates the downloading, 
5. unpacking, standardizing, and caching of Commitments of Traders (COT) report 
6. archives from the CFTC.
5. **Persistence & Access (`05_save_and-get.R`)**: Implements atomic write 
6. patterns (`.tmp` swap to prevent file corruption) using Apache Arrow Parquet 
7. files, alongside retrieval functions (`get_dataset`) featuring advanced 
8. filtering options.

---

## Data Extraction Strategy: Hybrid Local & Web Scraping

Data ingestion combines high-frequency local files with dynamic web extraction. 
A critical consideration in this pipeline is the handling of **Stooq** 
data sources.

### The Stooq Challenge & Manual Workflow
* **The Restriction**: Stooq (`https://stooq.com/`) enforces strict security 
* policies, rate limits, and anti-scraping mechanisms (including blocking 
* automated script access or enforcing CAPTCHAs on rapid programmatic queries). 
* Attempts to write direct automated scraping loops for bulk historical files 
* typically result in IP blocks or connection drops.
* **The Solution**: Bulk point-in-time snapshots and daily datasets from Stooq 
* (`https://stooq.com/db/h/`) are **manually downloaded** and placed into the 
* local directory base (`config$paths$local_base`). 
* **Pipeline Integration**: Once populated locally, the extraction modules 
* (`extract_local_daily`) seamlessly parse, validate, and convert these manual 
* downloads into clean, standardized structures, bypassing live network 
* restrictions while retaining access to deep historical series.
* **Alternative Web Scrapers**: For sources permitting polite scraping, 
* `web_scraper()` utilizes `polite` (respecting `robots.txt` rules with 
* controlled delays) alongside headless Google Chrome sessions via `chromote` 
* with stealth modifications (overriding `navigator.webdriver` flags) to safely 
* scrape rendered HTML tables from supported web endpoints.

---

## Key Modules & Core Functions

### 1. Configuration & Metadata (`01_config.R`)
* `load_config(config_path)`: Reads the YAML configuration file and runs strict 
* validation checks across top-level sections (`paths`, `web`, `asset_classes`, 
* `directory_map`, `data_format`, `transformation`, `analysis`, `database`, 
* `pipeline`).
* `ticker_metadata(config)`: Expands asset classes, frequencies, and source 
* mappings into an exhaustive, unique ticker-level configuration table.

### 2. Extraction Engine (`02_extract.R`)
* `extract_hourly_data(ticker_row, source_paths, config)`: Reads raw hourly 
* feeds, applies column mappings, forces UTC timestamps, and filters columns.
* `extract_daily_data(ticker_row, sources, config)`: Intelligent router 
* directing execution to `extract_local_daily()` or `web_scraper()` based on 
* the asset's defined `data_source`.
* `web_scraper(...)`: Manages headless browser sessions with user-agent 
* spoofing and DOM element tracking to extract daily tables.

### 3. Update & Timeframe Aggregation (`03_dataupadate.R`)
* `update_h1_data(...)` / `update_daily_data(...)`: Preloads existing parquet
*  partitions, performs global anti-joins to isolate fresh records, cleans 
* duplicates, and merges data while protecting time-series continuity.
* `aggregate_hourly_to_4hour(...)`: Summarizes hourly ticks into aligned 4-hour 
* OHLC candles.
* `aggregate_daily_to_weekly(...)`, `aggregate_daily_to_monthly(...)`, 
* `aggregate_daily_to_quarterly(...)`: Compresses daily records into weekly, 
* monthly, and quarterly candles with weekend-adjustment logic for period-start 
* boundaries.

### 4. CFTC Commitments of Traders (`04_cot_report.R`)
* `cot_pipeline(config)`: Downloads annual ZIP archives of Legacy, 
* Disaggregated, and Traders in Financial Futures reports from the CFTC website.
* `clean_cot_dataset(...)`: Sanitizes raw column headers, strips unwanted 
* prefixes, and standardizes market exchange nomenclature across legacy formats.

### 5. Storage & Serving Layer (`05_save_and-get.R`)
* `save_dataset(dt, config)` / `save_cot_dataset(cot_list, config)`: Implements 
* atomic file writing by outputting data to temporary `.tmp` files before 
* renaming them into place, guaranteeing resilience against system crashes or 
* partial writes.
* `get_dataset(...)`: Serves data back out of the parquet pantry with flexible 
* filtering options (`ticker`, `asset_class`, `from`, `to`, `columns`).

---

## Installation & Requirements

### Prerequisites
* R (version 4.0 or higher recommended)
* Required R Packages:
  * `data.table` (High-performance data manipulation)
  * `arrow` (Parquet file reading and writing)
  * `yaml` (Configuration parsing)
  * `fs` (File system operations)
  * `logger` (Logging and diagnostics)
  * `polite` & `robotstxt` (Polite web interaction)
  * `rvest` (HTML parsing)
  * `chromote` (Headless browser automation)
  * `lubridate` (Date-time manipulation)

### Quick Start Example

```r
library(forexdatasets)

# 1. Load and validate pipeline configuration
config <- load_config("path/to/config.yml")

# 2. Build metadata mapping
meta <- ticker_metadata(config)

# 3. Extract and update daily data for a specific asset row
sample_row <- meta[ticker == "EURUSD" & frequency == "daily"][1]
raw_data <- extract_daily_data(sample_row, sample_row$source_path, config)
updated_data <- update_daily_data(raw_data, config)

# 4. Save processed data to the Parquet pantry
save_dataset(updated_data, config)

# 5. Retrieve data back from the pantry with date filtering
eurusd_history <- get_dataset(
  timeframe = "daily",
  ticker = "EURUSD",
  asset_class = "Currency_Pairs",
  from = "2023-01-01",
  to = "2026-01-01",
  config = config
)

head(eurusd_history)
```

---

## Error Handling & Logging Best Practices

* **Fail-Fast Configuration**: Configuration errors are caught immediately upon 
* startup via `load_config()`, preventing long-running batch jobs from failing 
* halfway through execution due to typos or missing paths.
* **Comprehensive Audit Trails**: The `logger` package logs debug info, 
* execution warnings, and success metrics at every phase of data ingestion and 
* transformation.
* **Safe Caching**: COT reports and Stooq archives utilize year-stamped `.rds` 
* caching and local file registries to minimize redundant network calls while 
* ensuring historical integrity.
