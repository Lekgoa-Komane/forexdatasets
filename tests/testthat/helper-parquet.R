# tests/testthat/helper-parquet.R
# Shared by the transform tests (testthat loads every file called helper*.R).
# Writes a fake "old" parquet file into the TEMPORARY test pantry that
# make_valid_config() creates. It never touches your real data folder.

write_fake_parquet <- function(config, timeframe, asset_class, ticker, dt) {
  path <- build_data_path(config, timeframe, asset_class, ticker)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(dt, path)
  invisible(path)
}

