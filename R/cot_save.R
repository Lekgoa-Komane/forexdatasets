#' Build the file path of a saved COT report (the COT "one map")
#'
#' Every function that saves or reads a COT report should use this helper, so
#' the saver and the readers can never disagree about where a file lives:
#'
#'   output_base / cot / report_key.parquet
#'
#' The folder name is `cot` unless `config$cot_report$output_folder` says
#' otherwise.
#'
#' @param config The config list from `load_config()`. Needs
#'   `paths$output_base`.
#' @param report_key One or more report keys, for example "legacy". These are
#'   the names of the list returned by [cot_pipeline()].
#'
#' @return A character vector of file paths, one per report key.
#' @export
build_cot_path <- function(config, report_key) {
  if (
    !is.character(report_key) ||
      length(report_key) == 0L ||
      anyNA(report_key) ||
      any(!nzchar(report_key))
  ) {
    stop(
      "`report_key` must be one or more non-empty text values.",
      call. = FALSE
    )
  }

  output_base <- config$paths$output_base
  if (is.null(output_base)) {
    stop("Config is missing `paths$output_base`.", call. = FALSE)
  }

  folder <- config$cot_report$output_folder
  if (is.null(folder)) {
    folder <- "cot"
  }

  file.path(output_base, folder, paste0(report_key, ".parquet"))
}


#' Save the COT reports to parquet, one file per report type
#'
#' The "packer" for COT data. It takes the named list returned by
#' [cot_pipeline()] and stores each report in its own parquet file, using the
#' same safe pattern as [save_dataset()]: write to a temporary file first,
#' then swap it into place, so a crash never leaves a half-written file.
#'
#' Two safety rules:
#' * An EMPTY report (for example when every download failed) is skipped, and
#'   any file saved earlier is left untouched. It is never overwritten with
#'   nothing.
#' * Each report must have a `Report_Date_as_YYYY_MM_DD` column of class Date.
#'
#' @param cot_list A named list of data.tables, as returned by
#'   [cot_pipeline()].
#' @param config The config list from `load_config()`.
#'
#' @return Invisibly, the file paths that were written.
#' @export
save_cot_dataset <- function(cot_list, config) {
  logger::log_info("Starting to save COT reports")

  # ---- 1. Safety checks ----
  if (!is.list(cot_list) || is.data.frame(cot_list) || length(cot_list) == 0L) {
    stop(
      "`cot_list` must be a non-empty named list of data.tables.",
      call. = FALSE
    )
  }

  keys <- names(cot_list)
  if (
    is.null(keys) ||
      anyNA(keys) ||
      any(!nzchar(keys)) ||
      anyDuplicated(keys) > 0L
  ) {
    stop(
      "`cot_list` must have a unique, non-empty name for every report.",
      call. = FALSE
    )
  }

  not_dt <- keys[!vapply(cot_list, data.table::is.data.table, logical(1L))]
  if (length(not_dt) > 0L) {
    stop(
      "These reports are not data.tables: ",
      paste(not_dt, collapse = ", "),
      call. = FALSE
    )
  }

  date_col <- "Report_Date_as_YYYY_MM_DD"

  # ---- 2. Work out every address ONCE, before writing anything ----
  paths <- build_cot_path(config, keys)
  names(paths) <- keys

  # ---- 3. Pack each report safely ----
  saved <- character()

  for (key in keys) {
    dt <- cot_list[[key]]

    if (nrow(dt) == 0L) {
      logger::log_warn(paste0(
        "COT report '",
        key,
        "' is empty. Skipped; any saved file is left as it is."
      ))
      next
    }

    if (!date_col %in% names(dt)) {
      stop(
        "COT report '",
        key,
        "' is missing column `",
        date_col,
        "`.",
        call. = FALSE
      )
    }
    if (!inherits(dt[[date_col]], "Date")) {
      stop(
        "Column `",
        date_col,
        "` of COT report '",
        key,
        "' must be a Date.",
        call. = FALSE
      )
    }

    # Sort by date on a copy, so the caller's table is never changed
    dt <- data.table::copy(dt)
    data.table::setorderv(dt, date_col)

    final <- paths[[key]]
    dir.create(dirname(final), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(final, ".tmp")

    tryCatch(
      {
        arrow::write_parquet(dt, tmp, compression = "zstd")
        if (!file.rename(tmp, final)) {
          stop("Could not move the temporary file into place: ", final)
        }
      },
      error = function(e) {
        unlink(tmp)
        logger::log_error(paste0(
          "Saving COT report '",
          key,
          "' failed: ",
          conditionMessage(e)
        ))
        stop(e)
      }
    )

    saved <- c(saved, final)
  }

  logger::log_info(paste0("Saved ", length(saved), " COT report(s)"))
  invisible(unname(saved))
}
