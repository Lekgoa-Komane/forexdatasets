# ---------------------------------------------------------------------------
# Suite-wide Chrome detritus cleanup.
# Snapshots tempdir() now (suite start) and, at suite teardown, removes any
# com.google.Chrome.* / chromium scratch dirs that appeared during the run.
# Must live at top level (not inside a function) so it registers once.
# ---------------------------------------------------------------------------
local({
  before <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)

  withr::defer(
    {
      if (requireNamespace("chromote", quietly = TRUE)) {
        try(
          if (chromote::has_default_chromote_object()) {
            chromote::default_chromote_object()$close()
          },
          silent = TRUE
        )
      }

      Sys.sleep(1)

      after <- list.files(tempdir(), all.files = TRUE, no.. = TRUE)
      new_items <- setdiff(after, before)
      chrome_items <- grep(
        "^(com\\.google\\.Chrome\\.|org\\.chromium\\.Chromium|scoped_dir)",
        new_items,
        value = TRUE
      )
      if (length(chrome_items) > 0) {
        unlink(
          file.path(tempdir(), chrome_items),
          recursive = TRUE,
          force = TRUE
        )
      }
    },
    envir = testthat::teardown_env()
  )
})
