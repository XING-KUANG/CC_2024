options(stringsAsFactors = FALSE)

PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(PROJECT_ROOT, "run_all.R"))) {
  stop("Run the analysis from the repository root.")
}

RAW_DATA_DIR <- file.path(PROJECT_ROOT, "data", "raw")
DERIVED_DATA_DIR <- file.path(PROJECT_ROOT, "data", "derived")
FIGURE_OUTPUT_DIR <- file.path(PROJECT_ROOT, "outputs", "figures")
TABLE_OUTPUT_DIR <- file.path(PROJECT_ROOT, "outputs", "tables")
EXPORT_OUTPUTS <- identical(tolower(Sys.getenv("EXPORT_OUTPUTS", "false")), "true")

required_packages <- c(
  "dplyr", "ggplot2", "ggrepel", "maps", "patchwork", "readxl",
  "scales", "tibble", "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install the following R packages before running the analysis: ",
    paste(missing_packages, collapse = ", ")
  )
}

dir.create(DERIVED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
if (EXPORT_OUTPUTS) {
  dir.create(FIGURE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(TABLE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

require_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Required file(s) not found:\n", paste(missing, collapse = "\n"))
  }
  invisible(paths)
}

export_plot <- function(filename, plot, width, height, dpi = 600) {
  if (EXPORT_OUTPUTS) {
    ggplot2::ggsave(
      filename = file.path(FIGURE_OUTPUT_DIR, filename), plot = plot,
      width = width, height = height, dpi = dpi, bg = "white"
    )
  }
  invisible(plot)
}

export_table <- function(filename, object) {
  if (EXPORT_OUTPUTS) {
    utils::write.csv(
      object, file.path(TABLE_OUTPUT_DIR, filename), row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  invisible(object)
}

