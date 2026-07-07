#' Create a SeRiouS capsule folder skeleton
#'
#' @param path Path where the capsule folder should be created.
#' @param title Capsule title used in the generated files.
#' @param overwrite Logical. If `TRUE`, existing template files may be overwritten.
#'
#' @return Invisibly returns the normalized path.
#' @family main user functions
#' @export
create_capsule_skeleton <- function(path,
                                    title = "My SeRiouS capsule",
                                    overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1) {
    stop("'path' must be a single character string.", call. = FALSE)
  }

  if (!is.character(title) || length(title) != 1) {
    stop("'title' must be a single character string.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }

  if (dir.exists(path) && !isTRUE(overwrite)) {
    existing_files <- list.files(path, all.files = FALSE, no.. = TRUE)

    if (length(existing_files) > 0) {
      stop(
        "Directory already exists and is not empty. ",
        "Use overwrite = TRUE or choose another path.",
        call. = FALSE
      )
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  resource_dirs <- c("cells", "data", "pdf", "img", "www")
  for (dir in resource_dirs) {
    dir.create(file.path(path, dir), recursive = TRUE, showWarnings = FALSE)
  }

  readme_file <- file.path(path, "README.md")
  yml_file <- file.path(path, "serious.yml")
  css_file <- file.path(path, "www", "custom.css")

  if (!file.exists(yml_file) || isTRUE(overwrite)) {
    writeLines(
      c(
        "id: my_capsule",
        paste0("title: \"", title, "\""),
        "subtitle: \"A shareable SeRiouS capsule folder\"",
        "method: \"Introduction R\"",
        "description: \"This capsule demonstrates the basic SeRiouS workflow.\"",
        "language: en",
        "type: serious_capsule",
        "version: 0.2.0",
        "start_cell: intro",
        "",
        "packages:",
        "  - datasets",
        "  - graphics",
        "  - stats",
        "",
        "sections:",
        "  - id: data",
        "    label: \"Data\"",
        "    color: \"#E3F2FD\"",
        "    border: \"#1565C0\"",
        "  - id: summary",
        "    label: \"Summary\"",
        "    color: \"#E8F5E9\"",
        "    border: \"#2E7D32\"",
        "  - id: visual",
        "    label: \"Visualisation\"",
        "    color: \"#FFF3E0\"",
        "    border: \"#EF6C00\"",
        "  - id: conclusion",
        "    label: \"Conclusion\"",
        "    color: \"#F3E5F5\"",
        "    border: \"#6A1B9A\""
      ),
      yml_file
    )
  }

  if (!file.exists(css_file) || isTRUE(overwrite)) {
    writeLines(
      c(
        "/* Optional custom CSS for this SeRiouS capsule. */",
        "",
        ".serious-board-legend {",
        "  margin-bottom: 10px;",
        "}",
        "",
        ".legend-item {",
        "  display: inline-block;",
        "  margin-right: 14px;",
        "  margin-bottom: 6px;",
        "}",
        "",
        ".legend-swatch {",
        "  display: inline-block;",
        "  width: 18px;",
        "  height: 12px;",
        "  border: 1px solid #555;",
        "  margin-right: 5px;",
        "  vertical-align: middle;",
        "  border-radius: 3px;",
        "}"
      ),
      css_file
    )
  }

  capsule_add_cell(
    path,
    id = "intro",
    title = "Discover the data",
    section = "data",
    x = 0,
    y = 0,
    objective = "Display the first rows of the iris dataset.",
    content = paste(
      "This first cell introduces the dataset used in the capsule.",
      "Run the code and answer the unlock question to continue.",
      sep = "\n\n"
    ),
    code = "head(iris)",
    outputs = "console",
    question = "Which dataset is used in this capsule?",
    expected_answer = "iris",
    next_cells = "summary",
    overwrite = overwrite
  )

  capsule_add_cell(
    path,
    id = "summary",
    title = "Summarise the data",
    section = "summary",
    x = 250,
    y = 0,
    objective = "Produce a simple statistical summary of the iris dataset.",
    content = paste(
      "This cell asks you to compute a basic summary of the data.",
      "The goal is to inspect the variables before producing a graph.",
      sep = "\n\n"
    ),
    code = "summary(iris)",
    outputs = "console",
    question = "Which R function gives a simple summary of an object?",
    expected_answer = "summary",
    next_cells = "plot",
    overwrite = overwrite
  )

  capsule_add_cell(
    path,
    id = "plot",
    title = "Create a plot",
    section = "visual",
    x = 500,
    y = 0,
    objective = "Plot sepal length against petal length.",
    content = paste(
      "This cell produces a simple exploratory graph.",
      "The graph is shown in the plot output area.",
      sep = "\n\n"
    ),
    code = paste(
      "plot(",
      "  iris$Sepal.Length,",
      "  iris$Petal.Length,",
      "  pch = 16,",
      "  xlab = 'Sepal.Length',",
      "  ylab = 'Petal.Length',",
      "  main = 'Iris: sepal length and petal length'",
      ")",
      sep = "\n"
    ),
    outputs = c("console", "plot"),
    question = "Which base R function creates this graph?",
    expected_answer = "plot",
    next_cells = "conclusion",
    overwrite = overwrite
  )

  capsule_add_cell(
    path,
    id = "conclusion",
    title = "Conclude",
    section = "conclusion",
    x = 750,
    y = 0,
    objective = "Summarise what has been done in the capsule.",
    content = paste(
      "This final cell concludes the capsule.",
      "You have inspected, summarised and visualised the iris dataset.",
      sep = "\n\n"
    ),
    code = "cat('The iris dataset has been inspected, summarised and visualised.\\n')",
    outputs = "console",
    question = "What was the dataset used in this capsule?",
    expected_answer = "iris",
    next_cells = character(),
    overwrite = overwrite
  )

  if (!file.exists(readme_file) || isTRUE(overwrite)) {
    writeLines(
      c(
        paste0("# ", title),
        "",
        "This folder contains a SeRiouS learning capsule.",
        "",
        "## What is inside?",
        "",
        "- `serious.yml`: capsule metadata, sections, legend and global settings.",
        "- `cells/`: one YAML file per board cell.",
        "- `data/`: optional datasets.",
        "- `pdf/`: optional PDF resources.",
        "- `img/`: optional images.",
        "- `www/`: optional web resources and custom CSS.",
        "",
        "## How to run this capsule",
        "",
        "From R, run:",
        "",
        "```r",
        "library(SeRiouS)",
        "run_capsule(\".\")",
        "```",
        "",
        "Or, from the parent directory:",
        "",
        "```r",
        "library(SeRiouS)",
        paste0("run_capsule(\"", basename(normalizePath(path, mustWork = FALSE)), "\")"),
        "```",
        "",
        "## How to inspect the cells",
        "",
        "```r",
        "library(SeRiouS)",
        "capsule_cells(\".\")",
        "capsule_get_cell(\".\", \"intro\")",
        "```",
        "",
        "## How to edit this capsule",
        "",
        "You can edit the YAML files in `cells/` directly, or use helper functions:",
        "",
        "```r",
        "library(SeRiouS)",
        "",
        "capsule_add_cell(",
        "  \".\",",
        "  id = \"new_cell\",",
        "  title = \"A new cell\",",
        "  section = \"summary\",",
        "  x = 1000,",
        "  y = 0,",
        "  content = \"Write your pedagogical content here.\",",
        "  code = \"1 + 1\",",
        "  question = \"What is 1 + 1?\",",
        "  expected_answer = \"2\"",
        ")",
        "",
        "capsule_connect_cells(",
        "  \".\",",
        "  from = \"conclusion\",",
        "  to = \"new_cell\"",
        ")",
        "```",
        "",
        "## How to check before sharing",
        "",
        "```r",
        "library(SeRiouS)",
        "check_capsule_dir(\".\")",
        "```",
        "",
        "If the check is successful, you can zip this folder and share it.",
        "",
        "## Important convention",
        "",
        "A SeRiouS capsule is composed of cells connected by links.",
        "Each cell is stored as a YAML file in `cells/`."
      ),
      readme_file
    )
  }

  invisible(normalizePath(path, mustWork = TRUE))
}

#' Load a SeRiouS capsule from a folder
#'
#' @param path Path to a capsule folder.
#'
#' @return A `"learning_capsule"` object.
#' @family main user functions
#' @export
load_capsule_dir <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("'path' must be a single character string.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    stop("Capsule directory does not exist: ", path, call. = FALSE)
  }

  capsule_dir <- normalizePath(path, mustWork = TRUE)

  # New cell-based format.
  cells_dir <- file.path(capsule_dir, "cells")
  has_cell_files <- dir.exists(cells_dir) &&
    length(list.files(cells_dir, pattern = "\\.ya?ml$", full.names = TRUE)) > 0

  if (isTRUE(has_cell_files)) {
    return(serious_load_capsule_cells_dir(capsule_dir))
  }

  # Legacy capsule.R format.
  capsule_file <- file.path(capsule_dir, "capsule.R")

  if (!file.exists(capsule_file)) {
    stop("No 'capsule.R' file found in: ", path, call. = FALSE)
  }

  env <- new.env(parent = globalenv())
  env$capsule_dir <- capsule_dir

  sys.source(capsule_file, envir = env)

  if (!exists("create_capsule", envir = env, inherits = FALSE)) {
    stop(
      "'capsule.R' must define a function named create_capsule().",
      call. = FALSE
    )
  }

  create_capsule <- get("create_capsule", envir = env)

  if (!is.function(create_capsule)) {
    stop("'create_capsule' must be a function.", call. = FALSE)
  }

  capsule <- create_capsule()

  if (!inherits(capsule, "learning_capsule")) {
    stop(
      "create_capsule() must return a learning_capsule object.",
      call. = FALSE
    )
  }

  validate_capsule(capsule, parse_code = FALSE, verbose = FALSE)

  if (is.null(capsule$resources)) {
    capsule$resources <- list()
  }

  capsule$resources$capsule_dir <- capsule_dir

  capsule
}

#' Run a SeRiouS capsule from a folder
#'
#' @param path Path to a capsule folder.
#'
#' @return Launches a Shiny application.
#' @export
run_capsule_dir <- function(path) {
  capsule <- load_capsule_dir(path)
  run_learning_capsule(capsule)
}

#' Check a SeRiouS capsule folder
#'
#' @param path Path to a capsule folder.
#' @param verbose Logical. If `TRUE`, prints diagnostic messages.
#'
#' @return Invisibly returns `TRUE` if the capsule folder is valid.
#' @family main user functions
#' @export
check_capsule_dir <- function(path, verbose = TRUE) {
  if (!is.character(path) || length(path) != 1) {
    stop("'path' must be a single character string.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    stop("Capsule directory does not exist: ", path, call. = FALSE)
  }

  capsule_dir <- normalizePath(path, mustWork = TRUE)

  capsule_file <- file.path(capsule_dir, "capsule.R")
  cells_dir <- file.path(capsule_dir, "cells")

  has_capsule_file <- file.exists(capsule_file)
  has_cell_files <- dir.exists(cells_dir) &&
    length(list.files(cells_dir, pattern = "\\.ya?ml$", full.names = TRUE)) > 0

  if (!has_capsule_file && !has_cell_files) {
    stop(
      "Invalid SeRiouS capsule folder. Expected either:\n",
      "- a legacy 'capsule.R' file, or\n",
      "- a 'cells/' folder containing .yml files.",
      call. = FALSE
    )
  }

  yml_file <- file.path(capsule_dir, "serious.yml")

  if (!file.exists(yml_file) && isTRUE(verbose)) {
    message("Optional file missing: serious.yml")
  }

  expected_dirs <- c("data", "pdf", "img", "www")
  missing_dirs <- expected_dirs[!dir.exists(file.path(capsule_dir, expected_dirs))]

  if (length(missing_dirs) > 0 && isTRUE(verbose)) {
    message(
      "Optional resource directories are missing: ",
      paste(missing_dirs, collapse = ", ")
    )
  }

  capsule <- load_capsule_dir(capsule_dir)

  validate_capsule(
    capsule,
    parse_code = FALSE,
    verbose = FALSE
  )

  pdf_fields <- c("pdf", "pdf_on_run")

  for (step in capsule$steps) {
    for (field in pdf_fields) {
      pdf_path <- step[[field]]

      if (is.null(pdf_path)) {
        next
      }

      if (!is.character(pdf_path) ||
          length(pdf_path) != 1 ||
          !nzchar(pdf_path)) {
        stop(
          "Step '", step$id, "' has an invalid '", field, "' field. ",
          "It must be a single non-empty character string.",
          call. = FALSE
        )
      }

      pdf_path <- gsub("\\\\", "/", pdf_path)

      if (grepl("^https?://", pdf_path)) {
        next
      }

      if (!grepl("\\.pdf$", pdf_path, ignore.case = TRUE)) {
        stop(
          "Step '", step$id, "' refers to a file that does not look like a PDF ",
          "in field '", field, "': ", pdf_path,
          call. = FALSE
        )
      }

      is_absolute_path <- grepl("^/", pdf_path) ||
        grepl("^[A-Za-z]:/", pdf_path)

      full_pdf_path <- if (is_absolute_path) {
        pdf_path
      } else {
        file.path(capsule_dir, pdf_path)
      }

      if (!file.exists(full_pdf_path)) {
        stop(
          "Step '", step$id, "' refers to a missing PDF file in field '",
          field, "': ", pdf_path,
          call. = FALSE
        )
      }
    }
  }

  if (isTRUE(verbose)) {
    message("Capsule directory is valid: ", capsule_dir)
    message("Capsule format: ", if (has_cell_files) "cell-based" else "legacy capsule.R")
    message("Capsule id: ", capsule$id)
    message("Capsule title: ", capsule$title)
    message("Number of cells: ", length(capsule$steps))
    message("Start cell: ", capsule$start_step)
  }

  invisible(TRUE)
}
