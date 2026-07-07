#' Write a SeRiouS capsule as a folder
#'
#' `write_capsule_dir()` writes a SeRiouS capsule as a shareable cell-based
#' capsule folder. The resulting folder contains a `serious.yml` file and one
#' YAML file per pedagogical cell in `cells/`.
#'
#' @param capsule A capsule directory, a built-in capsule name, or a
#'   `"learning_capsule"` object.
#' @param path Output directory.
#' @param overwrite Logical. If `TRUE`, overwrite an existing non-empty
#'   directory.
#' @param include_resources Logical. If `TRUE`, copy static resource folders
#'   (`data`, `pdf`, `img`, `www`) when the source capsule directory is known.
#'
#' @return Invisibly returns the normalized output directory.
#'
#' @seealso [run_capsule()], [create_capsule_skeleton()],
#'   [check_capsule_dir()], [write_capsule_app()]
#'
#' @family main user functions
#' @export
#'
#' @examples
#' if (interactive()) {
#'   write_capsule_dir("demo_iris", "demo_iris_folder", overwrite = TRUE)
#'   run_capsule("demo_iris_folder")
#' }
write_capsule_dir <- function(capsule,
                              path,
                              overwrite = FALSE,
                              include_resources = TRUE) {
  if (missing(capsule)) {
    stop(
      "Please provide a capsule directory, a built-in capsule name, ",
      "or a learning_capsule object.",
      call. = FALSE
    )
  }

  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(include_resources) || length(include_resources) != 1) {
    stop("'include_resources' must be TRUE or FALSE.", call. = FALSE)
  }

  capsule <- serious_as_learning_capsule(capsule, arg = "capsule")

  validate_capsule(
    capsule,
    parse_code = FALSE,
    check_packages = FALSE,
    verbose = FALSE
  )

  source_dir <- serious_write_capsule_dir_source_dir(capsule)
  target_dir <- normalizePath(path, mustWork = FALSE)

  if (!is.null(source_dir) &&
      identical(
        normalizePath(source_dir, mustWork = TRUE),
        target_dir
      )) {
    stop(
      "'path' cannot be the source capsule directory. ",
      "Choose a separate output directory.",
      call. = FALSE
    )
  }

  if (dir.exists(path)) {
    existing_files <- list.files(path, all.files = TRUE, no.. = TRUE)

    if (length(existing_files) > 0 && !isTRUE(overwrite)) {
      stop(
        "'path' already exists and is not empty. ",
        "Use overwrite = TRUE or choose another directory.",
        call. = FALSE
      )
    }

    if (isTRUE(overwrite)) {
      unlink(path, recursive = TRUE, force = TRUE)
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  resource_dirs <- c("cells", "data", "pdf", "img", "www")
  for (resource_dir in resource_dirs) {
    dir.create(
      file.path(path, resource_dir),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (isTRUE(include_resources) && !is.null(source_dir)) {
    serious_write_capsule_dir_copy_resources(
      source_dir = source_dir,
      target_dir = path
    )
  }

  serious_write_capsule_dir_metadata(
    capsule = capsule,
    path = path
  )

  serious_write_capsule_dir_cells(
    capsule = capsule,
    path = path
  )

  serious_write_capsule_dir_readme(
    capsule = capsule,
    path = path
  )

  invisible(normalizePath(path, mustWork = TRUE))
}


serious_write_capsule_dir_source_dir <- function(capsule) {
  if (!is.null(capsule$resources) &&
      !is.null(capsule$resources$capsule_dir) &&
      is.character(capsule$resources$capsule_dir) &&
      length(capsule$resources$capsule_dir) == 1 &&
      nzchar(capsule$resources$capsule_dir) &&
      dir.exists(capsule$resources$capsule_dir)) {
    return(normalizePath(capsule$resources$capsule_dir, mustWork = TRUE))
  }

  NULL
}


serious_write_capsule_dir_copy_resources <- function(source_dir, target_dir) {
  resource_dirs <- c("data", "pdf", "img", "www")

  for (resource_dir in resource_dirs) {
    source_resource_dir <- file.path(source_dir, resource_dir)
    target_resource_dir <- file.path(target_dir, resource_dir)

    if (!dir.exists(source_resource_dir)) {
      next
    }

    files <- list.files(
      source_resource_dir,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )

    if (length(files) == 0) {
      next
    }

    dir.create(
      target_resource_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    ok <- file.copy(
      from = files,
      to = target_resource_dir,
      recursive = TRUE,
      overwrite = TRUE,
      copy.date = FALSE
    )

    if (any(!ok)) {
      warning(
        "Some resource files could not be copied from '",
        source_resource_dir,
        "'.",
        call. = FALSE
      )
    }
  }

  invisible(target_dir)
}


serious_write_capsule_dir_metadata <- function(capsule, path) {
  metadata <- serious_drop_null(list(
    id = capsule$id %||% basename(normalizePath(path, mustWork = FALSE)),
    title = capsule$title %||% capsule$id %||% "SeRiouS capsule",
    subtitle = capsule$subtitle %||% NULL,
    method = capsule$method %||% "SeRiouS capsule",
    description = capsule$description %||% NULL,
    language = capsule$language %||% "en",
    type = capsule$type %||% "serious_capsule",
    version = capsule$version %||% "0.2.0",
    start_cell = capsule$start_step %||% capsule$start_cell %||% NULL,
    packages = serious_write_capsule_dir_packages(capsule),
    sections = serious_write_capsule_dir_sections(capsule)
  ))

  yaml::write_yaml(
    metadata,
    file.path(path, "serious.yml")
  )

  invisible(metadata)
}


serious_write_capsule_dir_packages <- function(capsule) {
  packages <- capsule$packages %||% character()

  packages <- as.character(packages)
  packages <- packages[!is.na(packages) & nzchar(packages)]
  packages <- unique(packages)

  if (length(packages) == 0) {
    return(NULL)
  }

  packages
}


serious_write_capsule_dir_sections <- function(capsule) {
  sections <- capsule$sections %||% NULL

  if (is.data.frame(sections) && nrow(sections) > 0) {
    out <- lapply(seq_len(nrow(sections)), function(i) {
      row <- as.list(sections[i, , drop = FALSE])
      row <- lapply(row, serious_write_capsule_dir_clean_value)
      serious_drop_null(row)
    })

    names(out) <- NULL

    return(out)
  }

  section_ids <- vapply(
    capsule$steps %||% list(),
    function(step) {
      step$section %||% step$partie %||% NA_character_
    },
    character(1)
  )

  section_ids <- unique(section_ids[!is.na(section_ids) & nzchar(section_ids)])

  if (length(section_ids) == 0) {
    return(NULL)
  }

  lapply(section_ids, function(section_id) {
    list(
      id = section_id,
      label = serious_board_section_label(section_id),
      color = serious_board_section_color(section_id),
      border = "#666666"
    )
  })
}


serious_write_capsule_dir_cells <- function(capsule, path) {
  steps <- capsule$steps %||% list()

  if (!is.list(steps) || length(steps) == 0) {
    stop("The capsule does not contain any steps/cells.", call. = FALSE)
  }

  next_cells_by_id <- serious_write_capsule_dir_next_cells(capsule)

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    step_id <- step$id %||% names(steps)[[i]] %||% paste0("cell_", i)

    serious_check_cell_id(step_id)

    position <- serious_write_capsule_dir_position(
      capsule = capsule,
      step = step,
      id = step_id
    )

    next_cells <- next_cells_by_id[[step_id]] %||%
      serious_as_character_vector(step$next_steps %||% step$next_cells)

    cell <- serious_drop_null(list(
      id = step_id,
      title = step$title %||% step_id,
      section = step$section %||% step$partie %||% NULL,
      x = position$x %||% NULL,
      y = position$y %||% NULL,
      objective = step$objective %||% NULL,
      text = step$text %||% NULL,
      content = step$content %||% step$text %||% NULL,
      code = step$code %||% NULL,
      code_display = step$code_display %||% NULL,
      outputs = step$outputs %||% NULL,
      expected_output = step$expected_output %||% NULL,
      concepts = step$concepts %||% NULL,
      question = step$question %||% NULL,
      expected_answer = step$expected_answer %||% NULL,
      case_sensitive = step$case_sensitive %||% NULL,
      success = step$success %||% NULL,
      failure = step$failure %||% NULL,
      pdf = step$pdf %||% NULL,
      pdf_on_run = step$pdf_on_run %||% NULL,
      next_cells = serious_as_character_vector(next_cells)
    ))

    class(cell) <- c("serious_cell", class(cell))

    serious_write_cell(
      cell,
      serious_cell_file(path, step_id)
    )
  }

  invisible(path)
}


serious_write_capsule_dir_next_cells <- function(capsule) {
  edges <- tryCatch(
    capsule_edges(capsule),
    error = function(e) NULL
  )

  if (is.null(edges) ||
      !is.data.frame(edges) ||
      !all(c("from", "to") %in% names(edges)) ||
      nrow(edges) == 0) {
    return(list())
  }

  from <- as.character(edges$from)
  to <- as.character(edges$to)

  ok <- !is.na(from) & nzchar(from) & !is.na(to) & nzchar(to)

  if (!any(ok)) {
    return(list())
  }

  split(to[ok], from[ok])
}


serious_write_capsule_dir_position <- function(capsule, step, id) {
  x <- step$x %||% NULL
  y <- step$y %||% NULL

  if (!is.null(x) || !is.null(y)) {
    return(list(x = x, y = y))
  }

  layout <- capsule$layout %||% NULL

  if (is.data.frame(layout) && nrow(layout) > 0) {
    row_index <- NULL

    if ("id" %in% names(layout)) {
      row_index <- match(id, as.character(layout$id))
    } else if (!is.null(rownames(layout))) {
      row_index <- match(id, rownames(layout))
    }

    if (!is.na(row_index)) {
      return(list(
        x = layout$x[[row_index]] %||% NULL,
        y = layout$y[[row_index]] %||% NULL
      ))
    }
  }

  list(x = NULL, y = NULL)
}


serious_write_capsule_dir_clean_value <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(x) == 1 && is.na(x)) {
    return(NULL)
  }

  if (is.character(x)) {
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      return(NULL)
    }

    return(x)
  }

  if (is.numeric(x) || is.integer(x) || is.logical(x)) {
    if (length(x) == 1 && is.na(x)) {
      return(NULL)
    }

    return(x)
  }

  x
}


serious_write_capsule_dir_readme <- function(capsule, path) {
  readme_file <- file.path(path, "README.md")

  title <- capsule$title %||% capsule$id %||% "SeRiouS capsule"

  writeLines(
    c(
      paste0("# ", title),
      "",
      "This folder contains a SeRiouS learning capsule.",
      "",
      "## Structure",
      "",
      "- `serious.yml`: capsule metadata, sections, colours and packages.",
      "- `cells/`: one YAML file per pedagogical cell.",
      "- `data/`: optional datasets.",
      "- `pdf/`: optional PDF resources.",
      "- `img/`: optional images.",
      "- `www/`: optional web resources.",
      "",
      "## Run this capsule",
      "",
      "```r",
      "library(SeRiouS)",
      "run_capsule(\".\")",
      "```",
      "",
      "## Check this capsule",
      "",
      "```r",
      "library(SeRiouS)",
      "check_capsule_dir(\".\")",
      "```"
    ),
    readme_file
  )

  invisible(readme_file)
}
