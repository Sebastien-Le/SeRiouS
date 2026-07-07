# Internal helpers -------------------------------------------------------------

serious_cells_dir <- function(path) {
  file.path(path, "cells")
}

serious_cell_file <- function(path, id) {
  file.path(serious_cells_dir(path), paste0(id, ".yml"))
}

serious_check_capsule_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single non-empty character string.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    stop("Capsule directory does not exist: ", path, call. = FALSE)
  }

  invisible(normalizePath(path, mustWork = TRUE))
}

serious_check_cell_id <- function(id) {
  if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
    stop("'id' must be a single non-empty character string.", call. = FALSE)
  }

  if (!grepl("^[A-Za-z0-9_\\-]+$", id)) {
    stop(
      "'id' must contain only letters, numbers, underscores or hyphens.",
      call. = FALSE
    )
  }

  invisible(id)
}

serious_drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

serious_as_character_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (length(x) == 0) {
    return(character())
  }

  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

serious_read_cell <- function(file) {
  cell <- yaml::read_yaml(file)

  if (is.null(cell$id) || !nzchar(cell$id)) {
    stop("Cell file has no valid 'id': ", file, call. = FALSE)
  }

  class(cell) <- c("serious_cell", class(cell))
  cell
}

serious_write_cell <- function(cell, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(cell, file)
  invisible(file)
}


#' List cells in a capsule folder
#'
#' @param path Path to a capsule folder.
#'
#' @return A data frame describing the cells.
#'
#' @family cell-based capsule API
#' @export
capsule_cells <- function(path) {
  serious_check_capsule_path(path)

  cells_dir <- serious_cells_dir(path)

  if (!dir.exists(cells_dir)) {
    return(data.frame(
      id = character(),
      title = character(),
      section = character(),
      x = numeric(),
      y = numeric(),
      next_cells = character(),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(
    cells_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    return(data.frame(
      id = character(),
      title = character(),
      section = character(),
      x = numeric(),
      y = numeric(),
      next_cells = character(),
      stringsAsFactors = FALSE
    ))
  }

  cells <- lapply(files, serious_read_cell)

  rows <- lapply(cells, function(cell) {
    data.frame(
      id = cell$id %||% NA_character_,
      title = cell$title %||% NA_character_,
      section = cell$section %||% NA_character_,
      x = as.numeric(cell$x %||% NA_real_),
      y = as.numeric(cell$y %||% NA_real_),
      next_cells = paste(serious_as_character_vector(cell$next_cells), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


#' Get a cell from a capsule folder
#'
#' @param path Path to a capsule folder.
#' @param id Cell id.
#'
#' @return A list of class `"serious_cell"`.
#'
#' @family cell-based capsule API
#' @export
capsule_get_cell <- function(path, id) {
  serious_check_capsule_path(path)
  serious_check_cell_id(id)

  file <- serious_cell_file(path, id)

  if (!file.exists(file)) {
    stop("Cell does not exist: ", id, call. = FALSE)
  }

  serious_read_cell(file)
}

#' Add a cell to a capsule folder
#'
#' @param path Path to a capsule folder.
#' @param id Cell id.
#' @param title Cell title.
#' @param section Section id used for board color and legend.
#' @param x,y Numeric coordinates on the board.
#' @param objective Optional pedagogical objective.
#' @param text Optional pedagogical text.
#' @param content Optional pedagogical content. If `text` is missing, this can
#'   be used as the main pedagogical text.
#' @param code Optional R code.
#' @param outputs Character vector of expected output zones.
#' @param expected_output Optional human-readable description of expected output.
#' @param concepts Optional character vector of pedagogical concepts.
#' @param question Optional unlock question.
#' @param expected_answer Optional expected answer.
#' @param next_cells Character vector of next cell ids.
#' @param overwrite Logical. If `TRUE`, overwrite an existing cell.
#'
#' @return Invisibly returns the cell file path.
#'
#' @family cell-based capsule API
#' @export
capsule_add_cell <- function(path,
                             id,
                             title,
                             section = NULL,
                             x = NULL,
                             y = NULL,
                             objective = NULL,
                             text = NULL,
                             content = NULL,
                             code = NULL,
                             outputs = NULL,
                             expected_output = NULL,
                             concepts = NULL,
                             question = NULL,
                             expected_answer = NULL,
                             next_cells = character(),
                             overwrite = FALSE) {
  serious_check_capsule_path(path)
  serious_check_cell_id(id)

  if (!is.character(title) || length(title) != 1 || !nzchar(title)) {
    stop("'title' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }

  file <- serious_cell_file(path, id)

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(
      "Cell already exists: ", id, ". ",
      "Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  cell <- serious_drop_null(list(
    id = id,
    title = title,
    section = section,
    x = x,
    y = y,
    objective = objective,
    text = text,
    content = content,
    code = code,
    outputs = outputs,
    expected_output = expected_output,
    concepts = concepts,
    question = question,
    expected_answer = expected_answer,
    next_cells = serious_as_character_vector(next_cells)
  ))

  class(cell) <- c("serious_cell", class(cell))

  serious_write_cell(cell, file)
}

#' Update a cell in a capsule folder
#'
#' @param path Path to a capsule folder.
#' @param id Cell id.
#' @param ... Cell fields to update.
#'
#' @return Invisibly returns the updated cell.
#'
#' @family cell-based capsule API
#' @export
capsule_update_cell <- function(path, id, ...) {
  cell <- capsule_get_cell(path, id)

  updates <- list(...)

  if (length(updates) == 0) {
    return(invisible(cell))
  }

  for (nm in names(updates)) {
    cell[[nm]] <- updates[[nm]]
  }

  serious_write_cell(cell, serious_cell_file(path, id))

  invisible(cell)
}


#' Move a cell on the board
#'
#' @param path Path to a capsule folder.
#' @param id Cell id.
#' @param x,y Numeric coordinates.
#'
#' @return Invisibly returns the updated cell.
#'
#' @family cell-based capsule API
#' @export
capsule_move_cell <- function(path, id, x, y) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("'x' must be a single numeric value.", call. = FALSE)
  }

  if (!is.numeric(y) || length(y) != 1 || is.na(y)) {
    stop("'y' must be a single numeric value.", call. = FALSE)
  }

  capsule_update_cell(path, id, x = x, y = y)
}


#' Set the unlock question of a cell
#'
#' @param path Path to a capsule folder.
#' @param id Cell id.
#' @param question Unlock question.
#' @param expected_answer Expected answer.
#' @param case_sensitive Logical. Should answer matching be case-sensitive?
#' @param success Optional success message.
#' @param failure Optional failure message.
#'
#' @return Invisibly returns the updated cell.
#'
#' @family cell-based capsule API
#' @export
capsule_set_cell_unlock <- function(path,
                                    id,
                                    question,
                                    expected_answer,
                                    case_sensitive = FALSE,
                                    success = NULL,
                                    failure = NULL) {
  if (!is.character(question) || length(question) != 1 || !nzchar(question)) {
    stop("'question' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.character(expected_answer) ||
      length(expected_answer) != 1 ||
      !nzchar(expected_answer)) {
    stop("'expected_answer' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.logical(case_sensitive) || length(case_sensitive) != 1) {
    stop("'case_sensitive' must be TRUE or FALSE.", call. = FALSE)
  }

  capsule_update_cell(
    path,
    id,
    question = question,
    expected_answer = expected_answer,
    case_sensitive = case_sensitive,
    success = success,
    failure = failure
  )
}


#' Connect two cells
#'
#' @param path Path to a capsule folder.
#' @param from Source cell id.
#' @param to Target cell id.
#'
#' @return Invisibly returns the updated source cell.
#'
#' @family cell-based capsule API
#' @export
capsule_connect_cells <- function(path, from, to) {
  serious_check_cell_id(from)
  serious_check_cell_id(to)

  from_cell <- capsule_get_cell(path, from)

  if (!file.exists(serious_cell_file(path, to))) {
    stop("Target cell does not exist: ", to, call. = FALSE)
  }

  next_cells <- unique(c(
    serious_as_character_vector(from_cell$next_cells),
    to
  ))

  capsule_update_cell(path, from, next_cells = next_cells)
}


#' Disconnect two cells
#'
#' @param path Path to a capsule folder.
#' @param from Source cell id.
#' @param to Target cell id.
#'
#' @return Invisibly returns the updated source cell.
#'
#' @family cell-based capsule API
#' @export
capsule_disconnect_cells <- function(path, from, to) {
  serious_check_cell_id(from)
  serious_check_cell_id(to)

  from_cell <- capsule_get_cell(path, from)

  next_cells <- setdiff(
    serious_as_character_vector(from_cell$next_cells),
    to
  )

  capsule_update_cell(path, from, next_cells = next_cells)
}

#' Print a SeRiouS cell
#'
#' @param x A `"serious_cell"` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.serious_cell <- function(x, ...) {
  cat("SeRiouS cell\n")
  cat("  id:      ", x$id %||% "", "\n", sep = "")
  cat("  title:   ", x$title %||% "", "\n", sep = "")
  cat("  section: ", x$section %||% "", "\n", sep = "")

  if (!is.null(x$x) || !is.null(x$y)) {
    cat(
      "  position: x = ",
      x$x %||% NA,
      ", y = ",
      x$y %||% NA,
      "\n",
      sep = ""
    )
  }

  next_cells <- serious_as_character_vector(x$next_cells)

  if (length(next_cells) > 0) {
    cat("  next:    ", paste(next_cells, collapse = ", "), "\n", sep = "")
  } else {
    cat("  next:    none\n", sep = "")
  }

  if (!is.null(x$question)) {
    cat("  unlock:  ", x$question, "\n", sep = "")
  }

  invisible(x)
}

serious_read_capsule_metadata <- function(path) {
  yml_file <- file.path(path, "serious.yml")

  if (!file.exists(yml_file)) {
    return(list())
  }

  metadata <- yaml::read_yaml(yml_file)

  if (is.null(metadata)) {
    metadata <- list()
  }

  metadata
}

serious_sections_from_metadata <- function(metadata, cells = NULL) {
  sections <- metadata$sections

  if (!is.null(sections)) {
    if (is.data.frame(sections)) {
      sections_df <- sections
    } else if (is.list(sections)) {
      sections_df <- do.call(
        rbind,
        lapply(sections, function(x) {
          x <- as.list(x)
          data.frame(
            id = x$id %||% NA_character_,
            label = x$label %||% x$title %||% x$name %||% x$id %||% NA_character_,
            color = x$color %||% "#ECEFF1",
            border = x$border %||% "#607D8B",
            stringsAsFactors = FALSE
          )
        })
      )
    } else {
      sections_df <- NULL
    }

    if (!is.null(sections_df) &&
        all(c("id", "label", "color", "border") %in% names(sections_df))) {
      return(make_sections(
        id = sections_df$id,
        label = sections_df$label,
        color = sections_df$color,
        border = sections_df$border
      ))
    }
  }

  section_ids <- character()

  if (!is.null(cells) && length(cells) > 0) {
    section_ids <- unique(vapply(
      cells,
      function(cell) cell$section %||% "default",
      character(1)
    ))
  }

  section_ids <- section_ids[!is.na(section_ids) & nzchar(section_ids)]

  if (length(section_ids) == 0) {
    section_ids <- "default"
  }

  palette <- serious_board_default_section_colors()

  colors <- vapply(section_ids, function(id) {
    if (id %in% names(palette)) {
      unname(palette[[id]])
    } else {
      "#ECEFF1"
    }
  }, character(1))

  make_sections(
    id = section_ids,
    label = section_ids,
    color = colors,
    border = rep("#607D8B", length(section_ids))
  )
}

serious_cell_files <- function(path) {
  cells_dir <- serious_cells_dir(path)

  if (!dir.exists(cells_dir)) {
    return(character())
  }

  list.files(
    cells_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )
}

serious_load_cells <- function(path) {
  files <- serious_cell_files(path)

  if (length(files) == 0) {
    stop("No cell files found in: ", serious_cells_dir(path), call. = FALSE)
  }

  cells <- lapply(files, serious_read_cell)

  ids <- vapply(cells, function(cell) cell$id, character(1))

  if (anyDuplicated(ids)) {
    stop(
      "Duplicated cell ids: ",
      paste(unique(ids[duplicated(ids)]), collapse = ", "),
      call. = FALSE
    )
  }

  names(cells) <- ids
  cells
}

serious_guess_start_cell <- function(cells, metadata = list()) {
  explicit_start <- metadata$start_cell %||%
    metadata$start_step %||%
    metadata$start

  if (!is.null(explicit_start) &&
      is.character(explicit_start) &&
      length(explicit_start) == 1 &&
      nzchar(explicit_start)) {
    return(explicit_start)
  }

  ids <- names(cells)

  targets <- unique(unlist(lapply(cells, function(cell) {
    serious_as_character_vector(cell$next_cells)
  }), use.names = FALSE))

  candidates <- setdiff(ids, targets)

  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  ids[[1]]
}

serious_order_cells <- function(cells, start_id) {
  ids <- names(cells)
  seen <- character()
  ordered <- character()

  visit <- function(id) {
    if (id %in% seen || !(id %in% ids)) {
      return(NULL)
    }

    seen <<- c(seen, id)
    ordered <<- c(ordered, id)

    next_cells <- serious_as_character_vector(cells[[id]]$next_cells)

    for (next_id in next_cells) {
      visit(next_id)
    }

    NULL
  }

  visit(start_id)

  remaining <- setdiff(ids, ordered)

  c(ordered, remaining)
}

serious_cell_to_step <- function(cell) {
  main_text <- cell$text %||% cell$content %||% NULL

  args <- list(
    id = cell$id,
    title = cell$title %||% cell$id,
    section = cell$section %||% NULL,
    objective = cell$objective %||% NULL,
    text = main_text,
    code = cell$code %||% NULL,
    outputs = cell$outputs %||% NULL,
    expected_output = cell$expected_output %||% NULL,
    concepts = cell$concepts %||% NULL,
    question = cell$question %||% NULL,
    expected_answer = cell$expected_answer %||% NULL,
    pdf = cell$pdf %||% NULL,
    pdf_on_run = cell$pdf_on_run %||% NULL
  )

  args <- serious_drop_null(args)

  formals_names <- names(formals(make_step))
  args <- args[names(args) %in% formals_names]

  step <- do.call(make_step, args)

  # Keep cell-specific fields even if make_step() does not know them.
  step$section <- cell$section %||% step$section %||% NULL
  step$partie <- cell$section %||% step$partie %||% NULL
  step$text <- main_text %||% step$text %||% NULL
  step$content <- cell$content %||% main_text %||% step$content %||% NULL
  step$objective <- cell$objective %||% step$objective %||% NULL
  step$expected_output <- cell$expected_output %||% step$expected_output %||% NULL
  step$concepts <- cell$concepts %||% step$concepts %||% NULL
  step$x <- cell$x %||% step$x %||% NULL
  step$y <- cell$y %||% step$y %||% NULL
  step$next_steps <- serious_as_character_vector(cell$next_cells)
  step$pdf <- cell$pdf %||% step$pdf %||% NULL
  step$pdf_on_run <- cell$pdf_on_run %||% step$pdf_on_run %||% NULL

  if (!is.null(cell$case_sensitive)) {
    step$case_sensitive <- cell$case_sensitive
  }

  if (!is.null(cell$success)) {
    step$success <- cell$success
  }

  if (!is.null(cell$failure)) {
    step$failure <- cell$failure
  }

  step
}

serious_cells_to_edges <- function(cells) {
  edges_list <- lapply(cells, function(cell) {
    next_cells <- serious_as_character_vector(cell$next_cells)

    if (length(next_cells) == 0) {
      return(NULL)
    }

    data.frame(
      from = cell$id,
      to = next_cells,
      stringsAsFactors = FALSE
    )
  })

  edges <- do.call(rbind, edges_list)

  if (is.null(edges) || nrow(edges) == 0) {
    return(data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ))
  }

  edges
}

serious_cells_to_layout <- function(cells) {
  rows <- lapply(cells, function(cell) {
    if (is.null(cell$x) || is.null(cell$y)) {
      return(NULL)
    }

    data.frame(
      id = cell$id,
      x = as.numeric(cell$x),
      y = as.numeric(cell$y),
      stringsAsFactors = FALSE
    )
  })

  layout <- do.call(rbind, rows)

  if (is.null(layout) || nrow(layout) == 0) {
    return(NULL)
  }

  layout
}

serious_load_capsule_cells_dir <- function(path) {
  capsule_dir <- serious_check_capsule_path(path)

  metadata <- serious_read_capsule_metadata(capsule_dir)
  cells <- serious_load_cells(capsule_dir)

  start_step <- serious_guess_start_cell(cells, metadata)
  ordered_ids <- serious_order_cells(cells, start_step)
  cells <- cells[ordered_ids]

  steps <- lapply(cells, serious_cell_to_step)
  names(steps) <- vapply(steps, function(step) step$id, character(1))

  sections <- serious_sections_from_metadata(
    metadata = metadata,
    cells = cells
  )

  edges <- serious_cells_to_edges(cells)
  layout <- serious_cells_to_layout(cells)

  capsule_args <- list(
    id = metadata$id %||% basename(normalizePath(capsule_dir, mustWork = TRUE)),
    title = metadata$title %||% basename(normalizePath(capsule_dir, mustWork = TRUE)),
    subtitle = metadata$subtitle %||% NULL,
    method = metadata$method %||% "SeRiouS capsule",
    description = metadata$description %||% NULL,
    steps = steps,
    sections = sections,
    edges = edges,
    layout = layout,
    data = list(),
    packages = metadata$packages %||% character(),
    start_step = start_step
  )

  capsule_args <- serious_drop_null(capsule_args)

  formals_names <- names(formals(build_capsule))
  capsule_args_for_builder <- capsule_args[names(capsule_args) %in% formals_names]

  capsule <- do.call(build_capsule, capsule_args_for_builder)

  # Ensure fields are present even if build_capsule() did not accept them.
  capsule$edges <- edges
  capsule$layout <- layout
  capsule$sections <- sections
  capsule$start_step <- start_step

  if (is.null(capsule$resources)) {
    capsule$resources <- list()
  }

  capsule$resources$capsule_dir <- normalizePath(capsule_dir, mustWork = TRUE)

  validate_capsule(capsule, parse_code = FALSE, verbose = FALSE)

  capsule
}
