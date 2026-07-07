#' Define capsule sections
#'
#' @param id Character vector of section identifiers.
#' @param label Character vector of section labels.
#' @param color Character vector of background colors.
#' @param border Character vector of border colors.
#'
#' @return A data frame describing capsule sections.
#'
#' @family advanced capsule builders
#' @export
make_sections <- function(id,
                          label = id,
                          color = "#EEEEEE",
                          border = "#999999") {
  stopifnot(is.character(id))
  stopifnot(is.character(label))
  stopifnot(is.character(color))
  stopifnot(is.character(border))

  n <- length(id)

  if (length(label) == 1) label <- rep(label, n)
  if (length(color) == 1) color <- rep(color, n)
  if (length(border) == 1) border <- rep(border, n)

  if (!all(lengths(list(label, color, border)) == n)) {
    stop("'id', 'label', 'color', and 'border' must have compatible lengths.",
         call. = FALSE)
  }

  data.frame(
    id = id,
    label = label,
    color = color,
    border = border,
    stringsAsFactors = FALSE
  )
}

#' Create linear edges between ordered steps
#'
#' @param ids Ordered character vector of step identifiers.
#'
#' @return A data frame with columns `from` and `to`.
#' @export
make_linear_edges <- function(ids) {
  stopifnot(is.character(ids))

  if (length(ids) < 2) {
    return(data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    from = ids[-length(ids)],
    to = ids[-1],
    stringsAsFactors = FALSE
  )
}


#' Apply graph edges to a list of steps
#'
#' @param steps List of `"serious_step"` objects.
#' @param edges Data frame with columns `from` and `to`.
#'
#' @return A list of updated `"serious_step"` objects.
#'
#' @keywords internal
#' @export
apply_edges_to_steps <- function(steps, edges) {
  stopifnot(is.list(steps))

  if (!all(c("from", "to") %in% names(edges))) {
    stop("'edges' must contain columns 'from' and 'to'.", call. = FALSE)
  }

  ids <- vapply(steps, function(x) x$id, character(1))

  for (id in ids) {
    idx <- which(ids == id)
    steps[[idx]]$next_steps <- edges$to[edges$from == id]
  }

  steps
}

#' Create a simple board layout
#'
#' @param ids Character vector of step identifiers.
#' @param ncol Number of columns in the board.
#' @param x_spacing Horizontal spacing.
#' @param y_spacing Vertical spacing.
#' @param snake Logical. If `TRUE`, alternates direction every row.
#'
#' @return A data frame with columns `id`, `x`, and `y`.
#' @export
make_board_layout <- function(ids,
                              ncol = 5,
                              x_spacing = 220,
                              y_spacing = 140,
                              snake = TRUE) {
  stopifnot(is.character(ids))
  stopifnot(length(ncol) == 1, ncol >= 1)

  n <- length(ids)

  row <- (seq_len(n) - 1) %/% ncol
  col <- (seq_len(n) - 1) %% ncol

  if (isTRUE(snake)) {
    odd_row <- row %% 2 == 1
    col[odd_row] <- (ncol - 1) - col[odd_row]
  }

  data.frame(
    id = ids,
    x = col * x_spacing,
    y = row * y_spacing,
    stringsAsFactors = FALSE
  )
}

#' Apply board layout to steps
#'
#' @param steps List of `"serious_step"` objects.
#' @param layout Data frame with columns `id`, `x`, and `y`.
#'
#' @return Updated list of `"serious_step"` objects.
#'
#' @keywords internal
#' @export
apply_layout_to_steps <- function(steps, layout) {
  stopifnot(is.list(steps))

  if (!all(c("id", "x", "y") %in% names(layout))) {
    stop("'layout' must contain columns 'id', 'x', and 'y'.", call. = FALSE)
  }

  ids <- vapply(steps, function(x) x$id, character(1))

  for (id in ids) {
    if (id %in% layout$id) {
      idx <- which(ids == id)
      pos <- layout[layout$id == id, , drop = FALSE]

      steps[[idx]]$layout <- list(
        x = pos$x[[1]],
        y = pos$y[[1]]
      )
    }
  }

  steps
}

#' Build a linear sequence of steps
#'
#' @param steps List of `"serious_step"` objects.
#' @param ncol Number of columns for automatic board layout.
#' @param snake Logical. If `TRUE`, creates a snake-like board layout.
#'
#' @return A list of updated `"serious_step"` objects.
#' @export
make_step_sequence <- function(steps,
                               ncol = 5,
                               snake = TRUE) {
  stopifnot(is.list(steps))

  ids <- vapply(steps, function(x) x$id, character(1))

  edges <- make_linear_edges(ids)
  layout <- make_board_layout(ids, ncol = ncol, snake = snake)

  steps <- apply_edges_to_steps(steps, edges)
  steps <- apply_layout_to_steps(steps, layout)

  steps
}

#' Create capsule edges
#'
#' @param from Character vector of source step ids.
#' @param to Character vector of target step ids.
#'
#' @return A data frame with columns `from` and `to`.
#'
#' @family advanced capsule builders
#' @export
make_edges <- function(from, to) {
  if (!is.character(from)) {
    stop("'from' must be a character vector.", call. = FALSE)
  }

  if (!is.character(to)) {
    stop("'to' must be a character vector.", call. = FALSE)
  }

  if (length(from) != length(to)) {
    stop("'from' and 'to' must have the same length.", call. = FALSE)
  }

  data.frame(
    from = from,
    to = to,
    stringsAsFactors = FALSE
  )
}


#' Create a manual capsule layout
#'
#' @param id Character vector of step ids.
#' @param x Numeric vector of x positions.
#' @param y Numeric vector of y positions.
#'
#' @return A data frame with columns `id`, `x`, and `y`.
#'
#' @family advanced capsule builders
#' @export
make_layout <- function(id, x, y) {
  if (!is.character(id)) {
    stop("'id' must be a character vector.", call. = FALSE)
  }

  if (!is.numeric(x)) {
    stop("'x' must be numeric.", call. = FALSE)
  }

  if (!is.numeric(y)) {
    stop("'y' must be numeric.", call. = FALSE)
  }

  if (length(id) != length(x) || length(id) != length(y)) {
    stop("'id', 'x', and 'y' must have the same length.", call. = FALSE)
  }

  data.frame(
    id = id,
    x = x,
    y = y,
    stringsAsFactors = FALSE
  )
}


#' Build a learning capsule from steps, edges, layout, and metadata
#'
#' @param id Capsule id.
#' @param title Capsule title.
#' @param method Method name.
#' @param steps List of `"serious_step"` objects.
#' @param edges Optional edge data frame with columns `from` and `to`.
#' @param layout Optional layout data frame with columns `id`, `x`, and `y`.
#' @param sections Optional sections data frame.
#' @param subtitle Optional subtitle.
#' @param description Optional description.
#' @param data Named list of datasets or objects available in the capsule.
#' @param packages Character vector of package names used by the capsule.
#' @param resources Optional named list of external resources.
#' @param start_step Optional id of the starting step.
#'
#' @return A `"learning_capsule"` object.
#'
#' @family advanced capsule builders
#' @export
build_capsule <- function(id,
                          title,
                          method,
                          steps,
                          edges = NULL,
                          layout = NULL,
                          sections = NULL,
                          subtitle = NULL,
                          description = NULL,
                          data = list(),
                          packages = character(),
                          resources = list(),
                          start_step = NULL) {
  if (!is.list(steps)) {
    stop("'steps' must be a list of serious_step objects.", call. = FALSE)
  }

  if (length(steps) == 0) {
    stop("'steps' cannot be empty.", call. = FALSE)
  }

  is_step <- vapply(steps, inherits, logical(1), what = "serious_step")

  if (!all(is_step)) {
    stop("All elements of 'steps' must be serious_step objects.", call. = FALSE)
  }

  if (!is.null(edges)) {
    steps <- apply_edges_to_steps(
      steps = steps,
      edges = edges
    )
  }

  if (!is.null(layout)) {
    steps <- apply_layout_to_steps(
      steps = steps,
      layout = layout
    )
  }

  step_ids <- vapply(steps, function(x) x$id, character(1))

  if (anyDuplicated(step_ids)) {
    stop("Step ids must be unique.", call. = FALSE)
  }

  names(steps) <- step_ids

  if (is.null(start_step)) {
    start_step <- step_ids[[1]]
  }

  if (!start_step %in% step_ids) {
    stop("'start_step' must be one of the step ids.", call. = FALSE)
  }

  capsule <- new_learning_capsule(
    id = id,
    title = title,
    subtitle = subtitle,
    method = method,
    description = description,
    steps = steps,
    data = data,
    packages = packages,
    resources = resources,
    sections = sections,
    start_step = start_step
  )

  validate_capsule(capsule, parse_code = FALSE, verbose = FALSE)

  capsule
}

#' Build a linear learning capsule
#'
#' @param id Capsule id.
#' @param title Capsule title.
#' @param method Method name.
#' @param steps List of `"serious_step"` objects.
#' @param sections Optional sections data frame.
#' @param subtitle Optional subtitle.
#' @param description Optional description.
#' @param data Named list of datasets or objects available in the capsule.
#' @param packages Character vector of package names used by the capsule.
#' @param resources Optional named list of external resources.
#' @param start_step Optional id of the starting step.
#' @param ncol Number of columns for the automatic board layout.
#' @param snake Logical. If `TRUE`, creates a snake-like board layout.
#' @param x_spacing Horizontal spacing between nodes.
#' @param y_spacing Vertical spacing between nodes.
#'
#' @return A `"learning_capsule"` object.
#'
#' @family advanced capsule builders
#' @export
build_linear_capsule <- function(id,
                                 title,
                                 method,
                                 steps,
                                 sections = NULL,
                                 subtitle = NULL,
                                 description = NULL,
                                 data = list(),
                                 packages = character(),
                                 resources = list(),
                                 start_step = NULL,
                                 ncol = 5,
                                 snake = TRUE,
                                 x_spacing = 220,
                                 y_spacing = 140) {
  if (!is.list(steps)) {
    stop("'steps' must be a list of serious_step objects.", call. = FALSE)
  }

  if (length(steps) == 0) {
    stop("'steps' cannot be empty.", call. = FALSE)
  }

  is_step <- vapply(steps, inherits, logical(1), what = "serious_step")

  if (!all(is_step)) {
    stop("All elements of 'steps' must be serious_step objects.", call. = FALSE)
  }

  step_ids <- vapply(steps, function(x) x$id, character(1))

  if (anyDuplicated(step_ids)) {
    stop("Step ids must be unique.", call. = FALSE)
  }

  edges <- make_linear_edges(step_ids)

  layout <- make_board_layout(
    ids = step_ids,
    ncol = ncol,
    x_spacing = x_spacing,
    y_spacing = y_spacing,
    snake = snake
  )

  build_capsule(
    id = id,
    title = title,
    method = method,
    steps = steps,
    edges = edges,
    layout = layout,
    sections = sections,
    subtitle = subtitle,
    description = description,
    data = data,
    packages = packages,
    resources = resources,
    start_step = start_step
  )
}
