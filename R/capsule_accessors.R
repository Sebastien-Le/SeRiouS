#' Get all step identifiers from a capsule
#'
#' @param capsule A `"learning_capsule"` object.
#' @return A character vector of step identifiers.
#' @export
capsule_step_ids <- function(capsule) {
  if (!inherits(capsule, "learning_capsule")) {
    stop("The object is not a valid 'learning_capsule'.", call. = FALSE)
  }

  vapply(capsule$steps, function(x) x$id, character(1))
}

#' Retrieve one step from a capsule
#'
#' @param capsule A `"learning_capsule"` object.
#' @param id Step identifier.
#' @return A `"serious_step"` object.
#' @export
get_step <- function(capsule, id) {
  ids <- capsule_step_ids(capsule)

  if (!id %in% ids) {
    stop("Unknown step id: ", id, call. = FALSE)
  }

  capsule$steps[[which(ids == id)]]
}

#' Convert capsule steps to a node data frame
#'
#' @param capsule A `"learning_capsule"` object.
#' @return A data frame with one row per step.
#' @export
capsule_nodes <- function(capsule) {
  steps <- capsule$steps

  nodes <- data.frame(
    id = vapply(steps, function(x) x$id, character(1)),
    label = vapply(steps, function(x) x$title, character(1)),
    group = vapply(steps, function(x) x$section, character(1)),
    stringsAsFactors = FALSE
  )

  has_layout <- vapply(steps, function(x) {
    !is.null(x$layout) &&
      !is.null(x$layout$x) &&
      !is.null(x$layout$y)
  }, logical(1))

  if (any(has_layout)) {
    nodes$x <- NA_real_
    nodes$y <- NA_real_

    nodes$x[has_layout] <- vapply(
      steps[has_layout],
      function(x) x$layout$x,
      numeric(1)
    )

    nodes$y[has_layout] <- vapply(
      steps[has_layout],
      function(x) x$layout$y,
      numeric(1)
    )

    nodes$fixed <- has_layout
  }

  nodes
}

#' Convert capsule links to an edge data frame
#'
#' @param capsule A `"learning_capsule"` object.
#' @return A data frame with columns `from` and `to`.
#' @export
capsule_edges <- function(capsule) {
  edges <- lapply(capsule$steps, function(step) {
    if (length(step$next_steps) == 0) {
      return(NULL)
    }

    data.frame(
      from = step$id,
      to = step$next_steps,
      stringsAsFactors = FALSE
    )
  })

  edges <- Filter(Negate(is.null), edges)

  if (length(edges) == 0) {
    return(data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, edges)
}
