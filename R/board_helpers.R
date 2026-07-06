serious_board_step_ids <- function(capsule) {
  ids <- names(capsule$steps)

  if (is.null(ids) || any(!nzchar(ids))) {
    ids <- vapply(capsule$steps, function(x) x$id, character(1))
  }

  ids
}

serious_board_get_step <- function(capsule, id) {
  ids <- serious_board_step_ids(capsule)
  idx <- match(id, ids)

  if (is.na(idx)) {
    stop("Unknown step id: ", id, call. = FALSE)
  }

  capsule$steps[[idx]]
}

serious_board_step_status <- function(step_id,
                                      unlocked_steps,
                                      visited_steps) {
  if (step_id %in% visited_steps) {
    "visited"
  } else if (step_id %in% unlocked_steps) {
    "unlocked"
  } else {
    "locked"
  }
}

serious_board_status_icon <- function(status) {
  switch(
    status,
    visited = "\u2713",
    unlocked = "\U0001F513",
    locked = "\U0001F512",
    "?"
  )
}

serious_board_status_label <- function(status) {
  switch(
    status,
    visited = "visited",
    unlocked = "unlocked",
    locked = "locked",
    status
  )
}

serious_board_status_border_color <- function(status) {
  switch(
    status,
    visited = "#1565C0",
    unlocked = "#2E7D32",
    locked = "#C62828",
    "#424242"
  )
}

serious_board_section_id <- function(step) {
  step$partie %||% step$section %||% step$section_id %||% NA_character_
}

serious_board_default_section_colors <- function() {
  c(
    donnees = "#DDEEFF",
    stat = "#E8F5E9",
    r_sorties = "#FFF3E0",
    entrainer = "#F3E5F5",
    r_auto = "#E0F7FA",
    factominer = "#E8EAF6",
    nailer = "#FCE4EC",
    latent = "#F1F8E9",
    texte = "#ECEFF1",

    data = "#DDEEFF",
    summary = "#E8F5E9",
    visual = "#FFF3E0",
    conclusion = "#F3E5F5"
  )
}

serious_board_section_color <- function(section_id, sections = NULL) {
  if (is.null(section_id) ||
      length(section_id) != 1 ||
      is.na(section_id) ||
      !nzchar(section_id)) {
    return("#ECEFF1")
  }

  if (is.data.frame(sections) &&
      "id" %in% names(sections) &&
      "color" %in% names(sections)) {
    idx <- match(section_id, sections$id)

    if (!is.na(idx)) {
      return(sections$color[[idx]])
    }
  }

  palette <- serious_board_default_section_colors()

  if (section_id %in% names(palette)) {
    return(unname(palette[[section_id]]))
  }

  "#ECEFF1"
}

serious_board_get_position <- function(capsule, step, id, i) {
  if (!is.null(step$x) && !is.null(step$y)) {
    return(list(x = step$x, y = step$y))
  }

  layout <- capsule$layout

  if (is.data.frame(layout) &&
      all(c("id", "x", "y") %in% names(layout))) {
    idx <- match(id, layout$id)

    if (!is.na(idx)) {
      return(list(
        x = layout$x[[idx]],
        y = layout$y[[idx]]
      ))
    }
  }

  if (is.list(layout) && !is.null(layout[[id]])) {
    pos <- layout[[id]]

    if (!is.null(pos$x) && !is.null(pos$y)) {
      return(list(x = pos$x, y = pos$y))
    }
  }

  list(
    x = ((i - 1) %% 5) * 220,
    y = floor((i - 1) / 5) * 140
  )
}

serious_board_make_nodes <- function(capsule,
                                     unlocked_steps,
                                     visited_steps,
                                     selected_step = NULL) {
  ids <- serious_board_step_ids(capsule)

  nodes <- lapply(seq_along(ids), function(i) {
    id <- ids[[i]]
    step <- serious_board_get_step(capsule, id)

    status <- serious_board_step_status(
      step_id = id,
      unlocked_steps = unlocked_steps,
      visited_steps = visited_steps
    )

    section_id <- serious_board_section_id(step)

    pos <- serious_board_get_position(
      capsule = capsule,
      step = step,
      id = id,
      i = i
    )

    background <- serious_board_section_color(
      section_id = section_id,
      sections = capsule$sections
    )

    border <- serious_board_status_border_color(status)

    border_width <- if (!is.null(selected_step) &&
                        identical(id, selected_step)) {
      5
    } else {
      3
    }

    data.frame(
      id = id,
      label = paste0(
        serious_board_status_icon(status),
        " ",
        step$title %||% id
      ),
      title = paste0(
        serious_board_status_label(status),
        "\n",
        step$objective %||% step$title %||% id
      ),
      x = as.numeric(pos$x),
      y = as.numeric(pos$y),

      fixed = TRUE,
      fixed.x = TRUE,
      fixed.y = TRUE,
      physics = FALSE,

      shape = "box",
      margin = 12,

      widthConstraint.minimum = 170,
      widthConstraint.maximum = 170,
      heightConstraint.minimum = 60,

      font.size = 18,
      font.multi = FALSE,

      color.background = background,
      color.border = border,
      color.highlight.background = background,
      color.highlight.border = border,
      color.hover.background = background,
      color.hover.border = border,

      borderWidth = border_width,
      borderWidthSelected = 5,

      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, nodes)
}

serious_board_make_edges <- function(capsule) {
  if (!is.null(capsule$edges) &&
      is.data.frame(capsule$edges) &&
      all(c("from", "to") %in% names(capsule$edges))) {
    edges <- capsule$edges[, c("from", "to"), drop = FALSE]
  } else {
    ids <- serious_board_step_ids(capsule)

    edges_list <- lapply(ids, function(id) {
      step <- serious_board_get_step(capsule, id)

      if (is.null(step$next_steps) || length(step$next_steps) == 0) {
        return(NULL)
      }

      data.frame(
        from = id,
        to = step$next_steps,
        stringsAsFactors = FALSE
      )
    })

    edges <- do.call(rbind, edges_list)
  }

  if (is.null(edges) || nrow(edges) == 0) {
    edges <- data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    )
  }

  edges$arrows <- "to"
  edges$smooth <- FALSE
  edges$physics <- FALSE
  edges$color <- "#757575"
  edges$width <- 2

  edges
}

serious_board_widget <- function(nodes,
                                 edges,
                                 height = "420px") {
  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    width = "100%",
    height = height
  ) |>
    visNetwork::visNodes(
      shape = "box",
      physics = FALSE,
      fixed = list(x = TRUE, y = TRUE),
      margin = 12,
      widthConstraint = list(
        minimum = 170,
        maximum = 170
      ),
      heightConstraint = list(
        minimum = 60
      )
    ) |>
    visNetwork::visEdges(
      arrows = "to",
      smooth = FALSE,
      physics = FALSE
    ) |>
    visNetwork::visPhysics(
      enabled = FALSE,
      stabilization = FALSE
    ) |>
    visNetwork::visLayout(
      randomSeed = 123,
      improvedLayout = FALSE
    ) |>
    visNetwork::visInteraction(
      dragNodes = FALSE,
      dragView = FALSE,
      zoomView = FALSE,
      hover = TRUE,
      selectable = TRUE
    ) |>
    visNetwork::visOptions(
      highlightNearest = FALSE,
      nodesIdSelection = FALSE
    )
}
