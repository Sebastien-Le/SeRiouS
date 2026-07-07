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

serious_board_normalize_ids <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }

  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  unique(x)
}

serious_board_step_status <- function(step_id,
                                      unlocked_steps,
                                      visited_steps) {
  unlocked_steps <- serious_board_normalize_ids(unlocked_steps)
  visited_steps <- serious_board_normalize_ids(visited_steps)
  step_id <- as.character(step_id)

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

serious_board_status_background_color <- function(status, section_background) {
  switch(
    status,
    locked = "#FFCDD2",
    unlocked = section_background,
    visited = section_background,
    section_background
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

serious_board_section_label <- function(section_id, sections = NULL) {
  if (is.null(section_id) ||
      length(section_id) != 1 ||
      is.na(section_id) ||
      !nzchar(section_id)) {
    return("Autre")
  }

  if (is.data.frame(sections) && "id" %in% names(sections)) {
    idx <- match(section_id, sections$id)

    if (!is.na(idx)) {
      label_cols <- intersect(
        c("label", "title", "name"),
        names(sections)
      )

      if (length(label_cols) > 0) {
        label <- sections[[label_cols[[1]]]][[idx]]

        if (!is.null(label) && !is.na(label) && nzchar(label)) {
          return(as.character(label))
        }
      }
    }
  }

  default_labels <- c(
    donnees = "Données",
    stat = "Statistiques",
    r_sorties = "Sorties R",
    entrainer = "EnTraineR",
    r_auto = "R automatique",
    factominer = "FactoMineR",
    nailer = "NaileR",
    latent = "Analyse latente",
    texte = "Texte",

    data = "Data",
    summary = "Summary",
    visual = "Visualisation",
    conclusion = "Conclusion"
  )

  if (section_id %in% names(default_labels)) {
    return(unname(default_labels[[section_id]]))
  }

  section_id
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

  unlocked_steps <- serious_board_normalize_ids(unlocked_steps)
  visited_steps <- serious_board_normalize_ids(visited_steps)

  if (!is.null(selected_step)) {
    selected_step <- as.character(selected_step[[1]])
  }

  nodes <- lapply(seq_along(ids), function(i) {
    id <- as.character(ids[[i]])
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

    section_background <- serious_board_section_color(
      section_id = section_id,
      sections = capsule$sections
    )

    background <- serious_board_status_background_color(
      status = status,
      section_background = section_background
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
      section_id = section_id %||% NA_character_,
      status = status,

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

serious_board_legend_nodes <- function(sections = NULL,
                                       section_ids = NULL) {
  if (!is.null(section_ids)) {
    section_ids <- serious_board_normalize_ids(section_ids)
  } else if (is.data.frame(sections) && "id" %in% names(sections)) {
    section_ids <- serious_board_normalize_ids(sections$id)
  } else {
    section_ids <- names(serious_board_default_section_colors())
  }

  section_ids <- section_ids[!is.na(section_ids) & nzchar(section_ids)]
  section_ids <- unique(section_ids)

  if (length(section_ids) == 0) {
    return(NULL)
  }

  data.frame(
    label = vapply(
      section_ids,
      serious_board_section_label,
      character(1),
      sections = sections
    ),
    shape = "box",
    color = vapply(
      section_ids,
      serious_board_section_color,
      character(1),
      sections = sections
    ),
    stringsAsFactors = FALSE
  )
}

serious_board_status_legend_nodes <- function() {
  data.frame(
    label = c(
      "\U0001F512 verrouillé",
      "\U0001F513 débloqué",
      "\u2713 visité"
    ),
    shape = "box",
    color = c(
      "#FFCDD2",
      "#E8F5E9",
      "#DDEEFF"
    ),
    stringsAsFactors = FALSE
  )
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

serious_board_legend_ui <- function(sections = NULL) {
  if (is.data.frame(sections) &&
      "id" %in% names(sections)) {
    section_ids <- as.character(sections$id)
  } else {
    section_ids <- names(serious_board_default_section_colors())
  }

  section_ids <- section_ids[!is.na(section_ids) & nzchar(section_ids)]
  section_ids <- unique(section_ids)

  shiny::tagList(
    lapply(section_ids, function(section_id) {
      color <- serious_board_section_color(
        section_id = section_id,
        sections = sections
      )

      label <- serious_board_section_label(
        section_id = section_id,
        sections = sections
      )

      shiny::div(
        class = "legend-item",
        shiny::span(
          class = "legend-swatch",
          style = paste0(
            "background-color:", color,
            "; border-color:#666666;"
          )
        ),
        label
      )
    })
  )
}
