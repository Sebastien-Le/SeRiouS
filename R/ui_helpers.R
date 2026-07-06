serious_ui_css <- function() {
  shiny::HTML(
    "
    body {
      background: #F7F8FA;
    }

    .serious-header {
      background: white;
      border-radius: 14px;
      padding: 18px 22px;
      margin-bottom: 16px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    }

    .serious-card {
      background: white;
      border-radius: 14px;
      padding: 18px;
      margin-bottom: 16px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    }

    .serious-board {
      background: white;
      border-radius: 14px;
      padding: 12px;
      margin-bottom: 16px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    }

    .serious-step-card {
      border: 3px solid #BDBDBD;
    }

    .serious-step-card-locked {
      border-color: #C62828;
    }

    .serious-step-card-unlocked {
      border-color: #2E7D32;
    }

    .serious-badge {
      display: inline-block;
      padding: 5px 12px;
      border-radius: 999px;
      font-weight: 700;
      margin-left: 8px;
      font-size: 0.75em;
    }

    .serious-visited {
      background: #E3F2FD;
      color: #1565C0;
    }

    .serious-unlocked {
      background: #E8F5E9;
      color: #2E7D32;
    }

    .serious-locked {
      background: #FFEBEE;
      color: #C62828;
    }

    pre {
      white-space: pre-wrap;
      word-break: break-word;
    }
    "
  )
}

serious_header_ui <- function(capsule) {
  shiny::div(
    class = "serious-header",
    shiny::h1(capsule$title %||% "SeRiouS capsule"),
    if (!is.null(capsule$subtitle)) {
      shiny::h3(capsule$subtitle)
    },
    if (!is.null(capsule$description)) {
      shiny::p(capsule$description)
    }
  )
}

serious_step_card_class <- function(status) {
  status_class <- switch(
    status,
    visited = "serious-step-card-unlocked",
    unlocked = "serious-step-card-unlocked",
    locked = "serious-step-card-locked",
    "serious-step-card-locked"
  )

  paste("serious-card serious-step-card", status_class)
}

serious_badge_class <- function(status) {
  switch(
    status,
    visited = "serious-badge serious-visited",
    unlocked = "serious-badge serious-unlocked",
    locked = "serious-badge serious-locked",
    "serious-badge serious-locked"
  )
}
