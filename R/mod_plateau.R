#' Plateau module UI
#'
#' @param id Module id.
#'
#' @return Shiny UI.
#' @export
mod_plateau_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    visNetwork::visNetworkOutput(ns("plateau"), height = "420px")
  )
}

#' Plateau module server
#'
#' @param id Module id.
#' @param capsule A `"learning_capsule"` object.
#' @param state Shared reactive state.
#'
#' @return A Shiny module server.
#' @export
mod_plateau_server <- function(id, capsule, state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$plateau <- visNetwork::renderVisNetwork({
      nodes <- serious_board_make_nodes(
        capsule = capsule,
        unlocked_steps = shiny::isolate(state$unlocked_steps),
        visited_steps = shiny::isolate(state$visited_steps),
        selected_step = shiny::isolate(state$selected_step)
      )

      edges <- serious_board_make_edges(capsule)

      serious_board_widget(
        nodes = nodes,
        edges = edges,
        height = "420px"
      ) |>
        visNetwork::visEvents(
          selectNode = sprintf(
            "function(nodes) {
              if (nodes.nodes.length > 0) {
                Shiny.setInputValue('%s', nodes.nodes[0], {priority: 'event'});
              }
            }",
            session$ns("selected_node")
          )
        )
    })

    shiny::observe({
      state$unlocked_steps
      state$visited_steps
      state$selected_step

      nodes <- serious_board_make_nodes(
        capsule = capsule,
        unlocked_steps = state$unlocked_steps,
        visited_steps = state$visited_steps,
        selected_step = state$selected_step
      )

      proxy <- visNetwork::visNetworkProxy(
        "plateau",
        session = session
      )

      visNetwork::visUpdateNodes(
        proxy,
        nodes = nodes
      )
    })

    shiny::observeEvent(input$selected_node, {
      selected <- input$selected_node

      if (is.null(selected)) {
        return()
      }

      state$selected_step <- selected

      if (selected %in% state$unlocked_steps) {
        state$locked_step_clicked <- NULL
      } else {
        state$locked_step_clicked <- selected
      }
    })

  })
}
