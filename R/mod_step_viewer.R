#' Step viewer module UI
#'
#' @param id Module id.
#'
#' @return Shiny UI.
#'
#' @keywords internal
#' @export
mod_step_viewer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("challenge_panel")),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(
          class = "serious-card",
          shiny::h3("Code debloque"),
          shiny::uiOutput(ns("code_panel"))
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(
          class = "serious-card",
          shiny::h3("Sortie console"),
          shiny::verbatimTextOutput(ns("console_output"))
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(
          class = "serious-card",
          shiny::h3("Graphique"),
          shiny::uiOutput(ns("plot_panel"))
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(
          class = "serious-card",
          shiny::h3("PDF / ressource"),
          shiny::uiOutput(ns("pdf_panel"))
        )
      )
    )
  )
}


#' Step viewer module server
#'
#' @param id Module id.
#' @param capsule A `"learning_capsule"` object.
#' @param state Shared reactive state.
#'
#' @return A Shiny module server.
#'
#' @keywords internal
#' @export
mod_step_viewer_server <- function(id, capsule, state) {
  shiny::moduleServer(id, function(input, output, session) {

    current_step <- shiny::reactive({
      shiny::req(state$selected_step)
      serious_board_get_step(capsule, state$selected_step)
    })

    is_unlocked <- shiny::reactive({
      current_step()$id %in% state$unlocked_steps
    })

    is_visited <- shiny::reactive({
      current_step()$id %in% state$visited_steps
    })

    step_output <- shiny::reactiveVal("")
    step_error <- shiny::reactiveVal(NULL)
    answer_feedback <- shiny::reactiveVal(NULL)
    plot_code <- shiny::reactiveVal(NULL)
    show_plot <- shiny::reactiveVal(FALSE)
    show_full_code <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(state$selected_step, {
      step_output("")
      step_error(NULL)
      answer_feedback(NULL)
      plot_code(NULL)
      show_plot(FALSE)
      show_full_code(FALSE)

      state$current_pdf <- NULL
      state$current_pdf_step <- NULL
    })

    check_answer <- function(step, answer) {
      answer <- trimws(answer %||% "")

      if (!is.null(step$validator) && is.function(step$validator)) {
        return(isTRUE(step$validator(answer, state$tutorial_env)))
      }

      if (!is.null(step$expected_answer)) {
        expected <- trimws(step$expected_answer)

        return(
          identical(
            tolower(answer),
            tolower(expected)
          )
        )
      }

      FALSE
    }

    output$challenge_panel <- shiny::renderUI({
      step <- current_step()

      status <- serious_board_step_status(
        step_id = step$id,
        unlocked_steps = state$unlocked_steps,
        visited_steps = state$visited_steps
      )

      badge_class <- serious_badge_class(status)
      card_class <- serious_step_card_class(status)

      authorization_ui <- if (is_unlocked()) {
        shiny::tagList(
          shiny::h4("Autorisation"),
          shiny::p("Case deverrouillee.")
        )
      } else if (!is.null(step$question)) {
        shiny::tagList(
          shiny::h4("Autorisation"),
          shiny::p("Reponds correctement a la question pour deverrouiller cette case."),

          shiny::div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            shiny::strong("Question : "),
            shiny::span(step$question)
          ),

          shiny::textInput(
            session$ns("answer"),
            label = NULL,
            placeholder = "Reponse"
          ),

          shiny::actionButton(
            session$ns("validate_answer"),
            "Valider",
            class = "btn-primary"
          ),

          shiny::uiOutput(session$ns("answer_feedback"))
        )
      } else {
        shiny::tagList(
          shiny::h4("Autorisation"),
          shiny::p("Cette case est verrouillee et ne contient pas de question de deverrouillage.")
        )
      }

      shiny::div(
        class = card_class,

        shiny::h2(
          step$title,
          shiny::span(
            class = badge_class,
            serious_board_status_label(status)
          )
        ),

        shiny::hr(),

        shiny::h4("Defi"),
        shiny::p(step$objective %||% "Aucun objectif indique."),

        if (!is.null(step$text)) {
          shiny::p(step$text)
        },

        shiny::h4("Sortie attendue"),
        shiny::p(step$expected_output %||% "Aucune sortie attendue indiquee."),

        shiny::hr(),

        authorization_ui,

        shiny::hr(),

        shiny::actionButton(
          session$ns("run_code"),
          "Executer",
          class = "btn-success"
        ),

        shiny::span(" "),

        shiny::actionButton(
          session$ns("toggle_code"),
          "Afficher / masquer le code complet",
          class = "btn-warning"
        )
      )
    })

    output$answer_feedback <- shiny::renderUI({
      feedback <- answer_feedback()

      if (is.null(feedback)) {
        return(NULL)
      }

      color <- if (isTRUE(feedback$ok)) "#2E7D32" else "#C62828"

      shiny::div(
        style = paste0(
          "color:",
          color,
          "; font-weight:700; margin-top:8px;"
        ),
        feedback$message
      )
    })

    shiny::observeEvent(input$validate_answer, {
      step <- current_step()

      if (is_unlocked()) {
        answer_feedback(list(
          ok = TRUE,
          message = "Case deja deverrouillee."
        ))
        return()
      }

      if (is.null(step$question)) {
        answer_feedback(list(
          ok = FALSE,
          message = "Cette case n'a pas de question de deverrouillage."
        ))
        return()
      }

      ok <- check_answer(step, input$answer)

      if (isTRUE(ok)) {
        state$unlocked_steps <- unique(c(state$unlocked_steps, step$id))
        state$locked_step_clicked <- NULL

        answer_feedback(list(
          ok = TRUE,
          message = "Bonne reponse. Case deverrouillee."
        ))
      } else {
        answer_feedback(list(
          ok = FALSE,
          message = "Reponse incorrecte. Essaie encore."
        ))
      }
    })

    shiny::observeEvent(input$toggle_code, {
      show_full_code(!isTRUE(show_full_code()))
    })

    output$code_panel <- shiny::renderUI({
      step <- current_step()

      if (!is_unlocked()) {
        return(
          shiny::p("Le code sera disponible quand la case sera deverrouillee.")
        )
      }

      code_to_show <- if (isTRUE(show_full_code())) {
        step$code
      } else {
        step$code_display %||% step$code
      }

      shiny::tags$pre(
        style = paste(
          "max-height: 420px;",
          "overflow-y: auto;",
          "background: #F7F7F7;",
          "padding: 12px;"
        ),
        shiny::tags$code(code_to_show %||% "")
      )
    })

    output$console_output <- shiny::renderText({
      if (!is_unlocked()) {
        return("Deverrouille la case pour executer le code.")
      }

      err <- step_error()

      if (!is.null(err)) {
        return(paste("Erreur :", err))
      }

      out <- step_output()

      if (is.null(out) || !nzchar(out)) {
        return("Clique sur Executer pour afficher la sortie.")
      }

      out
    })

    shiny::observeEvent(input$run_code, {
      step <- current_step()

      if (!is_unlocked()) {
        step_error("Cette case est verrouillee. Reponds d'abord a la question.")
        return()
      }

      step_error(NULL)
      step_output("")
      plot_code(NULL)
      show_plot(FALSE)
      state$current_pdf <- NULL
      state$current_pdf_step <- NULL

      output_text <- tryCatch(
        {
          paste(
            utils::capture.output(
              eval(parse(text = step$code), envir = state$tutorial_env)
            ),
            collapse = "\n"
          )
        },
        error = function(e) {
          step_error(conditionMessage(e))
          ""
        }
      )

      step_output(output_text)

      if ("plot" %in% (step$outputs %||% character())) {
        plot_code(step$code)
        show_plot(TRUE)
      }

      step_pdf <- NULL

      if (!is.null(step$pdf_on_run) &&
          is.character(step$pdf_on_run) &&
          length(step$pdf_on_run) == 1 &&
          nzchar(step$pdf_on_run)) {
        step_pdf <- step$pdf_on_run
      } else if (!is.null(step$pdf) &&
                 is.character(step$pdf) &&
                 length(step$pdf) == 1 &&
                 nzchar(step$pdf)) {
        step_pdf <- step$pdf
      }

      if (!is.null(step_pdf)) {
        state$current_pdf <- step_pdf
        state$current_pdf_step <- step$id
      }

      state$visited_steps <- unique(c(state$visited_steps, step$id))
    })

    output$plot_panel <- shiny::renderUI({
      if (!is_unlocked()) {
        return(
          shiny::p("Le graphique sera disponible quand la case sera deverrouillee.")
        )
      }

      if (!isTRUE(show_plot())) {
        return(
          shiny::p("Aucun graphique disponible pour cette case.")
        )
      }

      shiny::plotOutput(session$ns("step_plot"), height = "420px")
    })

    output$step_plot <- shiny::renderPlot({
      shiny::req(plot_code())

      eval(
        parse(text = plot_code()),
        envir = state$tutorial_env
      )
    })

    pdf_to_src <- function(pdf_path) {
      if (is.null(pdf_path) || length(pdf_path) != 1 || !nzchar(pdf_path)) {
        return(NULL)
      }

      pdf_path <- gsub("\\\\", "/", pdf_path)

      if (grepl("^https?://", pdf_path)) {
        return(pdf_path)
      }

      if (!is.null(capsule$resources) &&
          !is.null(capsule$resources$capsule_dir)) {
        return(paste0("serious_capsule/", pdf_path))
      }

      pdf_path
    }

    output$pdf_panel <- shiny::renderUI({
      if (is.null(state$current_pdf) ||
          length(state$current_pdf) != 1 ||
          !nzchar(state$current_pdf)) {
        return(shiny::p("Aucun PDF disponible pour cette case."))
      }

      pdf_src <- pdf_to_src(state$current_pdf)

      shiny::tags$iframe(
        src = pdf_src,
        width = "100%",
        height = "600px",
        style = "border: none;"
      )
    })
  })
}
