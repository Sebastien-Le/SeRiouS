#' SeRiouS: Shareable interactive learning capsules
#'
#' SeRiouS is an R package for creating, running, validating, and sharing
#' interactive learning capsules built with Shiny.
#'
#' A SeRiouS capsule is organised as a pedagogical board made of cells
#' connected by links. Each cell may contain explanatory text, a learning
#' objective, R code, console output, plots, questions, and additional
#' resources such as PDF files.
#'
#' @section Main workflow:
#'
#' The recommended workflow is to create a shareable capsule folder:
#'
#' \preformatted{
#' my_capsule/
#'   serious.yml
#'   cells/
#'   data/
#'   pdf/
#'   img/
#'   www/
#' }
#'
#' A capsule can then be checked and run with:
#'
#' \preformatted{
#' create_capsule_skeleton("my_capsule")
#' check_capsule_dir("my_capsule")
#' run_capsule("my_capsule")
#' }
#'
#' @section Main user functions:
#'
#' - [run_capsule()] launches a capsule.
#' - [available_capsules()] lists internal demonstration capsules.
#' - [get_capsule()] retrieves an internal capsule as an R object.
#' - [create_capsule_skeleton()] creates a new capsule folder.
#' - [check_capsule_dir()] checks a capsule folder before sharing.
#' - [load_capsule_dir()] loads a capsule folder as a `"learning_capsule"` object.
#'
#' @section Cell-based API:
#'
#' - [capsule_cells()] lists cells in a capsule folder.
#' - [capsule_get_cell()] retrieves one cell.
#' - [capsule_add_cell()] adds a new cell.
#' - [capsule_update_cell()] modifies an existing cell.
#' - [capsule_move_cell()] changes the board position of a cell.
#' - [capsule_connect_cells()] connects two cells.
#' - [capsule_disconnect_cells()] removes a link between cells.
#' - [capsule_set_cell_unlock()] sets an unlocking question.
#'
#' @section Advanced API:
#'
#' SeRiouS also provides lower-level functions for users who want to build
#' capsules directly in R, including [make_step()], [make_sections()],
#' [build_capsule()], [build_linear_capsule()] and [validate_capsule()].
#'
#' @docType package
#' @name SeRiouS-package
#' @aliases SeRiouS
#' @keywords internal
"_PACKAGE"
