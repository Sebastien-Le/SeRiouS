#' tAIdyverse capsule
#'
#' @return A `"learning_capsule"` object.
#' @export
capsule_taidyverse <- function() {

  build_capsule(
    id = "taidyverse",
    title = "SeRiouS: Playing with Data Seriously",
    subtitle = "Episode One: Making Sense of Stats through Prompting",
    method = "tAIdyverse",
    description = paste(
      "A guided workflow from statistical outputs to controlled",
      "LLM-based interpretation."
    ),
    steps = taidyverse_steps(),
    edges = taidyverse_edges(),
    layout = taidyverse_layout(),
    sections = taidyverse_sections(),
    data = list(
      questionnaire = get_taidyverse_questionnaire()
    ),
    packages = c("FactoMineR", "EnTraineR", "NaileR"),
    start_step = "donnees"
  )
}


taidyverse_sections <- function() {
  make_sections(
    id = c("stat", "r_sorties", "entrainer", "nailer", "latent", "texte"),
    label = c(
      "Statistics",
      "R outputs",
      "EnTraineR",
      "NaileR",
      "Latent structure",
      "Text synthesis"
    ),
    color = c(
      "#E3F2FD",
      "#E8F5E9",
      "#FFF3E0",
      "#F3E5F5",
      "#E0F7FA",
      "#FCE4EC"
    ),
    border = c(
      "#1565C0",
      "#2E7D32",
      "#EF6C00",
      "#6A1B9A",
      "#00838F",
      "#AD1457"
    )
  )
}


taidyverse_step_ids <- function() {
  c(
    "donnees",
    "exploration",
    "linearmodel",
    "aovsum",
    "recuperer_sorties",
    "prompt_manuel",
    "prompt_manuel_n2",
    "entrainer_presentation",
    "entrainer_intro",
    "boucle_y_x",
    "condes",
    "catdes",
    "manip_condes_catdes",
    "nailer_presentation",
    "nailer_catdes_exemple",
    "acp_hcpc_classes",
    "decrire_classes",
    "preparer_textes_classes",
    "preparer_artefacts_classes",
    "synthese_contextualisee_classes"
  )
}


taidyverse_edges <- function() {
  make_edges(
    from = taidyverse_step_ids()[-length(taidyverse_step_ids())],
    to = taidyverse_step_ids()[-1]
  )
}


taidyverse_layout <- function() {
  make_layout(
    id = taidyverse_step_ids(),
    x = c(
      0, 260, 520, 780, 1040, 1300, 1560,
      1560, 1300, 1040, 780, 520, 260, 0,
      0, 260, 520, 780, 1040, 1300
    ),
    y = c(
      0, 0, 0, 0, 0, 0, 0,
      210, 210, 210, 210, 210, 210, 210,
      420, 420, 420, 420, 420, 420
    )
  )
}
