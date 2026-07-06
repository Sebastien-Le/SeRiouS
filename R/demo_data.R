#' Get the tAIdyverse questionnaire dataset
#'
#' @return A data frame.
#' @export
get_taidyverse_questionnaire <- function() {
  env <- new.env(parent = emptyenv())

  utils::data(
    "questionnaire_alimentaire_typologie_textes",
    package = "SeRiouS",
    envir = env
  )

  if (!exists("questionnaire_alimentaire_typologie_textes", envir = env)) {
    stop(
      "Dataset 'questionnaire_alimentaire_typologie_textes' was not found in the SeRiouS package.",
      call. = FALSE
    )
  }

  q <- get("questionnaire_alimentaire_typologie_textes", envir = env)

  variables_qualitatives <- c(
    "type_produit",
    "budget_contraint",
    "sexe",
    "age_classe",
    "lieu_achat",
    "profil_alim"
  )

  variables_qualitatives <- intersect(variables_qualitatives, names(q))
  q[variables_qualitatives] <- lapply(q[variables_qualitatives], factor)

  if ("commentaire" %in% names(q)) {
    q$commentaire <- as.character(q$commentaire)
  }

  q
}
