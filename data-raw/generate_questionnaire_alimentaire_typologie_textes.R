# data-raw/generate_questionnaire_alimentaire_typologie_textes.R

questionnaire_alimentaire_typologie_textes <- read.csv(
  "data-raw/questionnaire_alimentaire_typologie_textes.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

variables_qualitatives <- c(
  "type_produit",
  "budget_contraint",
  "sexe",
  "age_classe",
  "lieu_achat",
  "profil_alim"
)

variables_qualitatives <- intersect(
  variables_qualitatives,
  names(questionnaire_alimentaire_typologie_textes)
)

questionnaire_alimentaire_typologie_textes[variables_qualitatives] <-
  lapply(
    questionnaire_alimentaire_typologie_textes[variables_qualitatives],
    factor
  )

if ("commentaire" %in% names(questionnaire_alimentaire_typologie_textes)) {
  questionnaire_alimentaire_typologie_textes$commentaire <-
    as.character(questionnaire_alimentaire_typologie_textes$commentaire)
}

usethis::use_data(
  questionnaire_alimentaire_typologie_textes,
  overwrite = TRUE
)
