# data-raw/precompute_case17_nailer.R

library(FactoMineR)
library(NaileR)

# Charger les données du package
data("questionnaire_alimentaire_typologie_textes", package = "SeRiouS")

questionnaire <- questionnaire_alimentaire_typologie_textes

# ------------------------------------------------------------
# 1. Reproduire la création des classes HCPC
# ------------------------------------------------------------

variables_typologie <- c(
  "attention_prix",
  "contrainte_temps",
  "cuisine_maison",
  "lecture_labels",
  "achat_local",
  "ouverture_innovation",
  "usage_appli_alim",
  "preoccupation_sante",
  "autonomie_alimentaire",
  "confiance_labels"
)

donnees_typologie <- questionnaire[, variables_typologie]

res_pca_typologie <- FactoMineR::PCA(
  donnees_typologie,
  scale.unit = TRUE,
  graph = FALSE
)

set.seed(123)

res_hcpc_typologie <- FactoMineR::HCPC(
  res_pca_typologie,
  nb.clust = 3,
  graph = FALSE
)

questionnaire$classe_hcpc <- factor(
  res_hcpc_typologie$data.clust$clust
)

# ------------------------------------------------------------
# 2. Préparer le dataset textuel
# ------------------------------------------------------------

variables_textuelles_classes <- c(
  "classe_hcpc",
  "commentaire",
  "satisfaction",
  "intention_achat",
  "prix_percu",
  "plaisir",
  "naturalite",
  "confiance",
  "ancrage_local",
  "usage_numerique",
  "sensibilite_env",
  "attention_prix",
  "contrainte_temps",
  "cuisine_maison",
  "lecture_labels",
  "achat_local",
  "ouverture_innovation",
  "usage_appli_alim",
  "preoccupation_sante",
  "autonomie_alimentaire",
  "confiance_labels",
  "type_produit",
  "budget_contraint",
  "sexe",
  "age_classe",
  "lieu_achat",
  "profil_alim"
)

variables_textuelles_classes <- intersect(
  variables_textuelles_classes,
  names(questionnaire)
)

dataset_textuel_classes <- questionnaire[, variables_textuelles_classes]

dataset_textuel_classes$classe_hcpc <- factor(dataset_textuel_classes$classe_hcpc)
dataset_textuel_classes$commentaire <- as.character(dataset_textuel_classes$commentaire)

dataset_textuel_classes <- dataset_textuel_classes[
  !is.na(dataset_textuel_classes$commentaire) &
    nzchar(trimws(dataset_textuel_classes$commentaire)),
]

dataset_textuel_classes$classe_hcpc <- droplevels(
  factor(dataset_textuel_classes$classe_hcpc)
)

# ------------------------------------------------------------
# 3. Préparer les deux artefacts NaileR
# ------------------------------------------------------------

num_var_classes <- which(names(dataset_textuel_classes) == "classe_hcpc")
num_text_classes <- which(names(dataset_textuel_classes) == "commentaire")

res_textual_prep_classes <- NaileR::nail_textual_prep(
  dataset = dataset_textuel_classes,
  num.var = num_var_classes,
  num.text = num_text_classes,
  model = "llama3",
  generate = TRUE
)

dataset_profil_classes <- dataset_textuel_classes[
  ,
  setdiff(names(dataset_textuel_classes), "commentaire")
]

num_var_profil_classes <- which(
  names(dataset_profil_classes) == "classe_hcpc"
)

res_group_profile_classes <- NaileR::nail_group_profile_prep(
  dataset = dataset_profil_classes,
  num.var = num_var_profil_classes,
  model = "llama3",
  generate = TRUE
)

# ------------------------------------------------------------
# 4. Synthèse contextualisée générée une seule fois
# ------------------------------------------------------------

res_contextualized_classes <- NaileR::nail_textual_contextualized(
  group_profile_prep = res_group_profile_classes,
  textual_prep = res_textual_prep_classes,
  interpretation_mode = "comparative",
  model = "llama3",
  generate = TRUE
)

# ------------------------------------------------------------
# 5. Sauvegarder tous les objets utiles
# ------------------------------------------------------------

precomputed_case17 <- list(
  dataset_textuel_classes = dataset_textuel_classes,
  res_textual_prep_classes = res_textual_prep_classes,
  res_group_profile_classes = res_group_profile_classes,
  res_contextualized_classes = res_contextualized_classes,
  metadata = list(
    package = "SeRiouS",
    case = "synthese_contextualisee_classes",
    model = "llama3",
    generated_on = as.character(Sys.time()),
    note = "Résultats pré-calculés pour éviter une génération LLM en direct pendant le tutoriel."
  )
)

dir.create("inst/app/precomputed", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  precomputed_case17,
  file = "inst/app/precomputed/case17_nailer_contextualized.rds"
)
