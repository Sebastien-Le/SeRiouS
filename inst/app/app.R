library(shiny)
library(visNetwork)

if (!exists("questionnaire")) {
  questionnaire <- SeRiouS::questionnaire_alimentaire_typologie_textes
}

# ============================================================
# MVP — Plateau interactif : de l'explicite au latent
# ============================================================
# Dépendances : install.packages(c("shiny", "visNetwork", "FactoMineR"))
# Packages optionnels : EnTraineR, NaileR
# Lancement : shiny::runApp()
# ============================================================

parties <- data.frame(
  id = c("stat", "r_sorties", "entrainer"),
  label = c(
    "Statistique",
    "Langage R",
    "LLM"
  ),
  color = c("#BBDEFB", "#C8E6C9", "#FFE0B2"),
  border = c("#1976D2", "#2E7D32", "#EF6C00"),
  stringsAsFactors = FALSE
)

partie_color <- setNames(parties$color, parties$id)
partie_border <- setNames(parties$border, parties$id)
partie_label <- setNames(parties$label, parties$id)

make_case <- function(partie, titre, objectif, code,
                      sortie_attendue,
                      transition,
                      has_plot = FALSE,
                      question = NULL,
                      reponse = NULL,
                      validator = NULL,
                      pdf = NULL,
                      pdf_on_run = NULL,
                      code_display = NULL) {
  # `code` est le code complet exécuté par l'application.
  # Il peut contenir des cat(), print(), tryCatch() et messages pédagogiques.
  #
  # `code_display` est optionnel : il sert uniquement à afficher un code
  # plus épuré dans le bloc "Code essentiel" et dans le bouton "Ouvrir code".
  # Si aucun code_display n'est fourni, on affiche le code complet par défaut.
  if (is.null(code_display)) {
    code_display <- code
  }

  list(
    partie = partie,
    titre = titre,
    objectif = objectif,
    code = trimws(code),
    code_display = trimws(code_display),
    sortie_attendue = sortie_attendue,
    transition = transition,
    has_plot = has_plot,
    question = question,
    reponse = reponse,
    validator = validator,
    pdf = pdf,
    pdf_on_run = pdf_on_run
  )
}

get_case_display_code <- function(case) {
  # Renvoie le code à afficher à l'étudiant.
  # L'exécution continue d'utiliser `case$code` dans execute_case().
  if (!is.null(case$code_display) && nzchar(trimws(case$code_display))) {
    return(trimws(case$code_display))
  }

  case$code
}

load_precomputed_asset <- function(filename) {

  chemins_candidats <- c(
    # Cas package installé
    system.file(
      'app',
      'precomputed',
      filename,
      package = 'SeRiouS'
    ),

    # Cas développement lancé depuis la racine du package
    file.path(
      'inst',
      'app',
      'precomputed',
      filename
    ),

    # Cas développement lancé depuis inst/app
    file.path(
      'precomputed',
      filename
    ),

    # Cas où le working directory est dans inst/app mais on veut remonter
    file.path(
      '..',
      '..',
      'inst',
      'app',
      'precomputed',
      filename
    )
  )

  chemins_candidats <- unique(chemins_candidats[nzchar(chemins_candidats)])

  chemins_existants <- chemins_candidats[file.exists(chemins_candidats)]

  if (length(chemins_existants) > 0) {
    return(readRDS(chemins_existants[1]))
  }

  stop(
    paste(
      c(
        paste0('Fichier pré-calculé introuvable : ', filename),
        '',
        'Répertoire de travail courant :',
        getwd(),
        '',
        'Chemins testés :',
        paste0('- ', chemins_candidats)
      ),
      collapse = '\n'
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Ressources PDF robustes
# ------------------------------------------------------------
#
# Le dossier `www/` de Shiny fonctionne bien quand l'application est
# lancée depuis son dossier. Mais si l'on lance seulement app.R, ou si
# le répertoire de travail n'est pas celui de l'application, un iframe
# pointant vers "entrainer_presentation.pdf" peut retourner "Not found".
#
# Pour éviter cela, on sert les PDF via un chemin de ressource Shiny
# explicite : plateau_pdf/<nom_du_fichier>.pdf
#
# Si le PDF existe dans www/, il est copié dans un dossier temporaire.
# Sinon, une petite présentation PDF de secours est générée.


# ============================================================
# Gestion robuste des PDF affichés dans le plateau
# ============================================================

ensure_pdf_asset <- function(pdf_file) {
  if (is.null(pdf_file) || !nzchar(pdf_file)) {
    return(NULL)
  }

  # Si c'est déjà une URL, on la renvoie telle quelle.
  if (grepl("^(http|https)://", pdf_file)) {
    return(pdf_file)
  }

  # Cas le plus simple :
  # le PDF est dans le dossier www/ de l'application Shiny.
  #
  # Exemple :
  # inst/app/www/questionnaire_SeRiouS_description.pdf
  #
  # Dans ce cas, Shiny le sert directement avec :
  # src = "questionnaire_SeRiouS_description.pdf"
  if (file.exists(file.path("www", pdf_file))) {
    return(pdf_file)
  }

  # Cas package installé :
  # on cherche dans le dossier app/www du package SeRiouS.
  app_www <- system.file("app", "www", package = "SeRiouS")

  if (nzchar(app_www)) {
    packaged_pdf <- file.path(app_www, pdf_file)

    if (file.exists(packaged_pdf)) {
      return(pdf_file)
    }
  }

  # Cas où pdf_file serait un chemin complet ou relatif vers un fichier existant.
  # On le copie alors dans un dossier temporaire servi explicitement par Shiny.
  if (file.exists(pdf_file)) {
    pdf_asset_dir <- file.path(tempdir(), "serious_pdf_assets")
    dir.create(pdf_asset_dir, recursive = TRUE, showWarnings = FALSE)

    shiny::addResourcePath(
      prefix = "serious_pdf",
      directoryPath = pdf_asset_dir
    )

    dest <- file.path(pdf_asset_dir, basename(pdf_file))
    file.copy(pdf_file, dest, overwrite = TRUE)

    return(file.path("serious_pdf", basename(pdf_file)))
  }

  warning(
    "PDF introuvable : ",
    pdf_file,
    call. = FALSE
  )

  NULL
}

plateau_pdf_asset_dir <- file.path(tempdir(), "plateau_pdf_assets")

if (!dir.exists(plateau_pdf_asset_dir)) {
  dir.create(plateau_pdf_asset_dir, recursive = TRUE, showWarnings = FALSE)
}

try(
  shiny::addResourcePath("plateau_pdf", plateau_pdf_asset_dir),
  silent = TRUE
)

create_fallback_entrainer_pdf <- function(path) {
  grDevices::pdf(path, width = 11, height = 6.2, onefile = TRUE)

  op <- par(mar = c(0, 0, 0, 0))

  plot.new()
  text(0.5, 0.84, "EnTraineR", cex = 2.6, font = 2, col = "#EF6C00")
  text(
    0.5, 0.70,
    "Transformer des sorties statistiques explicites en prompts pédagogiques contrôlés",
    cex = 1.15
  )
  text(0.08, 0.55, "• Objectif : rendre inspectable le passage analyse statistique → prompt.", adj = 0, cex = 1.0)
  text(0.08, 0.45, "• Principe : extraire, structurer, puis formuler une tâche interprétative.", adj = 0, cex = 1.0)
  text(0.08, 0.35, "• En atelier : montrer que le prompt est un objet construit.", adj = 0, cex = 1.0)

  plot.new()
  text(0.5, 0.84, "Position dans le workflow", cex = 2.0, font = 2, col = "#EF6C00")
  text(0.08, 0.66, "Analyse statistique", adj = 0, cex = 1.1)
  arrows(0.34, 0.66, 0.46, 0.66, length = 0.08)
  text(0.52, 0.66, "Sortie structurée", adj = 0, cex = 1.1)
  arrows(0.72, 0.66, 0.84, 0.66, length = 0.08)
  text(0.08, 0.49, "Prompt contrôlé", adj = 0, cex = 1.1)
  arrows(0.31, 0.49, 0.43, 0.49, length = 0.08)
  text(0.50, 0.49, "Réponse LLM éventuelle", adj = 0, cex = 1.1)
  text(0.08, 0.28, "Le modèle intervient après la structuration des résultats.", adj = 0, cex = 1.0)

  plot.new()
  text(0.5, 0.84, "Point clé", cex = 2.0, font = 2, col = "#EF6C00")
  text(0.08, 0.64, "generate = FALSE", adj = 0, cex = 1.4, font = 2)
  text(0.08, 0.52, "permet de contrôler le prompt avant tout appel au modèle.", adj = 0, cex = 1.1)
  text(0.08, 0.38, "L'analyste reste maître du workflow.", adj = 0, cex = 1.1)
  text(0.08, 0.28, "Le LLM ne remplace pas l'analyse : il intervient en aval.", adj = 0, cex = 1.1)

  par(op)
  grDevices::dev.off()

  invisible(path)
}

find_packaged_pdf <- function(filename) {
  filename <- basename(filename)

  candidates <- unique(c(
    file.path(getwd(), "www", filename),
    file.path("www", filename),
    file.path(dirname(normalizePath("app.R", mustWork = FALSE)), "www", filename)
  ))

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    return(NULL)
  }

  existing[[1]]
}

materialise_pdf_for_shiny <- function(filename) {
  filename <- basename(filename)
  target <- file.path(plateau_pdf_asset_dir, filename)

  if (!file.exists(target)) {
    source_pdf <- find_packaged_pdf(filename)

    if (!is.null(source_pdf)) {
      file.copy(source_pdf, target, overwrite = TRUE)
    } else {
      create_fallback_entrainer_pdf(target)
    }
  }

  paste0("plateau_pdf/", utils::URLencode(filename, reserved = TRUE))
}

cases <- list(
  donnees = make_case(
    partie = "stat",
    titre = "1. Questionnaire",
    objectif = "Présenter le questionnaire alimentaire utilisé dans tout le tutoriel : variables quantitatives, qualitatives et textuelles.",
    has_plot = FALSE,
    pdf = "questionnaire_SeRiouS_description.pdf",
    code = "
# ============================================================
# Case 1 : présenter le questionnaire
# ============================================================

# Objectif de cette case :
# comprendre le jeu de données utilisé dans tout le tutoriel.
#
# Le questionnaire n est plus simulé ici.
# Il est supposé déjà disponible dans le package.
#
# Il contient :
# - des variables d évaluation du produit ;
# - des variables décrivant le rapport à l alimentation ;
# - des variables qualitatives de contexte ;
# - une variable textuelle ;
# - des variables actives qui serviront ensuite à construire une typologie.

# ------------------------------------------------------------
# 1. Récupérer l objet questionnaire
# ------------------------------------------------------------

# Dans le package, le jeu de données pourra être chargé sous le nom :
# questionnaire_alimentaire_typologie_textes
#
# Pour simplifier le reste du tutoriel, on crée ou vérifie l objet
# questionnaire, qui sera le nom utilisé dans toutes les cases suivantes.

if (!exists('questionnaire')) {

  if (exists('questionnaire_alimentaire_typologie_textes')) {

    questionnaire <- questionnaire_alimentaire_typologie_textes

  } else if (requireNamespace('SeRiouS', quietly = TRUE)) {

    questionnaire <- SeRiouS::questionnaire_alimentaire_typologie_textes

  } else {

    stop(
      'Aucun objet questionnaire disponible. ',
      'Le jeu de données questionnaire_alimentaire_typologie_textes doit être intégré au package SeRiouS.'
    )
  }
}

cat('\\n============================================================\\n')
cat('1. Jeu de données utilisé\\n')
cat('============================================================\\n')
cat('Objet utilisé : questionnaire\\n')
cat('Nombre de répondants : ', nrow(questionnaire), '\\n', sep = '')
cat('Nombre de variables : ', ncol(questionnaire), '\\n', sep = '')


# ------------------------------------------------------------
# 2. Sécuriser les types de variables
# ------------------------------------------------------------

# Certaines variables doivent être traitées comme qualitatives.
# On les force donc en factor si elles existent dans le jeu de données.

variables_qualitatives <- c(
  'type_produit',
  'budget_contraint',
  'sexe',
  'age_classe',
  'lieu_achat',
  'profil_alim'
)

variables_qualitatives <- intersect(
  variables_qualitatives,
  names(questionnaire)
)

questionnaire[variables_qualitatives] <- lapply(
  questionnaire[variables_qualitatives],
  factor
)

if ('commentaire' %in% names(questionnaire)) {
  questionnaire$commentaire <- as.character(questionnaire$commentaire)
}

cat('\\n============================================================\\n')
cat('2. Types de variables vérifiés\\n')
cat('============================================================\\n')
cat('Variables qualitatives converties en factor :\\n')
print(variables_qualitatives)

if ('commentaire' %in% names(questionnaire)) {
  cat('\\nVariable textuelle : commentaire\\n')
}


# ------------------------------------------------------------
# 3. Construire un dictionnaire simplifié des variables
# ------------------------------------------------------------

dictionnaire_variables <- data.frame(
  variable = c(
    'id',
    'satisfaction',
    'intention_achat',
    'prix_percu',
    'plaisir',
    'naturalite',
    'confiance',
    'ancrage_local',
    'usage_numerique',
    'sensibilite_env',
    'attention_prix',
    'contrainte_temps',
    'cuisine_maison',
    'lecture_labels',
    'achat_local',
    'ouverture_innovation',
    'usage_appli_alim',
    'preoccupation_sante',
    'autonomie_alimentaire',
    'confiance_labels',
    'type_produit',
    'budget_contraint',
    'sexe',
    'age_classe',
    'lieu_achat',
    'profil_alim',
    'commentaire'
  ),
  famille = c(
    'Identifiant',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Rapport à l information',
    'Rapport à l environnement',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Contexte produit',
    'Contexte répondant',
    'Contexte répondant',
    'Contexte répondant',
    'Contexte d achat',
    'Profil alimentaire explicite',
    'Texte libre'
  ),
  role_dans_le_tutoriel = c(
    'repérer les individus',
    'variable réponse possible',
    'variable réponse pour la régression',
    'variable explicative et descriptive',
    'variable explicative',
    'variable explicative et descriptive',
    'variable explicative',
    'variable explicative et descriptive',
    'variable descriptive',
    'variable descriptive',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'facteur pour ANOVA',
    'facteur pour ANOVA',
    'variable illustrative',
    'variable illustrative',
    'variable illustrative',
    'variable qualitative explicite pour catdes / nail_catdes',
    'variable textuelle pour le flow NaileR'
  ),
  stringsAsFactors = FALSE
)

# On ne garde dans le dictionnaire que les variables réellement présentes.
dictionnaire_variables <- dictionnaire_variables[
  dictionnaire_variables$variable %in% names(questionnaire),
]

cat('\\n============================================================\\n')
cat('3. Dictionnaire simplifié des variables\\n')
cat('============================================================\\n')
cat('Objet créé : dictionnaire_variables\\n')
print(dictionnaire_variables, row.names = FALSE)


# ------------------------------------------------------------
# 4. Vérifier la structure R du questionnaire
# ------------------------------------------------------------

structure_variables <- data.frame(
  variable = names(questionnaire),
  classe_R = vapply(questionnaire, function(x) class(x)[1], character(1)),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('4. Structure R du questionnaire\\n')
cat('============================================================\\n')
cat('Objet créé : structure_variables\\n')
print(structure_variables, row.names = FALSE)


# ------------------------------------------------------------
# 5. Présenter les variables clés du tutoriel
# ------------------------------------------------------------

variables_cles <- data.frame(
  etape_du_tutoriel = c(
    'Régression linéaire',
    'ANOVA',
    'condes()',
    'catdes() / nail_catdes()',
    'ACP + HCPC',
    'Flow textuel NaileR'
  ),
  variables_utilisees = c(
    'intention_achat ~ satisfaction + prix_percu',
    'satisfaction ~ type_produit * budget_contraint',
    'intention_achat décrite par les autres variables',
    'profil_alim décrit par les autres variables',
    'variables actives de typologie',
    'classe_hcpc + commentaire'
  ),
  objectif = c(
    'modéliser une intention d achat',
    'tester des effets de facteurs et une interaction',
    'décrire une variable quantitative',
    'décrire une variable qualitative explicite',
    'construire une classe latente',
    'interpréter les classes à partir des verbatims'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('5. Variables clés pour la suite du plateau\\n')
cat('============================================================\\n')
cat('Objet créé : variables_cles\\n')
print(variables_cles, row.names = FALSE)


# ------------------------------------------------------------
# 6. Vérifier les variables actives prévues pour la typologie
# ------------------------------------------------------------

variables_typologie <- c(
  'attention_prix',
  'contrainte_temps',
  'cuisine_maison',
  'lecture_labels',
  'achat_local',
  'ouverture_innovation',
  'usage_appli_alim',
  'preoccupation_sante',
  'autonomie_alimentaire',
  'confiance_labels'
)

variables_typologie_presentes <- intersect(
  variables_typologie,
  names(questionnaire)
)

cat('\\n============================================================\\n')
cat('6. Variables actives prévues pour la typologie\\n')
cat('============================================================\\n')
cat('Objet créé : variables_typologie\\n')
print(variables_typologie_presentes)

if (length(variables_typologie_presentes) < length(variables_typologie)) {
  cat('\\nAttention : certaines variables de typologie sont absentes du questionnaire.\\n')
  cat('Variables attendues mais absentes :\\n')
  print(setdiff(variables_typologie, variables_typologie_presentes))
}


# ------------------------------------------------------------
# 7. Aperçu du questionnaire
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('7. Aperçu des premières lignes\\n')
cat('============================================================\\n')
print(head(questionnaire, 3))


# ------------------------------------------------------------
# 8. Aperçu de la variable textuelle
# ------------------------------------------------------------

if ('commentaire' %in% names(questionnaire)) {

  cat('\\n============================================================\\n')
  cat('8. Aperçu des commentaires libres\\n')
  cat('============================================================\\n')
  cat('Nombre de verbatims : ', length(questionnaire$commentaire), '\\n', sep = '')
  cat('Nombre de verbatims uniques : ', length(unique(questionnaire$commentaire)), '\\n', sep = '')
  cat('\\nExemples :\\n')
  cat(paste('-', head(unique(questionnaire$commentaire), 6), collapse = '\\n'))
  cat('\\n')
}


# ------------------------------------------------------------
# 9. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('Le questionnaire est le fil rouge du tutoriel.\\n')
cat('Il contient des variables explicites, un profil alimentaire calculé,\\n')
cat('des variables actives pour construire une typologie,\\n')
cat('et une variable textuelle.\\n')
cat('\\n')
cat('La suite du plateau va montrer comment passer :\\n')
cat('- des analyses simples ;\\n')
cat('- aux sorties FactoMineR ;\\n')
cat('- aux prompts ;\\n')
cat('- puis à la compréhension des classes latentes.\\n')
",
code_display = "
# Récupérer le questionnaire utilisé dans tout le tutoriel
if (!exists('questionnaire')) {

  if (exists('questionnaire_alimentaire_typologie_textes')) {

    questionnaire <- questionnaire_alimentaire_typologie_textes

  } else if (requireNamespace('SeRiouS', quietly = TRUE)) {

    questionnaire <- SeRiouS::questionnaire_alimentaire_typologie_textes

  } else {

    stop(
      'Aucun objet questionnaire disponible. ',
      'Le jeu de données questionnaire_alimentaire_typologie_textes doit être intégré au package SeRiouS.'
    )
  }
}

# Dimensions du questionnaire
nrow(questionnaire)
ncol(questionnaire)

# Sécuriser les variables qualitatives
variables_qualitatives <- c(
  'type_produit',
  'budget_contraint',
  'sexe',
  'age_classe',
  'lieu_achat',
  'profil_alim'
)

variables_qualitatives <- intersect(
  variables_qualitatives,
  names(questionnaire)
)

questionnaire[variables_qualitatives] <- lapply(
  questionnaire[variables_qualitatives],
  factor
)

# Sécuriser la variable textuelle
if ('commentaire' %in% names(questionnaire)) {
  questionnaire$commentaire <- as.character(questionnaire$commentaire)
}

# Construire un dictionnaire simplifié des variables
dictionnaire_variables <- data.frame(
  variable = c(
    'id',
    'satisfaction',
    'intention_achat',
    'prix_percu',
    'plaisir',
    'naturalite',
    'confiance',
    'ancrage_local',
    'usage_numerique',
    'sensibilite_env',
    'attention_prix',
    'contrainte_temps',
    'cuisine_maison',
    'lecture_labels',
    'achat_local',
    'ouverture_innovation',
    'usage_appli_alim',
    'preoccupation_sante',
    'autonomie_alimentaire',
    'confiance_labels',
    'type_produit',
    'budget_contraint',
    'sexe',
    'age_classe',
    'lieu_achat',
    'profil_alim',
    'commentaire'
  ),
  famille = c(
    'Identifiant',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Évaluation du produit',
    'Rapport à l information',
    'Rapport à l environnement',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Variables actives de typologie',
    'Contexte produit',
    'Contexte répondant',
    'Contexte répondant',
    'Contexte répondant',
    'Contexte d achat',
    'Profil alimentaire explicite',
    'Texte libre'
  ),
  role_dans_le_tutoriel = c(
    'repérer les individus',
    'variable réponse possible',
    'variable réponse pour la régression',
    'variable explicative et descriptive',
    'variable explicative',
    'variable explicative et descriptive',
    'variable explicative',
    'variable explicative et descriptive',
    'variable descriptive',
    'variable descriptive',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'variable active pour ACP / typologie',
    'facteur pour ANOVA',
    'facteur pour ANOVA',
    'variable illustrative',
    'variable illustrative',
    'variable illustrative',
    'variable qualitative explicite pour catdes / nail_catdes',
    'variable textuelle pour le flow NaileR'
  ),
  stringsAsFactors = FALSE
)

dictionnaire_variables <- dictionnaire_variables[
  dictionnaire_variables$variable %in% names(questionnaire),
]

# Décrire la structure R du questionnaire
structure_variables <- data.frame(
  variable = names(questionnaire),
  classe_R = vapply(questionnaire, function(x) class(x)[1], character(1)),
  stringsAsFactors = FALSE
)

# Identifier les variables clés du tutoriel
variables_cles <- data.frame(
  etape_du_tutoriel = c(
    'Régression linéaire',
    'ANOVA',
    'condes()',
    'catdes() / nail_catdes()',
    'ACP + HCPC',
    'Flow textuel NaileR'
  ),
  variables_utilisees = c(
    'intention_achat ~ satisfaction + prix_percu',
    'satisfaction ~ type_produit * budget_contraint',
    'intention_achat décrite par les autres variables',
    'profil_alim décrit par les autres variables',
    'variables actives de typologie',
    'classe_hcpc + commentaire'
  ),
  objectif = c(
    'modéliser une intention d achat',
    'tester des effets de facteurs et une interaction',
    'décrire une variable quantitative',
    'décrire une variable qualitative explicite',
    'construire une classe latente',
    'interpréter les classes à partir des verbatims'
  ),
  stringsAsFactors = FALSE
)

# Définir les variables actives prévues pour la typologie
variables_typologie <- c(
  'attention_prix',
  'contrainte_temps',
  'cuisine_maison',
  'lecture_labels',
  'achat_local',
  'ouverture_innovation',
  'usage_appli_alim',
  'preoccupation_sante',
  'autonomie_alimentaire',
  'confiance_labels'
)

variables_typologie_presentes <- intersect(
  variables_typologie,
  names(questionnaire)
)

# Vérifier rapidement les objets créés
dictionnaire_variables
structure_variables
variables_cles
variables_typologie_presentes

# Aperçu du questionnaire et des commentaires libres
head(questionnaire, 3)

if ('commentaire' %in% names(questionnaire)) {
  head(unique(questionnaire$commentaire), 6)
}
",
sortie_attendue = "Une présentation structurée du questionnaire et des variables utilisées dans le tutoriel.",
transition = "On peut maintenant explorer le jeu de données avant de modéliser.",
question = "Quel objet contient le questionnaire utilisé dans le tutoriel ?",
reponse = "questionnaire"
  ),

exploration = make_case(
  partie = "stat",
  titre = "2. Explorer",
  objectif = "Identifier les types de variables et les premières relations visuelles.",
  has_plot = TRUE,
  code = "
# ============================================================
# Case 2 : explorer rapidement les données
# ============================================================

# Objectif de cette case :
# produire une première description du jeu de données

# ------------------------------------------------------------
# 1. Identifier les variables quantitatives
# ------------------------------------------------------------

variables_quanti_exploration <- setdiff(
  names(questionnaire)[vapply(questionnaire, is.numeric, logical(1))],
  'id'
)

cat('\\n============================================================\\n')
cat('1. Variables quantitatives identifiées\\n')
cat('============================================================\\n')
print(variables_quanti_exploration)


# ------------------------------------------------------------
# 2. Construire un résumé des variables quantitatives
# ------------------------------------------------------------

resume_quanti <- data.frame(
  variable = variables_quanti_exploration,
  min = vapply(
    questionnaire[variables_quanti_exploration],
    min,
    numeric(1),
    na.rm = TRUE
  ),
  q1 = vapply(
    questionnaire[variables_quanti_exploration],
    function(x) quantile(x, 0.25, na.rm = TRUE),
    numeric(1)
  ),
  mediane = vapply(
    questionnaire[variables_quanti_exploration],
    median,
    numeric(1),
    na.rm = TRUE
  ),
  moyenne = vapply(
    questionnaire[variables_quanti_exploration],
    mean,
    numeric(1),
    na.rm = TRUE
  ),
  q3 = vapply(
    questionnaire[variables_quanti_exploration],
    function(x) quantile(x, 0.75, na.rm = TRUE),
    numeric(1)
  ),
  max = vapply(
    questionnaire[variables_quanti_exploration],
    max,
    numeric(1),
    na.rm = TRUE
  ),
  row.names = NULL
)

resume_quanti[, -1] <- round(resume_quanti[, -1], 2)

cat('\\n============================================================\\n')
cat('2. Résumé des variables quantitatives\\n')
cat('============================================================\\n')
cat('Objet créé : resume_quanti\\n')
print(resume_quanti, row.names = FALSE)


# ------------------------------------------------------------
# 3. Identifier les variables qualitatives
# ------------------------------------------------------------

variables_quali_exploration <- names(questionnaire)[
  vapply(questionnaire, is.factor, logical(1))
]

cat('\\n============================================================\\n')
cat('3. Variables qualitatives identifiées\\n')
cat('============================================================\\n')
print(variables_quali_exploration)


# ------------------------------------------------------------
# 4. Construire un résumé des variables qualitatives
# ------------------------------------------------------------

resume_quali <- do.call(
  rbind,
  lapply(variables_quali_exploration, function(v) {
    tab <- table(questionnaire[[v]], useNA = 'ifany')

    data.frame(
      variable = v,
      modalite = names(tab),
      effectif = as.integer(tab),
      row.names = NULL
    )
  })
)

cat('\\n============================================================\\n')
cat('4. Résumé des variables qualitatives\\n')
cat('============================================================\\n')
cat('Objet créé : resume_quali\\n')
print(resume_quali, row.names = FALSE)


# ------------------------------------------------------------
# 5. Explorer la variable textuelle
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Variable textuelle\\n')
cat('============================================================\\n')
cat('Variable : commentaire\\n')
cat('Nombre de verbatims : ', length(questionnaire$commentaire), '\\n', sep = '')
cat('Nombre de verbatims uniques : ', length(unique(questionnaire$commentaire)), '\\n', sep = '')
cat('Exemples :\\n')
cat(paste('-', head(unique(questionnaire$commentaire), 5), collapse = '\\n'))
cat('\\n')


# ------------------------------------------------------------
# 6. Produire deux graphiques exploratoires
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('6. Graphiques exploratoires\\n')
cat('============================================================\\n')
cat('Graphique 1 : distribution de intention_achat\\n')
cat('Graphique 2 : satisfaction selon type_produit\\n')

op <- par(mfrow = c(1, 2))

hist(
  questionnaire$intention_achat,
  main = 'Intention d achat',
  xlab = 'Score',
  col = 'grey80',
  border = 'white'
)

boxplot(
  satisfaction ~ type_produit,
  data = questionnaire,
  main = 'Satisfaction selon produit',
  xlab = 'Groupe produit',
  ylab = 'Satisfaction',
  col = 'grey85'
)

par(op)


# ------------------------------------------------------------
# 7. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- variables_quanti_exploration : noms des variables quantitatives\\n')
cat('- resume_quanti : résumé numérique\\n')
cat('- variables_quali_exploration : noms des variables qualitatives\\n')
cat('- resume_quali : effectifs par modalité\\n')
",
code_display = "
# Identifier les variables quantitatives
variables_quanti_exploration <- setdiff(
  names(questionnaire)[vapply(questionnaire, is.numeric, logical(1))],
  'id'
)

variables_quanti_exploration

# Construire un résumé des variables quantitatives
resume_quanti <- data.frame(
  variable = variables_quanti_exploration,
  min = vapply(
    questionnaire[variables_quanti_exploration],
    min,
    numeric(1),
    na.rm = TRUE
  ),
  q1 = vapply(
    questionnaire[variables_quanti_exploration],
    function(x) quantile(x, 0.25, na.rm = TRUE),
    numeric(1)
  ),
  mediane = vapply(
    questionnaire[variables_quanti_exploration],
    median,
    numeric(1),
    na.rm = TRUE
  ),
  moyenne = vapply(
    questionnaire[variables_quanti_exploration],
    mean,
    numeric(1),
    na.rm = TRUE
  ),
  q3 = vapply(
    questionnaire[variables_quanti_exploration],
    function(x) quantile(x, 0.75, na.rm = TRUE),
    numeric(1)
  ),
  max = vapply(
    questionnaire[variables_quanti_exploration],
    max,
    numeric(1),
    na.rm = TRUE
  ),
  row.names = NULL
)

resume_quanti[, -1] <- round(resume_quanti[, -1], 2)

resume_quanti

# Identifier les variables qualitatives
variables_quali_exploration <- names(questionnaire)[
  vapply(questionnaire, is.factor, logical(1))
]

variables_quali_exploration

# Construire un résumé des variables qualitatives
resume_quali <- do.call(
  rbind,
  lapply(variables_quali_exploration, function(v) {
    tab <- table(questionnaire[[v]], useNA = 'ifany')

    data.frame(
      variable = v,
      modalite = names(tab),
      effectif = as.integer(tab),
      row.names = NULL
    )
  })
)

resume_quali

# Explorer la variable textuelle
length(questionnaire$commentaire)
length(unique(questionnaire$commentaire))
head(unique(questionnaire$commentaire), 5)

# Produire deux graphiques exploratoires
op <- par(mfrow = c(1, 2))

hist(
  questionnaire$intention_achat,
  main = 'Intention d achat',
  xlab = 'Score',
  col = 'grey80',
  border = 'white'
)

boxplot(
  satisfaction ~ type_produit,
  data = questionnaire,
  main = 'Satisfaction selon produit',
  xlab = 'Groupe produit',
  ylab = 'Satisfaction',
  col = 'grey85'
)

par(op)
",
sortie_attendue = "Des résumés lisibles des variables et deux graphiques descriptifs.",
transition = "On commence par une régression explicite : une variable Y expliquée par quelques X.",
question = "Combien de lignes contient le jeu de données `questionnaire` ?",
validator = function(answer, envir) {
  if (!exists("questionnaire", envir = envir)) return(FALSE)
  suppressWarnings(as.numeric(answer) == nrow(get("questionnaire", envir = envir)))
}
),

linearmodel = make_case(
  partie = "stat",
  titre = "3. Régression linéaire",
  objectif = "Ajuster une régression linéaire avec FactoMineR::LinearModel().",
  has_plot = TRUE,
  code = "
# ============================================================
# Case 3 : régression linéaire avec LinearModel()
# ============================================================

# Objectif de cette case :
# expliquer une variable quantitative Y par deux variables explicatives.
# Ici : intention_achat est expliquée par satisfaction et prix_percu.

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}


# ------------------------------------------------------------
# 1. Définir la formule du modèle
# ------------------------------------------------------------

formule_lm <- intention_achat ~ satisfaction + prix_percu

cat('\\n============================================================\\n')
cat('1. Formule du modèle linéaire\\n')
cat('============================================================\\n')
cat('Objet créé : formule_lm\\n')
print(formule_lm)

cat('\\nLecture :\\n')
cat('- Y : intention_achat\\n')
cat('- X : satisfaction et prix_percu\\n')


# ------------------------------------------------------------
# 2. Ajuster le modèle avec FactoMineR::LinearModel()
# ------------------------------------------------------------

res_lm_fm <- FactoMineR::LinearModel(
  formule_lm,
  data = questionnaire,
  selection = 'none'
)

cat('\\n============================================================\\n')
cat('2. Modèle ajusté avec FactoMineR::LinearModel()\\n')
cat('============================================================\\n')
cat('Objet créé : res_lm_fm\\n')
cat('Le modèle complet n est pas imprimé brutalement ici.\\n')
cat('On affiche un aperçu lisible ; les sous-objets seront récupérés dans la case 5.\\n')


# ------------------------------------------------------------
# 3. Afficher les résultats principaux avec des titres
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Test global des effets : res_lm_fm$Ftest\\n')
cat('============================================================\\n')
print(res_lm_fm$Ftest)

cat('\\n============================================================\\n')
cat('4. Coefficients du modèle : res_lm_fm$Ttest\\n')
cat('============================================================\\n')
print(res_lm_fm$Ttest)


# ------------------------------------------------------------
# 4. Graphique exploratoire de la relation principale
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Graphique\\n')
cat('============================================================\\n')
cat('Nuage de points : intention_achat en fonction de satisfaction.\\n')
cat('La droite affichée est une régression simple illustrative.\\n')

plot(
  questionnaire$satisfaction,
  questionnaire$intention_achat,
  pch = 16,
  col = 'grey40',
  xlab = 'Satisfaction',
  ylab = 'Intention d achat',
  main = 'Intention d achat ~ Satisfaction'
)

abline(
  lm(intention_achat ~ satisfaction, data = questionnaire),
  lwd = 2
)


# ------------------------------------------------------------
# 5. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- formule_lm : formule du modèle linéaire\\n')
cat('- res_lm_fm : résultat complet de FactoMineR::LinearModel()\\n')
cat('\\n')
cat('Ces objets seront utilisés pour récupérer les sorties et construire un prompt.\\n')
",
code_display = "
# Vérifier que FactoMineR est disponible
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}

# Définir la formule du modèle linéaire
formule_lm <- intention_achat ~ satisfaction + prix_percu

formule_lm

# Ajuster le modèle avec FactoMineR::LinearModel()
res_lm_fm <- FactoMineR::LinearModel(
  formule_lm,
  data = questionnaire,
  selection = 'none'
)

# Examiner les résultats principaux
res_lm_fm$Ftest
res_lm_fm$Ttest

# Graphique exploratoire de la relation entre satisfaction et intention d achat
plot(
  questionnaire$satisfaction,
  questionnaire$intention_achat,
  pch = 16,
  col = 'grey40',
  xlab = 'Satisfaction',
  ylab = 'Intention d achat',
  main = 'Intention d achat ~ Satisfaction'
)

abline(
  lm(intention_achat ~ satisfaction, data = questionnaire),
  lwd = 2
)
",
sortie_attendue = "Un objet `formule_lm` et un objet `res_lm_fm` contenant notamment F-tests, T-tests et résumé du modèle.",
transition = "On passe ensuite à une ANOVA explicite avec interaction.",
question = "Quelle fonction de FactoMineR est utilisée pour la régression linéaire ?",
reponse = "LinearModel"
),

aovsum = make_case(
  partie = "stat",
  titre = "4. Analyse de variance",
  objectif = "Ajuster une ANOVA avec interaction avec FactoMineR::AovSum().",
  has_plot = TRUE,
  code = "
# ============================================================
# Case 4 : ANOVA avec AovSum()
# ============================================================

# Objectif de cette case :
# expliquer une variable quantitative par des facteurs qualitatifs
# et examiner une interaction entre facteurs.

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}


# ------------------------------------------------------------
# 1. Définir la formule de l ANOVA
# ------------------------------------------------------------

formule_aov <- satisfaction ~ type_produit * budget_contraint

cat('\\n============================================================\\n')
cat('1. Formule de l ANOVA\\n')
cat('============================================================\\n')
cat('Objet créé : formule_aov\\n')
print(formule_aov)

cat('\\nLecture :\\n')
cat('- Y : satisfaction\\n')
cat('- Facteur 1 : type_produit\\n')
cat('- Facteur 2 : budget_contraint\\n')
cat('- Interaction : type_produit:budget_contraint\\n')


# ------------------------------------------------------------
# 2. Ajuster l ANOVA avec FactoMineR::AovSum()
# ------------------------------------------------------------

res_aovsum <- FactoMineR::AovSum(
  formule_aov,
  data = questionnaire
)

cat('\\n============================================================\\n')
cat('2. ANOVA ajustée avec FactoMineR::AovSum()\\n')
cat('============================================================\\n')
cat('Objet créé : res_aovsum\\n')
cat('Le résultat complet n est pas imprimé brutalement ici.\\n')
cat('On affiche un aperçu lisible ; les sous-objets seront récupérés dans la case 5.\\n')


# ------------------------------------------------------------
# 3. Afficher les résultats principaux avec des titres
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Effets principaux et interaction : res_aovsum$Ftest\\n')
cat('============================================================\\n')
print(res_aovsum$Ftest)

cat('\\n============================================================\\n')
cat('4. Coefficients et contrastes : res_aovsum$Ttest\\n')
cat('============================================================\\n')
print(res_aovsum$Ttest)


# ------------------------------------------------------------
# 4. Graphique d interaction
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Graphique d interaction\\n')
cat('============================================================\\n')
cat('On visualise la satisfaction moyenne selon le budget et le type de produit.\\n')

interaction.plot(
  x.factor = questionnaire$budget_contraint,
  trace.factor = questionnaire$type_produit,
  response = questionnaire$satisfaction,
  xlab = 'Budget contraint',
  ylab = 'Satisfaction moyenne',
  trace.label = 'Produit',
  main = 'Interaction produit x budget'
)


# ------------------------------------------------------------
# 5. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- formule_aov : formule de l ANOVA\\n')
cat('- res_aovsum : résultat complet de FactoMineR::AovSum()\\n')
cat('\\n')
cat('Ces objets seront utilisés pour récupérer les sorties et construire un prompt.\\n')
",
code_display = "
# Vérifier que FactoMineR est disponible
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}

# Définir la formule de l ANOVA
formule_aov <- satisfaction ~ type_produit * budget_contraint

formule_aov

# Ajuster l ANOVA avec FactoMineR::AovSum()
res_aovsum <- FactoMineR::AovSum(
  formule_aov,
  data = questionnaire
)

# Examiner les résultats principaux
res_aovsum$Ftest
res_aovsum$Ttest

# Visualiser l interaction entre type de produit et contrainte budgétaire
interaction.plot(
  x.factor = questionnaire$budget_contraint,
  trace.factor = questionnaire$type_produit,
  response = questionnaire$satisfaction,
  xlab = 'Budget contraint',
  ylab = 'Satisfaction moyenne',
  trace.label = 'Produit',
  main = 'Interaction produit x budget'
)
",
sortie_attendue = "Un objet `formule_aov` et un objet `res_aovsum` avec les F-tests et T-tests de l'ANOVA.",
transition = "Les sorties affichées deviennent maintenant des objets à récupérer.",
question = "Quelle fonction de FactoMineR est utilisée pour l'analyse de la variance ?",
reponse = "AovSum"
),

recuperer_sorties = make_case(
  partie = "r_sorties",
  titre = "5. Récupérer une sortie",
  objectif = "Ne plus seulement lire la console : récupérer les sorties dans des objets R.",
  has_plot = FALSE,
  pdf_on_run = "recuperer.pdf",
  code = "
# ============================================================
# Case 5 : récupérer les sorties
# ============================================================

# Objectif de cette case :
# passer d'une sortie affichée dans la console
# à des objets R que l'on pourra réutiliser ensuite.

# ------------------------------------------------------------
# 1. Inspecter les objets retournés par FactoMineR
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('1. Que contient l objet res_lm_fm ?\\n')
cat('Commande : names(res_lm_fm)\\n')
cat('============================================================\\n')
print(names(res_lm_fm))

cat('\\n============================================================\\n')
cat('2. Que contient l objet res_aovsum ?\\n')
cat('Commande : names(res_aovsum)\\n')
cat('============================================================\\n')
print(names(res_aovsum))


# ------------------------------------------------------------
# 2. Extraire les sorties importantes de LinearModel()
# ------------------------------------------------------------

lm_ftest <- res_lm_fm$Ftest
lm_ttest <- res_lm_fm$Ttest
lm_resume <- res_lm_fm$lmResult


# ------------------------------------------------------------
# 3. Extraire les sorties importantes de AovSum()
# ------------------------------------------------------------

aov_ftest <- res_aovsum$Ftest
aov_ttest <- res_aovsum$Ttest


# ------------------------------------------------------------
# 4. Afficher les objets extraits avec des titres explicites
# ------------------------------------------------------------

afficher_sortie <- function(titre, commande, objet) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('Objet créé : ', commande, '\\n', sep = '')
  cat('============================================================\\n')
  print(objet)
}

afficher_sortie(
  titre = 'LinearModel : test global des effets — Ftest',
  commande = 'lm_ftest <- res_lm_fm$Ftest',
  objet = lm_ftest
)

afficher_sortie(
  titre = 'LinearModel : coefficients du modèle — Ttest',
  commande = 'lm_ttest <- res_lm_fm$Ttest',
  objet = lm_ttest
)

afficher_sortie(
  titre = 'AovSum : effets principaux et interaction — Ftest',
  commande = 'aov_ftest <- res_aovsum$Ftest',
  objet = aov_ftest
)

afficher_sortie(
  titre = 'AovSum : coefficients et contrastes — Ttest',
  commande = 'aov_ttest <- res_aovsum$Ttest',
  objet = aov_ttest
)


# ------------------------------------------------------------
# 5. Capturer les sorties complètes sous forme de texte
# ------------------------------------------------------------

texte_linearmodel <- paste(
  capture.output(print(res_lm_fm)),
  collapse = '\\n'
)

texte_aovsum <- paste(
  capture.output(print(res_aovsum)),
  collapse = '\\n'
)


# ------------------------------------------------------------
# 6. Afficher un aperçu des textes capturés
# ------------------------------------------------------------

afficher_texte_capture <- function(titre, nom_objet, texte, n = 1200) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('Objet créé : ', nom_objet, '\\n', sep = '')
  cat('============================================================\\n')
  cat(substr(texte, 1, n))

  if (nchar(texte) > n) {
    cat('\\n\\n[... texte tronqué dans l affichage ...]\\n')
    cat('Longueur totale : ', nchar(texte), ' caractères\\n', sep = '')
  }
}

afficher_texte_capture(
  titre = 'Aperçu du texte capturé pour LinearModel',
  nom_objet = 'texte_linearmodel',
  texte = texte_linearmodel
)

afficher_texte_capture(
  titre = 'Aperçu du texte capturé pour AovSum',
  nom_objet = 'texte_aovsum',
  texte = texte_aovsum
)


# ------------------------------------------------------------
# 7. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- lm_ftest : F-test de LinearModel\\n')
cat('- lm_ttest : T-test de LinearModel\\n')
cat('- lm_resume : résumé du modèle linéaire\\n')
cat('- aov_ftest : F-test de AovSum\\n')
cat('- aov_ttest : T-test de AovSum\\n')
cat('- texte_linearmodel : sortie complète LinearModel transformée en texte\\n')
cat('- texte_aovsum : sortie complète AovSum transformée en texte\\n')
",
code_display = "
# Inspecter les objets retournés par FactoMineR
names(res_lm_fm)
names(res_aovsum)

# Extraire les sorties importantes de LinearModel()
lm_ftest <- res_lm_fm$Ftest
lm_ttest <- res_lm_fm$Ttest
lm_resume <- res_lm_fm$lmResult

# Extraire les sorties importantes de AovSum()
aov_ftest <- res_aovsum$Ftest
aov_ttest <- res_aovsum$Ttest

# Examiner les objets extraits
lm_ftest
lm_ttest
lm_resume

aov_ftest
aov_ttest

# Capturer les sorties complètes sous forme de texte
texte_linearmodel <- paste(
  capture.output(print(res_lm_fm)),
  collapse = '\\n'
)

texte_aovsum <- paste(
  capture.output(print(res_aovsum)),
  collapse = '\\n'
)

# Vérifier les textes capturés
substr(texte_linearmodel, 1, 1200)
substr(texte_aovsum, 1, 1200)

nchar(texte_linearmodel)
nchar(texte_aovsum)
",
sortie_attendue = "Des objets `lm_ftest`, `lm_ttest`, `aov_ftest`, `aov_ttest`, `texte_linearmodel`, `texte_aovsum`, affichés avec des titres explicites.",
transition = "Ces éléments sont le matériau brut d'un prompt contrôlé : on sait maintenant extraire, nommer et transformer les sorties statistiques.",
question = "Quel objet contient la sortie ANOVA créée à la case précédente ?",
reponse = "res_aovsum"
),

prompt_manuel = make_case(
  partie = "r_sorties",
  titre = "6a. Faire un prompt (1)",
  objectif = "Construire un premier prompt à la main à partir des sorties statistiques.",
  has_plot = FALSE,
  pdf_on_run = "prompt_manuel.pdf",
  code = "
# ============================================================
# Case 6a : construire un prompt manuellement
# ============================================================

# Objectif de cette case :
# transformer des sorties statistiques déjà capturées
# en texte organisé pour un LLM.

# ------------------------------------------------------------
# 1. Comprendre paste() et cat()
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('1. Principe : paste() construit une chaîne de caractères\\n')
cat('============================================================\\n')

exemple_texte <- paste(
  'Première ligne',
  'Deuxième ligne',
  'Troisième ligne',
  sep = '\\n'
)

cat('Objet créé : exemple_texte\\n')
cat('Affichage avec cat(exemple_texte) :\\n\\n')
cat(exemple_texte)

cat('\\n\\n')
cat('Remarque : paste(..., sep = \"\\\\n\") assemble les lignes.\\n')
cat('cat() affiche le texte en respectant les retours à la ligne.\\n')


# ------------------------------------------------------------
# 2. Construire un prompt pour LinearModel()
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('2. Construction du prompt pour LinearModel()\\n')
cat('============================================================\\n')

prompt_linearmodel <- paste(
  '# Interprétation d une régression linéaire',
  '',
  'Contexte : questionnaire consommateur sur un produit alimentaire.',
  'Variable à expliquer : intention_achat.',
  'Variables explicatives : satisfaction et prix_percu.',
  '',
  'Sortie FactoMineR::LinearModel :',
  texte_linearmodel,
  '',
  'Consignes :',
  '- interpréter uniquement les résultats fournis ;',
  '- distinguer effet global et coefficients ;',
  '- ne pas confondre association et causalité ;',
  '- produire une interprétation pédagogique courte.',
  sep = '\\n'
)

cat('Objet créé : prompt_linearmodel\\n')
cat('Aperçu du prompt :\\n\\n')
cat(substr(prompt_linearmodel, 1, 1500))

if (nchar(prompt_linearmodel) > 1500) {
  cat('\\n\\n[... prompt tronqué dans l affichage ...]\\n')
  cat('Longueur totale : ', nchar(prompt_linearmodel), ' caractères\\n', sep = '')
}


# ------------------------------------------------------------
# 3. Construire un prompt pour AovSum()
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Construction du prompt pour AovSum()\\n')
cat('============================================================\\n')

prompt_aovsum <- paste(
  '# Interprétation d une ANOVA avec interaction',
  '',
  'Variable à expliquer : satisfaction.',
  'Facteurs : type_produit et budget_contraint.',
  '',
  'Sortie FactoMineR::AovSum :',
  texte_aovsum,
  '',
  'Consignes :',
  '- identifier les effets significatifs ;',
  '- commenter prudemment l interaction ;',
  '- ne pas inventer de comparaisons post-hoc.',
  sep = '\\n'
)

cat('Objet créé : prompt_aovsum\\n')
cat('Aperçu du prompt :\\n\\n')
cat(substr(prompt_aovsum, 1, 1500))

if (nchar(prompt_aovsum) > 1500) {
  cat('\\n\\n[... prompt tronqué dans l affichage ...]\\n')
  cat('Longueur totale : ', nchar(prompt_aovsum), ' caractères\\n', sep = '')
}


# ------------------------------------------------------------
# 4. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- prompt_linearmodel : prompt construit pour la régression linéaire\\n')
cat('- prompt_aovsum : prompt construit pour l ANOVA avec interaction\\n')
cat('\\n')
cat('Ces deux objets pourront être envoyés à un LLM, ou repris par EnTraineR.\\n')
",
code_display = "

# Comprendre le principe de paste()

exemple_texte <- paste(
'Première ligne',
'Deuxième ligne',
'Troisième ligne',
sep = '\n'
)

exemple_texte
cat(exemple_texte)

# Construire un prompt pour LinearModel()

prompt_linearmodel <- paste(
'# Interprétation d une régression linéaire',
'',
'Contexte : questionnaire consommateur sur un produit alimentaire.',
'Variable à expliquer : intention_achat.',
'Variables explicatives : satisfaction et prix_percu.',
'',
'Sortie FactoMineR::LinearModel :',
texte_linearmodel,
'',
'Consignes :',
'- interpréter uniquement les résultats fournis ;',
'- distinguer effet global et coefficients ;',
'- ne pas confondre association et causalité ;',
'- produire une interprétation pédagogique courte.',
sep = '\n'
)

# Examiner le début du prompt construit

substr(prompt_linearmodel, 1, 1500)
nchar(prompt_linearmodel)

# Construire un prompt pour AovSum()

prompt_aovsum <- paste(
'# Interprétation d une ANOVA avec interaction',
'',
'Variable à expliquer : satisfaction.',
'Facteurs : type_produit et budget_contraint.',
'',
'Sortie FactoMineR::AovSum :',
texte_aovsum,
'',
'Consignes :',
'- identifier les effets significatifs ;',
'- commenter prudemment l interaction ;',
'- ne pas inventer de comparaisons post-hoc.',
sep = '\n'
)

# Examiner le début du prompt construit

substr(prompt_aovsum, 1, 1500)
nchar(prompt_aovsum)
",
sortie_attendue = "Deux prompts : `prompt_linearmodel` et `prompt_aovsum`, construits avec `paste()` puis affichés avec `cat()`.",
transition = "Deux chemins sont possibles : aller vers EnTraineR, ou approfondir la construction d un prompt générique sans coder en dur les noms de variables.",
question = "Quelle fonction R permet ici de rassembler des lignes avec des retours à la ligne ?",
reponse = "paste"
),

prompt_manuel_n2 = make_case(
  partie = "r_sorties",
  titre = "6b. Faire un prompt (2)",
  objectif = "Construire un prompt générique : récupérer automatiquement le nom de Y et les noms des X à partir des formules.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 6b : prompt manuel niveau 2, sans codage en dur
# ============================================================

# Dans la case précédente, les noms des variables étaient écrits à la main.
# Ici, on les récupère automatiquement à partir des formules utilisées
# dans les analyses.

# ------------------------------------------------------------
# 1. Observer les formules utilisées
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('1. Formules utilisées dans les analyses\\n')
cat('============================================================\\n')

cat('Objet formule_lm :\\n')
print(formule_lm)

cat('\\nObjet formule_aov :\\n')
print(formule_aov)


# ------------------------------------------------------------
# 2. Comprendre les fonctions utiles
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('2. Extraire les informations d une formule\\n')
cat('============================================================\\n')

cat('\\nall.vars(formule_lm) renvoie toutes les variables observées :\\n')
print(all.vars(formule_lm))

cat('\\nLa première variable est la variable à expliquer Y :\\n')
print(all.vars(formule_lm)[1])

cat('\\nattr(terms(formule_lm), \"term.labels\") renvoie les termes explicatifs :\\n')
print(attr(terms(formule_lm), 'term.labels'))

cat('\\nPour l ANOVA, la distinction est importante :\\n')
cat('all.vars(formule_aov) donne les variables observées.\\n')
print(all.vars(formule_aov))

cat('\\nattr(terms(formule_aov), \"term.labels\") donne les termes du modèle, y compris l interaction.\\n')
print(attr(terms(formule_aov), 'term.labels'))


# ------------------------------------------------------------
# 3. Créer une fonction pour automatiser cette extraction
# ------------------------------------------------------------

extraire_infos_formule <- function(formule) {

  variables <- all.vars(formule)
  termes <- attr(terms(formule), 'term.labels')

  list(
    formule = deparse(formule),
    y = variables[1],
    termes = termes,
    variables = variables
  )
}

infos_lm <- extraire_infos_formule(formule_lm)
infos_aov <- extraire_infos_formule(formule_aov)


# ------------------------------------------------------------
# 4. Afficher les informations extraites
# ------------------------------------------------------------

afficher_infos_formule <- function(titre, infos) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('============================================================\\n')
  cat('Formule : ', infos$formule, '\\n', sep = '')
  cat('Y       : ', infos$y, '\\n', sep = '')
  cat('Termes  : ', paste(infos$termes, collapse = ', '), '\\n', sep = '')
  cat('Variables observées : ', paste(infos$variables, collapse = ', '), '\\n', sep = '')
}

afficher_infos_formule(
  titre = 'Informations extraites de formule_lm',
  infos = infos_lm
)

afficher_infos_formule(
  titre = 'Informations extraites de formule_aov',
  infos = infos_aov
)


# ------------------------------------------------------------
# 5. Construire un prompt générique pour LinearModel()
# ------------------------------------------------------------

prompt_linearmodel_n2 <- paste(
  '# Interprétation d une régression linéaire',
  '',
  'Contexte : questionnaire consommateur sur un produit alimentaire.',
  paste0('Variable à expliquer : ', infos_lm$y, '.'),
  paste0('Variables explicatives : ', paste(infos_lm$termes, collapse = ', '), '.'),
  '',
  'Sortie FactoMineR::LinearModel :',
  texte_linearmodel,
  '',
  'Consignes :',
  '- interpréter uniquement les résultats fournis ;',
  '- distinguer effet global et coefficients ;',
  '- ne pas confondre association et causalité ;',
  '- produire une interprétation pédagogique courte.',
  sep = '\\n'
)


# ------------------------------------------------------------
# 6. Construire un prompt générique pour AovSum()
# ------------------------------------------------------------

prompt_aovsum_n2 <- paste(
  '# Interprétation d une ANOVA',
  '',
  paste0('Variable à expliquer : ', infos_aov$y, '.'),
  paste0('Termes du modèle : ', paste(infos_aov$termes, collapse = ', '), '.'),
  '',
  'Sortie FactoMineR::AovSum :',
  texte_aovsum,
  '',
  'Consignes :',
  '- identifier les effets significatifs ;',
  '- commenter prudemment les interactions éventuelles ;',
  '- ne pas inventer de comparaisons post-hoc.',
  sep = '\\n'
)


# ------------------------------------------------------------
# 7. Afficher un aperçu des prompts créés
# ------------------------------------------------------------

afficher_apercu_prompt <- function(titre, nom_objet, prompt, n = 1200) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('Objet créé : ', nom_objet, '\\n', sep = '')
  cat('============================================================\\n')
  cat(substr(prompt, 1, n))

  if (nchar(prompt) > n) {
    cat('\\n\\n[... prompt tronqué dans l affichage ...]\\n')
    cat('Longueur totale : ', nchar(prompt), ' caractères\\n', sep = '')
  }
}

afficher_apercu_prompt(
  titre = 'Aperçu du prompt générique LinearModel',
  nom_objet = 'prompt_linearmodel_n2',
  prompt = prompt_linearmodel_n2
)

afficher_apercu_prompt(
  titre = 'Aperçu du prompt générique AovSum',
  nom_objet = 'prompt_aovsum_n2',
  prompt = prompt_aovsum_n2
)


# ------------------------------------------------------------
# 8. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- infos_lm : informations extraites de formule_lm\\n')
cat('- infos_aov : informations extraites de formule_aov\\n')
cat('- prompt_linearmodel_n2 : prompt générique pour LinearModel\\n')
cat('- prompt_aovsum_n2 : prompt générique pour AovSum\\n')
cat('\\n')
cat('Idée clé : le prompt n est plus écrit entièrement à la main.\\n')
cat('Il est construit à partir des objets R qui décrivent le modèle.\\n')
",
code_display = "

# Observer les formules utilisées dans les analyses

formule_lm
formule_aov

# Extraire les variables observées dans une formule

all.vars(formule_lm)
all.vars(formule_aov)

# Extraire les termes explicatifs du modèle

attr(terms(formule_lm), 'term.labels')
attr(terms(formule_aov), 'term.labels')

# Créer une fonction pour automatiser l extraction

extraire_infos_formule <- function(formule) {

variables <- all.vars(formule)
termes <- attr(terms(formule), 'term.labels')

list(
formule = deparse(formule),
y = variables[1],
termes = termes,
variables = variables
)
}

# Appliquer la fonction aux deux formules

infos_lm <- extraire_infos_formule(formule_lm)
infos_aov <- extraire_infos_formule(formule_aov)

infos_lm
infos_aov

# Construire un prompt générique pour LinearModel()

prompt_linearmodel_n2 <- paste(
'# Interprétation d une régression linéaire',
'',
'Contexte : questionnaire consommateur sur un produit alimentaire.',
paste0('Variable à expliquer : ', infos_lm$y, '.'),
paste0('Variables explicatives : ', paste(infos_lm$termes, collapse = ', '), '.'),
'',
'Sortie FactoMineR::LinearModel :',
texte_linearmodel,
'',
'Consignes :',
'- interpréter uniquement les résultats fournis ;',
'- distinguer effet global et coefficients ;',
'- ne pas confondre association et causalité ;',
'- produire une interprétation pédagogique courte.',
sep = '\n'
)

# Construire un prompt générique pour AovSum()

prompt_aovsum_n2 <- paste(
'# Interprétation d une ANOVA',
'',
paste0('Variable à expliquer : ', infos_aov$y, '.'),
paste0('Termes du modèle : ', paste(infos_aov$termes, collapse = ', '), '.'),
'',
'Sortie FactoMineR::AovSum :',
texte_aovsum,
'',
'Consignes :',
'- identifier les effets significatifs ;',
'- commenter prudemment les interactions éventuelles ;',
'- ne pas inventer de comparaisons post-hoc.',
sep = '\n'
)

# Examiner les prompts créés

substr(prompt_linearmodel_n2, 1, 1200)
nchar(prompt_linearmodel_n2)

substr(prompt_aovsum_n2, 1, 1200)
nchar(prompt_aovsum_n2)
",
sortie_attendue = "Deux prompts génériques : `prompt_linearmodel_n2` et `prompt_aovsum_n2`, construits sans coder en dur les noms de Y et des termes explicatifs.",
transition = "Cette étape montre la mécanique R que EnTraineR va ensuite généraliser : récupérer les éléments du modèle et produire un prompt.",
question = "Quel objet contient la formule utilisée pour LinearModel ?",
reponse = "formule_lm"
),

  entrainer_presentation = make_case(
    partie = "entrainer",
    titre = "7b. Présentation EnTraineR",
    objectif = "Situer le package EnTraineR avant d'utiliser ses fonctions.",
    has_plot = FALSE,
    code = "
# ============================================================
# Case 7b : petite présentation du package EnTraineR
# ============================================================

entrainer_disponible <- requireNamespace('EnTraineR', quietly = TRUE)

if (entrainer_disponible) {
  version_entrainer <- as.character(utils::packageVersion('EnTraineR'))
  fonctions_entrainer <- grep(
    '^trainer_',
    getNamespaceExports('EnTraineR'),
    value = TRUE
  )
} else {
  version_entrainer <- 'non installé'
  fonctions_entrainer <- c(
    'trainer_linear_model',
    'trainer_LinearModel',
    'trainer_aovsum',
    'trainer_AovSum',
    'trainer_cor',
    'trainer_chisq_test',
    'trainer_var',
    'trainer_MCA'
  )
}

presentation_entrainer <- data.frame(
  question = c(
    'Quel est le rôle du package ?',
    'Que reçoit-il en entrée ?',
    'Que produit-il en sortie ?',
    'Pourquoi generate = FALSE est important ?',
    'Comment se situe-t-il par rapport à NaileR ?'
  ),
  reponse = c(
    'Transformer des résultats statistiques explicites en prompts pédagogiques contrôlés.',
    'Des objets ou sorties issus de fonctions statistiques comme LinearModel() ou AovSum().',
    'Un prompt inspectable, et éventuellement une réponse LLM si la génération est activée.',
    'Cette option permet de vérifier le prompt avant tout appel au modèle.',
    'EnTraineR traite les analyses explicites ; NaileR prolonge la logique vers condes(), catdes(), les axes, les classes et le texte.'
  ),
  stringsAsFactors = FALSE
)

schema_entrainer <- data.frame(
  etape = c(
    '1. Analyse statistique',
    '2. Extraction des résultats',
    '3. Construction du prompt',
    '4. Contrôle humain',
    '5. Appel optionnel au LLM'
  ),
  exemple = c(
    'LinearModel(), AovSum(), cor(), chisq.test()',
    'F-test, T-test, coefficients, p-values, R2',
    'Contexte + résultats + consignes + format attendu',
    'generate = FALSE',
    'llm_engine + llm_model'
  ),
  stringsAsFactors = FALSE
)

resume_package_entrainer <- data.frame(
  element = c(
    'Package installé',
    'Version détectée',
    'Nombre de fonctions trainer_* repérées ou attendues'
  ),
  valeur = c(
    as.character(entrainer_disponible),
    version_entrainer,
    as.character(length(fonctions_entrainer))
  ),
  stringsAsFactors = FALSE
)

cat('## Présentation rapide de EnTraineR\\n\\n')
print(resume_package_entrainer, row.names = FALSE)

cat('\\nFonctions repérées ou attendues :\\n')
print(data.frame(fonction = fonctions_entrainer), row.names = FALSE)

cat('\\nIdée générale :\\n')
print(presentation_entrainer, row.names = FALSE)

cat('\\nSchéma du workflow EnTraineR :\\n')
print(schema_entrainer, row.names = FALSE)

cat('\\nMessage pédagogique :\\n')
cat('EnTraineR ne remplace pas l analyse statistique.\\n')
cat('Il organise le passage entre une sortie statistique explicite et un prompt contrôlé.\\n')
cat('La logique centrale reste : analyser -> extraire -> structurer -> prompter -> éventuellement générer.\\n')
",
code_display = "

# Vérifier si EnTraineR est disponible

entrainer_disponible <- requireNamespace('EnTraineR', quietly = TRUE)

if (entrainer_disponible) {

version_entrainer <- as.character(
utils::packageVersion('EnTraineR')
)

fonctions_entrainer <- grep(
'^trainer_',
getNamespaceExports('EnTraineR'),
value = TRUE
)

} else {

version_entrainer <- 'non installé'

fonctions_entrainer <- c(
'trainer_linear_model',
'trainer_LinearModel',
'trainer_aovsum',
'trainer_AovSum',
'trainer_cor',
'trainer_chisq_test',
'trainer_var',
'trainer_MCA'
)
}

# Résumer l état du package

resume_package_entrainer <- data.frame(
element = c(
'Package installé',
'Version détectée',
'Nombre de fonctions trainer_* repérées ou attendues'
),
valeur = c(
as.character(entrainer_disponible),
version_entrainer,
as.character(length(fonctions_entrainer))
),
stringsAsFactors = FALSE
)

# Présenter le rôle de EnTraineR dans le workflow

presentation_entrainer <- data.frame(
question = c(
'Quel est le rôle du package ?',
'Que reçoit-il en entrée ?',
'Que produit-il en sortie ?',
'Pourquoi generate = FALSE est important ?',
'Comment se situe-t-il par rapport à NaileR ?'
),
reponse = c(
'Transformer des résultats statistiques explicites en prompts pédagogiques contrôlés.',
'Des objets ou sorties issus de fonctions statistiques comme LinearModel() ou AovSum().',
'Un prompt inspectable, et éventuellement une réponse LLM si la génération est activée.',
'Cette option permet de vérifier le prompt avant tout appel au modèle.',
'EnTraineR traite les analyses explicites ; NaileR prolonge la logique vers condes(), catdes(), les axes, les classes et le texte.'
),
stringsAsFactors = FALSE
)

# Schématiser le workflow EnTraineR

schema_entrainer <- data.frame(
etape = c(
'1. Analyse statistique',
'2. Extraction des résultats',
'3. Construction du prompt',
'4. Contrôle humain',
'5. Appel optionnel au LLM'
),
exemple = c(
'LinearModel(), AovSum(), cor(), chisq.test()',
'F-test, T-test, coefficients, p-values, R2',
'Contexte + résultats + consignes + format attendu',
'generate = FALSE',
'llm_engine + llm_model'
),
stringsAsFactors = FALSE
)

# Examiner les objets créés

resume_package_entrainer
data.frame(fonction = fonctions_entrainer)
presentation_entrainer
schema_entrainer
",
    sortie_attendue = "Une présentation courte de EnTraineR, de son rôle et de sa place dans le workflow.",
    transition = "On peut maintenant entrer dans la mécanique pratique de EnTraineR.",
    pdf = "entrainer_presentation.pdf",
    question = "Quel package est présenté dans cette case ?",
    reponse = "entrainer"
  ),

  entrainer_intro = make_case(
    partie = "entrainer",
    titre = "7a. EnTraineR",
    objectif = "Repérer les fonctions EnTraineR et préparer leur utilisation sur les prompts construits.",
    has_plot = FALSE,
    code = "
# ============================================================
# Case 7a : entrer dans EnTraineR
# ============================================================

# On cherche le package sous son nom attendu : EnTraineR.
entrainer_pkg <- if (requireNamespace('EnTraineR', quietly = TRUE)) {
  'EnTraineR'
} else {
  NA_character_
}

entrainer_disponible <- !is.na(entrainer_pkg)

if (entrainer_disponible) {
  fonctions_entrainer <- grep(
    '^trainer_',
    getNamespaceExports(entrainer_pkg),
    value = TRUE
  )
} else {
  message('Le package EnTraineR n est pas installé sur cette machine.')
  message('On conserve ici la logique pédagogique : analyse -> sortie -> prompt.')

  fonctions_entrainer <- c(
    'trainer_linear_model',
    'trainer_LinearModel',
    'trainer_aovsum',
    'trainer_AovSum',
    'trainer_cor',
    'trainer_chisq_test',
    'trainer_var',
    'trainer_MCA'
  )
}

# Si la branche niveau 2 a été exécutée, on utilise les prompts génériques.
# Sinon, on conserve les prompts manuels de niveau 1.
prompt_linearmodel_utilise <- if (exists('prompt_linearmodel_n2')) {
  prompt_linearmodel_n2
} else {
  prompt_linearmodel
}

prompt_aovsum_utilise <- if (exists('prompt_aovsum_n2')) {
  prompt_aovsum_n2
} else {
  prompt_aovsum
}

source_prompts <- if (exists('prompt_linearmodel_n2')) {
  'prompts génériques de niveau 2'
} else {
  'prompts manuels de niveau 1'
}

# Les objets complets sont conservés en mémoire, mais ne sont pas imprimés
# brutalement dans la sortie console.
objet_transition_entrainer <- list(
  analyse = 'LinearModel et AovSum',
  source_prompts = source_prompts,
  sorties_completes = list(
    linearmodel = texte_linearmodel,
    aovsum = texte_aovsum
  ),
  prompts_complets = list(
    linearmodel = prompt_linearmodel_utilise,
    aovsum = prompt_aovsum_utilise
  )
)

resume_entrainer <- data.frame(
  element = c(
    'Package EnTraineR installé ?',
    'Nom technique du package utilisé',
    'Source des prompts utilisés',
    'Fonctions trainer_* repérées',
    'Sortie LinearModel capturée',
    'Sortie AovSum capturée',
    'Prompt LinearModel utilisé',
    'Prompt AovSum utilisé'
  ),
  valeur = c(
    as.character(entrainer_disponible),
    ifelse(is.na(entrainer_pkg), 'non installé', entrainer_pkg),
    source_prompts,
    as.character(length(fonctions_entrainer)),
    paste0(nchar(texte_linearmodel), ' caractères'),
    paste0(nchar(texte_aovsum), ' caractères'),
    paste0(nchar(prompt_linearmodel_utilise), ' caractères'),
    paste0(nchar(prompt_aovsum_utilise), ' caractères')
  )
)

fonctions_entrainer_df <- data.frame(
  fonction = fonctions_entrainer
)

cat('## Entrée dans EnTraineR\n\n')

cat('Fonctions EnTraineR repérées ou attendues :\n')
print(fonctions_entrainer_df)

cat('\nRésumé des objets disponibles :\n')
print(resume_entrainer, row.names = FALSE)

cat('\nAperçu du prompt LinearModel utilisé :\n')
cat(substr(prompt_linearmodel_utilise, 1, 1000))
cat('\n\n[... prompt tronqué dans l affichage ...]\n\n')

cat('Aperçu du prompt AovSum utilisé :\n')
cat(substr(prompt_aovsum_utilise, 1, 1000))
cat('\n\n[... prompt tronqué dans l affichage ...]\n\n')

cat('Objets complets conservés en mémoire :\n')
cat('- objet_transition_entrainer\n')
cat('- texte_linearmodel\n')
cat('- texte_aovsum\n')
cat('- prompt_linearmodel_utilise\n')
cat('- prompt_aovsum_utilise\n')
",
code_display = "
# Vérifier si le package EnTraineR est disponible
entrainer_pkg <- if (requireNamespace('EnTraineR', quietly = TRUE)) {
  'EnTraineR'
} else {
  NA_character_
}

entrainer_disponible <- !is.na(entrainer_pkg)

# Repérer les fonctions trainer_* disponibles
if (entrainer_disponible) {

  fonctions_entrainer <- grep(
    '^trainer_',
    getNamespaceExports(entrainer_pkg),
    value = TRUE
  )

} else {

  fonctions_entrainer <- c(
    'trainer_linear_model',
    'trainer_LinearModel',
    'trainer_aovsum',
    'trainer_AovSum',
    'trainer_cor',
    'trainer_chisq_test',
    'trainer_var',
    'trainer_MCA'
  )
}

# Choisir les prompts disponibles
# Si les prompts génériques de niveau 2 existent, on les utilise.
# Sinon, on garde les prompts manuels de niveau 1.
prompt_linearmodel_utilise <- if (exists('prompt_linearmodel_n2')) {
  prompt_linearmodel_n2
} else {
  prompt_linearmodel
}

prompt_aovsum_utilise <- if (exists('prompt_aovsum_n2')) {
  prompt_aovsum_n2
} else {
  prompt_aovsum
}

source_prompts <- if (exists('prompt_linearmodel_n2')) {
  'prompts génériques de niveau 2'
} else {
  'prompts manuels de niveau 1'
}

# Construire un objet de transition vers EnTraineR
objet_transition_entrainer <- list(
  analyse = 'LinearModel et AovSum',
  source_prompts = source_prompts,
  sorties_completes = list(
    linearmodel = texte_linearmodel,
    aovsum = texte_aovsum
  ),
  prompts_complets = list(
    linearmodel = prompt_linearmodel_utilise,
    aovsum = prompt_aovsum_utilise
  )
)

# Résumer les objets disponibles
resume_entrainer <- data.frame(
  element = c(
    'Package EnTraineR installé ?',
    'Nom technique du package utilisé',
    'Source des prompts utilisés',
    'Fonctions trainer_* repérées',
    'Sortie LinearModel capturée',
    'Sortie AovSum capturée',
    'Prompt LinearModel utilisé',
    'Prompt AovSum utilisé'
  ),
  valeur = c(
    as.character(entrainer_disponible),
    ifelse(is.na(entrainer_pkg), 'non installé', entrainer_pkg),
    source_prompts,
    as.character(length(fonctions_entrainer)),
    paste0(nchar(texte_linearmodel), ' caractères'),
    paste0(nchar(texte_aovsum), ' caractères'),
    paste0(nchar(prompt_linearmodel_utilise), ' caractères'),
    paste0(nchar(prompt_aovsum_utilise), ' caractères')
  )
)

fonctions_entrainer_df <- data.frame(
  fonction = fonctions_entrainer
)

# Examiner les objets créés
fonctions_entrainer_df
resume_entrainer

# Examiner les prompts utilisés
substr(prompt_linearmodel_utilise, 1, 1000)
substr(prompt_aovsum_utilise, 1, 1000)

# Objet complet conservé pour la suite
objet_transition_entrainer
",
    sortie_attendue = "Une liste des fonctions EnTraineR disponibles si le package est installé, sinon une structure pédagogique de transition.",
    transition = "On examine maintenant les options communes : générer ou non, moteur LLM, modèle, style.",
    question = "Dans notre logique, EnTraineR automatise le passage de la sortie statistique vers quoi ?",
    reponse = "prompt"
  ),

#   entrainer_options = make_case(
#     partie = "entrainer",
#     titre = "7''. Options",
#     objectif = "Repérer les options communes des fonctions EnTraineR.",
#     has_plot = FALSE,
#     code = "
# # ============================================================
# # Case 7'' : options communes EnTraineR
# # ============================================================
#
# options_cibles <- c(
#   'generate',
#   'llm_engine',
#   'llm_model',
#   'prompt_style',
#   'audience',
#   'language'
# )
#
# if (exists('entrainer_pkg') && !is.na(entrainer_pkg)) {
#
#   fonctions_existantes <- fonctions_entrainer[
#     vapply(
#       fonctions_entrainer,
#       function(f) exists(f, envir = asNamespace(entrainer_pkg), inherits = FALSE),
#       logical(1)
#     )
#   ]
#
#   options_par_fonction <- lapply(fonctions_existantes, function(f) {
#     fun <- getExportedValue(entrainer_pkg, f)
#     noms_args <- names(formals(fun))
#
#     data.frame(
#       fonction = f,
#       option = options_cibles,
#       presente = options_cibles %in% noms_args,
#       stringsAsFactors = FALSE
#     )
#   })
#
#   options_entrainer <- do.call(rbind, options_par_fonction)
#
#   arguments_entrainer <- data.frame(
#     fonction = fonctions_existantes,
#     arguments = vapply(
#       fonctions_existantes,
#       function(f) {
#         fun <- getExportedValue(entrainer_pkg, f)
#         paste(names(formals(fun)), collapse = ', ')
#       },
#       character(1)
#     ),
#     stringsAsFactors = FALSE
#   )
#
# } else {
#
#   options_entrainer <- data.frame(
#     option = c(
#       'generate',
#       'llm_engine',
#       'llm_model',
#       'prompt_style',
#       'audience',
#       'language'
#     ),
#     role = c(
#       'retourner seulement le prompt ou appeler le modèle',
#       'choisir le moteur LLM : none, ollama, gemini...',
#       'choisir le modèle',
#       'contrôler le niveau de détail du prompt',
#       'adapter le niveau pédagogique',
#       'contrôler la langue de sortie'
#     ),
#     stringsAsFactors = FALSE
#   )
#
#   arguments_entrainer <- data.frame(
#     fonction = fonctions_entrainer,
#     arguments = 'Package EnTraineR non installé : arguments non inspectés',
#     stringsAsFactors = FALSE
#   )
# }
#
# cat('## Options communes EnTraineR\n\n')
# cat('Idée pédagogique : les fonctions EnTraineR partagent une logique commune.\n')
# cat('On veut produire seulement le prompt, ou appeler ensuite un moteur LLM.\n\n')
#
# cat('Options importantes :\n')
# print(options_entrainer)
#
# cat('\nArguments repérés par fonction :\n')
# print(arguments_entrainer, row.names = FALSE)
#
# cat('\nPoint clé : generate = FALSE permet de contrôler le prompt avant tout appel au modèle.\n')
# ",
#     sortie_attendue = "Un objet `options_entrainer` qui synthétise les options importantes.",
#     transition = "On revient à R : comment passer d'une analyse ponctuelle à une série d'analyses ?",
#     question = "Quel package vient d\'être introduit comme automatisation de la construction de prompts ?",
#     reponse = "entrainer"
#   ),

boucle_y_x = make_case(
  partie = "r_sorties",
  titre = "8. Boucler",
  objectif = "Passer de Y~X à Y~tous les X pour comprendre la logique d'automatisation.",
  has_plot = FALSE,
  pdf_on_run = "boucle_y_x.pdf",
  code = "
# ============================================================
# Case 8 : boucler sur plusieurs variables explicatives
# ============================================================

# Objectif de cette case :
# comprendre comment passer d une analyse unique :
# intention_achat ~ prix_percu
# à une série d analyses :
# intention_achat ~ prix_percu
# intention_achat ~ plaisir
# intention_achat ~ naturalite
# etc.

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}


# ------------------------------------------------------------
# 1. Définir la variable Y et les variables X
# ------------------------------------------------------------

y <- 'intention_achat'

variables_x <- c(
  'prix_percu',
  'plaisir',
  'naturalite',
  'confiance',
  'ancrage_local',
  'usage_numerique',
  'sensibilite_env'
)

cat('\\n============================================================\\n')
cat('1. Variables utilisées pour la boucle\\n')
cat('============================================================\\n')
cat('Variable à expliquer Y : ', y, '\\n', sep = '')
cat('Variables explicatives X :\\n')
print(variables_x)


# ------------------------------------------------------------
# 2. Comprendre la construction d une formule
# ------------------------------------------------------------

# Pour automatiser une analyse, on construit d abord la formule
# sous forme de texte, puis on la transforme en formule R.

premier_x <- variables_x[1]

formule_texte <- paste(y, '~', premier_x)
formule_exemple <- as.formula(formule_texte)

cat('\\n============================================================\\n')
cat('2. Construire une formule automatiquement\\n')
cat('============================================================\\n')
cat('Premier X utilisé comme exemple : ', premier_x, '\\n', sep = '')
cat('Formule sous forme de texte : ', formule_texte, '\\n', sep = '')
cat('Formule transformée en objet R avec as.formula() :\\n')
print(formule_exemple)


# ------------------------------------------------------------
# 3. Ajuster un premier modèle sans boucle
# ------------------------------------------------------------

modele_exemple <- FactoMineR::LinearModel(
  formule_exemple,
  data = questionnaire,
  selection = 'none'
)

cat('\\n============================================================\\n')
cat('3. Premier modèle ajusté sans boucle\\n')
cat('============================================================\\n')
cat('Objet créé : modele_exemple\\n')
cat('Modèle ajusté : ', formule_texte, '\\n', sep = '')

cat('\\nFtest du modèle exemple :\\n')
print(modele_exemple$Ftest)

cat('\\nTtest du modèle exemple :\\n')
print(modele_exemple$Ttest)


# ------------------------------------------------------------
# 4. Écrire une fonction pour répéter l analyse
# ------------------------------------------------------------

# Quand une suite d instructions doit être répétée,
# on peut l enfermer dans une fonction.

ajuster_un_modele <- function(x) {

  formule_texte <- paste(y, '~', x)
  formule <- as.formula(formule_texte)

  modele <- FactoMineR::LinearModel(
    formule,
    data = questionnaire,
    selection = 'none'
  )

  modele
}

cat('\\n============================================================\\n')
cat('4. Fonction créée pour automatiser une analyse\\n')
cat('============================================================\\n')
cat('Objet créé : ajuster_un_modele()\\n')
cat('Rôle : recevoir le nom d une variable X et retourner un modèle LinearModel.\\n')


# ------------------------------------------------------------
# 5. Appliquer la fonction à toutes les variables X
# ------------------------------------------------------------

# lapply() applique une même fonction à chaque élément d un vecteur.
# Ici, chaque élément de variables_x est un nom de variable explicative.

modeles_univaries <- lapply(
  variables_x,
  ajuster_un_modele
)

names(modeles_univaries) <- variables_x

cat('\\n============================================================\\n')
cat('5. Boucle avec lapply()\\n')
cat('============================================================\\n')
cat('Objet créé : modeles_univaries\\n')
cat('Nombre de modèles ajustés : ', length(modeles_univaries), '\\n', sep = '')
cat('Noms des modèles dans la liste :\\n')
print(names(modeles_univaries))


# ------------------------------------------------------------
# 6. Afficher un résumé lisible des modèles obtenus
# ------------------------------------------------------------

afficher_modele_univarie <- function(nom_x, modele) {
  cat('\\n============================================================\\n')
  cat('Modèle univarié : ', y, ' ~ ', nom_x, '\\n', sep = '')
  cat('Objet dans la liste : modeles_univaries[[\"', nom_x, '\"]]\\n', sep = '')
  cat('============================================================\\n')

  cat('\\nFtest :\\n')
  print(modele$Ftest)

  cat('\\nTtest :\\n')
  print(modele$Ttest)
}

# On affiche seulement les deux premiers modèles pour ne pas saturer la console.
afficher_modele_univarie(
  nom_x = names(modeles_univaries)[1],
  modele = modeles_univaries[[1]]
)

afficher_modele_univarie(
  nom_x = names(modeles_univaries)[2],
  modele = modeles_univaries[[2]]
)

cat('\\n[Seuls les deux premiers modèles sont affichés. Les autres sont bien stockés dans modeles_univaries.]\\n')


# ------------------------------------------------------------
# 7. Capturer les sorties sous forme de texte
# ------------------------------------------------------------

textes_modeles_univaries <- lapply(
  modeles_univaries,
  function(modele) {
    paste(
      capture.output(print(modele)),
      collapse = '\\n'
    )
  }
)

cat('\\n============================================================\\n')
cat('7. Sorties capturées sous forme de texte\\n')
cat('============================================================\\n')
cat('Objet créé : textes_modeles_univaries\\n')
cat('Chaque élément contient la sortie complète d un modèle sous forme de texte.\\n')
cat('Noms des textes capturés :\\n')
print(names(textes_modeles_univaries))


# ------------------------------------------------------------
# 8. Afficher un aperçu d une sortie capturée
# ------------------------------------------------------------

premier_texte <- textes_modeles_univaries[[1]]

cat('\\n============================================================\\n')
cat('8. Aperçu du premier texte capturé\\n')
cat('============================================================\\n')
cat('Objet affiché : textes_modeles_univaries[[1]]\\n')
cat('Variable X correspondante : ', names(textes_modeles_univaries)[1], '\\n\\n', sep = '')

cat(substr(premier_texte, 1, 1200))

if (nchar(premier_texte) > 1200) {
  cat('\\n\\n[... texte tronqué dans l affichage ...]\\n')
  cat('Longueur totale : ', nchar(premier_texte), ' caractères\\n', sep = '')
}


# ------------------------------------------------------------
# 9. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- y : nom de la variable à expliquer\\n')
cat('- variables_x : noms des variables explicatives testées\\n')
cat('- formule_exemple : première formule construite automatiquement\\n')
cat('- modele_exemple : premier modèle ajusté sans boucle\\n')
cat('- ajuster_un_modele() : fonction qui ajuste un modèle pour une variable X\\n')
cat('- modeles_univaries : liste des modèles LinearModel\\n')
cat('- textes_modeles_univaries : liste des sorties transformées en texte\\n')
cat('\\n')
cat('Idée clé : on vient d automatiser une série de modèles simples.\\n')
cat('La case suivante montrera que condes() généralise cette logique.\\n')
",
sortie_attendue = "Une liste de modèles `modeles_univaries` et une liste de sorties textuelles `textes_modeles_univaries`, une par variable explicative.",
transition = "On vient de faire à la main ce que condes() généralise pour décrire une variable quantitative.",
question = "Quelle fonction R applique une même fonction à tous les éléments d une liste ou d un vecteur ?",
reponse = "lapply"
),

condes = make_case(
  partie = "stat",
  titre = "9. condes",
  objectif = "Utiliser condes() pour décrire une variable quantitative par toutes les autres.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 9 : condes()
# ============================================================

# Objectif de cette case :
# comprendre que condes() décrit une variable quantitative
# à partir de toutes les autres variables du tableau.
#
# Pour une variable quantitative à décrire :
# - les variables quantitatives sont liées par corrélation ;
# - les variables qualitatives sont liées par comparaison de moyennes.
#
# On retrouve donc la logique des analyses simples vues avant,
# mais appliquée systématiquement.

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}


# ------------------------------------------------------------
# 1. Préparer le tableau de description
# ------------------------------------------------------------

# On retire la variable textuelle libre.
# condes() et catdes() travaillent ici sur des variables quantitatives
# et qualitatives structurées, pas directement sur les verbatims.

questionnaire_desc <- questionnaire[
  ,
  setdiff(names(questionnaire), 'commentaire')
]

y_condes <- 'intention_achat'

num_var_condes <- which(names(questionnaire_desc) == y_condes)

cat('\\n============================================================\\n')
cat('1. Variable quantitative à décrire\\n')
cat('============================================================\\n')
cat('Objet créé : questionnaire_desc\\n')
cat('Variable décrite : ', y_condes, '\\n', sep = '')
cat('Position de cette variable dans questionnaire_desc : ', num_var_condes, '\\n', sep = '')


# ------------------------------------------------------------
# 2. Retrouver manuellement la logique côté variables quantitatives
# ------------------------------------------------------------

variables_quanti_condes <- setdiff(
  names(questionnaire_desc)[vapply(questionnaire_desc, is.numeric, logical(1))],
  c('id', y_condes)
)

correlations_y <- data.frame(
  variable = variables_quanti_condes,
  correlation = vapply(
    variables_quanti_condes,
    function(v) {
      cor(
        questionnaire_desc[[y_condes]],
        questionnaire_desc[[v]],
        use = 'pairwise.complete.obs'
      )
    },
    numeric(1)
  ),
  row.names = NULL
)

correlations_y$abs_correlation <- abs(correlations_y$correlation)

correlations_y <- correlations_y[
  order(correlations_y$abs_correlation, decreasing = TRUE),
]

correlations_y$correlation <- round(correlations_y$correlation, 3)
correlations_y$abs_correlation <- round(correlations_y$abs_correlation, 3)

cat('\\n============================================================\\n')
cat('2. Logique statistique : liens avec les variables quantitatives\\n')
cat('============================================================\\n')
cat('Objet créé : correlations_y\\n')
cat('Lecture : plus la corrélation est forte en valeur absolue,\\n')
cat('plus la variable est associée à ', y_condes, '.\\n', sep = '')
print(correlations_y, row.names = FALSE)


# ------------------------------------------------------------
# 3. Retrouver manuellement la logique côté variables qualitatives
# ------------------------------------------------------------

variables_quali_condes <- names(questionnaire_desc)[
  vapply(questionnaire_desc, is.factor, logical(1))
]

calcul_eta2 <- function(variable_quali) {
  formule <- as.formula(paste(y_condes, '~', variable_quali))
  tab <- summary(aov(formule, data = questionnaire_desc))[[1]]

  ss <- tab[['Sum Sq']]
  eta2 <- ss[1] / sum(ss)

  data.frame(
    variable = variable_quali,
    eta2 = eta2,
    p_value = tab[['Pr(>F)']][1],
    row.names = NULL
  )
}

liaisons_quali_y <- do.call(
  rbind,
  lapply(variables_quali_condes, calcul_eta2)
)

liaisons_quali_y <- liaisons_quali_y[
  order(liaisons_quali_y$eta2, decreasing = TRUE),
]

liaisons_quali_y$eta2 <- round(liaisons_quali_y$eta2, 3)
liaisons_quali_y$p_value <- signif(liaisons_quali_y$p_value, 3)

cat('\\n============================================================\\n')
cat('3. Logique statistique : liens avec les variables qualitatives\\n')
cat('============================================================\\n')
cat('Objet créé : liaisons_quali_y\\n')
cat('Lecture : eta2 mesure la part de variabilité de ', y_condes, '\\n', sep = '')
cat('associée à chaque variable qualitative.\\n')
print(liaisons_quali_y, row.names = FALSE)


# ------------------------------------------------------------
# 4. Utiliser condes()
# ------------------------------------------------------------

res_condes <- FactoMineR::condes(
  questionnaire_desc,
  num.var = num_var_condes
)

cat('\\n============================================================\\n')
cat('4. Résultat FactoMineR::condes()\\n')
cat('============================================================\\n')
cat('Objet créé : res_condes\\n')
cat('condes() automatise la description de la variable quantitative.\\n')
cat('Variable décrite : ', y_condes, '\\n', sep = '')

cat('\\nÉléments contenus dans res_condes :\\n')
print(names(res_condes))


# ------------------------------------------------------------
# 5. Afficher la sortie avec un titre explicite
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Sortie complète de condes()\\n')
cat('============================================================\\n')
print(res_condes)


# ------------------------------------------------------------
# 6. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- questionnaire_desc : tableau sans la variable textuelle libre\\n')
cat('- y_condes : nom de la variable quantitative décrite\\n')
cat('- num_var_condes : position de cette variable dans questionnaire_desc\\n')
cat('- correlations_y : liens avec les variables quantitatives\\n')
cat('- liaisons_quali_y : liens avec les variables qualitatives\\n')
cat('- res_condes : résultat complet de FactoMineR::condes()\\n')
cat('\\n')
cat('Idée clé : condes() généralise des analyses simples pour décrire une variable quantitative.\\n')
",
sortie_attendue = "Une description de `intention_achat` par les variables quantitatives et qualitatives, avec un rappel de la logique statistique sous-jacente.",
transition = "On passe maintenant à la description d'une variable qualitative avec catdes().",
question = "Quelle fonction FactoMineR décrit une variable quantitative continue ?",
reponse = "condes"
),

catdes = make_case(
  partie = "stat",
  titre = "10. catdes",
  objectif = "Utiliser catdes() pour décrire une variable qualitative ou des groupes.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 10 : catdes()
# ============================================================

# Objectif de cette case :
# comprendre que catdes() décrit une variable qualitative,
# c est-à-dire des modalités ou des groupes.
#
# Pour une variable qualitative à décrire :
# - les variables quantitatives sont comparées entre les groupes ;
# - les variables qualitatives sont croisées avec les groupes.
#
# On retrouve donc la logique de l ANOVA et du khi-deux,
# mais appliquée systématiquement.

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('Installe FactoMineR : install.packages(\"FactoMineR\")')
}


# ------------------------------------------------------------
# 1. Identifier la variable qualitative à décrire
# ------------------------------------------------------------

y_catdes <- 'profil_alim'

num_var_catdes <- which(names(questionnaire_desc) == y_catdes)

cat('\\n============================================================\\n')
cat('1. Variable qualitative à décrire\\n')
cat('============================================================\\n')
cat('Variable décrite : ', y_catdes, '\\n', sep = '')
cat('Position de cette variable dans questionnaire_desc : ', num_var_catdes, '\\n', sep = '')

cat('\\nRépartition des groupes :\\n')
print(table(questionnaire_desc[[y_catdes]]))


# ------------------------------------------------------------
# 2. Logique statistique avec les variables quantitatives
# ------------------------------------------------------------

# Exemple : comparer quelques moyennes selon le profil alimentaire.

variables_quanti_exemple <- c(
  'satisfaction',
  'intention_achat',
  'prix_percu',
  'naturalite',
  'ancrage_local'
)

moyennes_par_profil <- aggregate(
  questionnaire_desc[variables_quanti_exemple],
  by = list(profil = questionnaire_desc[[y_catdes]]),
  FUN = mean
)

moyennes_par_profil[, -1] <- round(moyennes_par_profil[, -1], 2)

cat('\\n============================================================\\n')
cat('2. Logique statistique : moyennes par profil\\n')
cat('============================================================\\n')
cat('Objet créé : moyennes_par_profil\\n')
cat('Lecture : on compare les moyennes des variables quantitatives\\n')
cat('entre les modalités de ', y_catdes, '.\\n', sep = '')
print(moyennes_par_profil, row.names = FALSE)


# Exemple d ANOVA simple pour une variable quantitative.
anova_satisfaction_profil <- aov(
  satisfaction ~ profil_alim,
  data = questionnaire_desc
)

cat('\\n============================================================\\n')
cat('3. Exemple : ANOVA satisfaction ~ profil_alim\\n')
cat('============================================================\\n')
cat('Objet créé : anova_satisfaction_profil\\n')
print(summary(anova_satisfaction_profil))


# ------------------------------------------------------------
# 3. Logique statistique avec les variables qualitatives
# ------------------------------------------------------------

# Exemple : croiser profil_alim avec type_produit.

table_profil_produit <- table(
  questionnaire_desc[[y_catdes]],
  questionnaire_desc$type_produit
)

test_chi2_profil_produit <- chisq.test(table_profil_produit)

cat('\\n============================================================\\n')
cat('4. Exemple : tableau croisé profil_alim x type_produit\\n')
cat('============================================================\\n')
cat('Objet créé : table_profil_produit\\n')
print(table_profil_produit)

cat('\\nTest du khi-deux associé :\\n')
cat('Objet créé : test_chi2_profil_produit\\n')
print(test_chi2_profil_produit)


# ------------------------------------------------------------
# 4. Utiliser catdes()
# ------------------------------------------------------------

res_catdes <- FactoMineR::catdes(
  questionnaire_desc,
  num.var = num_var_catdes
)

cat('\\n============================================================\\n')
cat('5. Résultat FactoMineR::catdes()\\n')
cat('============================================================\\n')
cat('Objet créé : res_catdes\\n')
cat('catdes() automatise la description des groupes ou modalités.\\n')
cat('Variable décrite : ', y_catdes, '\\n', sep = '')

cat('\\nÉléments contenus dans res_catdes :\\n')
print(names(res_catdes))


# ------------------------------------------------------------
# 5. Afficher la sortie avec un titre explicite
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('6. Sortie complète de catdes()\\n')
cat('============================================================\\n')
print(res_catdes)


# ------------------------------------------------------------
# 6. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- y_catdes : nom de la variable qualitative décrite\\n')
cat('- num_var_catdes : position de cette variable dans questionnaire_desc\\n')
cat('- moyennes_par_profil : comparaison de moyennes par profil\\n')
cat('- anova_satisfaction_profil : exemple d ANOVA simple par profil\\n')
cat('- table_profil_produit : tableau croisé profil x produit\\n')
cat('- test_chi2_profil_produit : test du khi-deux associé\\n')
cat('- res_catdes : résultat complet de FactoMineR::catdes()\\n')
cat('\\n')
cat('Idée clé : catdes() généralise des ANOVA et tableaux croisés pour décrire des groupes.\\n')
",
sortie_attendue = "Une description des profils alimentaires par variables quantitatives et qualitatives, avec un rappel de la logique statistique sous-jacente.",
transition = "Les sorties condes/catdes sont plus riches : il faut apprendre à les manipuler.",
question = "Quelle fonction FactoMineR décrit une variable qualitative ou des groupes ?",
reponse = "catdes"
),

manip_condes_catdes = make_case(
  partie = "r_sorties",
  titre = "11. Faire un prompt (3)",
  objectif = "Inspecter et transformer les sorties condes/catdes en texte exploitable.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 11 : manipuler les sorties condes/catdes
# ============================================================

# Objectif de cette case :
# appliquer aux sorties condes() et catdes() la même logique
# que celle utilisée pour LinearModel() et AovSum().
#
# Mais ici, les objets sont plus riches :
# ils contiennent plusieurs niveaux d information.
# Il faut donc les inspecter avant de les transformer en texte.

# ------------------------------------------------------------
# 1. Inspecter les objets retournés par condes() et catdes()
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('1. Que contient l objet res_condes ?\\n')
cat('Commande : names(res_condes)\\n')
cat('============================================================\\n')
print(names(res_condes))

cat('\\n============================================================\\n')
cat('2. Que contient l objet res_catdes ?\\n')
cat('Commande : names(res_catdes)\\n')
cat('============================================================\\n')
print(names(res_catdes))


# ------------------------------------------------------------
# 2. Observer la structure sans tout afficher
# ------------------------------------------------------------

# str() est utile pour comprendre la structure d un objet R.
# max.level = 2 évite d afficher trop de détails.

cat('\\n============================================================\\n')
cat('3. Structure simplifiée de res_condes\\n')
cat('Commande : str(res_condes, max.level = 2)\\n')
cat('============================================================\\n')
str(res_condes, max.level = 2)

cat('\\n============================================================\\n')
cat('4. Structure simplifiée de res_catdes\\n')
cat('Commande : str(res_catdes, max.level = 2)\\n')
cat('============================================================\\n')
str(res_catdes, max.level = 2)


# ------------------------------------------------------------
# 3. Capturer les sorties complètes sous forme de texte
# ------------------------------------------------------------

texte_condes <- paste(
  capture.output(print(res_condes)),
  collapse = '\\n'
)

texte_catdes <- paste(
  capture.output(print(res_catdes)),
  collapse = '\\n'
)

cat('\\n============================================================\\n')
cat('5. Sorties capturées sous forme de texte\\n')
cat('============================================================\\n')
cat('Objet créé : texte_condes\\n')
cat('Longueur : ', nchar(texte_condes), ' caractères\\n', sep = '')
cat('\\n')
cat('Objet créé : texte_catdes\\n')
cat('Longueur : ', nchar(texte_catdes), ' caractères\\n', sep = '')


# ------------------------------------------------------------
# 4. Afficher un aperçu contrôlé des textes capturés
# ------------------------------------------------------------

afficher_apercu_texte <- function(titre, nom_objet, texte, n = 1200) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('Objet affiché : ', nom_objet, '\\n', sep = '')
  cat('============================================================\\n')
  cat(substr(texte, 1, n))

  if (nchar(texte) > n) {
    cat('\\n\\n[... texte tronqué dans l affichage ...]\\n')
    cat('Longueur totale : ', nchar(texte), ' caractères\\n', sep = '')
  }
}

afficher_apercu_texte(
  titre = 'Aperçu du texte capturé pour condes()',
  nom_objet = 'texte_condes',
  texte = texte_condes
)

afficher_apercu_texte(
  titre = 'Aperçu du texte capturé pour catdes()',
  nom_objet = 'texte_catdes',
  texte = texte_catdes
)


# ------------------------------------------------------------
# 5. Construire une fonction pour produire un prompt
# ------------------------------------------------------------

# On évite de réécrire deux fois la même structure de prompt.
# On crée une fonction qui reçoit :
# - un titre ;
# - la variable décrite ;
# - le texte statistique capturé ;
# - des consignes spécifiques.

construire_prompt_description <- function(titre,
                                          variable_decrite,
                                          type_variable,
                                          texte_sortie,
                                          consignes) {

  paste(
    titre,
    '',
    paste0('Variable décrite : ', variable_decrite, '.'),
    paste0('Type de variable décrite : ', type_variable, '.'),
    '',
    'Sortie statistique utilisée :',
    texte_sortie,
    '',
    'Consignes :',
    paste0('- ', consignes, collapse = '\\n'),
    sep = '\\n'
  )
}

cat('\\n============================================================\\n')
cat('6. Fonction de construction de prompt\\n')
cat('============================================================\\n')
cat('Objet créé : construire_prompt_description()\\n')
cat('Rôle : produire un prompt à partir d une sortie statistique capturée.\\n')


# ------------------------------------------------------------
# 6. Construire le prompt pour condes()
# ------------------------------------------------------------

consignes_condes <- c(
  'identifier les variables les plus liées à intention_achat',
  'distinguer variables quantitatives et variables qualitatives',
  'interpréter les associations sans parler de causalité',
  'produire une synthèse courte et structurée'
)

prompt_condes <- construire_prompt_description(
  titre = '# Interprétation de condes()',
  variable_decrite = 'intention_achat',
  type_variable = 'quantitative',
  texte_sortie = texte_condes,
  consignes = consignes_condes
)

cat('\\n============================================================\\n')
cat('7. Prompt construit pour condes()\\n')
cat('============================================================\\n')
cat('Objet créé : prompt_condes\\n')
cat('Variable décrite : intention_achat\\n')


# ------------------------------------------------------------
# 7. Construire le prompt pour catdes()
# ------------------------------------------------------------

consignes_catdes <- c(
  'décrire chaque profil alimentaire',
  'séparer les preuves quantitatives et les modalités qualitatives',
  'ne pas surinterpréter les associations faibles',
  'proposer une synthèse lisible des profils'
)

prompt_catdes <- construire_prompt_description(
  titre = '# Interprétation de catdes()',
  variable_decrite = 'profil_alim',
  type_variable = 'qualitative',
  texte_sortie = texte_catdes,
  consignes = consignes_catdes
)

cat('\\n============================================================\\n')
cat('8. Prompt construit pour catdes()\\n')
cat('============================================================\\n')
cat('Objet créé : prompt_catdes\\n')
cat('Variable décrite : profil_alim\\n')


# ------------------------------------------------------------
# 8. Afficher un aperçu des prompts
# ------------------------------------------------------------

afficher_apercu_prompt <- function(titre, nom_objet, prompt, n = 1500) {
  cat('\\n============================================================\\n')
  cat(titre, '\\n')
  cat('Objet affiché : ', nom_objet, '\\n', sep = '')
  cat('============================================================\\n')
  cat(substr(prompt, 1, n))

  if (nchar(prompt) > n) {
    cat('\\n\\n[... prompt tronqué dans l affichage ...]\\n')
    cat('Longueur totale : ', nchar(prompt), ' caractères\\n', sep = '')
  }
}

afficher_apercu_prompt(
  titre = 'Aperçu du prompt condes()',
  nom_objet = 'prompt_condes',
  prompt = prompt_condes
)

afficher_apercu_prompt(
  titre = 'Aperçu du prompt catdes()',
  nom_objet = 'prompt_catdes',
  prompt = prompt_catdes
)


# ------------------------------------------------------------
# 9. Résumé pédagogique de la case
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- texte_condes : sortie complète de condes() transformée en texte\\n')
cat('- texte_catdes : sortie complète de catdes() transformée en texte\\n')
cat('- construire_prompt_description() : fonction générique de construction de prompt\\n')
cat('- consignes_condes : consignes utilisées pour interpréter condes()\\n')
cat('- consignes_catdes : consignes utilisées pour interpréter catdes()\\n')
cat('- prompt_condes : prompt construit pour la variable quantitative intention_achat\\n')
cat('- prompt_catdes : prompt construit pour la variable qualitative profil_alim\\n')
cat('\\n')
cat('Idée clé : on sait maintenant transformer des sorties FactoMineR riches\\n')
cat('en prompts structurés. C est précisément ce que NaileR va systématiser.\\n')
",
sortie_attendue = "Des textes et prompts issus de condes/catdes : `texte_condes`, `texte_catdes`, `prompt_condes`, `prompt_catdes`, construits de manière plus générique.",
transition = "NaileR prolonge cette logique en produisant des artefacts et prompts plus contrôlés.",
question = "Quelle fonction vient-on d'utiliser pour décrire la variable qualitative `profil_alim` ?",
reponse = "catdes"
),

nailer_catdes_exemple = make_case(
  partie = "entrainer",
  titre = "12a. Exemple NaileR",
  objectif = "Utiliser nail_catdes() sur une variable qualitative du jeu de données.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 12a : premier exemple avec NaileR et nail_catdes()
# ============================================================

# Objectif de cette case :
# utiliser NaileR sur une variable qualitative déjà décrite par catdes().
#
# Jusqu ici, nous avons fait à la main :
# 1. catdes() pour décrire profil_alim ;
# 2. capture.output() pour transformer la sortie en texte ;
# 3. paste() pour construire un prompt.
#
# nail_catdes() vise à systématiser cette logique.

# ------------------------------------------------------------
# 1. Vérifier la disponibilité de NaileR
# ------------------------------------------------------------

nailer_disponible <- requireNamespace('NaileR', quietly = TRUE)

cat('\\n============================================================\\n')
cat('1. Disponibilité de NaileR\\n')
cat('============================================================\\n')
cat('Package NaileR installé : ', nailer_disponible, '\\n', sep = '')


# ------------------------------------------------------------
# 2. Définir la variable qualitative à décrire
# ------------------------------------------------------------

var_catdes_nailer <- 'profil_alim'

num_var_catdes_nailer <- which(
  names(questionnaire_desc) == var_catdes_nailer
)

cat('\\n============================================================\\n')
cat('2. Variable qualitative décrite\\n')
cat('============================================================\\n')
cat('Variable décrite : ', var_catdes_nailer, '\\n', sep = '')
cat('Position dans questionnaire_desc : ', num_var_catdes_nailer, '\\n', sep = '')

cat('\\nRépartition de la variable :\\n')
print(table(questionnaire_desc[[var_catdes_nailer]]))


# ------------------------------------------------------------
# 3. Rappeler le travail fait à la main
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Ce que nous avons déjà construit à la main\\n')
cat('============================================================\\n')
cat('Objet res_catdes      : sortie FactoMineR::catdes()\\n')
cat('Objet texte_catdes    : sortie catdes() transformée en texte\\n')
cat('Objet prompt_catdes   : prompt construit à la main\\n')

if (exists('prompt_catdes')) {
  cat('\\nAperçu du prompt manuel prompt_catdes :\\n\\n')
  cat(substr(prompt_catdes, 1, 1000))

  if (nchar(prompt_catdes) > 1000) {
    cat('\\n\\n[... prompt manuel tronqué dans l affichage ...]\\n')
  }
}


# ------------------------------------------------------------
# 4. Appeler nail_catdes() si NaileR est disponible
# ------------------------------------------------------------

res_nail_catdes <- NULL

if (nailer_disponible) {

  exports_nailer <- getNamespaceExports('NaileR')

  if ('nail_catdes' %in% exports_nailer) {

    cat('\\n============================================================\\n')
    cat('4. Appel de NaileR::nail_catdes()\\n')
    cat('============================================================\\n')
    cat('Commande exécutée :\\n')
    cat('NaileR::nail_catdes(questionnaire_desc, num.var = num_var_catdes_nailer, generate = FALSE)\\n')

    res_nail_catdes <- tryCatch(
      NaileR::nail_catdes(
        questionnaire_desc,
        num.var = num_var_catdes_nailer,
        generate = FALSE
      ),
      error = function(e) {
        paste('nail_catdes non exécuté :', conditionMessage(e))
      }
    )

  } else {

    res_nail_catdes <- 'La fonction nail_catdes() n est pas exportée par cette version de NaileR.'
  }

} else {

  res_nail_catdes <- 'NaileR n est pas installé : on conserve le prompt manuel prompt_catdes comme point de comparaison.'
}


# ------------------------------------------------------------
# 5. Inspecter l objet retourné sans l afficher brutalement
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Objet retourné par nail_catdes()\\n')
cat('============================================================\\n')
cat('Objet créé : res_nail_catdes\\n')
cat('Classe de l objet : ', paste(class(res_nail_catdes), collapse = ', '), '\\n', sep = '')

if (is.list(res_nail_catdes)) {
  cat('Champs disponibles :\\n')
  print(names(res_nail_catdes))
} else {
  cat('Objet non listé ou message de repli :\\n')
  cat(as.character(res_nail_catdes), '\\n')
}


# ------------------------------------------------------------
# 6. Essayer d extraire un prompt depuis l objet NaileR
# ------------------------------------------------------------

extraire_champ_possible <- function(objet, noms_possibles) {

  if (!is.list(objet)) {
    return(NULL)
  }

  noms_objet <- names(objet)

  if (is.null(noms_objet)) {
    return(NULL)
  }

  for (nom in noms_possibles) {
    if (nom %in% noms_objet) {
      return(objet[[nom]])
    }
  }

  NULL
}

prompt_nail_catdes <- extraire_champ_possible(
  res_nail_catdes,
  c('prompt', 'prompts', 'request', 'text_prompt')
)

cat('\\n============================================================\\n')
cat('6. Prompt éventuellement produit par nail_catdes()\\n')
cat('============================================================\\n')

if (!is.null(prompt_nail_catdes)) {

  prompt_nail_catdes <- paste(
    capture.output(print(prompt_nail_catdes)),
    collapse = '\\n'
  )

  cat('Objet créé : prompt_nail_catdes\\n')
  cat('Aperçu :\\n\\n')
  cat(substr(prompt_nail_catdes, 1, 1500))

  if (nchar(prompt_nail_catdes) > 1500) {
    cat('\\n\\n[... prompt NaileR tronqué dans l affichage ...]\\n')
    cat('Longueur totale : ', nchar(prompt_nail_catdes), ' caractères\\n', sep = '')
  }

} else {

  cat('Aucun champ prompt standard détecté dans res_nail_catdes.\\n')
  cat('Ce n est pas bloquant : l objet complet reste disponible pour inspection.\\n')
}


# ------------------------------------------------------------
# 7. Comparaison pédagogique
# ------------------------------------------------------------

comparaison_catdes_nailer <- data.frame(
  etape = c(
    'catdes()',
    'capture.output()',
    'prompt manuel',
    'nail_catdes()'
  ),
  role = c(
    'décrire statistiquement une variable qualitative',
    'transformer la sortie en texte',
    'organiser une demande interprétative',
    'systématiser cette logique dans NaileR'
  ),
  objet = c(
    'res_catdes',
    'texte_catdes',
    'prompt_catdes',
    'res_nail_catdes'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('7. Comparaison avec le travail manuel\\n')
cat('============================================================\\n')
cat('Objet créé : comparaison_catdes_nailer\\n')
print(comparaison_catdes_nailer, row.names = FALSE)


# ------------------------------------------------------------
# 8. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé : objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- var_catdes_nailer : variable qualitative décrite\\n')
cat('- num_var_catdes_nailer : position de cette variable dans questionnaire_desc\\n')
cat('- res_nail_catdes : résultat ou message issu de nail_catdes()\\n')
cat('- prompt_nail_catdes : prompt extrait si disponible\\n')
cat('- comparaison_catdes_nailer : comparaison entre travail manuel et NaileR\\n')
cat('\\n')
cat('Idée clé : nail_catdes() prolonge le travail manuel réalisé autour de catdes().\\n')
cat('La case suivante prend du recul pour présenter le package NaileR.\\n')
",
sortie_attendue = "Un premier exemple de `nail_catdes()` appliqué à `profil_alim`, avec comparaison entre le travail manuel et l'automatisation NaileR.",
transition = "On prend ensuite du recul pour présenter le rôle général du package NaileR.",
question = "Quelle fonction NaileR utilise-t-on ici pour prolonger catdes() ?",
reponse = "nail_catdes"
),

nailer_presentation = make_case(
  partie = "entrainer",
  titre = "12b. Présenter NaileR",
  objectif = "Présenter le rôle de NaileR dans le workflow : de FactoMineR aux prompts, puis aux variables latentes et aux textes.",
  has_plot = FALSE,
  pdf_on_run = "NaileR.pdf",
  code = "
# ============================================================
# Case 12b : présenter le package NaileR
# ============================================================

# Objectif de cette case :
# prendre du recul après un premier exemple avec nail_catdes().
#
# NaileR est présenté ici comme un package qui prolonge
# les sorties de FactoMineR vers des prompts, artefacts
# et interprétations contrôlées.

# ------------------------------------------------------------
# 1. Vérifier les fonctions disponibles
# ------------------------------------------------------------

nailer_disponible <- requireNamespace('NaileR', quietly = TRUE)

if (nailer_disponible) {

  exports_nailer <- getNamespaceExports('NaileR')

  fonctions_nailer <- grep(
    '^nail_',
    exports_nailer,
    value = TRUE
  )

} else {

  exports_nailer <- character(0)

  fonctions_nailer <- c(
    'nail_condes',
    'nail_catdes',
    'nail_textual',
    'nail_textual_prep',
    'nail_textual_contextualized'
  )
}

fonctions_nailer_df <- data.frame(
  fonction = fonctions_nailer,
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('1. Fonctions NaileR repérées ou attendues\\n')
cat('============================================================\\n')
cat('Package NaileR installé : ', nailer_disponible, '\\n', sep = '')
cat('Objet créé : fonctions_nailer\\n')
print(fonctions_nailer_df, row.names = FALSE)


# ------------------------------------------------------------
# 2. Situer NaileR dans le workflow du tutoriel
# ------------------------------------------------------------

schema_nailer <- data.frame(
  etape = c(
    'FactoMineR',
    'R',
    'Prompt manuel',
    'NaileR',
    'LLM éventuel'
  ),
  role = c(
    'produire des sorties statistiques structurées',
    'inspecter, extraire et transformer les objets',
    'construire une demande interprétative contrôlée',
    'systématiser la production de prompts et d artefacts',
    'générer une interprétation à partir d un prompt contrôlé'
  ),
  exemple = c(
    'catdes(), condes(), PCA(), HCPC()',
    'names(), str(), capture.output(), paste()',
    'prompt_catdes, prompt_condes',
    'nail_catdes(), nail_condes(), nail_textual()',
    'Ollama, Gemini ou autre moteur'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('2. Place de NaileR dans le workflow\\n')
cat('============================================================\\n')
cat('Objet créé : schema_nailer\\n')
print(schema_nailer, row.names = FALSE)


# ------------------------------------------------------------
# 3. Distinguer EnTraineR et NaileR
# ------------------------------------------------------------

comparaison_entrainer_nailer <- data.frame(
  package = c('EnTraineR', 'NaileR'),
  point_de_depart = c(
    'analyses statistiques simples ou pédagogiques',
    'sorties FactoMineR plus riches et objets descriptifs'
  ),
  objectif = c(
    'apprendre à construire et contrôler des prompts statistiques',
    'produire des prompts et artefacts à partir de sorties complexes'
  ),
  exemples = c(
    'régression, ANOVA, corrélation, tests',
    'condes, catdes, dimensions latentes, classes, verbatims'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('3. Différence pédagogique entre EnTraineR et NaileR\\n')
cat('============================================================\\n')
cat('Objet créé : comparaison_entrainer_nailer\\n')
print(comparaison_entrainer_nailer, row.names = FALSE)


# ------------------------------------------------------------
# 4. Préparer la suite : explicite vers latent
# ------------------------------------------------------------

flow_suivant_nailer <- data.frame(
  objet = c(
    'variable qualitative explicite',
    'variable de classe latente',
    'dimension factorielle',
    'verbatims par classe',
    'synthèse statistique + textuelle'
  ),
  exemple = c(
    'profil_alim',
    'classe_hcpc',
    'Dim.1 issue de l ACP',
    'commentaires regroupés par classe',
    'interprétation finale des profils'
  ),
  fonction_ou_logique = c(
    'nail_catdes()',
    'catdes() puis logique NaileR',
    'condes() puis prompt latent',
    'nail_textual() / nail_textual_prep()',
    'nail_textual_contextualized()'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('4. Suite du workflow : de l explicite au latent\\n')
cat('============================================================\\n')
cat('Objet créé : flow_suivant_nailer\\n')
print(flow_suivant_nailer, row.names = FALSE)


# ------------------------------------------------------------
# 5. Message pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('NaileR ne remplace pas FactoMineR.\\n')
cat('NaileR prolonge les sorties de FactoMineR.\\n')
cat('\\n')
cat('Dans les cases précédentes, nous avons construit la logique à la main :\\n')
cat('1. produire une sortie statistique ;\\n')
cat('2. récupérer cette sortie dans R ;\\n')
cat('3. transformer cette sortie en texte ;\\n')
cat('4. construire un prompt contrôlé.\\n')
cat('\\n')
cat('Avec nail_catdes(), nous avons vu un premier exemple de cette automatisation.\\n')
cat('La suite du tutoriel va déplacer cette logique vers des objets moins explicites :\\n')
cat('- classes issues d une classification ;\\n')
cat('- dimensions factorielles ;\\n')
cat('- profils textuels ;\\n')
cat('- interprétation de variables latentes.\\n')


# ------------------------------------------------------------
# 6. Objets disponibles pour la suite
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Objets disponibles pour la suite\\n')
cat('============================================================\\n')
cat('- fonctions_nailer : fonctions nail_* repérées ou attendues\\n')
cat('- schema_nailer : place de NaileR dans le workflow\\n')
cat('- comparaison_entrainer_nailer : distinction EnTraineR / NaileR\\n')
cat('- flow_suivant_nailer : transition vers latent et données textuelles\\n')
",
sortie_attendue = "Une présentation structurée de NaileR : fonctions, rôle dans le workflow, différence avec EnTraineR et transition vers les variables latentes et les textes.",
transition = "On passe ensuite au flow : variable de classe latente, description statistique et analyse textuelle.",
question = "Quel package prolonge les sorties FactoMineR vers des prompts et artefacts interprétables ?",
reponse = "nailer"
),

acp_hcpc_classes = make_case(
  partie = "stat",
  titre = "13. ACP + HCPC",
  objectif = "Construire une variable de classe (latente) à partir de variables actives.",
  has_plot = TRUE,
  code = "
# ============================================================
# Case 13 : construire une classe latente par ACP + HCPC
# ============================================================

# Objectif de cette case :
# passer d un questionnaire avec des variables explicites
# à une variable de classe, latente (implicite).
#
# Ici, on réalise :
# - une ACP sur des variables quantitatives actives ;
# - une classification ascendante hiérarchiques sur les coordonnées factorielles.
#
# On obtient une variable de classe. Cette variable n était pas directement posée
# dans le questionnaire. Elle est construite par l analyse : c est pourquoi on parlera
# de variable de classe latente ou de classe construite.

# ------------------------------------------------------------
# 1. Vérifier les prérequis
# ------------------------------------------------------------

if (!exists('questionnaire')) {
  stop(
    'L objet questionnaire est absent. Exécute d abord la case 1.',
    call. = FALSE
  )
}

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop(
    'Le package FactoMineR est nécessaire pour cette case.',
    call. = FALSE
  )
}


# -----------------------------------------------------------------
# 2. Définir les variables actives qui vont construire la typologie
# -----------------------------------------------------------------

variables_typologie <- c(
  'attention_prix',
  'contrainte_temps',
  'cuisine_maison',
  'lecture_labels',
  'achat_local',
  'ouverture_innovation',
  'usage_appli_alim',
  'preoccupation_sante',
  'autonomie_alimentaire',
  'confiance_labels'
)

variables_absentes <- setdiff(
  variables_typologie,
  names(questionnaire)
)

if (length(variables_absentes) > 0) {
  stop(
    'Variables de typologie absentes du questionnaire : ',
    paste(variables_absentes, collapse = ', '),
    call. = FALSE
  )
}

cat('\\n============================================================\\n')
cat('1. Variables actives de typologie\\n')
cat('============================================================\\n')
cat('Objet créé : variables_typologie\\n')
print(variables_typologie)


# ------------------------------------------------------------
# 3. Préparer le tableau actif
# ------------------------------------------------------------

donnees_typologie <- questionnaire[, variables_typologie]

variables_non_numeriques <- names(donnees_typologie)[
  !vapply(donnees_typologie, is.numeric, logical(1))
]

if (length(variables_non_numeriques) > 0) {
  stop(
    'Les variables suivantes ne sont pas numériques : ',
    paste(variables_non_numeriques, collapse = ', '),
    '. Une ACP nécessite ici des variables quantitatives.',
    call. = FALSE
  )
}

cat('\\n============================================================\\n')
cat('2. Tableau actif pour ACP et HCPC\\n')
cat('============================================================\\n')
cat('Objet créé : donnees_typologie\\n')
cat('Nombre de lignes : ', nrow(donnees_typologie), '\\n', sep = '')
cat('Nombre de variables actives : ', ncol(donnees_typologie), '\\n', sep = '')

cat('\\nAperçu des premières lignes :\\n')
print(head(donnees_typologie, 3))


cat('\\n============================================================\\n')
cat('Transition : de NaileR aux classes latentes\\n')
cat('============================================================\\n')
cat('Nous avons vu que NaileR peut prolonger des fonctions de FactoMineR comme catdes().\\n')
cat('Jusqu ici, nous avons surtout travaillé avec des variables explicites.\\n')
cat('Pour appliquer cette logique à une variable latente, il faut d abord la construire.\\n')
cat('C est le rôle de cette case : créer classe_hcpc par ACP + HCPC.\\n')

# ------------------------------------------------------------
# 4. Réaliser l ACP
# ------------------------------------------------------------

res_pca <- FactoMineR::PCA(
  donnees_typologie,
  scale.unit = TRUE,
  graph = FALSE
)

cat('\\n============================================================\\n')
cat('3. ACP sur les variables de typologie\\n')
cat('============================================================\\n')
cat('Objet créé : res_pca\\n')

cat('\\nValeurs propres et pourcentages d inertie :\\n')
print(res_pca$eig)


# ------------------------------------------------------------
# 5. Réaliser la classification HCPC
# ------------------------------------------------------------

set.seed(123)

res_hcpc <- FactoMineR::HCPC(
  res_pca,
  nb.clust = 3,
  graph = FALSE
)

cat('\\n============================================================\\n')
cat('4. Classification HCPC\\n')
cat('============================================================\\n')
cat('Objet créé : res_hcpc\\n')
cat('Variable créée : res_hcpc$data.clust$clust\\n')

cat('\\nRépartition des classes :\\n')
print(table(res_hcpc$data.clust$clust))


# ------------------------------------------------------------
# 6. Visualiser les classes sur le premier plan factoriel
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Graphique ACP coloré par classe HCPC\\n')
cat('============================================================\\n')
cat('Le graphique représente les individus sur le plan Dim.1 x Dim.2.\\n')
cat('La couleur correspond à la classe construite par HCPC.\\n')

FactoMineR::plot.HCPC(res_hcpc, choice = 'map', draw.tree = FALSE)

# ------------------------------------------------------------
# 7. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('Nous avons créé une nouvelle variable : res_hcpc$data.clust$clust.\\n')
cat('Cette variable n est pas une réponse directe du questionnaire.\\n')
cat('Elle résulte d une ACP puis d une classification.\\n')
cat('\\n')
cat('La prochaine étape consiste à décrire statistiquement ces classes.\\n')
",
sortie_attendue = "Une ACP `res_pca`, une classification `res_hcpc` et une variable `res_hcpc$data.clust$clust`.",
transition = "On décrit maintenant les classes construites avec catdes(), puis avec nail_catdes().",
question = "Quelle fonction de FactoMineR permet de construire une classification à partir d'une ACP ?",
reponse = "HCPC"
),

decrire_classes = make_case(
  partie = "stat",
  titre = "14. Décrire les classes",
  objectif = "Décrire la variable de classe construite avec catdes() et nail_catdes().",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 14 : décrire les classes HCPC
# ============================================================

# Objectif de cette case :
# interpréter une variable de classe construite.
#
# classe_hcpc n est pas une variable directement posée aux répondants.
# Elle doit donc être décrite, caractérisée et nommée avec prudence.
#
# On commence par FactoMineR::catdes(), puis on regarde comment
# NaileR peut prolonger cette logique avec nail_catdes().

# ------------------------------------------------------------
# 1. Vérifier les prérequis
# ------------------------------------------------------------

if (!exists('questionnaire')) {
  stop(
    'L objet questionnaire est absent. Exécute d abord la case 1.',
    call. = FALSE
  )
}

if (!exists('res_hcpc')) {
  stop(
    'L objet res_hcpc est absent. Exécute d abord la case 13.',
    call. = FALSE
  )
}

questionnaire$classe_hcpc <- res_hcpc$data.clust$clust

if (!'classe_hcpc' %in% names(questionnaire)) {
  stop(
    'La variable classe_hcpc est absente. Exécute d abord la case 13.',
    call. = FALSE
  )
}

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop(
    'Le package FactoMineR est nécessaire pour cette case.',
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 2. Préparer le tableau de description des classes
# ------------------------------------------------------------

questionnaire_desc_classes <- questionnaire[
  ,
  setdiff(names(questionnaire), c('id', 'commentaire'))
]

num_var_classe <- which(
  names(questionnaire_desc_classes) == 'classe_hcpc'
)

cat('\\n============================================================\\n')
cat('1. Variable de classe à décrire\\n')
cat('============================================================\\n')
cat('Objet créé : questionnaire_desc_classes\\n')
cat('Variable décrite : classe_hcpc\\n')
cat('Position de classe_hcpc : ', num_var_classe, '\\n', sep = '')

cat('\\nRépartition des classes :\\n')
print(table(questionnaire_desc_classes$classe_hcpc))


# ------------------------------------------------------------
# 3. Décrire les classes avec catdes()
# ------------------------------------------------------------

res_catdes_classes <- FactoMineR::catdes(
  questionnaire_desc_classes,
  num.var = num_var_classe
)

cat('\\n============================================================\\n')
cat('2. Description des classes avec FactoMineR::catdes()\\n')
cat('============================================================\\n')
cat('Objet créé : res_catdes_classes\\n')

cat('\\nÉléments contenus dans res_catdes_classes :\\n')
print(names(res_catdes_classes))

cat('\\nAperçu de la sortie catdes() :\\n')
texte_catdes_classes <- paste(
  capture.output(print(res_catdes_classes)),
  collapse = '\\n'
)

cat(substr(texte_catdes_classes, 1, 2500))

if (nchar(texte_catdes_classes) > 2500) {
  cat('\\n\\n[... sortie catdes tronquée dans l affichage ...]\\n')
  cat('Longueur totale : ', nchar(texte_catdes_classes), ' caractères\\n', sep = '')
}


# ------------------------------------------------------------
# 4. Prolonger avec nail_catdes() si NaileR est disponible
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Prolongement avec NaileR::nail_catdes()\\n')
cat('============================================================\\n')

if (!requireNamespace('NaileR', quietly = TRUE)) {

  res_nail_catdes_classes <- 'NaileR n est pas installé : nail_catdes() ne peut pas être exécuté.'
  cat(res_nail_catdes_classes, '\\n')

} else {

  res_nail_catdes_classes <- tryCatch(
    NaileR::nail_catdes(
      questionnaire_desc_classes,
      num.var = num_var_classe,
      generate = FALSE,
      interpretation_mode = 'latent'
    ),
    error = function(e) {
      paste('nail_catdes() non exécuté :', conditionMessage(e))
    }
  )

  cat('Objet créé : res_nail_catdes_classes\\n')
  cat('Classe : ', paste(class(res_nail_catdes_classes), collapse = ', '), '\\n', sep = '')

  if (is.list(res_nail_catdes_classes)) {
    cat('\\nNoms disponibles :\\n')
    print(names(res_nail_catdes_classes))
  }

# ------------------------------------------------------------
# Affichage lisible de la sortie nail_catdes()
# ------------------------------------------------------------

extraire_texte_lisible <- function(objet) {

  if (is.list(objet)) {

    champs_possibles <- c(
      'prompt',
      'request',
      'response',
      'interpretation',
      'text',
      'result'
    )

    champ_trouve <- intersect(champs_possibles, names(objet))

    if (length(champ_trouve) > 0) {
      texte <- objet[[champ_trouve[1]]]
    } else {
      texte <- capture.output(str(objet, max.level = 2))
    }

  } else {
    texte <- capture.output(print(objet))
  }

  texte <- paste(texte, collapse = '\n')

  # Transforme les retours à la ligne échappés en vrais retours à la ligne
  texte <- gsub('\\\\n', '\n', texte)

  # Transforme les tabulations échappées si besoin
  texte <- gsub('\\\\t', '\t', texte)

  texte
}

afficher_apercu_lisible <- function(texte, n_lignes = 60, largeur = 100) {

  lignes <- unlist(strsplit(texte, '\n', fixed = TRUE))

  lignes_wrappees <- unlist(
    lapply(lignes, function(ligne) {
      if (!nzchar(trimws(ligne))) {
        return('')
      }
      strwrap(ligne, width = largeur)
    })
  )

  lignes_affichees <- head(lignes_wrappees, n_lignes)

  cat(paste(lignes_affichees, collapse = '\n'))

  if (length(lignes_wrappees) > n_lignes) {
    cat('\n\n[... aperçu tronqué ...]\n')
    cat('Nombre total de lignes : ', length(lignes_wrappees), '\n', sep = '')
  }
}

texte_nail_catdes_classes <- extraire_texte_lisible(
  res_nail_catdes_classes
)

cat('\nAperçu lisible :\n\n')

afficher_apercu_lisible(
  texte = texte_nail_catdes_classes,
  n_lignes = 60,
  largeur = 100
)
}


# ------------------------------------------------------------
# 5. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('Une classe construite doit être décrite avant d être nommée.\\n')
cat('catdes() donne les variables et modalités qui caractérisent les classes.\\n')
cat('nail_catdes() prolonge cette logique vers un prompt ou une interprétation contrôlée.\\n')
cat('\\n')
cat('La prochaine étape ajoute les verbatims associés à ces classes.\\n')
",
sortie_attendue = "Une description statistique `res_catdes_classes` et, si NaileR est disponible, un objet `res_nail_catdes_classes`.",
transition = "On prépare maintenant les verbatims associés aux classes construites.",
question = "Quelle fonction FactoMineR permet de décrire des classes ou une variable qualitative ?",
reponse = "catdes"
),

preparer_textes_classes = make_case(
  partie = "r_sorties",
  titre = "15. Relier classes et verbatims",
  objectif = "Préparer le tableau qui permettra à NaileR de croiser classes, commentaires libres et variables du questionnaire.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 15 : préparer les verbatims par classe
# ============================================================

# Objectif de cette case :
# relier la variable de classe construite aux commentaires libres.
#
# La classe_hcpc donne une structure latente.
# La variable commentaire apporte des verbatims.
#
# On prépare donc un tableau qui associe :
# - une classe construite ;
# - un texte libre ;
# - des variables structurées utiles pour contextualiser l interprétation.

# ------------------------------------------------------------
# 1. Vérifier les prérequis
# ------------------------------------------------------------

if (!exists('questionnaire')) {
  stop(
    'L objet questionnaire est absent. Exécute d abord la case 1.',
    call. = FALSE
  )
}

variables_requises <- c('classe_hcpc', 'commentaire')

variables_absentes <- setdiff(
  variables_requises,
  names(questionnaire)
)

if (length(variables_absentes) > 0) {
  stop(
    'Variables absentes : ',
    paste(variables_absentes, collapse = ', '),
    '. Exécute d abord les cases précédentes.',
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 2. Construire le tableau textuel latent
# ------------------------------------------------------------

variables_textuelles_classes <- c(
  'classe_hcpc',
  'commentaire',
  'satisfaction',
  'intention_achat',
  'prix_percu',
  'plaisir',
  'naturalite',
  'confiance',
  'ancrage_local',
  'usage_numerique',
  'sensibilite_env',
  'attention_prix',
  'contrainte_temps',
  'cuisine_maison',
  'lecture_labels',
  'achat_local',
  'ouverture_innovation',
  'usage_appli_alim',
  'preoccupation_sante',
  'autonomie_alimentaire',
  'confiance_labels',
  'type_produit',
  'budget_contraint',
  'sexe',
  'age_classe',
  'lieu_achat',
  'profil_alim'
)

variables_textuelles_classes <- intersect(
  variables_textuelles_classes,
  names(questionnaire)
)

dataset_textuel_classes <- questionnaire[, variables_textuelles_classes]

dataset_textuel_classes$classe_hcpc <- factor(dataset_textuel_classes$classe_hcpc)
dataset_textuel_classes$commentaire <- as.character(dataset_textuel_classes$commentaire)

cat('\\n============================================================\\n')
cat('1. Tableau textuel par classe créé\\n')
cat('============================================================\\n')
cat('Objet créé : dataset_textuel_classes\\n')
cat('Variable de groupe : classe_hcpc\\n')
cat('Variable textuelle : commentaire\\n')
cat('Nombre de lignes : ', nrow(dataset_textuel_classes), '\\n', sep = '')
cat('Nombre de colonnes : ', ncol(dataset_textuel_classes), '\\n', sep = '')

cat('\\nVariables conservées :\\n')
print(names(dataset_textuel_classes))


# ------------------------------------------------------------
# 3. Examiner la répartition des classes
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('2. Répartition des classes\\n')
cat('============================================================\\n')
print(table(dataset_textuel_classes$classe_hcpc))


# ------------------------------------------------------------
# 4. Examiner les verbatims
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('3. Aperçu des commentaires libres\\n')
cat('============================================================\\n')
cat('Nombre total de verbatims : ', length(dataset_textuel_classes$commentaire), '\\n', sep = '')
cat('Nombre de verbatims uniques : ', length(unique(dataset_textuel_classes$commentaire)), '\\n', sep = '')

cat('\\nExemples de verbatims :\\n')
cat(paste('-', head(unique(dataset_textuel_classes$commentaire), 8), collapse = '\\n'))
cat('\\n')


# ------------------------------------------------------------
# 5. Verbatims par classe
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('4. Exemples de verbatims par classe\\n')
cat('============================================================\\n')

for (cl in levels(dataset_textuel_classes$classe_hcpc)) {
  cat('\\n--- Classe ', cl, ' ---\\n', sep = '')

  verbatims_classe <- unique(
    dataset_textuel_classes$commentaire[
      dataset_textuel_classes$classe_hcpc == cl
    ]
  )

  cat(paste('-', head(verbatims_classe, 4), collapse = '\\n'))
  cat('\\n')
}


# ------------------------------------------------------------
# 6. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('Cette case ne produit pas une nouvelle analyse statistique.\n')
cat('Elle prépare le format de données nécessaire pour passer de catdes() aux fonctions textuelles de NaileR.\n')
cat('On conserve une ligne par répondant afin de garder le lien entre classe, commentaire et variables structurées.\n')
",
sortie_attendue = "Un tableau `dataset_textuel_classes` associant `classe_hcpc`, `commentaire` et les variables structurées.",
transition = "On prépare maintenant deux artefacts : un profil textuel et un profil structuré des classes.",
question = "Quelle variable construite sert à regrouper les commentaires libres ?",
reponse = "classe_hcpc"
),

preparer_artefacts_classes = make_case(
  partie = "entrainer",
  titre = "16. Artefacts NaileR",
  objectif = "Préparer les artefacts textuels et structurés des classes avec NaileR.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 16 : préparer les artefacts textuels et structurés
# ============================================================

# Objectif de cette case :
# préparer deux types d artefacts pour interpréter les classes :
#
# 1. un artefact textuel issu des verbatims ;
# 2. un artefact structuré issu des variables du questionnaire.
#
# Ces artefacts sont inspectables et pourront être combinés
# dans une synthèse contextualisée.

# ------------------------------------------------------------
# 1. Vérifier les prérequis
# ------------------------------------------------------------

if (!exists('dataset_textuel_classes')) {
  stop(
    'L objet dataset_textuel_classes est absent. Exécute d abord la case 15.',
    call. = FALSE
  )
}

if (!requireNamespace('NaileR', quietly = TRUE)) {
  res_textual_prep_classes <- 'NaileR n est pas installé : nail_textual_prep() ne peut pas être exécuté.'
  res_group_profile_classes <- 'NaileR n est pas installé : nail_group_profile_prep() ne peut pas être exécuté.'

  cat('\\n============================================================\\n')
  cat('NaileR indisponible\\n')
  cat('============================================================\\n')
  cat('Les artefacts NaileR ne peuvent pas être produits sur cette machine.\\n')

} else {

# ----------------------------------------------------------
# Fonction utilitaire : afficher proprement un objet NaileR
# ----------------------------------------------------------

extraire_texte_lisible <- function(objet) {

  if (is.list(objet)) {

    champs_possibles <- c(
      'prompt',
      'prompts',
      'request',
      'response',
      'text',
      'texts',
      'interpretation',
      'summary',
      'profile'
    )

    champs_trouves <- intersect(champs_possibles, names(objet))

    if (length(champs_trouves) > 0) {
      texte <- objet[[champs_trouves[1]]]
    } else {
      texte <- capture.output(str(objet, max.level = 2))
    }

  } else {
    texte <- objet
  }

  texte <- paste(as.character(texte), collapse = '\n')

  # Convertir les retours à la ligne échappés en vrais retours à la ligne
  texte <- gsub('\\\\n', '\n', texte)
  texte <- gsub('\\\\t', '\t', texte)

  texte
}

afficher_apercu_lisible <- function(texte, n_lignes = 70, largeur = 100) {

  lignes <- unlist(strsplit(texte, '\n', fixed = TRUE))

  lignes_wrappees <- unlist(
    lapply(lignes, function(ligne) {
      if (!nzchar(trimws(ligne))) {
        return('')
      }
      strwrap(ligne, width = largeur)
    })
  )

  lignes_affichees <- head(lignes_wrappees, n_lignes)

  cat(paste(lignes_affichees, collapse = '\n'))

  if (length(lignes_wrappees) > n_lignes) {
    cat('\n\n[... aperçu tronqué ...]\n')
    cat('Nombre total de lignes : ', length(lignes_wrappees), '\n', sep = '')
  }
}


  # ----------------------------------------------------------
  # 2. Identifier les colonnes pour le profil textuel
  # ----------------------------------------------------------

  num_var_classes <- which(names(dataset_textuel_classes) == 'classe_hcpc')
  num_text_classes <- which(names(dataset_textuel_classes) == 'commentaire')

  cat('\\n============================================================\\n')
  cat('1. Colonnes utilisées par les fonctions textuelles\\n')
  cat('============================================================\\n')
  cat('num.var  = ', num_var_classes, '  # classe_hcpc\\n', sep = '')
  cat('num.text = ', num_text_classes, '  # commentaire\\n', sep = '')


  # ----------------------------------------------------------
  # 3. Préparer l artefact textuel
  # ----------------------------------------------------------

  cat('\\n============================================================\\n')
  cat('2. Préparation de l artefact textuel\\n')
  cat('============================================================\\n')
  cat('Fonction : NaileR::nail_textual_prep()\\n')
  cat('generate = FALSE : préparation sans génération LLM.\\n')

  res_textual_prep_classes <- tryCatch(
    NaileR::nail_textual_prep(
      dataset = dataset_textuel_classes,
      num.var = num_var_classes,
      num.text = num_text_classes,
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) {
      paste('nail_textual_prep() non exécuté :', conditionMessage(e))
    }
  )

  cat('\\nObjet créé : res_textual_prep_classes\\n')
  cat('Classe : ', paste(class(res_textual_prep_classes), collapse = ', '), '\\n', sep = '')

  if (is.list(res_textual_prep_classes)) {
    cat('\\nNoms disponibles :\\n')
    print(names(res_textual_prep_classes))

   texte_premier_artefact <- extraire_texte_lisible(
  res_textual_prep_classes[[1]]
)

cat('\nAperçu lisible du premier artefact textuel :\n\n')

afficher_apercu_lisible(
  texte = texte_premier_artefact,
  n_lignes = 70,
  largeur = 100
)
  } else {
    cat('\\nMessage ou sortie :\\n')
    cat(as.character(res_textual_prep_classes), '\\n')
  }


  # ----------------------------------------------------------
  # 4. Préparer l artefact structuré
  # ----------------------------------------------------------

  dataset_profil_classes <- dataset_textuel_classes[
    ,
    setdiff(names(dataset_textuel_classes), 'commentaire')
  ]

  num_var_profil_classes <- which(
    names(dataset_profil_classes) == 'classe_hcpc'
  )

  cat('\\n============================================================\\n')
  cat('3. Préparation de l artefact structuré\\n')
  cat('============================================================\\n')
  cat('Objet créé : dataset_profil_classes\\n')
  cat('Fonction : NaileR::nail_group_profile_prep()\\n')
  cat('generate = FALSE : préparation sans génération LLM.\\n')

  res_group_profile_classes <- tryCatch(
    NaileR::nail_group_profile_prep(
      dataset = dataset_profil_classes,
      num.var = num_var_profil_classes,
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) {
      paste('nail_group_profile_prep() non exécuté :', conditionMessage(e))
    }
  )

  cat('\\nObjet créé : res_group_profile_classes\\n')
  cat('Classe : ', paste(class(res_group_profile_classes), collapse = ', '), '\\n', sep = '')

  if (is.list(res_group_profile_classes)) {
    cat('\\nNoms disponibles :\\n')
    print(names(res_group_profile_classes))

    texte_premier_profil <- extraire_texte_lisible(
  res_group_profile_classes[[1]]
)

cat('\nAperçu lisible du premier artefact structuré :\n\n')

afficher_apercu_lisible(
  texte = texte_premier_profil,
  n_lignes = 70,
  largeur = 100
)
  } else {
    cat('\\nMessage ou sortie :\\n')
    cat(as.character(res_group_profile_classes), '\\n')
  }
}


# ------------------------------------------------------------
# 5. Comparaison pédagogique
# ------------------------------------------------------------

comparaison_artefacts_classes <- data.frame(
  fonction = c(
    'nail_textual_prep()',
    'nail_group_profile_prep()'
  ),
  materiau = c(
    'commentaires libres',
    'variables structurées du questionnaire'
  ),
  objet_cree = c(
    'res_textual_prep_classes',
    'res_group_profile_classes'
  ),
  role = c(
    'préparer un profil textuel par classe',
    'préparer un profil statistique ou descriptif par classe'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('4. Deux artefacts complémentaires\\n')
cat('============================================================\\n')
cat('Objet créé : comparaison_artefacts_classes\\n')
print(comparaison_artefacts_classes, row.names = FALSE)


# ------------------------------------------------------------
# 6. Résumé pédagogique
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('On dispose maintenant de deux objets complémentaires :\\n')
cat('- res_textual_prep_classes : ce que disent les verbatims ;\\n')
cat('- res_group_profile_classes : ce que disent les variables structurées.\\n')
cat('\\n')
cat('La dernière case combine ces deux sources.\\n')
",
code_display = "
# Identifier la variable de classe et la variable textuelle
num_var_classes <- which(names(dataset_textuel_classes) == 'classe_hcpc')
num_text_classes <- which(names(dataset_textuel_classes) == 'commentaire')

# Préparer l artefact textuel à partir des verbatims
res_textual_prep_classes <- NaileR::nail_textual_prep(
  dataset = dataset_textuel_classes,
  num.var = num_var_classes,
  num.text = num_text_classes,
  model = 'llama3',
  generate = FALSE
)

# Retirer la colonne textuelle pour préparer le profil structuré
dataset_profil_classes <- dataset_textuel_classes[
  ,
  setdiff(names(dataset_textuel_classes), 'commentaire')
]

num_var_profil_classes <- which(
  names(dataset_profil_classes) == 'classe_hcpc'
)

# Préparer l artefact structuré à partir des variables du questionnaire
res_group_profile_classes <- NaileR::nail_group_profile_prep(
  dataset = dataset_profil_classes,
  num.var = num_var_profil_classes,
  model = 'llama3',
  generate = FALSE
)
",
sortie_attendue = "Deux artefacts : `res_textual_prep_classes` et `res_group_profile_classes`.",
transition = "On combine maintenant les artefacts textuels et structurés dans une synthèse contextualisée.",
question = "Quelle fonction de NaileR prépare les commentaires libres par classe ?",
reponse = "nail_textual_prep"
),

synthese_contextualisee_classes = make_case(
  partie = "entrainer",
  titre = "17. Synthèse contextualisée",
  objectif = "Combiner artefact textuel et artefact structuré pour interpréter les classes.",
  has_plot = FALSE,
  code = "
# ============================================================
# Case 17 : synthèse contextualisée des classes
# ============================================================

# Objectif de cette case :
# combiner deux sources préparées :
# - les verbatims ;
# - les variables structurées.
#
# L idée centrale est un workflow centré sur les artefacts :
# on ne demande pas au LLM d interpréter directement des textes bruts.
# On lui donne des objets intermédiaires préparés et inspectables.
#
# Dans cette version du tutoriel, la génération LLM n est pas faite
# en direct. Si un résultat pré-calculé est disponible dans le package,
# il est chargé pour illustrer le résultat final.


# ------------------------------------------------------------
# 2. Fonctions utilitaires d affichage lisible
# ------------------------------------------------------------

trouver_champ_texte <- function(objet, champs, profondeur = 0) {

  if (profondeur > 3) {
    return(NULL)
  }

  if (is.list(objet)) {

    noms <- names(objet)

    if (!is.null(noms)) {
      champ_direct <- intersect(champs, noms)

      if (length(champ_direct) > 0) {
        return(objet[[champ_direct[1]]])
      }
    }

    for (element in objet) {
      resultat <- trouver_champ_texte(
        element,
        champs = champs,
        profondeur = profondeur + 1
      )

      if (!is.null(resultat)) {
        return(resultat)
      }
    }
  }

  NULL
}

extraire_champ_texte <- function(objet, champs) {

  champ <- trouver_champ_texte(
    objet = objet,
    champs = champs
  )

  if (is.null(champ)) {
    texte <- paste(
      capture.output(str(objet, max.level = 2)),
      collapse = '\\n'
    )
  } else {
    texte <- paste(
      as.character(champ),
      collapse = '\\n'
    )
  }

  # Conversion des retours à la ligne échappés en vrais retours à la ligne
  texte <- gsub(
    paste0(intToUtf8(92), 'n'),
    '\\n',
    texte,
    fixed = TRUE
  )

  texte <- gsub(
    paste0(intToUtf8(92), 't'),
    '\\t',
    texte,
    fixed = TRUE
  )

  texte
}

afficher_texte_lisible <- function(texte, n_lignes = 90, largeur = 100) {

  lignes <- unlist(
    strsplit(
      texte,
      '\\n',
      fixed = TRUE
    )
  )

  lignes_wrappees <- unlist(
    lapply(
      lignes,
      function(ligne) {
        if (!nzchar(trimws(ligne))) {
          return('')
        }

        strwrap(
          ligne,
          width = largeur
        )
      }
    )
  )

  lignes_affichees <- head(
    lignes_wrappees,
    n_lignes
  )

  cat(
    paste(
      lignes_affichees,
      collapse = '\\n'
    )
  )

  if (length(lignes_wrappees) > n_lignes) {
    cat('\\n\\n[... sortie tronquée ...]\\n')
    cat(
      'Nombre total de lignes : ',
      length(lignes_wrappees),
      '\\n',
      sep = ''
    )
  }
}


# ------------------------------------------------------------
# 3. Chercher une génération pré-calculée
# ------------------------------------------------------------

precomputed_error <- NULL

precomputed_case17 <- tryCatch(
  load_precomputed_asset('case17_nailer_contextualized.rds'),
  error = function(e) {
    precomputed_error <<- conditionMessage(e)
    NULL
  }
)

generation_precalculee <- !is.null(precomputed_case17)

if (generation_precalculee) {

  res_textual_prep_classes <- precomputed_case17$res_textual_prep_classes
  res_group_profile_classes <- precomputed_case17$res_group_profile_classes
  res_contextualized_classes <- precomputed_case17$res_contextualized_classes

  cat('\\n============================================================\\n')
  cat('1. Génération pré-calculée chargée\\n')
  cat('============================================================\\n')
  cat('La génération LLM n est pas réalisée en direct.\\n')
  cat('Un résultat pré-calculé est chargé depuis le package.\\n\\n')

  if (!is.null(precomputed_case17$metadata)) {
    cat('Métadonnées :\\n')
    print(precomputed_case17$metadata)
  }

} else {

  cat('\\n============================================================\\n')
  cat('1. Aucun résultat pré-calculé trouvé\\n')
  cat('============================================================\\n')
  cat('On prépare uniquement la structure avec generate = FALSE.\\n')

  if (!is.null(precomputed_error)) {
  cat('\nDiagnostic de recherche du fichier pré-calculé :\n')
  cat(precomputed_error)
  cat('\n')
}

  # ----------------------------------------------------------
  # Vérifier les prérequis seulement si aucun pré-calcul
  # n est disponible.
  # ----------------------------------------------------------

  if (!exists('res_textual_prep_classes')) {
    stop(
      'L objet res_textual_prep_classes est absent. Exécute d abord la case 16.',
      call. = FALSE
    )
  }

  if (!exists('res_group_profile_classes')) {
    stop(
      'L objet res_group_profile_classes est absent. Exécute d abord la case 16.',
      call. = FALSE
    )
  }

  if (!requireNamespace('NaileR', quietly = TRUE)) {
    stop(
      'NaileR est nécessaire si aucun résultat pré-calculé n est disponible.',
      call. = FALSE
    )
  }

  if (!is.list(res_textual_prep_classes) ||
      !is.list(res_group_profile_classes)) {
    stop(
      'Les artefacts attendus ne sont pas disponibles sous forme de listes. ',
      'Vérifie que la case 16 a bien été exécutée avec NaileR installé.',
      call. = FALSE
    )
  }

  res_contextualized_classes <- tryCatch(
    NaileR::nail_textual_contextualized(
      group_profile_prep = res_group_profile_classes,
      textual_prep = res_textual_prep_classes,
      interpretation_mode = 'comparative',
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) {
      paste(
        'nail_textual_contextualized() non exécuté :',
        conditionMessage(e)
      )
    }
  )
}


# ------------------------------------------------------------
# 4. Artefacts disponibles
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('2. Artefacts disponibles\\n')
cat('============================================================\\n')
cat('Artefact textuel : res_textual_prep_classes\\n')
cat('Artefact structuré : res_group_profile_classes\\n')
cat('Synthèse contextualisée : res_contextualized_classes\\n')


# ------------------------------------------------------------
# 5. Présenter le principe de contextualisation
# ------------------------------------------------------------

flow_contextualisation_classes <- data.frame(
  source = c(
    'Verbatims',
    'Variables structurées',
    'Synthèse contextualisée'
  ),
  objet_R = c(
    'res_textual_prep_classes',
    'res_group_profile_classes',
    'res_contextualized_classes'
  ),
  role = c(
    'faire émerger les thèmes et formulations associés aux classes',
    'décrire les classes à partir des variables du questionnaire',
    'combiner les deux sources pour interpréter les classes'
  ),
  stringsAsFactors = FALSE
)

cat('\\n============================================================\\n')
cat('3. Principe de contextualisation\\n')
cat('============================================================\\n')
cat('Objet créé : flow_contextualisation_classes\\n')
print(flow_contextualisation_classes, row.names = FALSE)


# ------------------------------------------------------------
# 6. Inspecter la sortie contextualisée
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('4. Sortie contextualisée\\n')
cat('============================================================\\n')
cat('Objet créé : res_contextualized_classes\\n')
cat(
  'Classe : ',
  paste(class(res_contextualized_classes), collapse = ', '),
  '\\n',
  sep = ''
)

cat('\\nMode de production : ')
if (generation_precalculee) {
  cat('résultat pré-calculé chargé depuis le package\\n')
} else {
  cat('préparation locale avec generate = FALSE\\n')
}


# ------------------------------------------------------------
# 7. Afficher le prompt ou l objet préparé
# ------------------------------------------------------------

cat('\\n============================================================\\n')
cat('5. Prompt ou objet préparé\\n')
cat('============================================================\\n')

texte_prompt_contextualized <- extraire_champ_texte(
  res_contextualized_classes,
  champs = c(
    'prompt',
    'prompts',
    'request',
    'instruction',
    'instructions'
  )
)

afficher_texte_lisible(
  texte = texte_prompt_contextualized,
  n_lignes = 90,
  largeur = 100
)


# ------------------------------------------------------------
# 8. Afficher la réponse pré-calculée si elle existe
# ------------------------------------------------------------

cat('\\n\\n============================================================\\n')
cat('6. Réponse ou interprétation générée\\n')
cat('============================================================\\n')

texte_reponse_contextualized <- extraire_champ_texte(
  res_contextualized_classes,
  champs = c(
    'response',
    'answer',
    'result',
    'interpretation',
    'summary',
    'text'
  )
)

afficher_texte_lisible(
  texte = texte_reponse_contextualized,
  n_lignes = 120,
  largeur = 100
)


# ------------------------------------------------------------
# 9. Résumé pédagogique final
# ------------------------------------------------------------

cat('\\n\\n============================================================\\n')
cat('Résumé pédagogique\\n')
cat('============================================================\\n')
cat('Nous avons construit une variable de classe latente : classe_hcpc.\\n')
cat('Nous l avons décrite statistiquement avec catdes().\\n')
cat('Nous avons ensuite préparé deux artefacts :\\n')
cat('- un artefact textuel issu des commentaires ;\\n')
cat('- un artefact structuré issu des variables du questionnaire.\\n')
cat('\\n')
cat('La synthèse contextualisée combine ces deux sources.\\n')
cat('Dans le tutoriel, la génération peut être pré-calculée afin d éviter\\n')
cat('un appel LLM en direct pendant la séance.\\n')
cat('\\n')
cat('C est le principe central du workflow : stabiliser l interprétation\\n')
cat('par des objets intermédiaires visibles et inspectables.\\n')
",
sortie_attendue = "Un objet `res_contextualized_classes` combinant profils textuels et profils structurés des classes.",
transition = "Le tutoriel se termine sur l'idée d'un workflow artefact-centré : les interprétations reposent sur des objets intermédiaires inspectables.",
question = "Quelle fonction de NaileR combine les artefacts textuels et structurés ?",
reponse = "nail_textual_contextualized"
)
)

case_ids <- names(cases)

# Le plateau n'est plus strictement linéaire :
# - `prompt_manuel` ouvre une branche vers le niveau 2 ;
# - les deux chemins convergent vers une présentation du package EnTraineR ;
# - puis on entre dans la pratique avec les fonctions EnTraineR.
edges <- data.frame(
  from = c(
    'donnees', 'exploration', 'linearmodel', 'aovsum', 'recuperer_sorties',
    'prompt_manuel', 'prompt_manuel_n2',
    'entrainer_intro',
    'entrainer_presentation', 'boucle_y_x', 'condes', 'catdes',
    'manip_condes_catdes', 'nailer_catdes_exemple', 'nailer_presentation', 'acp_hcpc_classes',
    'decrire_classes', 'preparer_textes_classes', 'preparer_artefacts_classes'
  ),
  to = c(
    'exploration', 'linearmodel', 'aovsum', 'recuperer_sorties', 'prompt_manuel',
    'prompt_manuel_n2','entrainer_intro',
    'entrainer_presentation',
    'boucle_y_x', 'condes', 'catdes', 'manip_condes_catdes',
    'nailer_catdes_exemple', 'nailer_presentation', 'acp_hcpc_classes', 'decrire_classes',
    'preparer_textes_classes', 'preparer_artefacts_classes', 'synthese_contextualisee_classes'
  ),
  arrows = 'to',
  smooth = FALSE,
  stringsAsFactors = FALSE
)

plateau_positions <- data.frame(
  id = c(
    'donnees', 'exploration', 'linearmodel', 'aovsum', 'recuperer_sorties', 'prompt_manuel',
    'prompt_manuel_n2', 'entrainer_intro', 'entrainer_presentation',
    'boucle_y_x', 'condes', 'catdes', 'manip_condes_catdes', 'nailer_catdes_exemple',
    'nailer_presentation', 'acp_hcpc_classes', 'decrire_classes', 'preparer_textes_classes',
    'preparer_artefacts_classes', 'synthese_contextualisee_classes'
  ),
  x = c(
    0, 260, 520, 780, 1040, 1300, 1560,
    1560, 1300,
    1040, 780, 520, 260, 0,
    0, 260, 520, 780, 1040, 1300
  ),
  y = c(
    0, 0, 0, 0, 0, 0, 0,
    210, 210,
    210, 210, 210, 210, 210,
    420, 420, 420, 420, 420, 420
  ),
  stringsAsFactors = FALSE
)

normaliser <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub(",", ".", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("\\(\\)$", "", x)
  x
}

make_blank_plot <- function() {
  f <- tempfile(fileext = ".png")
  png(filename = f, width = 1100, height = 700, res = 120)
  plot.new()
  text(0.5, 0.5, "Aucun graphique pour le moment", cex = 1.4)
  dev.off()
  f
}

# Affichage pédagogique des objets visibles.
# L'objectif est d'éviter les sorties illisibles :
# - chaînes avec \n imprimées comme texte brut ;
# - listes contenant des prompts complets ;
# - objets EnTraineR / NaileR trop longs.
render_value_pedagogique <- function(x,
                                      max_chars = 1800,
                                      max_depth = 2,
                                      depth = 0,
                                      name = NULL) {

  prefix <- if (!is.null(name)) paste0("$", name, "\n") else ""

  if (is.null(x)) {
    cat(prefix)
    cat("NULL\n")
    return(invisible(NULL))
  }

  if (is.character(x)) {
    txt <- paste(x, collapse = "\n")

    if (nchar(txt) > max_chars) {
      txt <- paste0(
        substr(txt, 1, max_chars),
        "\n\n[... sortie tronquée : ",
        nchar(txt),
        " caractères au total ...]\n"
      )
    }

    cat(prefix)
    cat(txt)
    cat("\n")
    return(invisible(NULL))
  }

  if (is.data.frame(x) || is.matrix(x) || inherits(x, "table")) {
    cat(prefix)
    print(x)
    return(invisible(NULL))
  }

  if (is.atomic(x)) {
    cat(prefix)
    print(x)
    return(invisible(NULL))
  }

  if (is.list(x)) {
    cat(prefix)
    cat("Objet de type liste")

    if (!is.null(class(x))) {
      cat(" — classe : ", paste(class(x), collapse = ", "), sep = "")
    }

    nms <- names(x)

    if (!is.null(nms)) {
      cat("\nChamps disponibles : ", paste(nms, collapse = ", "), "\n", sep = "")
    } else {
      cat("\nLongueur : ", length(x), "\n", sep = "")
    }

    if (depth >= max_depth) {
      cat("[Affichage limité : profondeur maximale atteinte]\n")
      return(invisible(NULL))
    }

    if (length(x) == 0) {
      return(invisible(NULL))
    }

    n_show <- min(length(x), 8)

    for (i in seq_len(n_show)) {
      nm <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
      cat("\n--- ", nm, " ---\n", sep = "")
      render_value_pedagogique(
        x[[i]],
        max_chars = max_chars,
        max_depth = max_depth,
        depth = depth + 1
      )
    }

    if (length(x) > n_show) {
      cat("\n[... ", length(x) - n_show, " élément(s) non affiché(s) ...]\n", sep = "")
    }

    return(invisible(NULL))
  }

  cat(prefix)
  print(x)
  invisible(NULL)
}

eval_code_capture <- function(code, envir = .GlobalEnv) {
  # Largeur de console volontairement large pour préserver les sorties tabulaires
  # comme summary.data.frame, LinearModel(), AovSum(), condes() ou catdes().
  old_width <- getOption("width")
  on.exit(options(width = old_width), add = TRUE)
  options(width = 160)

  exprs <- parse(text = code)
  out <- character()

  for (expr in exprs) {
    one <- capture.output({
      withCallingHandlers({
        res <- withVisible(eval(expr, envir = envir))

        if (isTRUE(res$visible) && !is.null(res$value)) {
          render_value_pedagogique(res$value)
        }
      },
      message = function(m) {
        cat(conditionMessage(m), "\n")
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        cat("Warning: ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      })
    })

    out <- c(out, one)
  }

  paste(out, collapse = "\n")
}

execute_case <- function(case, envir = .GlobalEnv, blank_plot = NULL) {
  code <- case$code
  if (isTRUE(case$has_plot)) {
    plot_file <- tempfile(fileext = ".png")
    png(filename = plot_file, width = 1200, height = 760, res = 120)
    sortie <- tryCatch(eval_code_capture(code, envir = envir), error = function(e) paste("Erreur :", conditionMessage(e)), finally = dev.off())
    list(output = sortie, plot_file = plot_file, has_plot = TRUE)
  } else {
    sortie <- tryCatch(eval_code_capture(code, envir = envir), error = function(e) paste("Erreur :", conditionMessage(e)))
    list(output = sortie, plot_file = blank_plot, has_plot = FALSE)
  }
}

check_answer <- function(case, answer, envir = .GlobalEnv) {
  if (!is.null(case$validator) && is.function(case$validator)) return(isTRUE(case$validator(answer, envir)))
  if (is.null(case$reponse)) return(TRUE)
  identical(normaliser(answer), normaliser(case$reponse))
}

# ------------------------------------------------------------
# Sauvegarde locale
# ------------------------------------------------------------
#
# L'application est locale : on sauvegarde dans le dossier de travail,
# dans un sous-dossier _plateau_sauvegarde.
#
# Deux choses sont sauvegardées :
# 1. l'état du plateau : cases déverrouillées, visitées, case sélectionnée ;
# 2. les objets R créés par les cases : questionnaire, modèles, PCA, HCPC, prompts...
#
# Cela permet de reprendre même si la session Shiny est perdue après mise en veille.

plateau_save_dir <- function() {
  dir <- tools::R_user_dir("SeRiouS", which = "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

plateau_state_file <- function() {
  file.path(plateau_save_dir(), "state.rds")
}

plateau_objects_file <- function() {
  file.path(plateau_save_dir(), "objects.rds")
}

plateau_plot_file <- function() {
  file.path(plateau_save_dir(), "last_plot.png")
}

plateau_object_names <- function() {
  c(
    # Données et variables intermédiaires
    "questionnaire", "questionnaire_desc", "questionnaire_dim", "questionnaire_hcpc",
    "type_produit", "budget_contraint", "sexe", "age_classe", "lieu_achat",
    "prix_percu", "naturalite", "ancrage_local", "plaisir", "confiance",
    "usage_numerique", "sensibilite_env", "satisfaction", "intention_achat",
    "score_engagement", "profil_alim", "commentaire", "variables_quanti",

    # LinearModel / AovSum / sorties
    "res_lm_fm", "res_aovsum", "lm_ftest", "lm_ttest", "lm_resume",
    "aov_ftest", "aov_ttest", "texte_linearmodel", "texte_aovsum",
    "formule_lm", "formule_aov", "prompt_linearmodel", "prompt_aovsum", "prompt_linearmodel_n2", "prompt_aovsum_n2", "prompt_linearmodel_utilise", "prompt_aovsum_utilise", "infos_lm", "infos_aov", "extraire_infos_formule", "source_prompts", "format_num",

    # EnTraineR / transitions
    "entrainer_pkg", "entrainer_disponible", "version_entrainer",
    "presentation_entrainer", "schema_entrainer", "resume_package_entrainer",
    "fonctions_entrainer", "fonctions_entrainer_df", "resume_entrainer", "objet_transition_entrainer",
    "options_entrainer", "arguments_entrainer",

    # Boucles
    "variables_x", "modeles_univaries", "textes_modeles_univaries",

    # condes / catdes / NaileR
    "res_condes", "res_catdes", "texte_condes", "texte_catdes",
    "prompt_condes", "prompt_catdes", "nailer_disponible",
    "exports_nailer", "fonctions_nailer", "res_nail_condes", "res_nail_catdes",

    # ACP / HCPC / latent
    "res_pca", "res_condes_dim1", "prompt_dim1",
    "res_hcpc", "res_catdes_classes",

    # Texte
    "verbatims_par_classe", "texte_verbatims", "texte_catdes_classes", "prompt_final",
    "variables_typologie",
    "donnees_typologie",
    "questionnaire_desc_classes",
    "num_var_classe",
    "res_nail_catdes_classes",
    "dataset_textuel_classes",
    "dataset_profil_classes",
    "num_var_classes",
    "num_text_classes",
    "num_var_profil_classes",
    "res_textual_prep_classes",
    "res_group_profile_classes",
    "res_contextualized_classes",
    "comparaison_artefacts_classes",
    "flow_contextualisation_classes"
  )
}

save_global_objects <- function(envir = .GlobalEnv) {
  objs <- plateau_object_names()
  objs <- objs[vapply(objs, exists, logical(1), envir = envir, inherits = FALSE)]

  if (length(objs) == 0) {
    saveRDS(list(), plateau_objects_file())
    return(invisible(character(0)))
  }

  obj_list <- mget(objs, envir = envir, inherits = FALSE)
  saveRDS(obj_list, plateau_objects_file())
  invisible(objs)
}

load_global_objects <- function(envir = .GlobalEnv) {
  f <- plateau_objects_file()

  if (!file.exists(f)) {
    return(invisible(character(0)))
  }

  obj_list <- readRDS(f)

  if (length(obj_list) > 0) {
    list2env(obj_list, envir = envir)
  }

  invisible(names(obj_list))
}

clear_plateau_save <- function() {
  d <- plateau_save_dir()
  files <- c(plateau_state_file(), plateau_objects_file(), plateau_plot_file())
  unlink(files[file.exists(files)], force = TRUE)
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  div(
    class = "serious-header",
    div(class = "serious-title", "SeRiouS"),
    div(
      class = "serious-subtitle",
      "De LinearModel à NaileR — de l’explicite au latent, par les artefacts"
    )
  ),
  div(
    class = "serious-card",
    uiOutput("legend"),
    hr(),
    fluidRow(
      column(3, actionButton("save_state", "Sauvegarder", class = "btn-primary")),
      column(3, actionButton("load_state", "Reprendre", class = "btn-warning")),
      column(3, actionButton("clear_state", "Effacer sauvegarde", class = "btn-danger")),
      column(3, uiOutput("save_status"))
    )
  ),
  br(),
  # ----------------------------------------------------------
  # Ligne 1 : plateau pleine largeur
  # ----------------------------------------------------------
  fluidRow(
    column(width = 12,
      div(class = "serious-card plateau-card",
        fluidRow(
          column(4, actionButton("fit_plateau", "Recentrer le plateau", class = "btn-primary")),
          column(4, actionButton("focus_selected", "Centrer sur la case", class = "btn-primary")),
          column(4, actionButton("zoom_case", "Zoom case", class = "btn-primary"))
        ),
        br(),
        visNetworkOutput("plateau", height = "680px"),
        div(class = "small-note", "Couleurs = parties pédagogiques ; icônes = statut : 🔒 verrouillée, 🔓 déverrouillée, ✅ visitée.")
      )
    )
  ),

  br(),

  # ----------------------------------------------------------
  # Ligne 1b : case sélectionnée pleine largeur, en deux blocs
  # ----------------------------------------------------------
  fluidRow(
    column(width = 12,
           div(class = "serious-card case-card",
               h3("Case sélectionnée"),
               uiOutput("case_info"),
               hr(),
               uiOutput("question_ui"),
               fluidRow(
                 column(4, actionButton("valider", "Valider", class = "btn-primary")),
                 column(4, actionButton("executer", "Exécuter", class = "btn-success")),
                 column(4, actionButton("ouvrir_code", "Ouvrir code", class = "btn-warning"))
               )
           )
    )
  ),

  hr(),

  # ----------------------------------------------------------
  # Ligne 2 : code débloqué + sortie console
  # ----------------------------------------------------------
  fluidRow(
    column(width = 6,
      div(class = "serious-card case-card",
        h3("Code essentiel"),
        div(
          class = "small-note",
          "Ce bloc affiche le code à comprendre. La sortie console peut être produite par une version enrichie avec des messages pédagogiques."
        ),
        verbatimTextOutput("code_affiche")
      )
    ),
    column(width = 6,
      div(class = "serious-card case-card",
        h3("Sortie console"),
        verbatimTextOutput("sortie")
      )
    )
  ),

  hr(),

  # ----------------------------------------------------------
  # Ligne 3 : graphique + PDF / beamer
  # ----------------------------------------------------------
  fluidRow(
    column(width = 6,
      div(class = "serious-card case-card",
        h3("Graphique généré"),
        uiOutput("plot_status"),
        imageOutput("graphique", height = "460px")
      )
    ),
    column(width = 6,
      div(class = "serious-card case-card",
        h3("Support PDF / beamer"),
        uiOutput("pdf_viewer")
      )
    )
  )
)

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Etat de l'application
  # ----------------------------------------------------------
  # Point important de la V3 :
  # - le graphe n'est plus reconstruit quand on visite ou déverrouille une case ;
  # - seul le style des noeuds est mis à jour via visNetworkProxy().
  # Cela évite les décalages entre case sélectionnée et code affiché.

  blank_plot <- make_blank_plot()

  selected_case <- reactiveVal("donnees")

  etat <- reactiveValues(
    unlocked = c("donnees"),
    visited = character(0),
    output_text = "",
    plot_file = blank_plot,
    last_has_plot = FALSE,
    run_id = 0,
    running = FALSE,
    current_pdf = NULL,
    current_pdf_case = NULL
  )


  # ----------------------------------------------------------
  # Sauvegarde/reprise de session
  # ----------------------------------------------------------

  last_save_time <- reactiveVal(NULL)

  if (file.exists(plateau_state_file())) {
    last_save_time(file.info(plateau_state_file())$mtime)
  }

  current_state_as_list <- function() {
    list(
      version = "mvp_v14",
      timestamp = Sys.time(),
      selected = selected_case(),
      unlocked = etat$unlocked,
      visited = etat$visited,
      output_text = etat$output_text,
      last_has_plot = etat$last_has_plot,
      run_id = etat$run_id,
      current_pdf = etat$current_pdf,
      current_pdf_case = etat$current_pdf_case
    )
  }

  save_current_session <- function(silent = FALSE) {
    plateau_save_dir()

    state <- current_state_as_list()
    saveRDS(state, plateau_state_file())

    saved_objects <- save_global_objects(envir = .GlobalEnv)

    # On copie seulement le dernier vrai graphique.
    # Si la dernière case n'a pas généré de graphique, on supprime l'ancien
    # pour éviter de restaurer un graphique périmé.
    if (isTRUE(etat$last_has_plot) &&
        !is.null(etat$plot_file) &&
        file.exists(etat$plot_file)) {

      src_plot <- normalizePath(etat$plot_file, mustWork = FALSE)
      dst_plot <- normalizePath(plateau_plot_file(), mustWork = FALSE)

      if (!identical(src_plot, dst_plot)) {
        file.copy(etat$plot_file, plateau_plot_file(), overwrite = TRUE)
      }

    } else if (file.exists(plateau_plot_file())) {
      unlink(plateau_plot_file(), force = TRUE)
    }

    last_save_time(Sys.time())

    if (!silent) {
      showNotification(
        paste0(
          "Sauvegarde effectuée : ",
          length(saved_objects),
          " objet(s) R sauvegardé(s)."
        ),
        type = "message"
      )
    }

    invisible(TRUE)
  }

  restore_current_session <- function() {
    if (!file.exists(plateau_state_file())) {
      showNotification("Aucune sauvegarde trouvée.", type = "warning")
      return(invisible(FALSE))
    }

    state <- readRDS(plateau_state_file())
    loaded_objects <- load_global_objects(envir = .GlobalEnv)

    selected <- state$selected %||% "donnees"
    if (!selected %in% names(cases)) {
      selected <- "donnees"
    }

    selected_case(selected)

    etat$unlocked <- union("donnees", intersect(state$unlocked %||% "donnees", names(cases)))
    etat$visited <- intersect(state$visited %||% character(0), names(cases))
    etat$output_text <- state$output_text %||% ""
    etat$last_has_plot <- isTRUE(state$last_has_plot)
    etat$run_id <- state$run_id %||% 0
    etat$current_pdf <- state$current_pdf %||% NULL
    etat$current_pdf_case <- state$current_pdf_case %||% NULL

    if (isTRUE(etat$last_has_plot) && file.exists(plateau_plot_file())) {
      etat$plot_file <- plateau_plot_file()
    } else {
      etat$plot_file <- blank_plot
      etat$last_has_plot <- FALSE
    }

    last_save_time(file.info(plateau_state_file())$mtime)
    updateTextInput(session, "reponse", value = "")
    update_plateau_nodes()

    showNotification(
      paste0(
        "Session reprise : ",
        length(loaded_objects),
        " objet(s) R restauré(s)."
      ),
      type = "message"
    )

    invisible(TRUE)
  }

  # ----------------------------------------------------------
  # Fonctions locales de statut des noeuds
  # ----------------------------------------------------------
  #
  # Principe retenu :
  # - la couleur d'un noeud représente toujours sa partie pédagogique ;
  # - le statut est indiqué par une icône dans le label.
  #
  # Ainsi, modifier `partie = "entrainer"` dans une case modifie
  # immédiatement sa couleur, même si la case est verrouillée ou visitée.

  node_status <- function(id) {
    if (id %in% etat$visited) {
      "visitée"
    } else if (id %in% etat$unlocked) {
      "déverrouillée"
    } else {
      "verrouillée"
    }
  }

  node_icon <- function(id) {
    status <- node_status(id)

    if (identical(status, "visitée")) {
      "✅ "
    } else if (identical(status, "déverrouillée")) {
      "🔓 "
    } else {
      "🔒 "
    }
  }

  node_label <- function(id) {
    paste0(node_icon(id), cases[[id]]$titre)
  }

  # node_title <- function(id) {
  #   paste0(
  #     cases[[id]]$objectif,
  #     "<br><br><b>Partie :</b> ", partie_label[[cases[[id]]$partie]],
  #     "<br><b>Statut :</b> ", node_status(id)
  #   )
  # }

  # Le groupe visNetwork porte la couleur.
  # Il doit donc toujours correspondre à la partie pédagogique.
  node_group <- function(id) {
    cases[[id]]$partie
  }

  make_nodes_for_current_state <- function() {
    ids <- names(cases)

    nodes <- data.frame(
      id = ids,
      label = vapply(ids, node_label, character(1)),
      partie = vapply(cases, function(x) x$partie, character(1)),
      group = vapply(ids, node_group, character(1)),
      shape = "box",
      fixed = TRUE,
      physics = FALSE,
      stringsAsFactors = FALSE
    )

    merge(nodes, plateau_positions, by = "id", all.x = TRUE, sort = FALSE)
  }

  make_nodes_initial <- function() {
    ids <- names(cases)

    # Le rendu initial garde la même logique :
    # couleur = partie pédagogique ;
    # statut = icône dans le label.
    initial_label <- vapply(
      ids,
      function(id) {
        if (identical(id, "donnees")) {
          paste0("🔓 ", cases[[id]]$titre)
        } else {
          paste0("🔒 ", cases[[id]]$titre)
        }
      },
      character(1)
    )


    nodes <- data.frame(
      id = ids,
      label = initial_label,
      partie = vapply(cases, function(x) x$partie, character(1)),
      group = vapply(ids, function(id) cases[[id]]$partie, character(1)),
      shape = "box",
      fixed = TRUE,
      physics = FALSE,
      stringsAsFactors = FALSE
    )

    merge(nodes, plateau_positions, by = "id", all.x = TRUE, sort = FALSE)
  }

  update_plateau_nodes <- function() {
    # Mise à jour légère : ne reconstruit pas le graphe.
    visNetworkProxy("plateau") |>
      visUpdateNodes(nodes = make_nodes_for_current_state())
  }

  current_case <- reactive({
    id <- selected_case()
    req(id)
    cases[[id]]
  })

  # ----------------------------------------------------------
  # Légende
  # ----------------------------------------------------------

  output$legend <- renderUI({
    tagList(lapply(seq_len(nrow(parties)), function(i) {
      div(
        class = "legend-item",
        span(
          class = "legend-swatch",
          style = paste0(
            "background-color:", parties$color[i],
            "; border-color:", parties$border[i], ";"
          )
        ),
        parties$label[i]
      )
    }))
  })


  output$save_status <- renderUI({
    t <- last_save_time()

    if (is.null(t) || !file.exists(plateau_state_file())) {
      return(span(class = "small-note", "Aucune sauvegarde"))
    }

    span(
      class = "small-note",
      paste0("Dernière sauvegarde : ", format(t, "%H:%M:%S"))
    )
  })

  observeEvent(input$save_state, {
    save_current_session(silent = FALSE)
  })

  observeEvent(input$load_state, {
    restore_current_session()
  })

  observeEvent(input$clear_state, {
    clear_plateau_save()
    last_save_time(NULL)
    showNotification("Sauvegarde effacée.", type = "message")
  })

  # ----------------------------------------------------------
  # Rendu initial du plateau
  # ----------------------------------------------------------
  # Important : ce rendu ne dépend plus de etat$visited ni de etat$unlocked.
  # Le plateau reste donc stable quand une case est visitée/déverrouillée.

  output$plateau <- renderVisNetwork({
    graph <- visNetwork(make_nodes_initial(), edges, height = "700px")

    for (i in seq_len(nrow(parties))) {
      graph <- visGroups(
        graph,
        groupname = parties$id[i],
        color = list(
          background = parties$color[i],
          border = parties$border[i]
        )
      )
    }

    graph |>
      visNodes(
        shape = "box",
        font = list(size = 17),
        margin = 12,
        borderWidth = 2
      ) |>
      visEdges(
        arrows = "to",
        smooth = FALSE,
        color = list(color = "#777777")
      ) |>
      visPhysics(enabled = FALSE, stabilization = FALSE) |>
      visInteraction(
        dragNodes = FALSE,
        dragView = TRUE,
        zoomView = FALSE,
        navigationButtons = FALSE,
        keyboard = FALSE,
        hover = FALSE
      ) |>
      visOptions(highlightNearest = FALSE) |>
      visEvents(
        selectNode = "
          function(nodes) {
            if (nodes.nodes.length > 0) {
              Shiny.setInputValue(
                'case_clicked',
                nodes.nodes[0],
                {priority: 'event'}
              );
            }
          }
        "
      )
  })

  session$onFlushed(function() {
    visNetworkProxy("plateau") |>
      visFit(nodes = names(cases), animation = FALSE)
  }, once = TRUE)

  # ----------------------------------------------------------
  # Sélection d'une case
  # ----------------------------------------------------------

  observeEvent(input$case_clicked, {
    id <- input$case_clicked

    if (is.null(id) || !id %in% names(cases)) {
      return()
    }

    selected_case(id)

    # Evite qu'une ancienne réponse saisie reste associée à une nouvelle case.
    updateTextInput(session, "reponse", value = "")
  }, ignoreNULL = TRUE)

  observeEvent(input$fit_plateau, {
    visNetworkProxy("plateau") |>
      visFit(
        nodes = names(cases),
        animation = list(duration = 500, easingFunction = "easeInOutQuad")
      )
  })

  observeEvent(input$focus_selected, {
    id <- selected_case()
    req(id)

    visNetworkProxy("plateau") |>
      visFocus(
        id = id,
        scale = 0.85,
        animation = list(duration = 500, easingFunction = "easeInOutQuad")
      )
  })

  observeEvent(input$zoom_case, {
    id <- selected_case()
    req(id)

    visNetworkProxy("plateau") |>
      visFocus(
        id = id,
        scale = 1.45,
        animation = list(duration = 500, easingFunction = "easeInOutQuad")
      )
  })

  # ----------------------------------------------------------
  # Panneau de droite : informations et verrou
  # ----------------------------------------------------------

  output$case_info <- renderUI({
    id <- selected_case()
    req(id)

    x <- cases[[id]]

    statut <- if (id %in% etat$unlocked) {
      "déverrouillée"
    } else {
      "verrouillée"
    }

    statut_class <- if (id %in% etat$unlocked) {
      "case-badge unlocked"
    } else {
      "case-badge locked"
    }

    visited_badge <- if (id %in% etat$visited) {
      span(class = "case-badge visited", "visitée")
    } else {
      NULL
    }

    tagList(
      div(class = "case-title", x$titre),

      div(
        span(class = "case-badge", partie_label[[x$partie]]),
        span(class = statut_class, statut),
        visited_badge
      ),

      fluidRow(
        column(
          width = 6,
          div(
            class = "case-section",
            div(class = "case-section-label", "Objectif"),
            div(class = "case-section-content", x$objectif)
          )
        ),
        column(
          width = 6,
          div(
            class = "case-section",
            div(class = "case-section-label", "Sortie attendue"),
            div(class = "case-section-content", x$sortie_attendue)
          ),
          div(
            class = "case-section",
            div(class = "case-section-label", "Transition"),
            div(class = "case-section-content", x$transition)
          )
        )
      )
    )
  })

  output$question_ui <- renderUI({
    id <- selected_case()
    req(id)

    x <- cases[[id]]

    if (id %in% etat$unlocked) {
      return(
        div(
          class = "case-section",
          div(class = "case-section-label", "Déverrouillage"),
          div(class = "case-section-content", "Cette case est déjà déverrouillée.")
        )
      )
    }

    tagList(
      div(
        class = "case-section",
        div(class = "case-section-label", "Question pour déverrouiller"),
        div(class = "case-section-content", x$question)
      ),
      textInput("reponse", "Réponse", value = "")
    )
  })


  output$pdf_viewer <- renderUI({
    id <- selected_case()
    req(id)

    # Force le recalcul après validation / exécution si run_id existe chez toi
    etat$run_id

    x <- cases[[id]]

    case_unlocked <- isTRUE(id %in% etat$unlocked)

    has_case_pdf <- !is.null(x$pdf) &&
      length(x$pdf) == 1 &&
      nzchar(x$pdf)

    has_current_pdf <- !is.null(etat$current_pdf) &&
      length(etat$current_pdf) == 1 &&
      nzchar(etat$current_pdf) &&
      identical(etat$current_pdf_case, id)

    pdf_file <- NULL

    # 1. PDF associé à la case : visible seulement si la case est déverrouillée
    if (case_unlocked && has_case_pdf) {
      pdf_file <- x$pdf
    }

    # 2. PDF déclenché après exécution de cette case
    if (is.null(pdf_file) && has_current_pdf) {
      pdf_file <- etat$current_pdf
    }

    # 3. Aucun PDF à afficher
    if (is.null(pdf_file)) {

      message_pdf <- if (has_case_pdf && !case_unlocked) {
        "PDF verrouillé : débloque d'abord cette case."
      } else {
        "Aucun PDF associé à cette case pour le moment."
      }

      return(
        div(
          class = "small-note",
          message_pdf
        )
      )
    }

    pdf_src <- ensure_pdf_asset(pdf_file)

    if (is.null(pdf_src)) {
      return(
        div(
          class = "small-note",
          paste("PDF introuvable :", pdf_file)
        )
      )
    }

    tagList(
      tags$iframe(
        src = pdf_src,
        class = "pdf-frame"
      ),
      tags$p(
        tags$a(
          href = pdf_src,
          target = "_blank",
          rel = "noopener noreferrer",
          "Ouvrir le PDF dans un nouvel onglet"
        )
      )
    )
  })

  observeEvent(input$valider, {
    id <- selected_case()
    req(id)

    x <- cases[[id]]

    if (id %in% etat$unlocked) {
      showNotification("Cette case est déjà déverrouillée.", type = "message")
      return()
    }

    if (check_answer(x, input$reponse, envir = .GlobalEnv)) {

      # 1. Déverrouiller la case
      etat$unlocked <- union(etat$unlocked, id)

      # 2. Effacer la sortie console précédente
      etat$output_text <- paste0(
        "Case déverrouillée : ", x$titre, "\n\n",
        "Le code de cette case est maintenant disponible.\n",
        "Cliquez sur Exécuter pour produire la sortie console correspondante."
      )

      # 3. Effacer aussi l'ancien graphique éventuel
      etat$plot_file <- blank_plot
      etat$last_has_plot <- FALSE

      # 4. Forcer le rafraîchissement des blocs réactifs
      etat$run_id <- etat$run_id + 1

      # 5. Mettre à jour le plateau et sauvegarder
      update_plateau_nodes()
      save_current_session(silent = TRUE)

      showNotification("Bonne réponse : case déverrouillée.", type = "message")

    } else {
      showNotification("Réponse incorrecte.", type = "error")
    }
  })

  # ----------------------------------------------------------
  # Code affiché
  # ----------------------------------------------------------

  output$code_affiche <- renderText({
    id <- selected_case()
    req(id)

    if (!id %in% etat$unlocked) {
      return("Code verrouillé.")
    }

    get_case_display_code(cases[[id]])
  })

  observeEvent(input$ouvrir_code, {
    id <- selected_case()
    req(id)

    if (!id %in% etat$unlocked) {
      showNotification("La case est encore verrouillée.", type = "error")
      return()
    }

    code <- get_case_display_code(cases[[id]])

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::documentNew(text = code, type = "r")
    } else {
      showNotification("RStudio API non disponible.", type = "warning")
    }
  })

  # ----------------------------------------------------------
  # Exécution d'une case
  # ----------------------------------------------------------

  observeEvent(input$executer, {
    id <- selected_case()
    req(id)

    if (isTRUE(etat$running)) {
      showNotification("Une case est déjà en cours d'exécution.", type = "warning")
      return()
    }

    if (!id %in% etat$unlocked) {
      showNotification("La case est encore verrouillée.", type = "error")
      return()
    }

    etat$running <- TRUE
    on.exit({ etat$running <- FALSE }, add = TRUE)

    x <- cases[[id]]

    withProgress(message = paste("Exécution :", x$titre), value = 0.2, {
      res <- execute_case(x, envir = .GlobalEnv, blank_plot = blank_plot)
      incProgress(0.6)

      etat$output_text <- res$output
      etat$plot_file <- res$plot_file
      etat$last_has_plot <- res$has_plot
      etat$visited <- union(etat$visited, id)
      etat$run_id <- etat$run_id + 1

      if (!is.null(x$pdf_on_run)) {
        etat$current_pdf <- x$pdf_on_run
        etat$current_pdf_case <- id
      }

      update_plateau_nodes()
      save_current_session(silent = TRUE)
      incProgress(0.2)
    })

    showNotification("Case exécutée et sauvegardée.", type = "message")
  })

  # ----------------------------------------------------------
  # Sorties texte et graphiques
  # ----------------------------------------------------------

  output$sortie <- renderText({
    if (is.null(etat$output_text) || !nzchar(etat$output_text)) {
      return(
        paste(
          "Aucune sortie console pour cette case pour le moment.",
          "Cliquez sur Exécuter pour produire la sortie correspondante.",
          sep = "\n"
        )
      )
    }

    etat$output_text
  })

  output$plot_status <- renderUI({
    etat$run_id

    if (isTRUE(etat$last_has_plot)) {
      p("Graphique généré par la dernière case exécutée.")
    } else {
      p("Aucun graphique généré par la dernière case exécutée.")
    }
  })

  output$graphique <- renderImage({
    etat$run_id

    list(
      src = etat$plot_file,
      contentType = "image/png",
      width = "100%"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
