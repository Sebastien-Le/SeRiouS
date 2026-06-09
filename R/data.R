#' Questionnaire alimentaire simulé pour le tutoriel Plateau NaileR
#'
#' Jeu de données simulé contenant des variables quantitatives,
#' qualitatives et textuelles. Il sert de fil rouge au tutoriel :
#' régression, ANOVA, condes/catdes, prompts, NaileR,
#' ACP, classification et analyse textuelle.
#'
#' @format Un data.frame avec 240 lignes et plusieurs variables :
#' \describe{
#'   \item{id}{Identifiant du répondant.}
#'   \item{satisfaction}{Score de satisfaction.}
#'   \item{intention_achat}{Score d'intention d'achat.}
#'   \item{prix_percu}{Perception du prix.}
#'   \item{naturalite}{Perception de naturalité.}
#'   \item{type_produit}{Type de produit évalué : standard, local ou bio.}
#'   \item{profil_alim}{Profil alimentaire explicite.}
#'   \item{commentaire}{Réponse textuelle libre.}
#' }
#'
#' @source Données simulées pour un tutoriel pédagogique.
"questionnaire_alimentaire_typologie_textes"
