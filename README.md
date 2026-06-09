# SeRiouS

**SeRiouS** est un package R pédagogique qui propose un tutoriel interactif sous forme de plateau de jeu Shiny.

Le tutoriel accompagne progressivement les apprenants depuis des analyses statistiques explicites jusqu’à l’interprétation de structures plus latentes et textuelles. Il met en scène un workflow allant de **FactoMineR** à **EnTraineR** et **NaileR**, autour d’un même jeu de données de questionnaire alimentaire.

## Objectif pédagogique

L’objectif de **SeRiouS** est d’aider les apprenants à comprendre comment passer :

1. d’une analyse statistique classique ;
2. à la récupération structurée des sorties R ;
3. à la construction de prompts contrôlés ;
4. à l’utilisation de fonctions d’aide à l’interprétation ;
5. puis à l’interprétation de classes latentes et de verbatims.

Le fil conducteur du tutoriel est le suivant :

```text
Statistiques explicites
→ sorties R
→ prompts
→ EnTraineR
→ condes() / catdes()
→ NaileR
→ ACP / HCPC
→ classes latentes
→ analyse textuelle contextualisée
```

## Contenu du tutoriel

Le plateau interactif guide l’apprenant à travers plusieurs étapes :

* découverte d’un questionnaire alimentaire simulé ;
* exploration de variables quantitatives, qualitatives et textuelles ;
* régression linéaire avec `FactoMineR::LinearModel()` ;
* analyse de variance avec `FactoMineR::AovSum()` ;
* récupération des sorties statistiques dans des objets R ;
* construction manuelle de prompts ;
* introduction à **EnTraineR** ;
* passage d’analyses simples à des analyses systématiques ;
* utilisation de `condes()` et `catdes()` dans **FactoMineR** ;
* introduction à **NaileR** ;
* construction d’une typologie par ACP et HCPC ;
* description statistique des classes ;
* préparation de verbatims par classe ;
* synthèse contextualisée à partir d’artefacts textuels et structurés.

## Jeu de données inclus

Le package inclut un jeu de données simulé :

```r
questionnaire_alimentaire_typologie_textes
```

Ce jeu de données contient un questionnaire alimentaire fictif avec :

* des variables quantitatives d’évaluation du produit ;
* des variables décrivant le rapport à l’alimentation ;
* des variables qualitatives de contexte ;
* une variable textuelle de commentaire libre ;
* des variables actives permettant de construire une typologie par ACP et classification.

Le jeu de données est utilisé comme fil rouge dans tout le tutoriel.

## Installation

Le package peut être installé depuis GitHub avec :

```r
install.packages("remotes")
remotes::install_github("Sebastien-Le/SeRiouS")
```

## Lancer le tutoriel

Après installation :

```r
library(SeRiouS)

run_plateau()
```

L’application Shiny s’ouvre alors dans le navigateur.

## Dépendances principales

Le package utilise notamment :

* `shiny`
* `visNetwork`
* `FactoMineR`

Les packages suivants peuvent être utilisés dans certaines étapes du tutoriel :

* `EnTraineR`
* `NaileR`

Selon l’installation locale, certaines étapes utilisant **EnTraineR** ou **NaileR** peuvent être présentées comme démonstrations ou exécutées si les packages sont disponibles.

## Organisation du package

La structure principale du package est la suivante :

```text
SeRiouS/
├── R/
│   ├── run_plateau.R
│   └── data.R
├── data/
│   └── questionnaire_alimentaire_typologie_textes.rda
├── inst/
│   └── app/
│       ├── app.R
│       └── www/
│           ├── styles.css
│           └── fichiers PDF de support
├── data-raw/
│   └── scripts de génération des données
└── DESCRIPTION
```

L’application Shiny est située dans :

```text
inst/app/app.R
```

Les fichiers statiques utilisés par l’application, comme le CSS ou les PDF, sont placés dans :

```text
inst/app/www/
```

## Utilisation en atelier

Le tutoriel est conçu pour être utilisé en séance projetée ou en autonomie guidée.

Chaque case du plateau contient :

* un objectif pédagogique ;
* une question de déverrouillage ;
* un bloc de code R ;
* une sortie console ;
* parfois un graphique ;
* parfois un support PDF ;
* une transition vers l’étape suivante.

Le système de progression permet de découvrir le code progressivement, au lieu de fournir un script complet dès le départ.

## Philosophie pédagogique

**SeRiouS** repose sur une idée centrale : les modèles de langage peuvent aider à interpréter des résultats statistiques, mais ils ne doivent pas être utilisés comme des générateurs isolés de réponses.

Le tutoriel insiste donc sur les objets intermédiaires :

```text
résultats statistiques
→ sorties récupérées
→ textes capturés
→ prompts contrôlés
→ artefacts structurés
→ interprétation contextualisée
```

Ces objets rendent le workflow plus visible, plus inspectable et plus contrôlable.

## Développement

Pendant le développement de l’application, il est souvent plus simple de lancer directement :

```r
shiny::runApp("inst/app")
```

Pour tester le package comme un utilisateur final :

```r
devtools::load_all()
run_plateau()
```

ou après installation locale :

```r
devtools::install()
library(SeRiouS)
run_plateau()
```

Avant diffusion :

```r
devtools::document()
devtools::check()
pkgbuild::build()
```

## Auteur

Sébastien Lê
Institut Agro Rennes-Angers

## Licence

Ce package est distribué sous licence MIT.
