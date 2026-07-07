# SeRiouS <img src="man/figures/logo.png" align="right" height="139" />

**SeRiouS** is an R package for creating, running, validating, and sharing
interactive learning capsules built with **Shiny**.

A SeRiouS capsule is a small pedagogical application organized as a
board-game-like sequence of **cells** connected by **links**. Each cell may
contain a learning objective, explanatory text, a question to unlock the next
cell, R code, console output, plots, and additional resources such as PDF files.

> SeRiouS provides the stage.  
> The user writes the play.

## Overview

SeRiouS is designed for teachers, trainers, and researchers who want to build
progressive, interactive learning activities in R.

The package provides tools to:

- create shareable capsule folders;
- define pedagogical cells;
- connect cells into a learning path;
- lock and unlock cells with questions;
- execute R code inside a controlled tutorial environment;
- display console output, plots, and PDF resources;
- validate capsule structure before distribution;
- run capsules locally as Shiny applications;
- export simple capsules as standalone Shiny applications;
- optionally export compatible capsules to `shinylive`.

SeRiouS is not tied to a single tutorial. It can be used to build different
types of learning capsules, from small demonstrations to longer training
workflows.

## Installation

You can install the development version from GitHub with:

```r
install.packages("remotes")
remotes::install_github("Sebastien-Le/SeRiouS")
```

Then load the package:

```r
library(SeRiouS)
```

## Run a capsule

The main function for launching a capsule is:

```r
run_capsule()
```

It can run an internal capsule:

```r
run_capsule("demo_iris")
```

or:

```r
run_capsule("demo_pca")
```

You can list available internal capsules with:

```r
available_capsules()
```

A capsule can also be retrieved as an R object:

```r
capsule <- get_capsule("demo_iris")
```

and then run:

```r
run_capsule(capsule)
```

The application opens in a browser and displays a board of learning cells.
Only the first cell is unlocked at the beginning. Locked cells can be unlocked
by answering their associated questions. Running a cell executes its R code and
displays the corresponding output.

## Capsule folders

The recommended way to create a new SeRiouS capsule is to create a capsule
folder.

```r
create_capsule_skeleton("my_capsule")
```

A capsule folder contains:

```text
my_capsule/
  serious.yml
  cells/
  data/
  pdf/
  img/
  www/
```

The main files are:

- `serious.yml`: global metadata, sections, colors, packages, and start cell;
- `cells/`: one YAML file per pedagogical cell;
- `data/`: optional datasets;
- `pdf/`: optional PDF resources;
- `img/`: optional images;
- `www/`: optional web assets such as CSS.

A SeRiouS capsule is therefore a shareable folder. It can be edited, checked,
zipped, sent to another user, or stored in a version-controlled repository.

## Inspect a capsule

You can inspect the cells in a capsule folder:

```r
capsule_cells("my_capsule")
```

You can retrieve one cell:

```r
capsule_get_cell("my_capsule", "intro")
```

You can check that the capsule is valid:

```r
check_capsule_dir("my_capsule")
```

and run it:

```r
run_capsule("my_capsule")
```

## Add and connect cells

A cell is a pedagogical unit displayed on the board.

You can add a new cell with:

```r
capsule_add_cell(
  path = "my_capsule",
  id = "new_cell",
  title = "A new cell",
  section = "summary",
  x = 1000,
  y = 0,
  objective = "Introduce a new concept.",
  text = "This cell explains a new idea before running code.",
  code = "1 + 1",
  outputs = "console",
  expected_output = "The result is 2.",
  question = "What is 1 + 1?",
  expected_answer = "2"
)
```

You can connect it to an existing cell:

```r
capsule_connect_cells(
  path = "my_capsule",
  from = "conclusion",
  to = "new_cell"
)
```

You can move a cell on the board:

```r
capsule_move_cell(
  path = "my_capsule",
  id = "new_cell",
  x = 1250,
  y = 0
)
```

You can update the unlocking question:

```r
capsule_set_cell_unlock(
  path = "my_capsule",
  id = "new_cell",
  question = "What is the result?",
  expected_answer = "2"
)
```

After editing the capsule, check it again:

```r
check_capsule_dir("my_capsule")
```

and run it:

```r
run_capsule("my_capsule")
```

## Cell-based model

SeRiouS is built around a simple model:

```text
capsule = metadata + board

board = cells + links + sections

cell = pedagogical content + code + output + unlocking rule

link = connection from one cell to another
```

This model makes capsules easy to inspect and edit. Each cell is stored as a
separate YAML file in the `cells/` folder. The links between cells define the
learning path.

## Advanced programmatic API

SeRiouS also provides a lower-level programmatic API for users who prefer to
build capsules directly in R.

A cell can be defined as a step:

```r
step1 <- make_step(
  id = "intro",
  title = "Inspect the data",
  section = "data",
  objective = "Display the first rows of the iris data set.",
  text = "We start by inspecting the iris data set.",
  code = "head(iris)",
  expected_output = "The first rows of the iris data set.",
  outputs = "console",
  question = "What is the name of the data set?",
  expected_answer = "iris"
)

step2 <- make_step(
  id = "summary",
  title = "Summarize the data",
  section = "summary",
  objective = "Compute a summary of the iris data set.",
  text = "We now compute a statistical summary.",
  code = "summary(iris)",
  expected_output = "A statistical summary of the variables.",
  outputs = "console"
)
```

Then a capsule can be built:

```r
capsule <- build_linear_capsule(
  id = "my_capsule",
  title = "My first SeRiouS capsule",
  method = "demo",
  steps = list(step1, step2),
  start_step = "intro",
  data = list(iris = iris)
)
```

The capsule can be validated and run:

```r
validate_capsule(capsule)
run_capsule(capsule)
```

This API is useful for package developers or for generating capsules
programmatically. For most users, the recommended workflow is to use capsule
folders with `serious.yml` and `cells/*.yml`.

## Export a capsule as a standalone Shiny app

A capsule can be written as a regular Shiny application:

```r
write_capsule_app(
  capsule = "demo_iris",
  appdir = "app_demo_iris",
  overwrite = TRUE
)
```

It can also be exported as a standalone app that does not require loading
SeRiouS at runtime:

```r
write_capsule_app(
  capsule = "demo_iris",
  appdir = "app_demo_iris_standalone",
  overwrite = TRUE,
  standalone = TRUE
)
```

The standalone export writes the capsule, the required runtime helpers, and a
self-contained `app.R` file.

## Optional shinylive export

For simple capsules compatible with WebAssembly constraints, SeRiouS can export
a standalone app to `shinylive`:

```r
export_capsule_shinylive(
  capsule = "demo_iris",
  destdir = "shinylive_demo_iris",
  appdir = "app_demo_iris_intermediate",
  overwrite = TRUE,
  standalone = TRUE
)
```

This feature is optional and experimental. Not all R packages or capsule code
are compatible with `shinylive`.

## Pedagogical principles

SeRiouS is built around progressive disclosure.

Instead of giving learners a complete script from the beginning, a capsule can
guide them step by step:

```text
challenge
→ question
→ unlocked code
→ execution
→ output
→ interpretation
→ next cell
```

This structure is useful when teaching data analysis, statistics, R
programming, reproducible workflows, or AI-assisted interpretation.

The board-game-like interface makes the learning path explicit. Learners see
where they are, what is already completed, and which cells remain locked.

## Included capsules

SeRiouS includes internal demonstration capsules:

```r
available_capsules()
```

Examples include:

- `demo_iris`: a minimal capsule based on the iris data set;
- `demo_pca`: a capsule introducing Principal Component Analysis with
  FactoMineR;
- `taidyverse`: a guided workflow from statistical outputs to controlled
  LLM-based interpretation.

The internal demonstrations are intended both as examples for learners and as
templates for capsule authors.

## Included data

The package includes teaching data used by internal demonstration capsules,
including a small food questionnaire data set used by the `taidyverse` capsule.

```r
data("questionnaire_alimentaire_typologie_textes")
```

## Development status

SeRiouS is under active development. The current version focuses on:

- local Shiny execution;
- internal and external capsules;
- cell-based capsule folders;
- capsule validation;
- standalone Shiny export;
- optional `shinylive` export for simple capsules.

The API may still evolve, but the recommended user-facing workflow is now based
on shareable capsule folders composed of `serious.yml` and `cells/*.yml`.

## License

SeRiouS is distributed under the license specified in the `DESCRIPTION` file.
