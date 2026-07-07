# Demo iris

This folder contains a SeRiouS learning capsule.

## What is inside?

- `serious.yml`: capsule metadata, sections, legend and global settings.
- `cells/`: one YAML file per board cell.
- `data/`: optional datasets.
- `pdf/`: optional PDF resources.
- `img/`: optional images.
- `www/`: optional web resources and custom CSS.

## How to run this capsule

From R, run:

```r
library(SeRiouS)
run_capsule_dir(".")
```

Or, from the parent directory:

```r
library(SeRiouS)
run_capsule_dir("demo_iris")
```

## How to inspect the cells

```r
library(SeRiouS)
capsule_cells(".")
capsule_get_cell(".", "intro")
```

## How to edit this capsule

You can edit the YAML files in `cells/` directly, or use helper functions:

```r
library(SeRiouS)

capsule_add_cell(
  ".",
  id = "new_cell",
  title = "A new cell",
  section = "summary",
  x = 1000,
  y = 0,
  content = "Write your pedagogical content here.",
  code = "1 + 1",
  question = "What is 1 + 1?",
  expected_answer = "2"
)

capsule_connect_cells(
  ".",
  from = "conclusion",
  to = "new_cell"
)
```

## How to check before sharing

```r
library(SeRiouS)
check_capsule_dir(".")
```

If the check is successful, you can zip this folder and share it.

## Important convention

A SeRiouS capsule is composed of cells connected by links.
Each cell is stored as a YAML file in `cells/`.
