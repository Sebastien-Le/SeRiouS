# data-raw/build_taidyverse_capsule.R
# Build the English, cell-based internal taidyverse capsule for SeRiouS.
#
# This script creates:
#
# inst/capsules/taidyverse/
#   serious.yml
#   cells/
#   data/
#   pdf/
#   img/
#   www/
#
# Important implementation note:
# large code blocks are written with R raw strings r"( ... )".
# This avoids parser errors caused by nested quotes such as install.packages("FactoMineR").

if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  try(devtools::load_all("."), silent = TRUE)
}

if (!requireNamespace("SeRiouS", quietly = TRUE)) {
  stop(
    "The SeRiouS package must be available before running this script.",
    call. = FALSE
  )
}

capsule_dir <- file.path("inst", "capsules", "taidyverse")

if (dir.exists(capsule_dir)) {
  unlink(capsule_dir, recursive = TRUE, force = TRUE)
}

dir.create(capsule_dir, recursive = TRUE, showWarnings = FALSE)

for (dir_name in c("cells", "data", "pdf", "img", "www")) {
  dir.create(file.path(capsule_dir, dir_name), recursive = TRUE, showWarnings = FALSE)
}

writeLines(
  c(
    "id: taidyverse",
    'title: "SeRiouS: Playing with Data Seriously"',
    'subtitle: "Episode One: From statistical outputs to controlled prompts"',
    'method: "tAIdyverse"',
    'description: "A guided workflow from statistical outputs to controlled LLM-based interpretation."',
    "language: en",
    "type: serious_capsule",
    "version: 1.0.0",
    "start_cell: data_start",
    "",
    "packages:",
    "  - SeRiouS",
    "  - FactoMineR",
    "  - graphics",
    "  - stats",
    "  - utils",
    "",
    "sections:",
    "  - id: data",
    '    label: "Data"',
    '    color: "#E3F2FD"',
    '    border: "#1565C0"',
    "  - id: statistics",
    '    label: "Statistics"',
    '    color: "#E8F5E9"',
    '    border: "#2E7D32"',
    "  - id: r_outputs",
    '    label: "R outputs"',
    '    color: "#FFF9C4"',
    '    border: "#F9A825"',
    "  - id: prompting",
    '    label: "Prompting"',
    '    color: "#FCE4EC"',
    '    border: "#AD1457"',
    "  - id: entrainer",
    '    label: "EnTraineR"',
    '    color: "#FFE0B2"',
    '    border: "#EF6C00"',
    "  - id: nailer",
    '    label: "NaileR"',
    '    color: "#E1BEE7"',
    '    border: "#6A1B9A"',
    "  - id: latent",
    '    label: "Latent structures"',
    '    color: "#D1C4E9"',
    '    border: "#512DA8"',
    "  - id: text",
    '    label: "Textual data"',
    '    color: "#B2DFDB"',
    '    border: "#00796B"',
    "  - id: synthesis",
    '    label: "Synthesis"',
    '    color: "#CFD8DC"',
    '    border: "#455A64"'
  ),
  file.path(capsule_dir, "serious.yml")
)

writeLines(
  c(
    "# SeRiouS: Playing with Data Seriously",
    "",
    "This folder contains the English cell-based taidyverse capsule.",
    "",
    "Run it with:",
    "",
    "```r",
    "library(SeRiouS)",
    "run_capsule(\".\")",
    "```",
    "",
    "or, from the package root:",
    "",
    "```r",
    "run_capsule(\"inst/capsules/taidyverse\")",
    "```"
  ),
  file.path(capsule_dir, "README.md")
)

add_cell <- function(id,
                     title,
                     section,
                     x,
                     y,
                     objective,
                     text,
                     code,
                     code_display = NULL,
                     outputs = "console",
                     expected_output,
                     concepts = character(),
                     question,
                     expected_answer,
                     next_cells = character()) {
  SeRiouS::capsule_add_cell(
    path = capsule_dir,
    id = id,
    title = title,
    section = section,
    x = x,
    y = y,
    objective = objective,
    text = text,
    code = code,
    outputs = outputs,
    expected_output = expected_output,
    concepts = concepts,
    question = question,
    expected_answer = expected_answer,
    next_cells = next_cells,
    overwrite = TRUE
  )

  if (!is.null(code_display)) {
    SeRiouS::capsule_update_cell(
      path = capsule_dir,
      id = id,
      code_display = code_display
    )
  }

  invisible(id)
}

add_cell(
  id = "data_start",
  title = "Data: the starting point",
  section = "data",
  x = 0,
  y = 0,
  objective = "Load and inspect the questionnaire used throughout the capsule.",
  text = r"(This capsule follows a single dataset: a food questionnaire with quantitative variables, qualitative variables and free-text comments.

The aim is to move progressively from explicit statistical outputs to structured prompts, and then to latent classes and textual interpretation.)",
  code = r"(
if (!exists('questionnaire')) {
  if (exists('questionnaire_alimentaire_typologie_textes')) {
    questionnaire <- questionnaire_alimentaire_typologie_textes
  } else {
    questionnaire <- SeRiouS::questionnaire_alimentaire_typologie_textes
  }
}

qualitative_variables <- c(
  'type_produit',
  'budget_contraint',
  'sexe',
  'age_classe',
  'lieu_achat',
  'profil_alim'
)

qualitative_variables <- intersect(qualitative_variables, names(questionnaire))
questionnaire[qualitative_variables] <- lapply(questionnaire[qualitative_variables], factor)

if ('commentaire' %in% names(questionnaire)) {
  questionnaire$commentaire <- as.character(questionnaire$commentaire)
}

structure_variables <- data.frame(
  variable = names(questionnaire),
  class = vapply(questionnaire, function(x) class(x)[1], character(1)),
  stringsAsFactors = FALSE
)

key_variables <- data.frame(
  step = c(
    'Linear model',
    'ANOVA',
    'condes()',
    'catdes()',
    'PCA + HCPC',
    'Textual workflow'
  ),
  variables = c(
    'intention_achat ~ satisfaction + prix_percu',
    'satisfaction ~ type_produit * budget_contraint',
    'intention_achat described by the other variables',
    'profil_alim described by the other variables',
    'active typology variables',
    'classe_hcpc + commentaire'
  ),
  purpose = c(
    'model a purchase intention',
    'test factor effects and interaction',
    'describe a quantitative variable',
    'describe a qualitative variable',
    'construct latent classes',
    'interpret classes with verbatims'
  ),
  stringsAsFactors = FALSE
)

cat('\nDataset object: questionnaire\n')
cat('Rows:', nrow(questionnaire), '\n')
cat('Columns:', ncol(questionnaire), '\n\n')

cat('Variable structure:\n')
print(structure_variables, row.names = FALSE)

cat('\nKey variables for the capsule:\n')
print(key_variables, row.names = FALSE)

cat('\nFirst rows:\n')
print(head(questionnaire, 3))

if ('commentaire' %in% names(questionnaire)) {
  cat('\nExamples of free-text comments:\n')
  cat(paste('-', head(unique(questionnaire$commentaire), 6), collapse = '\n'))
  cat('\n')
}
)",
  code_display = r"(
if (!exists('questionnaire')) {
  questionnaire <- SeRiouS::questionnaire_alimentaire_typologie_textes
}

qualitative_variables <- c(
  'type_produit',
  'budget_contraint',
  'sexe',
  'age_classe',
  'lieu_achat',
  'profil_alim'
)

qualitative_variables <- intersect(qualitative_variables, names(questionnaire))
questionnaire[qualitative_variables] <- lapply(questionnaire[qualitative_variables], factor)

structure_variables <- data.frame(
  variable = names(questionnaire),
  class = vapply(questionnaire, function(x) class(x)[1], character(1)),
  stringsAsFactors = FALSE
)

nrow(questionnaire)
ncol(questionnaire)
structure_variables
head(questionnaire, 3)
head(unique(questionnaire$commentaire), 6)
)",
  expected_output = "The questionnaire is loaded and its structure is displayed.",
  concepts = c("dataset", "variable roles", "questionnaire"),
  question = "Which R object contains the questionnaire used in this capsule?",
  expected_answer = "questionnaire",
  next_cells = "first_exploration"
)

add_cell(
  id = "first_exploration",
  title = "First exploration",
  section = "data",
  x = 260,
  y = 0,
  objective = "Inspect quantitative and qualitative variables before modelling.",
  text = r"(Before fitting models, we inspect the data. The goal is to identify numeric variables, factor variables, and the textual column. This step also produces simple exploratory plots.)",
  outputs = c("console", "plot"),
  code = r"(
if (!exists('questionnaire')) {
  stop('The object questionnaire is missing. Run the first cell first.', call. = FALSE)
}

quantitative_variables <- setdiff(
  names(questionnaire)[vapply(questionnaire, is.numeric, logical(1))],
  'id'
)

quantitative_summary <- data.frame(
  variable = quantitative_variables,
  min = vapply(questionnaire[quantitative_variables], min, numeric(1), na.rm = TRUE),
  q1 = vapply(questionnaire[quantitative_variables], function(x) quantile(x, 0.25, na.rm = TRUE), numeric(1)),
  median = vapply(questionnaire[quantitative_variables], median, numeric(1), na.rm = TRUE),
  mean = vapply(questionnaire[quantitative_variables], mean, numeric(1), na.rm = TRUE),
  q3 = vapply(questionnaire[quantitative_variables], function(x) quantile(x, 0.75, na.rm = TRUE), numeric(1)),
  max = vapply(questionnaire[quantitative_variables], max, numeric(1), na.rm = TRUE),
  row.names = NULL
)

quantitative_summary[, -1] <- round(quantitative_summary[, -1], 2)

qualitative_variables_exploration <- names(questionnaire)[
  vapply(questionnaire, is.factor, logical(1))
]

qualitative_summary <- do.call(
  rbind,
  lapply(qualitative_variables_exploration, function(v) {
    tab <- table(questionnaire[[v]], useNA = 'ifany')
    data.frame(
      variable = v,
      level = names(tab),
      n = as.integer(tab),
      row.names = NULL
    )
  })
)

cat('\nQuantitative variables:\n')
print(quantitative_variables)

cat('\nQuantitative summary:\n')
print(quantitative_summary, row.names = FALSE)

cat('\nQualitative variables:\n')
print(qualitative_variables_exploration)

cat('\nQualitative summary:\n')
print(qualitative_summary, row.names = FALSE)

cat('\nText column: commentaire\n')
cat('Number of comments:', length(questionnaire$commentaire), '\n')
cat('Number of unique comments:', length(unique(questionnaire$commentaire)), '\n')

op <- par(mfrow = c(1, 2))

hist(
  questionnaire$intention_achat,
  main = 'Purchase intention',
  xlab = 'Score',
  col = 'grey80',
  border = 'white'
)

boxplot(
  satisfaction ~ type_produit,
  data = questionnaire,
  main = 'Satisfaction by product type',
  xlab = 'Product type',
  ylab = 'Satisfaction',
  col = 'grey85'
)

par(op)
)",
  code_display = r"(
quantitative_variables <- setdiff(
  names(questionnaire)[vapply(questionnaire, is.numeric, logical(1))],
  'id'
)

quantitative_variables

quantitative_summary <- data.frame(
  variable = quantitative_variables,
  mean = vapply(questionnaire[quantitative_variables], mean, numeric(1), na.rm = TRUE),
  median = vapply(questionnaire[quantitative_variables], median, numeric(1), na.rm = TRUE),
  row.names = NULL
)

quantitative_summary

qualitative_variables_exploration <- names(questionnaire)[
  vapply(questionnaire, is.factor, logical(1))
]

qualitative_variables_exploration

op <- par(mfrow = c(1, 2))

hist(
  questionnaire$intention_achat,
  main = 'Purchase intention',
  xlab = 'Score',
  col = 'grey80',
  border = 'white'
)

boxplot(
  satisfaction ~ type_produit,
  data = questionnaire,
  main = 'Satisfaction by product type',
  xlab = 'Product type',
  ylab = 'Satisfaction',
  col = 'grey85'
)

par(op)
)",
  expected_output = "Quantitative and qualitative summaries, plus two exploratory plots.",
  concepts = c("exploration", "summary statistics", "graphics"),
  question = "Which variable contains the free-text comments?",
  expected_answer = "commentaire",
  next_cells = "linear_model"
)

add_cell(
  id = "linear_model",
  title = "Linear modelling",
  section = "statistics",
  x = 520,
  y = 0,
  objective = "Fit a linear model with FactoMineR::LinearModel().",
  text = r"(We first model a quantitative response: purchase intention. The predictors are satisfaction and perceived price. The point is not to claim causality, but to produce explicit statistical outputs.)",
  outputs = c("console", "plot"),
  code = r"(
if (!exists('questionnaire')) {
  stop('The object questionnaire is missing. Run the first cell first.', call. = FALSE)
}

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

formula_lm <- intention_achat ~ satisfaction + prix_percu

res_lm_fm <- FactoMineR::LinearModel(
  formula_lm,
  data = questionnaire,
  selection = 'none'
)

cat('\nLinear model formula:\n')
print(formula_lm)

cat('\nGlobal tests: res_lm_fm$Ftest\n')
print(res_lm_fm$Ftest)

cat('\nModel coefficients: res_lm_fm$Ttest\n')
print(res_lm_fm$Ttest)

plot(
  questionnaire$satisfaction,
  questionnaire$intention_achat,
  pch = 16,
  col = 'grey40',
  xlab = 'Satisfaction',
  ylab = 'Purchase intention',
  main = 'Purchase intention ~ satisfaction'
)

abline(
  lm(intention_achat ~ satisfaction, data = questionnaire),
  lwd = 2
)
)",
  code_display = r"(
formula_lm <- intention_achat ~ satisfaction + prix_percu

res_lm_fm <- FactoMineR::LinearModel(
  formula_lm,
  data = questionnaire,
  selection = 'none'
)

res_lm_fm$Ftest
res_lm_fm$Ttest

plot(
  questionnaire$satisfaction,
  questionnaire$intention_achat,
  pch = 16,
  col = 'grey40',
  xlab = 'Satisfaction',
  ylab = 'Purchase intention',
  main = 'Purchase intention ~ satisfaction'
)

abline(
  lm(intention_achat ~ satisfaction, data = questionnaire),
  lwd = 2
)
)",
  expected_output = "A FactoMineR LinearModel object named res_lm_fm and its main outputs.",
  concepts = c("linear model", "F-test", "T-test"),
  question = "Which FactoMineR function is used for the linear model?",
  expected_answer = "LinearModel",
  next_cells = "anova_model"
)

add_cell(
  id = "anova_model",
  title = "Analysis of variance",
  section = "statistics",
  x = 780,
  y = 0,
  objective = "Fit an ANOVA model with FactoMineR::AovSum().",
  text = r"(We now explain satisfaction using two qualitative factors and their interaction. This creates another structured statistical output that will later be transformed into text and prompts.)",
  outputs = c("console", "plot"),
  code = r"(
if (!exists('questionnaire')) {
  stop('The object questionnaire is missing. Run the first cell first.', call. = FALSE)
}

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

formula_aov <- satisfaction ~ type_produit * budget_contraint

res_aovsum <- FactoMineR::AovSum(
  formula_aov,
  data = questionnaire
)

cat('\nANOVA formula:\n')
print(formula_aov)

cat('\nGlobal effects and interaction: res_aovsum$Ftest\n')
print(res_aovsum$Ftest)

cat('\nCoefficients and contrasts: res_aovsum$Ttest\n')
print(res_aovsum$Ttest)

interaction.plot(
  x.factor = questionnaire$budget_contraint,
  trace.factor = questionnaire$type_produit,
  response = questionnaire$satisfaction,
  xlab = 'Budget constraint',
  ylab = 'Mean satisfaction',
  trace.label = 'Product',
  main = 'Product x budget interaction'
)
)",
  code_display = r"(
formula_aov <- satisfaction ~ type_produit * budget_contraint

res_aovsum <- FactoMineR::AovSum(
  formula_aov,
  data = questionnaire
)

res_aovsum$Ftest
res_aovsum$Ttest

interaction.plot(
  x.factor = questionnaire$budget_contraint,
  trace.factor = questionnaire$type_produit,
  response = questionnaire$satisfaction,
  xlab = 'Budget constraint',
  ylab = 'Mean satisfaction',
  trace.label = 'Product',
  main = 'Product x budget interaction'
)
)",
  expected_output = "A FactoMineR AovSum object named res_aovsum and its main outputs.",
  concepts = c("ANOVA", "interaction", "factor effects"),
  question = "Which FactoMineR function is used for the analysis of variance?",
  expected_answer = "AovSum",
  next_cells = "capture_outputs"
)

add_cell(
  id = "capture_outputs",
  title = "Capturing statistical outputs",
  section = "r_outputs",
  x = 1040,
  y = 0,
  objective = "Turn printed statistical outputs into R objects and text.",
  text = r"(Statistical outputs are often printed to the console. To use them in a controlled prompt, we first extract the relevant sub-objects and capture full outputs as text.)",
  code = r"(
required_objects <- c('res_lm_fm', 'res_aovsum')

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    'Missing objects: ',
    paste(missing_objects, collapse = ', '),
    '. Run the previous cells first.',
    call. = FALSE
  )
}

cat('\nWhat does res_lm_fm contain?\n')
print(names(res_lm_fm))

cat('\nWhat does res_aovsum contain?\n')
print(names(res_aovsum))

lm_ftest <- res_lm_fm$Ftest
lm_ttest <- res_lm_fm$Ttest
lm_summary <- res_lm_fm$lmResult

aov_ftest <- res_aovsum$Ftest
aov_ttest <- res_aovsum$Ttest

text_linearmodel <- paste(
  capture.output(print(res_lm_fm)),
  collapse = '\n'
)

text_aovsum <- paste(
  capture.output(print(res_aovsum)),
  collapse = '\n'
)

cat('\nExtracted objects:\n')
cat('- lm_ftest\n')
cat('- lm_ttest\n')
cat('- lm_summary\n')
cat('- aov_ftest\n')
cat('- aov_ttest\n')
cat('- text_linearmodel\n')
cat('- text_aovsum\n')

cat('\nPreview of text_linearmodel:\n')
cat(substr(text_linearmodel, 1, 1200))

cat('\n\nPreview of text_aovsum:\n')
cat(substr(text_aovsum, 1, 1200))
)",
  code_display = r"(
names(res_lm_fm)
names(res_aovsum)

lm_ftest <- res_lm_fm$Ftest
lm_ttest <- res_lm_fm$Ttest
lm_summary <- res_lm_fm$lmResult

aov_ftest <- res_aovsum$Ftest
aov_ttest <- res_aovsum$Ttest

text_linearmodel <- paste(
  capture.output(print(res_lm_fm)),
  collapse = '\n'
)

text_aovsum <- paste(
  capture.output(print(res_aovsum)),
  collapse = '\n'
)

substr(text_linearmodel, 1, 1200)
substr(text_aovsum, 1, 1200)
)",
  expected_output = "Extracted statistical tables and captured text outputs.",
  concepts = c("capture.output", "structured output", "R objects"),
  question = "Which object contains the captured text output of AovSum?",
  expected_answer = "text_aovsum",
  next_cells = "manual_prompt"
)

add_cell(
  id = "manual_prompt",
  title = "Building a prompt manually",
  section = "prompting",
  x = 1300,
  y = 0,
  objective = "Build controlled prompts manually from captured statistical outputs.",
  text = r"(A prompt is not just raw statistical output. It combines context, results, instructions, and boundaries. Here we build two prompts by hand using paste().)",
  code = r"(
required_objects <- c('text_linearmodel', 'text_aovsum')

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    'Missing objects: ',
    paste(missing_objects, collapse = ', '),
    '. Run the capture cell first.',
    call. = FALSE
  )
}

prompt_linearmodel <- paste(
  '# Interpretation of a linear model',
  '',
  'Context: consumer questionnaire about a food product.',
  'Response variable: intention_achat.',
  'Predictors: satisfaction and prix_percu.',
  '',
  'FactoMineR::LinearModel output:',
  text_linearmodel,
  '',
  'Instructions:',
  '- interpret only the results provided;',
  '- distinguish global effects from coefficients;',
  '- do not infer causality;',
  '- write a short pedagogical interpretation.',
  sep = '\n'
)

prompt_aovsum <- paste(
  '# Interpretation of an ANOVA with interaction',
  '',
  'Response variable: satisfaction.',
  'Factors: type_produit and budget_contraint.',
  '',
  'FactoMineR::AovSum output:',
  text_aovsum,
  '',
  'Instructions:',
  '- identify significant effects;',
  '- comment on the interaction carefully;',
  '- do not invent post-hoc comparisons.',
  sep = '\n'
)

cat('\nObject created: prompt_linearmodel\n')
cat(substr(prompt_linearmodel, 1, 1500))

cat('\n\nObject created: prompt_aovsum\n')
cat(substr(prompt_aovsum, 1, 1500))

cat('\n\nPrompt lengths:\n')
print(c(
  prompt_linearmodel = nchar(prompt_linearmodel),
  prompt_aovsum = nchar(prompt_aovsum)
))
)",
  code_display = r"(
prompt_linearmodel <- paste(
  '# Interpretation of a linear model',
  '',
  'Context: consumer questionnaire about a food product.',
  'Response variable: intention_achat.',
  'Predictors: satisfaction and prix_percu.',
  '',
  'FactoMineR::LinearModel output:',
  text_linearmodel,
  '',
  'Instructions:',
  '- interpret only the results provided;',
  '- distinguish global effects from coefficients;',
  '- do not infer causality;',
  '- write a short pedagogical interpretation.',
  sep = '\n'
)

prompt_aovsum <- paste(
  '# Interpretation of an ANOVA with interaction',
  '',
  'Response variable: satisfaction.',
  'Factors: type_produit and budget_contraint.',
  '',
  'FactoMineR::AovSum output:',
  text_aovsum,
  '',
  'Instructions:',
  '- identify significant effects;',
  '- comment on the interaction carefully;',
  '- do not invent post-hoc comparisons.',
  sep = '\n'
)

substr(prompt_linearmodel, 1, 1500)
substr(prompt_aovsum, 1, 1500)
)",
  expected_output = "Two manually built prompts: prompt_linearmodel and prompt_aovsum.",
  concepts = c("prompt", "paste", "instructions"),
  question = "Which R function is used here to assemble text lines?",
  expected_answer = "paste",
  next_cells = "generic_prompt"
)

add_cell(
  id = "generic_prompt",
  title = "Making the prompt generic",
  section = "prompting",
  x = 1560,
  y = 0,
  objective = "Avoid hard-coding variable names by extracting information from R formulas.",
  text = r"(The previous prompts contained variable names written by hand. Here, we use R formulas to extract the response variable and model terms automatically.)",
  code = r"(
required_objects <- c('formula_lm', 'formula_aov', 'text_linearmodel', 'text_aovsum')

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    'Missing objects: ',
    paste(missing_objects, collapse = ', '),
    '. Run the previous cells first.',
    call. = FALSE
  )
}

extract_formula_info <- function(formula) {
  variables <- all.vars(formula)
  model_terms <- attr(terms(formula), 'term.labels')

  list(
    formula = deparse(formula),
    y = variables[1],
    terms = model_terms,
    variables = variables
  )
}

info_lm <- extract_formula_info(formula_lm)
info_aov <- extract_formula_info(formula_aov)

prompt_linearmodel_generic <- paste(
  '# Interpretation of a linear model',
  '',
  'Context: consumer questionnaire about a food product.',
  paste0('Response variable: ', info_lm$y, '.'),
  paste0('Predictors: ', paste(info_lm$terms, collapse = ', '), '.'),
  '',
  'FactoMineR::LinearModel output:',
  text_linearmodel,
  '',
  'Instructions:',
  '- interpret only the results provided;',
  '- distinguish global effects from coefficients;',
  '- do not infer causality;',
  '- write a short pedagogical interpretation.',
  sep = '\n'
)

prompt_aovsum_generic <- paste(
  '# Interpretation of an ANOVA',
  '',
  paste0('Response variable: ', info_aov$y, '.'),
  paste0('Model terms: ', paste(info_aov$terms, collapse = ', '), '.'),
  '',
  'FactoMineR::AovSum output:',
  text_aovsum,
  '',
  'Instructions:',
  '- identify significant effects;',
  '- comment on interactions carefully;',
  '- do not invent post-hoc comparisons.',
  sep = '\n'
)

cat('\nFormula information for LinearModel:\n')
print(info_lm)

cat('\nFormula information for AovSum:\n')
print(info_aov)

cat('\nPreview of generic LinearModel prompt:\n')
cat(substr(prompt_linearmodel_generic, 1, 1200))

cat('\n\nPreview of generic AovSum prompt:\n')
cat(substr(prompt_aovsum_generic, 1, 1200))
)",
  code_display = r"(
extract_formula_info <- function(formula) {
  variables <- all.vars(formula)
  model_terms <- attr(terms(formula), 'term.labels')

  list(
    formula = deparse(formula),
    y = variables[1],
    terms = model_terms,
    variables = variables
  )
}

info_lm <- extract_formula_info(formula_lm)
info_aov <- extract_formula_info(formula_aov)

info_lm
info_aov
)",
  expected_output = "Generic prompts based on information extracted from R formulas.",
  concepts = c("formula", "terms", "generic prompt"),
  question = "Which object contains the formula used by LinearModel?",
  expected_answer = "formula_lm",
  next_cells = "entrainer_overview"
)

add_cell(
  id = "entrainer_overview",
  title = "Introducing EnTraineR",
  section = "entrainer",
  x = 1560,
  y = 210,
  objective = "Understand the role of EnTraineR in the workflow.",
  text = r"(Manual prompt construction is useful for learning, but repetitive. EnTraineR is positioned as a package that automates the passage from explicit statistical outputs to controlled prompts. In this capsule, EnTraineR is optional: if it is not installed, the workflow still explains what would happen.)",
  code = r"(
entrainer_available <- requireNamespace('EnTraineR', quietly = TRUE)

if (entrainer_available) {
  entrainer_functions <- grep(
    '^trainer_',
    getNamespaceExports('EnTraineR'),
    value = TRUE
  )
} else {
  entrainer_functions <- c(
    'trainer_linear_model',
    'trainer_LinearModel',
    'trainer_aovsum',
    'trainer_AovSum',
    'trainer_cor',
    'trainer_chisq_test'
  )
}

entrainer_overview <- data.frame(
  question = c(
    'What does EnTraineR receive?',
    'What does EnTraineR produce?',
    'Why is generate = FALSE important?'
  ),
  answer = c(
    'A statistical object or output.',
    'A prompt, and optionally an LLM answer.',
    'It lets the analyst inspect the prompt before any generation.'
  ),
  stringsAsFactors = FALSE
)

cat('\nIs EnTraineR installed?', entrainer_available, '\n')

cat('\nFunctions detected or expected:\n')
print(data.frame(function_name = entrainer_functions), row.names = FALSE)

cat('\nEnTraineR overview:\n')
print(entrainer_overview, row.names = FALSE)
)",
  code_display = r"(
entrainer_available <- requireNamespace('EnTraineR', quietly = TRUE)

if (entrainer_available) {
  entrainer_functions <- grep(
    '^trainer_',
    getNamespaceExports('EnTraineR'),
    value = TRUE
  )
} else {
  entrainer_functions <- c(
    'trainer_linear_model',
    'trainer_LinearModel',
    'trainer_aovsum',
    'trainer_AovSum'
  )
}

entrainer_available
entrainer_functions
)",
  expected_output = "A short overview of EnTraineR and its trainer_* functions.",
  concepts = c("EnTraineR", "prompt automation", "generate = FALSE"),
  question = "Which package automates prompt construction from statistical outputs?",
  expected_answer = "EnTraineR",
  next_cells = "entrainer_prompt"
)

add_cell(
  id = "entrainer_prompt",
  title = "Generating prompts with EnTraineR",
  section = "entrainer",
  x = 1300,
  y = 210,
  objective = "Use EnTraineR to generate a prompt from a LinearModel object when available.",
  text = r"(This cell compares the manual prompt with an automated EnTraineR prompt. The code is written defensively: if EnTraineR is not installed, it keeps the manual prompt as a fallback.)",
  code = r"(
if (!exists('res_lm_fm')) {
  stop('The object res_lm_fm is missing. Run the linear model cell first.', call. = FALSE)
}

entrainer_available <- requireNamespace('EnTraineR', quietly = TRUE)

prompt_used_as_fallback <- if (exists('prompt_linearmodel_generic')) {
  prompt_linearmodel_generic
} else if (exists('prompt_linearmodel')) {
  prompt_linearmodel
} else {
  'No manual prompt found.'
}

if (entrainer_available) {
  entrainer_exports <- getNamespaceExports('EnTraineR')

  trainer_name <- intersect(
    c('trainer_LinearModel', 'trainer_linear_model'),
    entrainer_exports
  )[1]

  if (is.na(trainer_name)) {
    prompt_auto_lm <- 'No LinearModel trainer was found in EnTraineR.'
  } else {
    trainer_lm <- getExportedValue('EnTraineR', trainer_name)

    prompt_auto_lm <- tryCatch(
      trainer_lm(res_lm_fm, generate = FALSE),
      error = function(e) paste('EnTraineR error:', conditionMessage(e))
    )
  }
} else {
  prompt_auto_lm <- paste(
    'EnTraineR is not installed.',
    'Fallback: the manual prompt remains the inspectable object.',
    '',
    prompt_used_as_fallback,
    sep = '\n'
  )
}

prompt_auto_lm_text <- paste(
  capture.output(print(prompt_auto_lm)),
  collapse = '\n'
)

comparison_manual_auto <- data.frame(
  method = c('Manual prompt', 'EnTraineR or fallback'),
  object = c('prompt_linearmodel_generic', 'prompt_auto_lm'),
  characters = c(nchar(prompt_used_as_fallback), nchar(prompt_auto_lm_text)),
  stringsAsFactors = FALSE
)

cat('\nComparison:\n')
print(comparison_manual_auto, row.names = FALSE)

cat('\nPreview of automated or fallback prompt:\n')
cat(substr(prompt_auto_lm_text, 1, 1500))
)",
  code_display = r"(
entrainer_available <- requireNamespace('EnTraineR', quietly = TRUE)

if (entrainer_available) {
  entrainer_exports <- getNamespaceExports('EnTraineR')

  trainer_name <- intersect(
    c('trainer_LinearModel', 'trainer_linear_model'),
    entrainer_exports
  )[1]

  trainer_lm <- getExportedValue('EnTraineR', trainer_name)

  prompt_auto_lm <- trainer_lm(
    res_lm_fm,
    generate = FALSE
  )
} else {
  prompt_auto_lm <- prompt_linearmodel_generic
}

prompt_auto_lm
)",
  expected_output = "An automated prompt when EnTraineR is available, otherwise a safe fallback prompt.",
  concepts = c("EnTraineR", "generate = FALSE", "fallback"),
  question = "Which option lets us inspect the prompt without calling an LLM?",
  expected_answer = "generate = FALSE",
  next_cells = "automate_models"
)

add_cell(
  id = "automate_models",
  title = "Automating Y ~ X analyses",
  section = "r_outputs",
  x = 1040,
  y = 210,
  objective = "Move from a single Y ~ X model to a set of models over several predictors.",
  text = r"(Before using FactoMineR descriptive functions, we reproduce the logic manually. We fit several univariate LinearModel analyses by looping over candidate predictors.)",
  code = r"(
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

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

missing_variables <- setdiff(c(y, variables_x), names(questionnaire))

if (length(missing_variables) > 0) {
  stop(
    'Missing variables: ',
    paste(missing_variables, collapse = ', '),
    call. = FALSE
  )
}

fit_one_model <- function(x) {
  formula_text <- paste(y, '~', x)
  formula <- as.formula(formula_text)

  FactoMineR::LinearModel(
    formula,
    data = questionnaire,
    selection = 'none'
  )
}

univariate_models <- lapply(variables_x, fit_one_model)
names(univariate_models) <- variables_x

univariate_model_texts <- lapply(
  univariate_models,
  function(model) {
    paste(capture.output(print(model)), collapse = '\n')
  }
)

cat('\nResponse variable:', y, '\n')
cat('Predictors:\n')
print(variables_x)

cat('\nNumber of fitted models:', length(univariate_models), '\n')
cat('\nFirst model Ftest:\n')
print(univariate_models[[1]]$Ftest)

cat('\nSecond model Ftest:\n')
print(univariate_models[[2]]$Ftest)

cat('\nPreview of the first captured model output:\n')
cat(substr(univariate_model_texts[[1]], 1, 1200))
)",
  code_display = r"(
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

fit_one_model <- function(x) {
  formula <- as.formula(paste(y, '~', x))

  FactoMineR::LinearModel(
    formula,
    data = questionnaire,
    selection = 'none'
  )
}

univariate_models <- lapply(variables_x, fit_one_model)
names(univariate_models) <- variables_x

names(univariate_models)
univariate_models[[1]]$Ftest
)",
  expected_output = "A list of univariate models and their captured textual outputs.",
  concepts = c("automation", "lapply", "formula construction"),
  question = "Which R function applies the same function to all elements of a vector or list?",
  expected_answer = "lapply",
  next_cells = "condes_description"
)

add_cell(
  id = "condes_description",
  title = "Describing a quantitative variable",
  section = "statistics",
  x = 780,
  y = 210,
  objective = "Use FactoMineR::condes() to describe a quantitative variable.",
  text = r"(condes() generalizes the idea of describing one quantitative variable using all the others. It uses correlations with quantitative variables and comparisons across qualitative variables.)",
  code = r"(
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

questionnaire_desc <- questionnaire[
  ,
  setdiff(names(questionnaire), 'commentaire')
]

y_condes <- 'intention_achat'
num_var_condes <- which(names(questionnaire_desc) == y_condes)

quantitative_predictors_condes <- setdiff(
  names(questionnaire_desc)[vapply(questionnaire_desc, is.numeric, logical(1))],
  c('id', y_condes)
)

correlations_y <- data.frame(
  variable = quantitative_predictors_condes,
  correlation = vapply(
    quantitative_predictors_condes,
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
correlations_y <- correlations_y[order(correlations_y$abs_correlation, decreasing = TRUE), ]
correlations_y$correlation <- round(correlations_y$correlation, 3)
correlations_y$abs_correlation <- round(correlations_y$abs_correlation, 3)

res_condes <- FactoMineR::condes(
  questionnaire_desc,
  num.var = num_var_condes
)

cat('\nVariable described:', y_condes, '\n')
cat('Position in questionnaire_desc:', num_var_condes, '\n')

cat('\nManual correlation overview:\n')
print(correlations_y, row.names = FALSE)

cat('\nNames in res_condes:\n')
print(names(res_condes))

cat('\ncondes() output:\n')
print(res_condes)
)",
  code_display = r"(
questionnaire_desc <- questionnaire[
  ,
  setdiff(names(questionnaire), 'commentaire')
]

y_condes <- 'intention_achat'
num_var_condes <- which(names(questionnaire_desc) == y_condes)

res_condes <- FactoMineR::condes(
  questionnaire_desc,
  num.var = num_var_condes
)

names(res_condes)
res_condes
)",
  expected_output = "A FactoMineR condes object describing intention_achat.",
  concepts = c("condes", "quantitative variable", "description"),
  question = "Which FactoMineR function describes a quantitative variable?",
  expected_answer = "condes",
  next_cells = "catdes_description"
)

add_cell(
  id = "catdes_description",
  title = "Describing groups",
  section = "statistics",
  x = 520,
  y = 210,
  objective = "Use FactoMineR::catdes() to describe a qualitative variable or groups.",
  text = r"(catdes() describes a qualitative variable. It compares quantitative variables across groups and crosses qualitative variables with those groups.)",
  code = r"(
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

if (!exists('questionnaire_desc')) {
  questionnaire_desc <- questionnaire[
    ,
    setdiff(names(questionnaire), 'commentaire')
  ]
}

y_catdes <- 'profil_alim'
num_var_catdes <- which(names(questionnaire_desc) == y_catdes)

variables_quantitative_examples <- c(
  'satisfaction',
  'intention_achat',
  'prix_percu',
  'naturalite',
  'ancrage_local'
)

means_by_profile <- aggregate(
  questionnaire_desc[variables_quantitative_examples],
  by = list(profile = questionnaire_desc[[y_catdes]]),
  FUN = mean
)

means_by_profile[, -1] <- round(means_by_profile[, -1], 2)

profile_product_table <- table(
  questionnaire_desc[[y_catdes]],
  questionnaire_desc$type_produit
)

profile_product_chisq <- chisq.test(profile_product_table)

res_catdes <- FactoMineR::catdes(
  questionnaire_desc,
  num.var = num_var_catdes
)

cat('\nVariable described:', y_catdes, '\n')
cat('Position in questionnaire_desc:', num_var_catdes, '\n')

cat('\nGroup sizes:\n')
print(table(questionnaire_desc[[y_catdes]]))

cat('\nMeans by profile:\n')
print(means_by_profile, row.names = FALSE)

cat('\nProfile x product table:\n')
print(profile_product_table)

cat('\nChi-square test:\n')
print(profile_product_chisq)

cat('\nNames in res_catdes:\n')
print(names(res_catdes))

cat('\ncatdes() output:\n')
print(res_catdes)
)",
  code_display = r"(
y_catdes <- 'profil_alim'
num_var_catdes <- which(names(questionnaire_desc) == y_catdes)

table(questionnaire_desc[[y_catdes]])

res_catdes <- FactoMineR::catdes(
  questionnaire_desc,
  num.var = num_var_catdes
)

names(res_catdes)
res_catdes
)",
  expected_output = "A FactoMineR catdes object describing profil_alim.",
  concepts = c("catdes", "qualitative variable", "groups"),
  question = "Which FactoMineR function describes a qualitative variable or groups?",
  expected_answer = "catdes",
  next_cells = "prompt_condes_catdes"
)

add_cell(
  id = "prompt_condes_catdes",
  title = "Turning condes/catdes outputs into prompts",
  section = "prompting",
  x = 260,
  y = 210,
  objective = "Capture condes() and catdes() outputs and turn them into controlled prompts.",
  text = r"(condes() and catdes() produce richer outputs than the earlier models. We inspect them, capture them as text, and build structured prompts.)",
  code = r"(
required_objects <- c('res_condes', 'res_catdes')

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    'Missing objects: ',
    paste(missing_objects, collapse = ', '),
    '. Run condes and catdes first.',
    call. = FALSE
  )
}

text_condes <- paste(
  capture.output(print(res_condes)),
  collapse = '\n'
)

text_catdes <- paste(
  capture.output(print(res_catdes)),
  collapse = '\n'
)

build_description_prompt <- function(title,
                                     described_variable,
                                     variable_type,
                                     statistical_output,
                                     instructions) {
  paste(
    title,
    '',
    paste0('Described variable: ', described_variable, '.'),
    paste0('Type of described variable: ', variable_type, '.'),
    '',
    'Statistical output:',
    statistical_output,
    '',
    'Instructions:',
    paste0('- ', instructions, collapse = '\n'),
    sep = '\n'
  )
}

instructions_condes <- c(
  'identify the variables most associated with intention_achat',
  'distinguish quantitative and qualitative evidence',
  'avoid causal language',
  'write a short structured synthesis'
)

instructions_catdes <- c(
  'describe each food profile',
  'separate quantitative evidence from qualitative modalities',
  'avoid over-interpreting weak associations',
  'write readable profile summaries'
)

prompt_condes <- build_description_prompt(
  title = '# Interpretation of condes()',
  described_variable = 'intention_achat',
  variable_type = 'quantitative',
  statistical_output = text_condes,
  instructions = instructions_condes
)

prompt_catdes <- build_description_prompt(
  title = '# Interpretation of catdes()',
  described_variable = 'profil_alim',
  variable_type = 'qualitative',
  statistical_output = text_catdes,
  instructions = instructions_catdes
)

cat('\nCaptured text lengths:\n')
print(c(
  text_condes = nchar(text_condes),
  text_catdes = nchar(text_catdes)
))

cat('\nPreview of prompt_condes:\n')
cat(substr(prompt_condes, 1, 1500))

cat('\n\nPreview of prompt_catdes:\n')
cat(substr(prompt_catdes, 1, 1500))
)",
  code_display = r"(
text_condes <- paste(
  capture.output(print(res_condes)),
  collapse = '\n'
)

text_catdes <- paste(
  capture.output(print(res_catdes)),
  collapse = '\n'
)

substr(text_condes, 1, 1200)
substr(text_catdes, 1, 1200)
)",
  expected_output = "Captured condes/catdes texts and two controlled prompts.",
  concepts = c("condes", "catdes", "prompt"),
  question = "Which object contains the captured text output of catdes?",
  expected_answer = "text_catdes",
  next_cells = "nailer_overview"
)

add_cell(
  id = "nailer_overview",
  title = "Introducing NaileR",
  section = "nailer",
  x = 0,
  y = 210,
  objective = "Understand how NaileR extends the FactoMineR-to-prompt workflow.",
  text = r"(NaileR is positioned as a companion for producing prompts and artefacts from richer FactoMineR outputs. It extends the workflow from explicit variables toward latent classes and textual data. In this capsule, NaileR is optional: the code falls back to inspectable prompt objects when it is not installed.)",
  code = r"(
nailer_available <- requireNamespace('NaileR', quietly = TRUE)

if (nailer_available) {
  nailer_functions <- grep(
    '^nail_',
    getNamespaceExports('NaileR'),
    value = TRUE
  )
} else {
  nailer_functions <- c(
    'nail_condes',
    'nail_catdes',
    'nail_textual',
    'nail_textual_prep',
    'nail_group_profile_prep',
    'nail_textual_contextualized'
  )
}

nailer_workflow <- data.frame(
  source = c(
    'FactoMineR',
    'R',
    'Manual prompt',
    'NaileR',
    'Optional LLM'
  ),
  role = c(
    'produce structured statistical outputs',
    'inspect and transform objects',
    'organize an interpretive request',
    'systematize prompts and artefacts',
    'generate an interpretation from a controlled prompt'
  ),
  example = c(
    'catdes(), condes(), PCA(), HCPC()',
    'names(), str(), capture.output(), paste()',
    'prompt_catdes, prompt_condes',
    'nail_catdes(), nail_textual_prep()',
    'Ollama, Gemini or another engine'
  ),
  stringsAsFactors = FALSE
)

cat('\nIs NaileR installed?', nailer_available, '\n')

cat('\nFunctions detected or expected:\n')
print(data.frame(function_name = nailer_functions), row.names = FALSE)

cat('\nNaileR workflow:\n')
print(nailer_workflow, row.names = FALSE)
)",
  code_display = r"(
nailer_available <- requireNamespace('NaileR', quietly = TRUE)

if (nailer_available) {
  nailer_functions <- grep(
    '^nail_',
    getNamespaceExports('NaileR'),
    value = TRUE
  )
} else {
  nailer_functions <- c(
    'nail_condes',
    'nail_catdes',
    'nail_textual_prep',
    'nail_group_profile_prep',
    'nail_textual_contextualized'
  )
}

nailer_available
nailer_functions
)",
  expected_output = "A structured overview of NaileR and its role.",
  concepts = c("NaileR", "FactoMineR", "artefacts"),
  question = "Which package extends FactoMineR outputs toward prompts and interpretive artefacts?",
  expected_answer = "NaileR",
  next_cells = "nailer_catdes"
)

add_cell(
  id = "nailer_catdes",
  title = "Using nail_catdes()",
  section = "nailer",
  x = 0,
  y = 420,
  objective = "Use NaileR::nail_catdes() on a qualitative variable when NaileR is available.",
  text = r"(We now compare the manual catdes prompt with nail_catdes(). If NaileR is not installed, the cell keeps the manual prompt as a fallback.)",
  code = r"(
required_objects <- c('questionnaire_desc', 'res_catdes', 'text_catdes')

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    'Missing objects: ',
    paste(missing_objects, collapse = ', '),
    '. Run the previous cells first.',
    call. = FALSE
  )
}

nailer_available <- requireNamespace('NaileR', quietly = TRUE)

var_catdes_nailer <- 'profil_alim'
num_var_catdes_nailer <- which(names(questionnaire_desc) == var_catdes_nailer)

if (nailer_available && 'nail_catdes' %in% getNamespaceExports('NaileR')) {
  res_nail_catdes <- tryCatch(
    NaileR::nail_catdes(
      questionnaire_desc,
      num.var = num_var_catdes_nailer,
      generate = FALSE
    ),
    error = function(e) paste('nail_catdes() error:', conditionMessage(e))
  )
} else {
  res_nail_catdes <- list(
    status = 'NaileR is not installed or nail_catdes() is not available.',
    fallback_prompt = prompt_catdes
  )
}

cat('\nVariable described:', var_catdes_nailer, '\n')
cat('Position in questionnaire_desc:', num_var_catdes_nailer, '\n')

cat('\nClass of res_nail_catdes:\n')
print(class(res_nail_catdes))

if (is.list(res_nail_catdes)) {
  cat('\nNames in res_nail_catdes:\n')
  print(names(res_nail_catdes))
}

cat('\nComparison with manual work:\n')
comparison_catdes_nailer <- data.frame(
  step = c('catdes()', 'capture.output()', 'manual prompt', 'nail_catdes()'),
  role = c(
    'describe a qualitative variable statistically',
    'turn the output into text',
    'organize an interpretive request',
    'systematize the same logic'
  ),
  object = c('res_catdes', 'text_catdes', 'prompt_catdes', 'res_nail_catdes'),
  stringsAsFactors = FALSE
)
print(comparison_catdes_nailer, row.names = FALSE)
)",
  code_display = r"(
nailer_available <- requireNamespace('NaileR', quietly = TRUE)

var_catdes_nailer <- 'profil_alim'
num_var_catdes_nailer <- which(names(questionnaire_desc) == var_catdes_nailer)

if (nailer_available && 'nail_catdes' %in% getNamespaceExports('NaileR')) {
  res_nail_catdes <- NaileR::nail_catdes(
    questionnaire_desc,
    num.var = num_var_catdes_nailer,
    generate = FALSE
  )
} else {
  res_nail_catdes <- list(
    status = 'NaileR unavailable',
    fallback_prompt = prompt_catdes
  )
}

class(res_nail_catdes)
names(res_nail_catdes)
)",
  expected_output = "A NaileR catdes artefact or a safe fallback object.",
  concepts = c("nail_catdes", "fallback", "prompt"),
  question = "Which NaileR function extends catdes() here?",
  expected_answer = "nail_catdes",
  next_cells = "pca_hcpc_classes"
)

add_cell(
  id = "pca_hcpc_classes",
  title = "Building latent classes",
  section = "latent",
  x = 260,
  y = 420,
  objective = "Construct latent classes from active quantitative variables using PCA and HCPC.",
  text = r"(We now leave explicit questionnaire variables and build a new class variable. This variable is latent in the sense that it is produced by the analysis: first PCA, then HCPC.)",
  outputs = c("console", "plot"),
  code = r"(
if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

typology_variables <- c(
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

missing_typology_variables <- setdiff(typology_variables, names(questionnaire))

if (length(missing_typology_variables) > 0) {
  stop(
    'Missing typology variables: ',
    paste(missing_typology_variables, collapse = ', '),
    call. = FALSE
  )
}

typology_data <- questionnaire[, typology_variables]

non_numeric_typology <- names(typology_data)[
  !vapply(typology_data, is.numeric, logical(1))
]

if (length(non_numeric_typology) > 0) {
  stop(
    'The following typology variables are not numeric: ',
    paste(non_numeric_typology, collapse = ', '),
    call. = FALSE
  )
}

res_pca <- FactoMineR::PCA(
  typology_data,
  scale.unit = TRUE,
  graph = FALSE
)

set.seed(123)

res_hcpc <- FactoMineR::HCPC(
  res_pca,
  nb.clust = 3,
  graph = FALSE
)

questionnaire$classe_hcpc <- factor(res_hcpc$data.clust$clust)

cat('\nActive typology variables:\n')
print(typology_variables)

cat('\nPCA eigenvalues:\n')
print(res_pca$eig)

cat('\nHCPC class distribution:\n')
print(table(questionnaire$classe_hcpc))

FactoMineR::plot.HCPC(
  res_hcpc,
  choice = 'map',
  draw.tree = FALSE
)
)",
  code_display = r"(
typology_variables <- c(
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

typology_data <- questionnaire[, typology_variables]

res_pca <- FactoMineR::PCA(
  typology_data,
  scale.unit = TRUE,
  graph = FALSE
)

set.seed(123)

res_hcpc <- FactoMineR::HCPC(
  res_pca,
  nb.clust = 3,
  graph = FALSE
)

questionnaire$classe_hcpc <- factor(res_hcpc$data.clust$clust)

res_pca$eig
table(questionnaire$classe_hcpc)

FactoMineR::plot.HCPC(
  res_hcpc,
  choice = 'map',
  draw.tree = FALSE
)
)",
  expected_output = "A PCA object, an HCPC object, and a new class variable named classe_hcpc.",
  concepts = c("PCA", "HCPC", "latent class"),
  question = "Which FactoMineR function builds a classification from a PCA object?",
  expected_answer = "HCPC",
  next_cells = "profile_classes"
)

add_cell(
  id = "profile_classes",
  title = "Profiling latent classes",
  section = "latent",
  x = 520,
  y = 420,
  objective = "Describe the latent classes with catdes(), and optionally with nail_catdes().",
  text = r"(A constructed class should not be named too quickly. First, it must be described statistically. We use catdes() to characterize the HCPC classes.)",
  code = r"(
if (!'classe_hcpc' %in% names(questionnaire)) {
  stop('The variable classe_hcpc is missing. Run the PCA + HCPC cell first.', call. = FALSE)
}

if (!requireNamespace('FactoMineR', quietly = TRUE)) {
  stop('FactoMineR is required. Install the FactoMineR package first.', call. = FALSE)
}

questionnaire_class_description <- questionnaire[
  ,
  setdiff(names(questionnaire), c('id', 'commentaire'))
]

num_var_class <- which(names(questionnaire_class_description) == 'classe_hcpc')

res_catdes_classes <- FactoMineR::catdes(
  questionnaire_class_description,
  num.var = num_var_class
)

text_catdes_classes <- paste(
  capture.output(print(res_catdes_classes)),
  collapse = '\n'
)

if (requireNamespace('NaileR', quietly = TRUE) &&
    'nail_catdes' %in% getNamespaceExports('NaileR')) {
  res_nail_catdes_classes <- tryCatch(
    NaileR::nail_catdes(
      questionnaire_class_description,
      num.var = num_var_class,
      generate = FALSE,
      interpretation_mode = 'latent'
    ),
    error = function(e) paste('nail_catdes() error:', conditionMessage(e))
  )
} else {
  res_nail_catdes_classes <- list(
    status = 'NaileR unavailable',
    fallback_text = text_catdes_classes
  )
}

cat('\nClass distribution:\n')
print(table(questionnaire_class_description$classe_hcpc))

cat('\nNames in res_catdes_classes:\n')
print(names(res_catdes_classes))

cat('\nPreview of class description:\n')
cat(substr(text_catdes_classes, 1, 2500))

cat('\n\nClass of res_nail_catdes_classes:\n')
print(class(res_nail_catdes_classes))

if (is.list(res_nail_catdes_classes)) {
  cat('\nNames in res_nail_catdes_classes:\n')
  print(names(res_nail_catdes_classes))
}
)",
  code_display = r"(
questionnaire_class_description <- questionnaire[
  ,
  setdiff(names(questionnaire), c('id', 'commentaire'))
]

num_var_class <- which(
  names(questionnaire_class_description) == 'classe_hcpc'
)

res_catdes_classes <- FactoMineR::catdes(
  questionnaire_class_description,
  num.var = num_var_class
)

text_catdes_classes <- paste(
  capture.output(print(res_catdes_classes)),
  collapse = '\n'
)

table(questionnaire_class_description$classe_hcpc)
names(res_catdes_classes)
substr(text_catdes_classes, 1, 2500)
)",
  expected_output = "A statistical description of the latent classes.",
  concepts = c("latent class", "catdes", "class profile"),
  question = "What is the name of the class variable added to the questionnaire?",
  expected_answer = "classe_hcpc",
  next_cells = "prepare_texts_by_class"
)

add_cell(
  id = "prepare_texts_by_class",
  title = "Preparing texts by class",
  section = "text",
  x = 780,
  y = 420,
  objective = "Associate the latent classes with free-text comments.",
  text = r"(The class variable gives a latent structure. The comment variable gives verbatims. This cell builds a dataset that keeps both together.)",
  code = r"(
if (!all(c('classe_hcpc', 'commentaire') %in% names(questionnaire))) {
  stop(
    'The variables classe_hcpc and commentaire must both be present. Run the previous cells first.',
    call. = FALSE
  )
}

textual_class_variables <- c(
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

textual_class_variables <- intersect(textual_class_variables, names(questionnaire))

class_text_dataset <- questionnaire[, textual_class_variables]

class_text_dataset$classe_hcpc <- factor(class_text_dataset$classe_hcpc)
class_text_dataset$commentaire <- as.character(class_text_dataset$commentaire)

verbatims_by_class <- split(
  class_text_dataset$commentaire,
  class_text_dataset$classe_hcpc
)

verbatims_by_class <- lapply(verbatims_by_class, unique)

cat('\nObject created: class_text_dataset\n')
cat('Rows:', nrow(class_text_dataset), '\n')
cat('Columns:', ncol(class_text_dataset), '\n')

cat('\nClass distribution:\n')
print(table(class_text_dataset$classe_hcpc))

cat('\nComment summary:\n')
cat('Total comments:', length(class_text_dataset$commentaire), '\n')
cat('Unique comments:', length(unique(class_text_dataset$commentaire)), '\n')

cat('\nExamples by class:\n')
for (cl in names(verbatims_by_class)) {
  cat('\n--- Class ', cl, ' ---\n', sep = '')
  cat(paste('-', head(verbatims_by_class[[cl]], 4), collapse = '\n'))
  cat('\n')
}
)",
  code_display = r"(
textual_class_variables <- c(
  'classe_hcpc',
  'commentaire',
  'satisfaction',
  'intention_achat',
  'prix_percu',
  'type_produit',
  'budget_contraint',
  'profil_alim'
)

textual_class_variables <- intersect(textual_class_variables, names(questionnaire))

class_text_dataset <- questionnaire[, textual_class_variables]

class_text_dataset$classe_hcpc <- factor(class_text_dataset$classe_hcpc)
class_text_dataset$commentaire <- as.character(class_text_dataset$commentaire)

verbatims_by_class <- split(
  class_text_dataset$commentaire,
  class_text_dataset$classe_hcpc
)

verbatims_by_class <- lapply(verbatims_by_class, unique)

dim(class_text_dataset)
table(class_text_dataset$classe_hcpc)
lapply(verbatims_by_class, head, 4)
)",
  expected_output = "A dataset associating classes, comments and structured variables.",
  concepts = c("textual data", "classes", "verbatims"),
  question = "Which constructed variable is used to group the comments?",
  expected_answer = "classe_hcpc",
  next_cells = "prepare_artefacts"
)

add_cell(
  id = "prepare_artefacts",
  title = "Preparing textual and structured artefacts",
  section = "text",
  x = 1040,
  y = 420,
  objective = "Prepare complementary artefacts for class interpretation.",
  text = r"(A class can be interpreted from two complementary sources: textual comments and structured questionnaire variables. NaileR can prepare these artefacts when available; otherwise, this cell creates simple inspectable summaries.)",
  code = r"(
if (!exists('class_text_dataset')) {
  stop('The object class_text_dataset is missing. Run the previous cell first.', call. = FALSE)
}

if (requireNamespace('NaileR', quietly = TRUE)) {
  num_var_classes <- which(names(class_text_dataset) == 'classe_hcpc')
  num_text_classes <- which(names(class_text_dataset) == 'commentaire')

  textual_prep_classes <- tryCatch(
    NaileR::nail_textual_prep(
      dataset = class_text_dataset,
      num.var = num_var_classes,
      num.text = num_text_classes,
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) paste('nail_textual_prep() error:', conditionMessage(e))
  )

  class_profile_dataset <- class_text_dataset[
    ,
    setdiff(names(class_text_dataset), 'commentaire')
  ]

  num_var_profile_classes <- which(names(class_profile_dataset) == 'classe_hcpc')

  group_profile_classes <- tryCatch(
    NaileR::nail_group_profile_prep(
      dataset = class_profile_dataset,
      num.var = num_var_profile_classes,
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) paste('nail_group_profile_prep() error:', conditionMessage(e))
  )

} else {
  textual_prep_classes <- lapply(
    split(class_text_dataset$commentaire, class_text_dataset$classe_hcpc),
    function(x) {
      list(
        n_comments = length(x),
        n_unique_comments = length(unique(x)),
        examples = head(unique(x), 8)
      )
    }
  )

  numeric_variables_for_profile <- names(class_text_dataset)[
    vapply(class_text_dataset, is.numeric, logical(1))
  ]

  group_profile_classes <- aggregate(
    class_text_dataset[numeric_variables_for_profile],
    by = list(class = class_text_dataset$classe_hcpc),
    FUN = mean
  )

  group_profile_classes[, -1] <- round(group_profile_classes[, -1], 2)
}

artefact_comparison <- data.frame(
  artefact = c('textual_prep_classes', 'group_profile_classes'),
  material = c('free-text comments', 'structured questionnaire variables'),
  role = c(
    'summarize themes and formulations by class',
    'summarize quantitative and qualitative profiles by class'
  ),
  stringsAsFactors = FALSE
)

cat('\nArtefact comparison:\n')
print(artefact_comparison, row.names = FALSE)

cat('\nTextual artefact class:\n')
print(class(textual_prep_classes))

if (is.list(textual_prep_classes)) {
  cat('\nTextual artefact names or length:\n')
  nm <- names(textual_prep_classes)
  if (is.null(nm)) {
    print(length(textual_prep_classes))
  } else {
    print(nm)
  }
}

cat('\nStructured artefact class:\n')
print(class(group_profile_classes))

cat('\nPreview of structured artefact:\n')
print(group_profile_classes)
)",
  code_display = r"(
if (requireNamespace('NaileR', quietly = TRUE)) {
  num_var_classes <- which(names(class_text_dataset) == 'classe_hcpc')
  num_text_classes <- which(names(class_text_dataset) == 'commentaire')

  textual_prep_classes <- NaileR::nail_textual_prep(
    dataset = class_text_dataset,
    num.var = num_var_classes,
    num.text = num_text_classes,
    model = 'llama3',
    generate = FALSE
  )

  class_profile_dataset <- class_text_dataset[
    ,
    setdiff(names(class_text_dataset), 'commentaire')
  ]

  num_var_profile_classes <- which(names(class_profile_dataset) == 'classe_hcpc')

  group_profile_classes <- NaileR::nail_group_profile_prep(
    dataset = class_profile_dataset,
    num.var = num_var_profile_classes,
    model = 'llama3',
    generate = FALSE
  )
} else {
  textual_prep_classes <- lapply(
    split(class_text_dataset$commentaire, class_text_dataset$classe_hcpc),
    function(x) head(unique(x), 8)
  )

  group_profile_classes <- aggregate(
    class_text_dataset[vapply(class_text_dataset, is.numeric, logical(1))],
    by = list(class = class_text_dataset$classe_hcpc),
    FUN = mean
  )
}

class(textual_prep_classes)
class(group_profile_classes)
)",
  expected_output = "Two complementary artefacts: textual_prep_classes and group_profile_classes.",
  concepts = c("artefact", "textual profile", "structured profile"),
  question = "Which NaileR function prepares free-text comments by class?",
  expected_answer = "nail_textual_prep",
  next_cells = "contextual_synthesis"
)

add_cell(
  id = "contextual_synthesis",
  title = "Contextualized synthesis",
  section = "synthesis",
  x = 1300,
  y = 420,
  objective = "Combine textual and structured artefacts into an inspectable synthesis prompt.",
  text = r"(The final step combines two sources: what the structured variables say about the classes, and what the verbatims say. The goal is not to ask an LLM to interpret raw data directly, but to pass through visible artefacts.)",
  code = r"(
if (!exists('textual_prep_classes')) {
  stop('The object textual_prep_classes is missing. Run the previous cell first.', call. = FALSE)
}

if (!exists('group_profile_classes')) {
  stop('The object group_profile_classes is missing. Run the previous cell first.', call. = FALSE)
}

if (requireNamespace('NaileR', quietly = TRUE) &&
    'nail_textual_contextualized' %in% getNamespaceExports('NaileR') &&
    is.list(textual_prep_classes) &&
    is.list(group_profile_classes)) {

  contextualized_classes <- tryCatch(
    NaileR::nail_textual_contextualized(
      group_profile_prep = group_profile_classes,
      textual_prep = textual_prep_classes,
      interpretation_mode = 'comparative',
      model = 'llama3',
      generate = FALSE
    ),
    error = function(e) paste('nail_textual_contextualized() error:', conditionMessage(e))
  )

} else {

  contextualized_classes <- list(
    prompt = paste(
      '# Contextualized interpretation of latent classes',
      '',
      'Use two sources of evidence:',
      '',
      '1. Structured class profiles:',
      paste(capture.output(print(group_profile_classes)), collapse = '\n'),
      '',
      '2. Textual evidence by class:',
      paste(capture.output(str(textual_prep_classes, max.level = 2)), collapse = '\n'),
      '',
      'Instructions:',
      '- describe each class cautiously;',
      '- separate structured evidence from textual evidence;',
      '- avoid causal claims;',
      '- propose short interpretable names for the classes only after summarizing evidence.',
      sep = '\n'
    ),
    status = 'Manual contextualized prompt created because NaileR contextualization is unavailable.'
  )
}

contextualization_flow <- data.frame(
  source = c(
    'Verbatims',
    'Structured variables',
    'Contextualized synthesis'
  ),
  object = c(
    'textual_prep_classes',
    'group_profile_classes',
    'contextualized_classes'
  ),
  role = c(
    'capture themes and formulations by class',
    'describe classes using questionnaire variables',
    'combine the two sources into an interpretation request'
  ),
  stringsAsFactors = FALSE
)

extract_text_field <- function(object, possible_names) {
  if (!is.list(object) || is.null(names(object))) {
    return(paste(capture.output(print(object)), collapse = '\n'))
  }

  found <- intersect(possible_names, names(object))

  if (length(found) > 0) {
    return(paste(as.character(object[[found[1]]]), collapse = '\n'))
  }

  paste(capture.output(str(object, max.level = 2)), collapse = '\n')
}

contextualized_prompt_text <- extract_text_field(
  contextualized_classes,
  c('prompt', 'prompts', 'request', 'instruction', 'instructions')
)

contextualized_response_text <- extract_text_field(
  contextualized_classes,
  c('response', 'answer', 'result', 'interpretation', 'summary', 'text')
)

cat('\nContextualization flow:\n')
print(contextualization_flow, row.names = FALSE)

cat('\nClass of contextualized_classes:\n')
print(class(contextualized_classes))

if (is.list(contextualized_classes)) {
  cat('\nNames in contextualized_classes:\n')
  print(names(contextualized_classes))
}

cat('\nPrompt or prepared object:\n')
cat(substr(contextualized_prompt_text, 1, 2500))

cat('\n\nResponse or interpretation field if available:\n')
cat(substr(contextualized_response_text, 1, 2500))

cat('\n\nFinal pedagogical message:\n')
cat('The workflow is artefact-centered: statistical outputs, prompts and textual profiles remain inspectable before interpretation.\n')
)",
  code_display = r"(
if (requireNamespace('NaileR', quietly = TRUE) &&
    'nail_textual_contextualized' %in% getNamespaceExports('NaileR')) {

  contextualized_classes <- NaileR::nail_textual_contextualized(
    group_profile_prep = group_profile_classes,
    textual_prep = textual_prep_classes,
    interpretation_mode = 'comparative',
    model = 'llama3',
    generate = FALSE
  )

} else {

  contextualized_classes <- list(
    prompt = paste(
      '# Contextualized interpretation of latent classes',
      'Use structured profiles and textual evidence.',
      'Describe each class cautiously.',
      sep = '\n'
    )
  )
}

class(contextualized_classes)
names(contextualized_classes)
)",
  expected_output = "A contextualized synthesis object or a manual fallback prompt.",
  concepts = c("contextualization", "artefact-centered workflow", "synthesis"),
  question = "Which NaileR function combines textual and structured artefacts?",
  expected_answer = "nail_textual_contextualized",
  next_cells = character()
)

message("Checking generated taidyverse capsule...")
SeRiouS::check_capsule_dir(capsule_dir)

message("Generated capsule: ", normalizePath(capsule_dir, mustWork = TRUE))
message("Run it with: SeRiouS::run_capsule('", capsule_dir, "')")
