test_that("UI CSS is generated", {
  css <- serious_ui_css()

  expect_s3_class(css, "html")
  expect_true(grepl("serious-header", as.character(css)))
  expect_true(grepl("serious-card", as.character(css)))
  expect_true(grepl("serious-board", as.character(css)))
})


test_that("header UI is generated", {
  cap <- get_capsule("demo_iris")

  header <- serious_header_ui(cap)

  expect_s3_class(header, "shiny.tag")
  expect_true(grepl(cap$title, as.character(header)))
})


test_that("step card classes depend on status", {
  expect_match(
    serious_step_card_class("locked"),
    "serious-step-card-locked"
  )

  expect_match(
    serious_step_card_class("unlocked"),
    "serious-step-card-unlocked"
  )

  expect_match(
    serious_step_card_class("visited"),
    "serious-step-card-unlocked"
  )
})


test_that("badge classes depend on status", {
  expect_equal(
    serious_badge_class("locked"),
    "serious-badge serious-locked"
  )

  expect_equal(
    serious_badge_class("unlocked"),
    "serious-badge serious-unlocked"
  )

  expect_equal(
    serious_badge_class("visited"),
    "serious-badge serious-visited"
  )
})
