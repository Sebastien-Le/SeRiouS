test_that("board helpers return valid step ids", {
  cap <- get_capsule("demo_iris")

  ids <- serious_board_step_ids(cap)

  expect_type(ids, "character")
  expect_gt(length(ids), 0)
  expect_true(cap$start_step %in% ids)
})


test_that("board nodes are fixed and non-physical", {
  cap <- get_capsule("demo_iris")

  nodes <- serious_board_make_nodes(
    capsule = cap,
    unlocked_steps = cap$start_step,
    visited_steps = character()
  )

  expect_s3_class(nodes, "data.frame")
  expect_true(all(c("id", "label", "x", "y") %in% names(nodes)))

  expect_true(all(nodes$fixed))
  expect_true(all(nodes$fixed.x))
  expect_true(all(nodes$fixed.y))
  expect_true(all(nodes$physics == FALSE))

  expect_true(all(grepl("\U0001F512|\U0001F513|\u2713", nodes$label)))
})


test_that("board nodes update status labels", {
  cap <- get_capsule("demo_iris")
  ids <- serious_board_step_ids(cap)

  nodes <- serious_board_make_nodes(
    capsule = cap,
    unlocked_steps = ids[1:2],
    visited_steps = ids[1]
  )

  expect_true(grepl("\u2713", nodes$label[nodes$id == ids[1]]))
  expect_true(grepl("\U0001F513", nodes$label[nodes$id == ids[2]]))

  if (length(ids) >= 3) {
    expect_true(grepl("\U0001F512", nodes$label[nodes$id == ids[3]]))
  }
})


test_that("board edges are valid and non-physical", {
  cap <- get_capsule("demo_iris")

  edges <- serious_board_make_edges(cap)

  expect_s3_class(edges, "data.frame")
  expect_true(all(c("from", "to", "arrows", "physics") %in% names(edges)))
  expect_true(all(edges$arrows == "to"))

  if (nrow(edges) > 0) {
    expect_true(all(edges$physics == FALSE))
  }
})


test_that("board widget can be created", {
  testthat::skip_if_not_installed("visNetwork")

  cap <- get_capsule("demo_iris")

  nodes <- serious_board_make_nodes(
    capsule = cap,
    unlocked_steps = cap$start_step,
    visited_steps = character()
  )

  edges <- serious_board_make_edges(cap)

  widget <- serious_board_widget(
    nodes = nodes,
    edges = edges,
    height = "420px"
  )

  expect_s3_class(widget, "visNetwork")
})
