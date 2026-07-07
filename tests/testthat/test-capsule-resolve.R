test_that("serious_as_learning_capsule resolves a capsule directory", {
  path <- tempfile("serious-resolve-test-")

  create_capsule_skeleton(path)

  cap <- serious_as_learning_capsule(path)

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "my_capsule")
})


test_that("serious_as_learning_capsule resolves an internal capsule name", {
  cap <- serious_as_learning_capsule("demo_iris")

  expect_s3_class(cap, "learning_capsule")
  expect_equal(cap$id, "demo_iris")
})


test_that("serious_as_learning_capsule returns a learning_capsule unchanged", {
  cap <- get_capsule("demo_iris")

  resolved <- serious_as_learning_capsule(cap)

  expect_s3_class(resolved, "learning_capsule")
  expect_equal(resolved$id, "demo_iris")
})


test_that("serious_as_learning_capsule rejects unknown capsules", {
  expect_error(
    serious_as_learning_capsule("unknown_capsule"),
    "Unknown capsule"
  )
})
