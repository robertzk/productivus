context("ed function")

test_that("it edits a simple numeric vector in-place", {
  edit <- function(x) 2 * y
  y <- 1:3
  ed(y)
  expect_equal(y, 2 * (1:3),
    info = "ed should modify its objects in-place")
})

