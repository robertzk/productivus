context("ed function")

test_that("it edits a simple numeric vector in-place", {
  edit <- function(x) 2 * x
  x <- 1:3
  ed(x)
  expect_equal(x, 2 * (1:3),
    info = "ed should modify its objects in-place")
})

