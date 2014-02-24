context("lastn function")

(function() {
  environment(lastn)$savehistory <- function(file) {
    lines <-
      vapply(1:9, function(i) paste0("x[", i, "] <- ", i + 1), character(1))
    writeLines(lines, file)
  }

  test_that("make sure the last n commands get executed", {
    x <- 1:9
    lastn(9)
    expect_equal(x, 2:10)
    for (i in 1:9) {
      x <- 1:9
      get(paste0("last", i))()
      expect_equal(x, c(seq_len(9 - i), (9 - i + 1) + seq_len(i)))
    }
  })

  test_that("make sure the last n commands get executed, excluding 'lastn' commands", {
    x <- 1:9
    environment(lastn)$savehistory <- function(file) {
      lines <-
        vapply(1:9, function(i) paste0("x[", i, "] <- ", i + 1), character(1))
      lines <- append(lines, "lastn(1)")
      writeLines(lines, file)
    }
    lastn(9)
    expect_equal(x, 2:10)
  })

  environment(lastn)$savehistory <- NULL

})()
