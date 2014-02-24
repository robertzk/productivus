raw_history <- function() {
  file1 <- tempfile("Rrawhist")
  savehistory(file1)
  rawhist <- readLines(file1)
  rawhist <- rawhist[setdiff(seq_len(length(rawhist)),
                             grep("^\\s*(?:last[1-9n]|ed)\\(.*\\)\\s*", rawhist))]
  unlink(file1)
  rawhist
}
