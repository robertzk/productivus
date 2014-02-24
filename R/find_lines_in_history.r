find_lines_in_history <- function(begin, to = NULL) { 
  stopifnot(is.character(begin) && (is.null(to) || is.character(to) || is.integer(to)))
  rawhist <- raw_history()
  rawhist <- rawhist[-length(rawhist)] # last one is a call to this function

  unregex <- function(string)
    gsub(paste0("(", paste0(vapply(strsplit("\\^$.|?*+()[{", "")[[1]],
      function(ch) paste0("\\", ch), character(1)), collapse = "|"), ")"),
      "\\\\\\1", string, perl = TRUE)

  linenum <- rev(grep(unregex(begin), rawhist))[1]

  if (is.na(linenum)) return(character(0))

  rawhist <- rawhist[seq_len(length(rawhist) - linenum + 1) + linenum - 1]
  if (!is.null(to)) {
    endlinenum <- grep(unregex(to), rawhist)[1]
    if (is.null(linenum)) endlinenum <- length(rawhist)
  }

  rawhist[seq_len(endlinenum)]
}
