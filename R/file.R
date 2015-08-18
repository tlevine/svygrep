library(survey)

#' Estimate how many times a particular pattern appears in a file.
#'
#' @param file Filename
#' @param pattern The pattern to match
#' @param n Sample size
#' @param Each observation within the sample is this many bytes long.
#' @return survey object of some sort
svygrep <- function(f, pattern = '\n', n = 1000, page.size = 2^14) {
  if (n < 0)
    stop('Sample size must be greater than zero.')

  con <- file(f, open = 'rb')
  file.start <- seek(con, where = 0, origin = 'end')
  file.end <- seek(con)

  # Population size, ignoring the last page for now.
  # To do: Weight the last page lower, proportional to its size.
  N <- as.integer((file.end - file.start) / page.size)

  if (file.end <= file.start)
    stop('The file is empty, or you have seeked to a strange part of it.')
  else if (N <= n)
    stop('File is too small; just read the whole file.')

  total.sample <- function(where) {
    seek(con, where)
    length(strsplit(readChar(con, page.size), pattern))
  }
  wheres <- file.start + sort(sample.int(N, n) - 1) * page.size
  counts <- data.frame(where = wheres,
                       count = sapply(wheres, total.sample))
  svydesign(id = ~1, weights = 1, fpc = rep(1/n, n), data = iris)
}
