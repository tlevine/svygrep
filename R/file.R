library(survey)

#' Estimate how many times a particular pattern appears in a file.
#' This is a cluster sample, with each page as a cluster and with
#' clusters selected simple-randomly.
#'
#' @param file Filename
#' @param pattern The pattern to match
#' @param n Sample size
#' @param Each observation within the sample is this many bytes long.
#' @return survey object of some sort
sample.totals <- function(f, pattern = '\n', n = 100, page.size = 2^14) {
  if (n < 0)
    stop('Sample size must be greater than zero.')

  con <- file(f, open = 'rb')
  file.start <- seek(con, where = 0, origin = 'end')
  file.end <- seek(con)
  file.size <- file.end - file.start
  if (file.end <= file.start)
    stop('The file is empty, or you have seeked to a strange part of it.')

  last.page.start <- file.end - file.start
  N <- ceiling(last.page.start / page.size)
  if (N <= n)
    stop('File is too small; just read the whole file.')

  total.sample <- function(where) {
    seek(con, where)
    length(strsplit(readChar(con, page.size), pattern)[[1]])
  }
  page.sizes <- c(rep(page.size, N - 1), file.end - last.page.start)
  pages <- sort(sample.int(N, n))
  wheres <- file.start + (pages - 1) * page.size

  counts <- data.frame(ids = wheres,
                       weights = page.sizes[pages] * (N / n),
                       fpc = page.sizes[pages],
                       p = sapply(wheres, total.sample) / page.sizes[pages])
  svydesign(~ids, weights = ~weights, fpc = ~fpc, data = counts)
}
