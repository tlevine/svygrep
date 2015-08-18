#' Estimate how many times a particular pattern appears in a file.
#'
#' @param file Filename
#' @param pattern The pattern to match
#' @param n Sample size
#' @param Each observation within the sample is this many bytes long.
#' @return survey object of some sort
estimate.count <- function(file, pattern = '\n', n = 1000, page.size = 2^14)
