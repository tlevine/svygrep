#' Estimate how many times a particular pattern appears in a file.
#'
#' @param file Filename
#' @param pattern The pattern to match
#' @param n Sample size
#' @param Each observation within the sample is this many bytes long.
#' @return survey object of some sort
estimate.count <- function(f, pattern = '\n', n = 1000, page.size = 2^14) {
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
  wheres <- file.start + sort(sample.int(0, N)) * page.size
  total.samples <- sapply(wheres, total.sample)
  

    ts = list(map(f, sorted(sample(range(0, N), n))))
    E_t_sample = sum(ts) / n
    Var_t_sample = sum((t - E_t_sample) ** 2 for t in ts) / (n - 1)

    # 99% gaussian confidence interval
    z = 2.575829

    # Levy & Lemeshow, page 51
    fpc = (N - n) / N
    E_t_population = N * E_t_sample * (page_size / actual_page_size)
    Var_t_population = (N ** 2) * fpc * Var_t_sample / n
    SE_t_population = Var_t_population ** 0.5

}
