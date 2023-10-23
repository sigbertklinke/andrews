#' Generate a Sequence of Prime Numbers
#'
#' Generates a vector of the first `n` primes using [gmp::nextprime()].
#'
#' @param n the number of primes to generate.
#' @param one should `1` included in the sequence (default: `FALSE`)
#'
#' @return an integer vector of prime numbers
#' @importFrom gmp nextprime
#' @export
#'
#' @examples
#' generate_n_primes(5)
#' generate_n_primes(5, TRUE)
generate_n_primes <- function(n, one=FALSE) {
  stopifnot(n>0)
  ret <- if (one) 1L else 2L
  if (n > 1) {
    ret <- c(ret, rep(NA_integer_, n-1))
    for (i in 2:n) ret[i] <- as.integer(nextprime(ret[i-1]))
  }
  ret
}
