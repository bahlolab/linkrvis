#' Match Values to Given Vector
#'
#' @param x A character vector of length 1 or 2
#' @param a A character vector of length 1
#' @param b A character vector of length 1
#'
#' @return A character vector of length 1 or 2, containing
#' the string in variable a, b or both.
#'
#' @examples
#' match_vec(c("beta", "alpha"))
#' match_vec("alpha")
#' match_vec("betaaaa")
#' match_vec("bettaaaa")
#' match_vec("alphaandbeta")
#' match_vec(c("this has thealpha", "beta in here"))
#'
#' @export
match_vec <- function(x, a = "alpha", b = "beta") {
  stopifnot(is.character(x), is.character(a), is.character(b))
  stopifnot(length(a) == 1, length(b) == 1)
  stopifnot(length(x) %in% c(1, 2))
  ai <- grep(a, x)
  bi <- grep(b, x)
  if (length(x) == 1) {
    if (length(ai) == 1 && length(bi) == 0) {
      return(a)
    } else if (length(ai) == 0 && length(bi) == 1) {
      return(b)
    } else {
      stop(paste("Could not find", a, "xor", b, "in:", x))
    }
  }
  if (length(x) == 2) {
    if (length(ai) == 1 && length(bi) == 1) {
      stopifnot(ai != bi)
      return(c(a, b))
    } else {
      stop(paste("Could not find both", a, "and", b, "in:", x, collapse = "\n"))
    }
  }
}

