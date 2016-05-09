#' Output Regions with Values Above Threshold
#'
#' Given a data.frame \code{x} with 'position' and 'value' columns
#' output those consecutive rows (at least of length \code{width}) where
#' 'value' > \code{threshold}.
#'
#' @param DF A data.frame with two numeric columns: 'position' and 'value'.
#' @param vcol A length-one character vector giving the name of the column containing
#'        the values.
#' @param threshold A length-one numeric vector.
#' @param width A length-one numeric vector.
#'
#' @return A list where each element is a data.frame containing subsets of \code{x}
#'         where value >= \code{threshold}. If possible, one flanking row is included
#'         on each side of the region. If no regions achieve the specified \code{threshold},
#'         returns NULL.
#'
#' @examples
#' @export
get_peaks <- function(DF, vcol, threshold = 0, width = 1) {
  stopifnot(is.data.frame(DF), nrow(DF) > 0)
  stopifnot(is.character(vcol), length(vcol) == 1)
  stopifnot(vcol %in% names(DF), is.unsorted(DF[[vcol]]))
  stopifnot(is.numeric(threshold), length(threshold) == 1)
  stopifnot(is.numeric(width), length(width) == 1)

  x <- rle(DF[[vcol]] >= threshold)
  short <- x$values & (x$lengths < width)
  if (any(short)) {
      x$values[short] <- FALSE
      x <- rle(inverse.rle(x))
  }
  if (!any(x$values)) return(NULL)
  n <- nrow(DF)
  start_ind <- c(0, cumsum(x$lengths))[which(x$values)]
  end_ind <- start_ind + x$lengths[x$values] + 1

  lapply(seq_len(length(start_ind)), function(ind) {
             s <- start_ind[ind]
             e <- end_ind[ind]
             telomeric <- c(if (s == 0) "start", if (e == n) "end")
             if (length(telomeric) == 0) telomeric <- "no"
             s <- max(s, 1)
             e <- min(e, n)
             structure(DF[s:e, , drop = FALSE], rownames = NULL, telomeric = telomeric)
  })
}
