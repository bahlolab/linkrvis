#' Output Rows with Values Above Threshold
#'
#' Given a data.frame \code{d} with 'position' and 'value' columns
#' output those consecutive rows where 'value' > \code{t}.
#'
#' For objects of class \code{\link{partbl}}, \code{link{npartbl}},
#' \code{link{famlod}} or \code{link{fampar}}, the main data frame
#' containing the LOD scores per marker is used.
#'
#' @param d An object of class \code{\link{partbl}}, \code{link{npartbl}},
#'        \code{link{famlod}} or \code{link{fampar}}.
#'        Can also be a data.frame with two numeric columns
#'        that make sense as 'position' and 'value'.
#' @param valcol A length-one character vector giving the name of the column in
#'        \code{d} containing the values.
#' @param poscol A length-one character vector giving the name of the column in
#'        \code{d} containing the positions.
#' @param t A length-one numeric vector specifying the threshold to be used.
#'
#' @return A list where each element is a data.frame containing subsets of \code{d}
#'         where 'value' >= \code{t}. If possible, one flanking row is included
#'         on each side of the region. If no regions achieve the specified
#'         \code{t}, returns NULL.
#'
#' @examples
#' partbl <- partbl::partbl("merlin_10_famA-parametric.tbl")
#' get_peaks(partbl$partbl, "lod")
#'
#' @export
get_peaks <- function(d, valcol = "lod", poscol = "pos", t = 0) {
  if (inherits(d, "partbl")) {
    d <- d$partbl
    split_by <- c("chr")
  } else if (inherits(d, "npartbl")) {
    d <- d$npartbl
    split_by <- c("chr", "analysis")
  } else if (inherits(d, "fampar")) {
    d <- d$fampar
    split_by <- c("chr", "family")
  } else if (inherits(d, "famlod")) {
    d <- d$famlod
    split_by <- c("chr", "analysis", "family")
  }
  stopifnot(is.data.frame(d))
  stopifnot("chr" %in% names(d))
  stopifnot(is.numeric(t), length(t) == 1)
  stopifnot(is.character(valcol), length(valcol) == 1)
  stopifnot(is.character(poscol), length(poscol) == 1)
  stopifnot(all(c(valcol, poscol) %in% names(d)))
  get_peaksPerChrom <- function(dchr) {
    stopifnot(is.data.frame(dchr),
              nrow(dchr) > 0,
              !is.unsorted(dchr[[poscol]]))

    x <- rle(dchr[[valcol]] >= t)
    if (!any(x$values)) return(NULL)
    n <- nrow(dchr)
    start_ind <- c(0, cumsum(x$lengths))[which(x$values)]
    end_ind <- start_ind + x$lengths[x$values] + 1

    regions <- lapply(seq_len(length(start_ind)), function(ind) {
      s <- start_ind[ind]
      e <- end_ind[ind]
      telomeric <- c(if (s == 0) "start", if (e == n) "end")
      if (length(telomeric) == 0) telomeric <- "no"
      s <- max(s, 1)
      e <- min(e, n)
      structure(dchr[s:e, , drop = FALSE], rownames = NULL, telomeric = telomeric)
    })
    regions
  }
  plyr::dlply(d, split_by, get_peaksPerChrom)
}