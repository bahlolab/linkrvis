#' MERLIN \code{npartbl} Object
#'
#' Returns an S3 object of class \code{npartbl}
#'
#' @param npartbl The name of a \code{fam_nonparametric.tbl}
#' file output by MERLIN
#'
#' @return S3 object of class \code{npartbl}, which is
#' a list with:
#' \itemize{
#'   \item chrom: character vector of length 1
#'   \item analysis: character vector of length 1 or 2
#'   \item pos_cm: numeric vector with cM positions
#'   \item n_markers: character vector of length 1
#'   \item lods: list of one or two data frames:
#'     \enumerate{
#'   \item all: data.frame with two numeric columns: lod and exlod
#'   \item pairs: data.frame with two numeric columns: lod and exlod
#'     }
#'   }
#'
#' @examples
#' npartbl_obj <- npartbl("merlin_10_famA-nonparametric.tbl")
#'
#' @export
npartbl <- function(npartbl) {
  stopifnot(!missing(npartbl))
  if (is.character(npartbl) && length(npartbl) == 1) {
    npartbl <- read_merlin_npartbl(npartbl)
  } else {
    stop("You need to give the full path to the nonparametric.tbl file.")
  }
  names(npartbl) <- tolower(names(npartbl))
  required_cols <- c("chr", "pos", "analysis", "lod", "exlod")
  stopifnot(all(required_cols %in% names(npartbl)))
  chrom <- unique(npartbl$chr)
  stopifnot(length(chrom) == 1, chrom %in% 1:23)

  ## Do we have one or two analyses? 'all' and/or 'pairs'?
  an_tab <- table(npartbl$analysis)
  an <- match_vec(names(an_tab), a = "all", b = "pairs")

  if (length(an) == 2) {
    # Number of 'all' and 'pairs' rows must be same
    n1 <- an_tab[1]
    n2 <- an_tab[2]
    stopifnot(n1 == n2)
    # check that pos is identical for both analyses
    stopifnot(identical(npartbl$pos[1:n1],
                        npartbl$pos[(n1 + 1):(n1 + n2)]))
    # keep the position vector separately
    pos <- npartbl$pos[1:n1]
  } else {
    pos <- npartbl$pos
  }

  lods <- split(npartbl[c("lod", "exlod")], npartbl$analysis) # returns list of one or two elements
  names(lods)[grepl("all", names(lods), ignore.case = TRUE)] <- "all"
  names(lods)[grepl("pairs", names(lods), ignore.case = TRUE)] <- "pairs"
  max_lods <- vapply(lods, function(DF) {
    c(max_lod = max(DF$lod),
      max_exlod = max(DF$exlod))
  }, FUN.VALUE = vector(mode = "numeric", length = 2))

  structure(list(lods = lods,
                 pos_cm = pos,
                 chrom = chrom,
                 analysis = an,
                 n_markers = nrow(lods[[1]]),
                 max_lods = max_lods),
            class = "npartbl")

}


