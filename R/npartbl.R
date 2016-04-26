#' MERLIN \code{npartbl} Object
#'
#' Returns an S3 object of class \code{npartbl}
#'
#' @param npartbl The name of a \code{fam_nonparametric.tbl}
#' file output by MERLIN
#'
#' @return S3 object of class \code{npartbl}
#' @export
#'
#' @examples
#' npartbl_obj <- npartbl("merlin_10_famA-nonparametric.tbl")
npartbl <- function(npartbl) {
  stopifnot(!missing(npartbl))
  if (is.character(npartbl) && length(npartbl) == 1) {
    npartbl <- read_merlin_npartbl(npartbl)
  } else {
    stop("You need to give the path to your nonparametric.tbl file.")
  }
  names(npartbl) <- tolower(names(npartbl))
  npartbl_cols <- names(npartbl)
  required_cols <- c("chr", "pos", "label", "analysis", "lod", "exlod")
  stopifnot(all(required_cols %in% npartbl_cols))
  stopifnot(identical(npartbl$pos, npartbl$label))
  chrom <- unique(npartbl[, "chr", drop = TRUE])
  stopifnot(length(chrom) == 1, chrom %in% 1:23)

  ## Do we have one or two analyses? 'all' and/or 'pairs'?
  analysis_tab <- table(npartbl[, "analysis", drop = TRUE], useNA = "ifany")
  names(analysis_tab) <- tolower(names(analysis_tab))
  analysis <- match_vec(names(analysis_tab), a = "all", b = "pairs")

  if (length(analysis) == 2) {
    # So number of 'all' and 'pairs' rows must be same
    n1 <- analysis_tab[1]
    n2 <- analysis_tab[2]
    stopifnot(n1 == n2)
    # check that pos is identical for both analyses
    stopifnot(identical(npartbl[1:n1, "pos"], npartbl[(n1 + 1):(n1 + n2), "pos"]))
  }

  npar_df <- npartbl[c("pos", "lod", "exlod", "analysis")]
  structure(list(npar_df = npar_df, chrom = chrom, analysis = analysis), class = "npartbl")

}

