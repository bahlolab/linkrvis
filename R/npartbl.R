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

#' Get \code{npartbl} Table
#'
#' @param npartbl An object of class \code{npartbl}.
#'
#' @return A data.frame with the chromosome, pos_cm, analysis,
#' lod and exlod columns from a \code{npartbl} object.
#'
#' @examples
#' get_npartbl(npartbl("fam_nonparametric.tbl"))
#'
#' @export
get_npartbl <- function(npartbl) {
  stopifnot(inherits(npartbl, "npartbl"))
  lods_per_analysis <- dplyr::bind_rows(npartbl$lods, .id = "analysis")
  data.frame(chrom = npartbl$chrom,
             pos_cm = npartbl$pos_cm,
             lods_per_analysis)
}


#' Print Head, Middle and Tail of \code{npartbl} Table
#'
#' Prints the head, middle and tail of a \code{npartbl} table.
#'
#' If the \code{npartbl} object has only one analysis (e.g. 'all'),
#' the middle
#'
#' @param npartbl An object of class \code{npartbl}.
#' @param n Number of rows to show in the head, middle and tail
#'
#' @export
print.npartbl <- function(npartbl, n = 6L) {
  stopifnot(inherits(npartbl, "npartbl"))
  stopifnot(length(n) == 1L, is.numeric(n), n > 0)
  if (n > 100L) {
    stop(paste("Giving an n of", n, "will clutter your screen.",
               "Use one lower than 100."))
  }
  tbl <- get_npartbl(npartbl)
  print(head(tbl, n = n))
  cat("--------\n")
  nr <- nrow(tbl)
  midn <- floor(n / 2)
  print(tbl[((nr / 2) - midn + 1):((nr / 2) + midn), ])
  cat("--------\n")
  print(tail(tbl))
}

#' Return Summary of \code{npartbl} Object
#'
#' @param npartbl An object of class \code{npartbl}.
#'
#' @return A list (of class \code{summary.npartbl}) containing summary information about
#' the \code{npartbl} object.
#'
#' @export
summary.npartbl <- function(npartbl) {
  stopifnot(inherits(npartbl, "npartbl"))
  structure(list(chrom = npartbl$chrom,
       n_markers = npartbl$n_markers,
       analysis = npartbl$analysis,
       max_lods = npartbl$max_lods), class = "summary.npartbl")
}


#' @param npartbl An object of class \code{summary.npartbl}
#'
#' @method print summary.npartbl
#' @export
print.summary.npartbl <- function(npartbl) {
  stopifnot(inherits(npartbl, "summary.npartbl"))
  cat("MERLIN nonparametric.tbl\n",
      "--------------------------",
      "chrom: ", npartbl$chrom, "\n",
      "n_markers: ", npartbl$n_markers, "\n",
      "analysis: ", paste(npartbl$analysis, collapse = " and "), "\n",
      "max_lods:\n", sep = "")
  print(npartbl$max_lods)
}
