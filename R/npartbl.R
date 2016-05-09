#' Read MERLIN \code{nonparametric.tbl}
#'
#' Read the \code{nonparametric.tbl} file output by MERLIN
#'
#' The \code{fam_nonparametric.tbl} file contains two rows at the
#' beginning of each 'all' and/or 'pairs' subtable, which
#' indicate the maximum possible scores for the specific dataset.
#' These rows are ignored by using the comment.char = "n"
#' option in \code{\link{read.table}}, which works since only these rows
#' contain an 'n' character. This is a bit hacky, but it should always work.
#'
#' @param fname The file name to read.
#' @param verbose Display warning message for chr23.
#'
#' @return A \code{data.frame} with the columns chr, pos, analysis, lod and exlod.
#' @export
#'
#' @examples read_merlin_npartbl("merlin_13_famA-nonparametric.tbl")
read_merlin_npartbl <- function(fname, verbose = TRUE) {
  npartbl <- read.table(fname, header = TRUE, comment.char = "n",
                        sep = "\t", stringsAsFactors = FALSE)
  names(npartbl) <- tolower(names(npartbl))
  required_cols <- c("chr", "pos", "analysis", "lod")
  stopifnot(all(required_cols %in% names(npartbl)))
  if (nrow(npartbl) > 0) {
    chrom <- unique(npartbl$chr)
    stopifnot(length(chrom) == 1, chrom %in% 1:23)
    # Check that all = pairs
    atab <- table(npartbl[["analysis"]])
    # if two counts, should be same
    stopifnot(length(atab) %in% c(1, 2),
              abs(max(atab) - min(atab)) < 0.001)
  } else {
    if (verbose) {
      message("'", fname, "'\n",
              "is empty. It is probably for chrX and MERLIN didn't output\n",
              "LOD scores for it since it was extremely unlikely to contain\n",
              "the disease-causing mutation under the selected genetic model.\n",
              "Returning a 0-row data.frame. This won't be shown in a plot.")
    }
  }
  if ("exlod" %in% names(npartbl)) {
    required_cols <- c(required_cols, "exlod")
  }
  npartbl <- npartbl[required_cols]
  npartbl

}

#' MERLIN \code{npartbl} Object
#'
#' Returns an S3 object of class \code{npartbl} which is basically
#' a compact version of MERLIN's \code{fam_nonparametric.tbl} files.
#'
#' MERLIN's \code{--tabulate} option is used to output convenient tables
#' summarising linkage analysis results in two files per chromosome:
#' \code{chrA_parametric.tbl} and \code{chrA_nonparametric.tbl}.
#'
#' The \code{nonparametric.tbl} file will be output if MERLIN is run with
#' at least one of the options \code{--npl} or \code{--pairs}.
#' These compute a LOD score using the Kong and Cox linear model, which is found
#' in the 'lod' column. The two options are
#' designated in the 'analysis' column of the file by character strings containing
#' 'ALL' and 'pairs', respectively.
#'
#' If the \code{--exp} option is also specified,
#' an additional LOD score is calculated using the
#' Kong and Cox exponential mode. This is found in the 'exlod' column.
#'
#' If the family consists of a single affected sibpair, the 'ALL' and 'pairs'
#' statistics will be identical. The 'pairs' statistic also considers sharing
#' within inbred individuals. For more information, see MERLIN's website:
#' \url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
#'
#' @param fname Character vector containing the \code{nonparametric.tbl} file name(s)
#' output by MERLIN.
#'
#' @return S3 object of class \code{npartbl}, which is
#' a list with:
#' \itemize{
#'   \item npartbl: data.frame with 4 or 5* columns
#'         (chr, pos, analysis, lod, exlod*)
#'   \item max_lods: data.frame with 3 columns
#'         (chr, max_lod, max_hlod)
#'   \item n_markers: data.frame with 2 columns
#'         (chr, n)
#'   \item merlin_options: character vector of length 3, indicating
#'   the options inferred to have been supplied to MERLIN for the nonparametric
#'   linkage analysis
#'   }
#'
#' @examples
#' npartbl_chr9 <- npartbl("chr9-nonparametric.tbl")
#' npartbl_chr24 <- npartbl(c("chr2-parametric.tbl", "chr4-nonparametric.tbl"))
#' npartbl_all <- npartbl(list.files(path = "merlin/", pattern = "-nonparametric.tbl"))
#'
#' @importFrom dplyr %>%
#'
#' @export
npartbl <- function(fname) {
  stopifnot(!missing(fname), is.character(fname))
  DF_list <- lapply(fname, read_merlin_npartbl)
  npartbl <- dplyr::bind_rows(DF_list) %>%
    dplyr::arrange_(~chr, ~analysis)


  # Guess which MERLIN options were specified
  merlin_options <- c(npl = FALSE, pairs = FALSE, exp = FALSE)
  if ("exlod" %in% names(npartbl)) {
    merlin_options["exp"] <- TRUE
  }

  atab <- table(npartbl[["analysis"]])
  npl_atab <- grep("all", names(atab), ignore.case = TRUE)
  pairs_atab <- grep("pairs", names(atab), ignore.case = TRUE)
  if (length(npl_atab) == 1 && length(pairs_atab) == 1 && npl_atab != pairs_atab) {
    merlin_options["npl"] <- merlin_options["pairs"] <- TRUE
  } else if (length(npl_atab) == 1 && length(pairs_atab) == 0 && length(atab) == 1) {
    merlin_options["npl"] <- TRUE
  } else if (length(npl_atab) == 0 && length(pairs_atab) == 1 && length(atab) == 1) {
    merlin_options["pairs"] <- TRUE
  } else {
    stop("You haven't used MERLIN with the --npl or --pairs option. ",
               "This file was probably not generated by MERLIN. Oops.")
  }

  npl_ind <- grep("all", npartbl$analysis, ignore.case = TRUE)
  pairs_ind <- grep("pairs", npartbl$analysis, ignore.case = TRUE)
  npartbl$analysis[npl_ind] <- "all"
  npartbl$analysis[pairs_ind] <- "pairs"

  # At this stage we have a data.frame with chr-pos-analysis-lod-exlod
  max_lods <- npartbl %>%
    dplyr::group_by_(~chr, ~analysis) %>%
    dplyr::summarise_(max_lod = ~max(lod),
                      max_exlod = ~max(exlod))

  n_markers <- npartbl %>%
    dplyr::group_by_(~chr, ~analysis) %>%
    dplyr::tally()


  structure(list(npartbl = npartbl,
                 n_markers = n_markers,
                 max_lods = max_lods,
                 merlin_options = merlin_options),
            class = "npartbl")
}


#' Print Head, Middle and Tail of \code{npartbl} Table
#'
#' Prints the head, middle and tail of a \code{npartbl} table.
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
               "Use n lower than 100."))
  }
  tbl <- npartbl$npartbl
  print(head(tbl, n = n))
  cat("--------\n")
  print(tail(tbl, n = n))
}

#' Return Summary of \code{npartbl} Object
#'
#' @param npartbl An object of class \code{npartbl}.
#'
#' @return A list (of class \code{summary.npartbl}) containing summary
#' information about the \code{npartbl} object.
#'
#' @export
summary.npartbl <- function(npartbl) {
  stopifnot(inherits(npartbl, "npartbl"))
  structure(list(n_markers = npartbl$n_markers,
                 merlin_options = npartbl$merlin_options,
                 max_lods = npartbl$max_lods),
            class = "summary.npartbl")
}

#' Print Summary of \code{npartbl} Object
#'
#' @param npartbl_summary An object of class \code{summary.npartbl}
#'
#' @method print summary.npartbl
#' @export
print.summary.npartbl <- function(npartbl_summary) {
  stopifnot(inherits(npartbl_summary, "summary.npartbl"))
  cat("MERLIN nonparametric.tbl\n",
      "--------------------------\n",
      "Number of markers per chromosome:\n", sep = "")
  print(as.data.frame(npartbl_summary$n_markers))
  cat("MERLIN options used:\n", sep = "")
  print(npartbl_summary$merlin_options)
  cat("Maximum LOD scores per chromosome:\n", sep = "")
  print(as.data.frame(npartbl_summary$max_lods))

}
