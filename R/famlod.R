#' Read MERLIN \code{fam.lod}
#'
#' Read the \code{.lod} file output by MERLIN.
#'
#' @param fname The file name to read.
#' @param chrom The chromosome number for this file. This is the number in
#'        the filename following "merlin_" e.g. for "merlin_13_famA.lod" it will
#'        be 13.
#' @param verbose
#'
#' @return A \code{data.frame} with the columns
#'
#' @examples read_merlin_famlod("merlin_13_famA.lod")
#'
#' @export
read_merlin_famlod <- function(fname,
                               chrom = as.numeric(sub(".*merlin_(\\d{1,2})_.*", "\\1", fname)),
                               verbose = TRUE) {
  stopifnot(chrom %in% 1:23, length(chrom) == 1)
  famlod <- read.table(fname, header = FALSE, stringsAsFactors = FALSE,
                     row.names = NULL, sep = "", skip = 1,
                     col.names = c( "family", "tmp", "analysis", "pos",
                                    "zscore", "plod", "delta", "lod"))

  if (nrow(famlod) > 1) {
    famlod <- data.frame(chr = chrom, famlod, stringsAsFactors = FALSE)

  } else {
    if (verbose) {
      message("'", fname, "'\n",
              "is empty. It is probably for chrX and MERLIN didn't output\n",
              "LOD scores for it since it was extremely unlikely to contain\n",
              "the disease-causing mutation under the selected genetic model.\n",
              "Returning a 0-row data.frame. This won't be shown in a plot.")
    }
    famlod <- data.frame(chr = numeric(0), famlod, stringsAsFactors = FALSE)
  }
  famlod <- famlod[c("chr", "pos", "family", "analysis", "lod", "plod")]
  famlod$analysis[famlod$analysis == "[ALL]"] <- "all"
  famlod$analysis[famlod$analysis == "[Pairs]"] <- "pairs"
  famlod

}

#' MERLIN \code{famlod} Object
#'
#' Returns an S3 object of class \code{famlod} which is basically
#' a compact version of MERLIN's \code{fam.lod} files.
#'
#' MERLIN's \code{--perFamily} option is used to output LOD scores for each family
#' based on if parametric and/or nonparametric analyses have been specified.
#'
#' The nonparametric analysis is specified using
#' at least one of the options \code{--npl} or \code{--pairs}.
#' These compute LOD scores using the Kong and Cox linear model
#' designated in the 'analysis' column of the file by character strings containing
#' 'ALL' and 'pairs', respectively.
#' The 'plod' score is calculated using the best fitting overall model. The 'lod'
#' score is maximized within each family (LOD)
#'
#' For more information, see MERLIN's website:
#' \url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
#'
#' @param fname Character vector containing the \code{fam.par} file name(s)
#' output by MERLIN.
#'
#' @return S3 object of class \code{famlod}, which is a list with:
#'         \itemize{
#'           \item famlod: data.frame with 5 columns
#'                (chr, pos, family, analysis, lod, plod)
#'           \item max_lods: data.frame with 3 columns
#'                (chr, family, max_lod, max_plod)
#'           \item n_markers: data.frame with 2 columns
#'                (chr, n)
#'                }
#'
#' @examples
#' famlod_chr9 <- famlod("merlin_9_famA.lod")
#' famlod_chr24 <- famlod(c("merlin_2_famA.lod", "merlin_4_famA.lod"))
#' famlod_all <- famlod(list.files(path = "merlin/", pattern = "lod$"))
#'
#' @importFrom dplyr %>%
#'
#' @export
famlod <- function(fname) {
  stopifnot(!missing(fname), is.character(fname))
  DF_list <- lapply(fname, read_merlin_famlod)
  famlod <- dplyr::bind_rows(DF_list) %>%
    dplyr::arrange_(~chr, ~family)

  # At this stage we have a data.frame with chr-model-family-position-lod
  max_lods <- famlod %>%
    dplyr::group_by_(~chr, ~family, ~analysis) %>%
    dplyr::summarise_(max_lod = ~max(lod),
                      max_plod = ~max(plod))

  n_markers <- famlod %>%
    dplyr::group_by_(~chr, ~family, ~analysis) %>%
    dplyr::tally()

  structure(list(famlod = famlod,
                 n_markers = n_markers,
                 max_lods = max_lods),
            class = "famlod")
}

#' Print Head and Tail of \code{famlod} Table
#'
#' Prints the head and tail of a \code{famlod} table.
#'
#' @param famlod An object of class \code{famlod}.
#' @param n Number of rows to show in the head and tail
#'
#' @export
print.famlod <- function(famlod, n = 6L) {
  stopifnot(inherits(famlod, "famlod"))
  stopifnot(length(n) == 1L, is.numeric(n), n > 0)
  if (n > 100L) {
    stop(paste("Giving an n of", n, "will clutter your screen.",
               "Use n lower than 100."))
  }
  tbl <- famlod$famlod
  print(head(tbl, n = n))
  cat("--------\n")
  print(tail(tbl))
}

#' Return Summary of \code{famlod} Object
#'
#' @param famlod An object of class \code{famlod}.
#'
#' @return A list (of class \code{summary.famlod}) containing summary
#' information about the \code{famlod} object.
#'
#' @export
summary.famlod <- function(famlod) {
  stopifnot(inherits(famlod, "famlod"))
  structure(list(n_markers = famlod$n_markers,
                 families = unique(famlod$famlod$family),
                 analyses = unique(famlod$famlod$analysis),
                 max_lods = famlod$max_lods),
            class = "summary.famlod")
}

#' Print Summary of \code{famlod} Object
#'
#' @param famlod_summary An object of class \code{summary.famlod}
#'
#' @method print summary.famlod
#' @export
print.summary.famlod <- function(famlod_summary) {
  stopifnot(inherits(famlod_summary, "summary.famlod"))
  cat("MERLIN fam.lod\n",
      "--------------------------\n", sep = "")
  cat("Families included in the pedigree:\n", sep = "")
  print(famlod_summary$families)
  cat("Nonparametric analyses performed:\n", sep = "")
  print(famlod_summary$analyses)
  cat("Number of markers per chromosome:\n", sep = "")
  print(as.data.frame(famlod_summary$n_markers))
  cat("Maximum LOD scores per chromosome per family:\n", sep = "")
  print(as.data.frame(famlod_summary$max_lods))
}