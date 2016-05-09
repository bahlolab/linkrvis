#' Read MERLIN \code{fam.par}
#'
#' Read the \code{.par} file output by MERLIN.
#'
#' @param fname The file name to read.
#' @param chrom The chromosome number for this file. This is the number in
#'        the filename following "merlin_" e.g. for "merlin_13_famA.par" it will
#'        be 13.
#' @param verbose
#'
#' @return A \code{data.frame} with the columns
#' @export
#'
#' @examples read_merlin_fampar("merlin_13_famA.par")
read_merlin_fampar <- function(fname,
                               chrom = as.numeric(sub(".*merlin_(\\d{1,2})_.*", "\\1", fname)),
                               verbose = TRUE) {
  stopifnot(chrom %in% 1:23, length(chrom) == 1)
  fampar <- read.table(fname, header = TRUE, stringsAsFactors = FALSE,
                       row.names = NULL, sep = "")
  names(fampar) <- tolower(names(fampar))
  stopifnot(all(c("model", "family", "position", "lod") %in% names(fampar)))

  if (nrow(fampar) > 1) {
    fampar <- data.frame(chr = chrom, fampar, stringsAsFactors = FALSE)

  } else {
    if (verbose) {
      message("'", fname, "'\n",
              "is empty. It is probably for chrX and MERLIN didn't output\n",
              "LOD scores for it since it was extremely unlikely to contain\n",
              "the disease-causing mutation under the selected genetic model.\n",
              "Returning a 0-row data.frame. This won't be shown in a plot.")
    }
    fampar <- data.frame(chr = numeric(0), fampar, stringsAsFactors = FALSE)
  }
  fampar

}

#' MERLIN \code{fampar} Object
#'
#' Returns an S3 object of class \code{fampar} which is basically
#' a compact version of MERLIN's \code{fam.par} files.
#'
#' MERLIN's \code{--perFamily} option is used to output LOD scores for each family
#' based on if parametric and/or nonparametric analyses have been specified.
#'
#' The parametric analysis is specified using the option
#' \code{--model genetic_model.txt}.
#' For more information, see MERLIN's website:
#' \url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
#'
#' @param fname Character vector containing the \code{fam.par} file name(s)
#' output by MERLIN.
#'
#' @return S3 object of class \code{fampar}, which is a list with:
#'         \itemize{
#'           \item fampar: data.frame with 5 columns
#'                (chr, family, position, lod, model)
#'           \item max_lods: data.frame with 3 columns
#'                (chr, family, max_lod)
#'           \item n_markers: data.frame with 2 columns
#'                (chr, n)
#'                }
#'
#' @examples
#' fampar_chr9 <- fampar("merlin_9_famA.par")
#' fampar_chr24 <- fampar(c("merlin_2_famA.par", "merlin_4_famA.par"))
#' fampar_all <- fampar(list.files(path = "merlin/", pattern = "par$"))
#'
#' @importFrom dplyr %>%
#'
#' @export
fampar <- function(fname) {
  stopifnot(!missing(fname), is.character(fname))
  DF_list <- lapply(fname, read_merlin_fampar)
  fampar <- dplyr::bind_rows(DF_list) %>%
    dplyr::arrange_(~chr, ~family)

  # At this stage we have a data.frame with chr-model-family-position-lod
  max_lods <- fampar %>%
    dplyr::group_by_(~chr, ~family) %>%
    dplyr::summarise_(max_lod = ~max(lod))

  n_markers <- fampar %>%
    dplyr::group_by_(~chr, ~family) %>%
    dplyr::tally()

  structure(list(fampar = fampar,
                 n_markers = n_markers,
                 max_lods = max_lods),
            class = "fampar")
}

#' Print Head and Tail of \code{fampar} Table
#'
#' Prints the head and tail of a \code{fampar} table.
#'
#' @param fampar An object of class \code{fampar}.
#' @param n Number of rows to show in the head and tail
#'
#' @export
print.fampar <- function(fampar, n = 6L) {
  stopifnot(inherits(fampar, "fampar"))
  stopifnot(length(n) == 1L, is.numeric(n), n > 0)
  if (n > 100L) {
    stop(paste("Giving an n of", n, "will clutter your screen.",
               "Use n lower than 100."))
  }
  tbl <- fampar$fampar
  print(head(tbl, n = n))
  cat("--------\n")
  print(tail(tbl))
}

#' Return Summary of \code{fampar} Object
#'
#' @param fampar An object of class \code{fampar}.
#'
#' @return A list (of class \code{summary.fampar}) containing summary
#' information about the \code{fampar} object.
#'
#' @export
summary.fampar <- function(fampar) {
  stopifnot(inherits(fampar, "fampar"))
  structure(list(n_markers = fampar$n_markers,
                 families = unique(fampar$fampar$family),
                 max_lods = fampar$max_lods),
            class = "summary.fampar")
}

#' Print Summary of \code{fampar} Object
#'
#' @param fampar_summary An object of class \code{summary.fampar}
#'
#' @method print summary.fampar
#' @export
print.summary.fampar <- function(fampar_summary) {
  stopifnot(inherits(fampar_summary, "summary.fampar"))
  cat("MERLIN fam.par\n",
      "--------------------------\n", sep = "")
  cat("Families included in the pedigree:\n", sep = "")
  print(fampar_summary$families)
  cat("Number of markers per chromosome:\n", sep = "")
  print(as.data.frame(fampar_summary$n_markers))
  cat("Maximum LOD scores per chromosome per family:\n", sep = "")
  print(as.data.frame(fampar_summary$max_lods))
}