#' Read MERLIN \code{parametric.tbl}
#'
#' Read the \code{parametric.tbl} file output by MERLIN.
#'
#' @param fname The file name to read.
#' @param verbose Display warning message for chr23.
#'
#' @return A \code{data.frame} with the columns chr, pos, model, lod, alpha, hlod.
#'
#' @examples read_merlin_partbl("merlin_chr13-parametric.tbl")
#'
#' @export
read_merlin_partbl <- function(fname, verbose = TRUE) {
  partbl <- read.table(fname, header = TRUE, comment.char = "",
                       stringsAsFactors = FALSE)
  names(partbl) <- tolower(names(partbl))
  required_cols <- c("chr", "pos", "label", "model", "lod", "alpha", "hlod")
  stopifnot(all(required_cols == names(partbl)))
  if (nrow(partbl) > 0) {
    chrom <- unique(partbl$chr)
    stopifnot(length(chrom) == 1, chrom %in% c(1:23, 999))
    if (chrom == 999) {
      partbl$chr[partbl$chr == 999] <- 23
    }
    gen_model <- unique(partbl$model)
    if (length(gen_model) != 1) {
      stop("You have probably specified more than one model in MERLIN's ",
           "--model input file. Oops.")
    }
    # See if 100*pos and label are the same
    # If so, rename label to pos and remove
    x <- (100 * partbl$pos) - partbl$label
    stopifnot(all(x < 0.0001))
  } else {
    if (verbose) {
      message("'", fname, "'\n",
              "is empty. It is probably for chrX and MERLIN didn't output\n",
              "LOD scores for it since it was extremely unlikely to contain\n",
              "the disease-causing mutation under the selected genetic model.\n",
              "Returning a 0-row data.frame. This won't be shown in a plot.")
    }
  }
  partbl$pos <- partbl$label
  partbl$label <- NULL
  return(partbl)
}

#' MERLIN \code{partbl} Object
#'
#' Returns an S3 object of class \code{partbl} which is basically
#' a compact version of MERLIN's \code{fam_parametric.tbl} files.
#'
#' MERLIN's \code{--tabulate} option is used to output convenient tables
#' summarising linkage analysis results in two files per chromosome:
#' \code{chrA_parametric.tbl} and \code{chrA_nonparametric.tbl}.
#'
#' The \code{parametric.tbl} file will be output if MERLIN is run with
#' the option \code{--model genetic_model.txt}.
#' This uses the genetic model specified in the input file to calculate three
#' statistics: a multipoint parametric LOD score ('lod' column),
#' an estimate of the proportion of linked families at a given locus
#' ('alpha' column), and the corresponding maximum heterogeneity LOD
#' score ('hlod' column).
#' For more information, see MERLIN's website:
#' \url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
#'
#' @param fname Character vector containing the \code{parametric.tbl} file name(s)
#' output by MERLIN.
#'
#' @return S3 object of class \code{partbl}, which is a list with:
#'         \itemize{
#'           \item partbl: data.frame with 6 columns
#'                (chr, pos, model, lod, alpha, hlod)
#'           \item max_lods: data.frame with 3 columns
#'                (chr, max_lod, max_hlod)
#'           \item n_markers: data.frame with 2 columns
#'                (chr, n)
#'                }
#'
#' @examples
#' partbl_chr9 <- partbl("chr9-parametric.tbl")
#' partbl_chr24 <- partbl(c("chr2-parametric.tbl", "chr4-parametric.tbl"))
#' partbl_all <- partbl(list.files(path = "merlin/", pattern = "-parametric.tbl"))
#'
#' @importFrom dplyr %>%
#'
#' @export
partbl <- function(fname) {
  stopifnot(!missing(fname), is.character(fname))
  DF_list <- lapply(fname, read_merlin_partbl)
  partbl <- dplyr::bind_rows(DF_list) %>%
    dplyr::arrange_(~chr)

  # At this stage we have a data.frame with chr-pos-model-lod-alpha-hlod
  max_lods <- partbl %>%
    dplyr::group_by_(~chr) %>%
    dplyr::summarise_(max_lod = ~max(lod),
                      max_hlod = ~max(hlod))

  n_markers <- partbl %>% dplyr::count_(~ chr)

  structure(list(partbl = partbl,
                 n_markers = n_markers,
                 max_lods = max_lods),
            class = "partbl")
}


#' Print Head and Tail of \code{partbl} Table
#'
#' Prints the head and tail of a \code{partbl} table.
#'
#' @param partbl An object of class \code{partbl}.
#' @param n Number of rows to show in the head and tail
#'
#' @export
print.partbl <- function(partbl, n = 6L) {
  stopifnot(inherits(partbl, "partbl"))
  stopifnot(length(n) == 1L, is.numeric(n), n > 0)
  if (n > 100L) {
    stop(paste("Giving an n of", n, "will clutter your screen.",
               "Use n lower than 100."))
  }
  tbl <- partbl$partbl
  print(head(tbl, n = n))
  cat("--------\n")
  print(tail(tbl))
}

#' Return Summary of \code{partbl} Object
#'
#' @param partbl An object of class \code{partbl}.
#'
#' @return A list (of class \code{summary.partbl}) containing summary
#' information about the \code{partbl} object.
#'
#' @export
summary.partbl <- function(partbl) {
  stopifnot(inherits(partbl, "partbl"))
  structure(list(n_markers = partbl$n_markers,
                 gen_model = unique(partbl$partbl$model),
                 max_lods = partbl$max_lods),
            class = "summary.partbl")
}

#' Print Summary of \code{partbl} Object
#'
#' @param partbl_summary An object of class \code{summary.partbl}
#'
#' @method print summary.partbl
#' @export
print.summary.partbl <- function(partbl_summary) {
  stopifnot(inherits(partbl_summary, "summary.partbl"))
  cat("MERLIN parametric.tbl\n",
      "--------------------------\n",
      "Number of markers per chromosome:\n", sep = "")
  print(as.data.frame(partbl_summary$n_markers))
  cat("MERLIN genetic model used: ", partbl_summary$gen_model, "\n", sep = "")
  cat("Maximum LOD scores per chromosome:\n", sep = "")
  print(as.data.frame(partbl_summary$max_lods))
}
