#' MERLIN \code{partbl} Object
#'
#' Returns an S3 object of class \code{partbl} which is basically
#' a cleaner version of MERLIN's \code{fam_parametric.tbl} file.
#'
#' MERLIN's \code{--tabulate} option is used to output convenient tables
#' summarising linkage analysis results in two files:
#' \code{fam_parametric.tbl} and \code{fam_nonparametric.tbl}.
#'
#' The \code{fam_parametric.tbl} file will be output if MERLIN is run with
#' the option \code{--model genetic_model.txt}.
#' This uses the genetic model specified in the input file to calculate three
#' statistics: a multipoint parametric LOD score ('LOD' column),
#' an estimate of the proportion of linked families at a given locus
#' ('ALPHA' column), and the corresponding maximum heterogeneity LOD
#' score ('HLOD' column).
#' For more information, see MERLIN's website:
#' \url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
#'
#' @param partbl The name of a \code{fam_parametric.tbl}
#' file output by MERLIN
#'
#' @return S3 object of class \code{partbl}, which is
#' a list with:
#' \itemize{
#'   \item partbl: data.frame with 6 columns (chr, pos, model, lod, alpha, hlod)
#'   \item chrom: character vector of length 1 giving chromosome number
#'   \item n_markers: character vector of length 1 giving number of markers used
#'   \item gen_model: character vector of length 1 giving model specified
#'   \item pos_range: numeric vector of length 2 giving range of pos column
#'   }
#'
#' @examples
#' partbl_obj <- partbl("merlin_10_famA-parametric.tbl")
#'
#' @export
partbl <- function(partbl) {

  stopifnot(!missing(partbl))
  if (is.character(partbl) && length(partbl) == 1) {
    partbl <- read_merlin_partbl(partbl)
  } else {
    stop("You need to give the full path to the parametric.tbl file.")
  }
  names(partbl) <- tolower(names(partbl))
  required_cols <- c("chr", "pos", "label", "model", "lod", "alpha", "hlod")
  stopifnot(all(required_cols %in% names(partbl)))
  chrom <- unique(partbl$chr)
  stopifnot(length(chrom) == 1, chrom %in% 1:23)
  gen_model <- unique(partbl$model)
  if (length(gen_model) != 1) {
    stop("You have probably specified more than one model in MERLIN's ",
         "--model input file. Oops.")
  }
  # See if 100*pos and label are the same
  # If so, rename label to pos and remove
  x <- (100 * partbl$pos) - partbl$label
  stopifnot(all(x < 0.0001))
  partbl$pos <- partbl$label
  partbl$label <- NULL

  # Get pos limits for plotting
  pos_range <- setNames(range(partbl$pos), c("min_pos", "max_pos"))

  structure(list(partbl = partbl,
                 chrom = chrom,
                 n_markers = nrow(partbl),
                 gen_model = gen_model,
                 pos_range = pos_range),
            class = "partbl")
}

#' Get \code{partbl} Table
#'
#' @param partbl An object of class \code{partbl}.
#'
#' @return A data.frame with the chromosome, position, model,
#' lod, alpha and hlod columns from a \code{partbl} object.
#'
#' @examples
#' get_partbl(partbl("fam_parametric.tbl"))
#'
#' @export
get_partbl <- function(partbl) {
  stopifnot(inherits(partbl, "partbl"))
  partbl$partbl
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
  tbl <- get_partbl(partbl)
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
  structure(list(chrom = partbl$chrom,
                 n_markers = partbl$n_markers,
                 gen_model = partbl$gen_model,
                 pos_range = partbl$pos_range),
            class = "summary.partbl")
}

#' Print Summary of \code{partbl} Object
#'
#' @param partbl An object of class \code{summary.partbl}
#'
#' @method print summary.partbl
#' @export
print.summary.partbl <- function(partbl) {
  stopifnot(inherits(partbl, "summary.partbl"))
  cat("MERLIN parametric.tbl\n",
      "--------------------------\n",
      "Chromosome number: ", partbl$chrom, "\n",
      "Chromosome range: ",
      partbl$pos_range["min_pos"], "cM - ", partbl$pos_range["max_pos"], "cM\n",
      "Number of markers: ", partbl$n_markers, "\n",
      "MERLIN genetic model used: ", partbl$gen_model, "\n", sep = "")
}

