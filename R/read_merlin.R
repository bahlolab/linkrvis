#' Read MERLIN \code{fam_parametric.tbl}
#'
#' Read the \code{fam_parametric.tbl} file output by MERLIN.
#'
#' @param fname The file name to read.
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples read_partbl("merlin_13_famA-parametric.tbl")
read_partbl <- function(fname) {
  read.table(fname, header = TRUE, stringsAsFactors = FALSE, comment.char = "")
}

#' Read MERLIN \code{fam_parametric.tbl}
#'
#' Read the \code{fam_nonparametric.tbl} file output by MERLIN
#'
#' The \code{fam_nonparametric.tbl} file contains two rows at the
#' beginning of each \code{ALL} and/or \code{Pairs} subtable, which
#' indicate the maximum possible scores for the specific dataset.
#' These rows are ignored by using the comment.char = "n"
#' option in \code{\link{read.table}}, which works since only these rows
#' contain an 'n' character. This is a bit hacky, but it should always work.
#'
#' @param fname The file name to read.
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples read_npartbl("merlin_13_famA-nonparametric.tbl")
read_npartbl <- function(fname) {
  read.table(fname, header = TRUE, stringsAsFactors = FALSE,
             comment.char = "n", sep = "\t")
}
