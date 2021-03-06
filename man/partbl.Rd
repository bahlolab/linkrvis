% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partbl.R
\name{partbl}
\alias{partbl}
\title{MERLIN \code{partbl} Object}
\usage{
partbl(fname)
}
\arguments{
\item{fname}{Character vector containing the \code{parametric.tbl} file name(s)
output by MERLIN.}
}
\value{
S3 object of class \code{partbl}, which is a list with:
        \itemize{
          \item partbl: data.frame with 6 columns
               (chr, pos, model, lod, alpha, hlod)
          \item max_lods: data.frame with 3 columns
               (chr, max_lod, max_hlod)
          \item n_markers: data.frame with 2 columns
               (chr, n)
               }
}
\description{
Returns an S3 object of class \code{partbl} which is basically
a compact version of MERLIN's \code{fam_parametric.tbl} files.
}
\details{
MERLIN's \code{--tabulate} option is used to output convenient tables
summarising linkage analysis results in two files per chromosome:
\code{chrA_parametric.tbl} and \code{chrA_nonparametric.tbl}.

The \code{parametric.tbl} file will be output if MERLIN is run with
the option \code{--model genetic_model.txt}.
This uses the genetic model specified in the input file to calculate three
statistics: a multipoint parametric LOD score ('lod' column),
an estimate of the proportion of linked families at a given locus
('alpha' column), and the corresponding maximum heterogeneity LOD
score ('hlod' column).
For more information, see MERLIN's website:
\url{http://csg.sph.umich.edu/abecasis/merlin/index.html}
}
\examples{
partbl_chr9 <- partbl("chr9-parametric.tbl")
partbl_chr24 <- partbl(c("chr2-parametric.tbl", "chr4-parametric.tbl"))
partbl_all <- partbl(list.files(path = "merlin/", pattern = "-parametric.tbl"))

}

