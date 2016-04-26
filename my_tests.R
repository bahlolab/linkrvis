## First try joint

data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))

tmp <- npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
str(npar)
str(par)
str(dat)
str(map)
str(ped)

npartbl <- function(npartbl) {
  stopifnot(!missing(npartbl))
  if (inherits(npartbl, "npartbl")) return(npartbl)
  if (is.character(npartbl) && length(npartbl) == 1) {
    npartbl <- read_merlin_npartbl(npartbl)
  } else {
    stop("You need to give a single file name. Aborting!")
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

# Given a character vector x:
# Check if it contains the strings
# a and b and if so,
# return a character vector with
# those strings accordingly.
match_vec <- function(x, a = "alpha", b = "beta") {
  stopifnot(is.character(x), is.character(a), is.character(b))
  stopifnot(length(a) == 1, length(b) == 1)
  stopifnot(length(x) %in% c(1, 2))
  ai <- grep(a, x)
  bi <- grep(b, x)
  if (length(x) == 1) {
    if (length(ai) == 1 && length(bi) == 0) {
      return(a)
    } else if (length(ai) == 0 && length(bi) == 1) {
      return(b)
    } else {
      stop(paste("Could not find", a, "xor", b, "in:", x))
    }
  }
  if (length(x) == 2) {
    if (length(ai) == 1 && length(bi) == 1) {
      stopifnot(ai != bi)
      return(c(a, b))
    } else {
      stop(paste("Could not find both", a, "and", b, "in:", x, collapse = "\n"))
    }
  }
}

match_vec(c("beta", "alpha"))
match_vec("alpha")
match_vec("betaaaa")
match_vec("bettaaaa")
match_vec("alphaandbeta")
match_vec(c("this has thealpha", "beta in here"))
