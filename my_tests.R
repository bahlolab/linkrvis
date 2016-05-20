rm(list = ls())
require(linkrvis)
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

# single
par <- partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
famlod <- famlod(file.path(data_path, "merlin_10_c10orf_extended.lod"))
fampar <- fampar(file.path(data_path, "merlin_10_c10orf_extended.par"))
par_peaks <- linkrvis::get_peaks(par)
npar_peaks <- linkrvis::get_peaks(npar)
famlod_peaks <- linkrvis::get_peaks(famlod)
fampar_peaks <- linkrvis::get_peaks(fampar)

# multi
par_fnames <- list.files(data_path, pattern = "-parametric\\.tbl$", full.names = TRUE)
npar_fnames <- list.files(data_path, pattern = "-nonparametric\\.tbl$", full.names = TRUE)
famlod_fnames <- list.files(data_path, pattern = ".lod$", full.names = TRUE)
fampar_fnames <- list.files(data_path, pattern = ".par$", full.names = TRUE)
par <- partbl(par_fnames)
npar <- npartbl(npar_fnames)
famlod <- famlod(famlod_fnames)
fampar <- fampar(fampar_fnames)
par_peaks <- linkrvis::get_peaks(par)
npar_peaks <- linkrvis::get_peaks(npar)
famlod_peaks <- linkrvis::get_peaks(famlod)
fampar_peaks <- linkrvis::get_peaks(fampar)


