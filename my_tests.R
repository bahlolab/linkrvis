rm(list = ls())
require(linkrvis)
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
annot <- readRDS("~/Desktop/annotHapMap2U.rds")
# data_path <- "~/Desktop/Barwon/analysis/linkr/femepl_merlin"

# single
# par <- partbl(file.path(data_path, "merlin_3_barwon_asd46-parametric.tbl"))
par <- partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
famlod <- famlod(file.path(data_path, "merlin_10_c10orf_extended.lod"))
fampar <- fampar(file.path(data_path, "merlin_10_c10orf_extended.par"))
par_peaks <- get_peaks(par)
npar_peaks <- get_peaks(npar)
famlod_peaks <- get_peaks(famlod)
fampar_peaks <- get_peaks(fampar)
cm2bp(peaks = par_peaks, annot = annot)
cm2bp(peaks = npar_peaks, annot = annot)
cm2bp(peaks = famlod_peaks, annot = annot)
cm2bp(peaks = fampar_peaks, annot = annot)

# multi
par_fnames <- list.files(data_path, pattern = "-parametric\\.tbl$", full.names = TRUE)
npar_fnames <- list.files(data_path, pattern = "-nonparametric\\.tbl$", full.names = TRUE)
famlod_fnames <- list.files(data_path, pattern = ".lod$", full.names = TRUE)
fampar_fnames <- list.files(data_path, pattern = ".par$", full.names = TRUE)
par <- partbl(par_fnames)
npar <- npartbl(npar_fnames)
famlod <- famlod(famlod_fnames)
fampar <- fampar(fampar_fnames)
par_peaks <- get_peaks(par)
npar_peaks <- get_peaks(npar)
famlod_peaks <- get_peaks(famlod)
fampar_peaks <- get_peaks(fampar)

tmp1 <- cm2bp(peaks = par_peaks, annot = annot)
tmp2 <- cm2bp(peaks = npar_peaks, annot = annot)
tmp3 <- cm2bp(peaks = famlod_peaks, annot = annot)
tmp4 <- cm2bp(peaks = fampar_peaks, annot = annot)

