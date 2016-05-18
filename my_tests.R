rm(list = ls())
require(linkrvis)
methods(class = "partbl")
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
head(list.files(data_path))

# single
par <- partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
famlod <- famlod(file.path(data_path, "merlin_10_c10orf_extended.lod"))
fampar <- fampar(file.path(data_path, "merlin_10_c10orf_extended.par"))
print(fampar)
summary(fampar)
plot(fampar)
x <- linkrvis::get_peaks(fampar, poscol = "position")

# multi
npar_fnames <- list.files(data_path, pattern = ".par$", full.names = TRUE)
fampar <- linkrvis::fampar(npar_fnames)
plot(fampar)
x <- linkrvis::get_peaks(fampar, poscol = "position")


