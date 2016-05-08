rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
methods(class = "npartbl")

# Single
npar <- linkrvis::npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
print(npar)
summary(npar)
plot(npar)

# Multi
npar_fnames <- list.files(data_path, pattern = "-nonparametric.tbl", full.names = TRUE)
npar <- linkrvis::npartbl(npar_fnames)
print(npar)
summary(npar)
plot(npar)

vars <- c("lod", "exlod")

