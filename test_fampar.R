rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
methods(class = "fampar")

# Single
fampar <- linkrvis::fampar(file.path(data_path, "merlin_10_c10orf_extended.par"))
print(fampar)
summary(fampar)
plot(fampar)

# Multi
fnames <- list.files(data_path, pattern = "par$", full.names = TRUE)
fampar <- linkrvis::fampar(fnames)
print(fampar)
summary(fampar)
plot(fampar)
