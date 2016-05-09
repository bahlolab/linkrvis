rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
methods(class = "famlod")

# Single
famlod <- linkrvis::famlod(file.path(data_path, "merlin_10_c10orf_extended.lod"))
print(famlod)
summary(famlod)
plot(famlod)

# Multi
fnames <- list.files(data_path, pattern = "lod$", full.names = TRUE)
famlod <- linkrvis::famlod(fnames)
print(famlod)
summary(famlod)
plot(famlod)
