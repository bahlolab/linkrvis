rm(list = ls())
# data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
data_path <- "~/Desktop/Barwon/analysis/linkr/femepl_merlin"
methods(class = "famlod")

# Single
famlod <- linkrvis::famlod(file.path(data_path, "merlin_3_barwon_asd46.lod"))
print(famlod)
summary(famlod)
plot(famlod) +
  geom_vline(xintercept = 106.74, linetype="dotted")

# Multi
fnames <- list.files(data_path, pattern = "lod$", full.names = TRUE)
famlod <- linkrvis::famlod(fnames)
print(famlod)
plot(famlod) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())
