rm(list = ls())
# data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
data_path <- "~/Desktop/Barwon/analysis/linkr/femepl_merlin"
methods(class = "fampar")

# Single
fampar <- linkrvis::fampar(file.path(data_path, "merlin_3_barwon_asd46.par"))
print(fampar)
summary(fampar)
plot(fampar) +
  geom_vline(xintercept = 106.74, linetype="dotted")

# Multi
fnames <- list.files(data_path, pattern = "par$", full.names = TRUE)
fampar <- linkrvis::fampar(fnames)
print(fampar)
summary(fampar)
plot(fampar) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

