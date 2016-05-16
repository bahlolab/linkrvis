rm(list = ls())
# data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
data_path <- "~/Desktop/Barwon/analysis/linkr/femepl_merlin"
methods(class = "npartbl")

# Single
npar <- linkrvis::npartbl(file.path(data_path, "merlin_3_barwon_asd46-nonparametric.tbl"))
print(npar)
summary(npar)
plot(npar) +
  geom_vline(xintercept = 106.74, linetype="dotted")

# Multi
npar_fnames <- list.files(data_path, pattern = "-nonparametric.tbl", full.names = TRUE)
npar <- linkrvis::npartbl(npar_fnames)
print(npar)
summary(npar)
plot(npar) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank())

