rm(list = ls())
# data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
data_path <- "~/Desktop/Barwon/analysis/linkr/femepl_merlin"
list.files(data_path)
methods(class = "partbl")

# Single
par <- linkrvis::partbl(file.path(data_path, "merlin_3_barwon_asd46-parametric.tbl"))
print(par)
summary(par)
plot(par, c("lod", "hlod"))+
  geom_vline(xintercept = 106.74, linetype="dotted")
plot(par, c("alpha"), c(0, 1))


# Multi
par_fnames <- list.files(data_path, pattern = "-parametric.tbl", full.names = TRUE)
par <- linkrvis::partbl(par_fnames)
print(par)
summary(par)
plot(par, c("lod", "hlod")) +
  theme(axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank())
plot(par, c("alpha"), c(0, 1))